source("R/manifest.R")
source("R/shared-objects.R")
library(rvest)
library(fuzzyjoin)

rating_area_year <- as.Date(Sys.Date()) %>% lubridate::year() %>% as.character()

base_url <- "http://www.cms.gov/CCIIO/Programs-and-Initiatives/Health-Insurance-Market-Reforms/STATE-gra.html"
states_lc <- tolower(states)

urls_to_get <- 
  states_lc %>% map_chr(~gsub("STATE",.x,base_url))

# Note: VT (47) and WI (49) did't work so created a separate call for them. 

rating_areas_raw <- list() 

for (.x in urls_to_get) {
  cat(.x)
  
  if (!grepl("wi-gra|vt-gra",.x)) {
    rating_areas_raw[[.x]] <-   
      .x %>% 
      read_html() %>% 
      # LN edit to test whether the code would work without the line below
      #html_nodes(xpath = '//*[@id="main_content"]/div[1]/table') %>% 
      html_table(header = TRUE,fill=TRUE) %>% 
      purrr::pluck(1) %>% 
      janitor::clean_names() %>% 
      mutate(state = gsub("http://www.cms.gov/CCIIO/Programs-and-Initiatives/Health-Insurance-Market-Reforms/","",.x)) %>% 
      mutate(state = gsub("-gra.html","",state)) %>% 
      mutate(state = toupper(state)) %>% 
      mutate_all(funs(as.character)) %>% 
      tbl_df()
    
  } else {
    rating_areas_raw[[.x]] <-   
      .x %>% 
      read_html() %>% 
      # LN edit to test whether the code would work without the line below
      #html_nodes(xpath = '//*[@id="offices"]/div/div/div/table') %>% 
      html_table(header = TRUE,fill=TRUE) %>% 
      purrr::pluck(1) %>% 
      janitor::clean_names() %>% 
      mutate(state = gsub("http://www.cms.gov/CCIIO/Programs-and-Initiatives/Health-Insurance-Market-Reforms/","",.x)) %>% 
      mutate(state = gsub("-gra.html","",state)) %>% 
      mutate(state = toupper(state)) %>% 
      mutate_all(funs(as.character)) %>% 
      tbl_df()
  }
  cat("\n")
}

df_rating_areas_raw <- 
  rating_areas_raw %>% bind_rows() %>% 
  rename(county = county_name) #%>% 
#mutate(county = ifelse(state=="MD",gsub(" County","",county),county))

county_to_fips <-
  read.csv(here("public-data/county-fips-crosswalk.txt"),header=FALSE) %>% 
  set_names(c("state","state_fips","county_fips","county","fips_class")) %>% 
  mutate(
    state_fips = stringr::str_pad(state_fips, 2, pad = "0"),
    county_fips = stringr::str_pad(county_fips, 3, pad = "0")
  ) %>%
  mutate(county_fips = as.character(county_fips)) %>% tbl_df() %>%
  mutate(county = gsub(" County", "", county)) %>%
  mutate(fips_code = paste0(state_fips, county_fips)) %>%
  select(county, state, fips_code) %>% data.frame() %>% 
  #Oglala Lakota County, SD (FIPS code=46102). 
  #Effective May 1, 2015, Shannon County, SD (FIPS code=46113) 
  # was renamed Oglala Lakota County and assigned a new FIPS code
  mutate(fips_code = ifelse(county =="Shannon" & state == "SD", "46102",fips_code)) %>% 
  mutate(county = ifelse(county =="Shannon" & state == "SD", "Oglala Lakota",county)) 

write_rds(county_to_fips,here("output/geographic-crosswalks/01_xw_county-to-fips.rds"))

df_rating_areas_exact <- 
  df_rating_areas_raw %>% 
  filter(!is.na(county) & county!="") %>% 
  inner_join(county_to_fips,c("county","state")) %>% 
  select(starts_with("rating"),county,state,fips_code) %>% 
  mutate(merge_type = "Exact")

# Get the nonmatching ones
df_unmatched_rating_areas <-
  df_rating_areas_raw %>% 
  filter(!is.na(county) & county!="") %>% 
  anti_join(county_to_fips,c("county","state"))

# Use fuzzy matching to get even further
county_to_fips_nested <- 
  county_to_fips %>% 
  group_by(state) %>% 
  nest() %>% 
  rename(xw = data)

df_rating_areas_fuzzy1 <- 
  df_unmatched_rating_areas  %>% 
  group_by(state) %>% 
  nest() %>% 
  inner_join(county_to_fips_nested,"state") %>% 
  mutate(merged = map2(data,xw,~(
    .x %>% stringdist_inner_join(.y,c("county"),max_dist=1) %>% 
      select(starts_with("rating"),county=county.x, fips_code) %>% 
      mutate(merge_type = "Fuzzy"))
    )) %>% 
  select(state,merged) %>% 
  unnest()

still_unmatched_rating_areas <- 
  df_unmatched_rating_areas  %>% 
  stringdist_anti_join(county_to_fips,c("county","state"),max_dist = 1) 

df_rating_areas_MD <- 
  still_unmatched_rating_areas %>% 
  filter(state =="MD") %>% 
  mutate(county2 = ifelse(state == "MD", gsub(" County","",county), county)) %>% 
  stringdist_inner_join(county_to_fips %>% filter(state=="MD"),c("county2" = "county","state"),max_dist = 1) %>% 
  select(starts_with("rating"),county=county.x,state = state.x, fips_code) %>% 
  mutate(merge_type = "Maryland")

df_rating_areas_counties <- 
  df_rating_areas_exact %>% 
  bind_rows(df_rating_areas_fuzzy1) %>% 
  bind_rows(df_rating_areas_MD) %>% 
  rename(rating_area = rating_area_id_for_federal_systems)  %>% 
  mutate(rating_area = gsub("Rating Area ","",rating_area)) %>% 
  mutate(rating_area = paste0(state,str_pad(rating_area,width=2,pad="0")))
  

write_rds(df_rating_areas_counties,here(paste0("output/geographic-crosswalks/01_rating-areas_counties_",rating_area_year,".rds")))

df_rating_areas_zip <- 
  df_rating_areas_raw %>% 
  filter(!is.na(x3_digit_zip_code_if_applicable)) %>% 
  select(rating_area = rating_area_id_for_federal_systems, 
         zip_code = x3_digit_zip_code_if_applicable,
         state,) %>% 
  mutate(zip_code = as.numeric(paste0(zip_code))) %>% 
  filter(zip_code!="" & !is.na(zip_code)) %>% 
  mutate(zip_code = str_pad(paste0(zip_code),width=3,pad="0")) %>% 
  mutate(rating_area = gsub("Rating Area ","",rating_area)) %>% 
  mutate(rating_area = paste0(state,str_pad(rating_area,width=2,pad="0")))

write_rds(df_rating_areas_zip,here(paste0("output/geographic-crosswalks/01_rating-areas_zip3_",rating_area_year,".rds")))
