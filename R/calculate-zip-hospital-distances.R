#' ---
#' output: github_document
#' ---
#' 

zip_xy <- read_csv(here("public-data/zcta-to-fips-county/zcta-to-fips-county.csv")) %>% 
  janitor::clean_names() %>% 
  filter(row_number() !=1) %>% 
  mutate(fips_code = county) %>% 
  rename(zip_code = zcta5) %>% 
  arrange(zip_code,fips_code,afact) %>% 
  group_by(zip_code) %>% 
  filter(afact==max(afact)) %>% 
  ungroup() %>% 
  select(zip_code,zip_lon = intptlon, zip_lat = intptlat ) 

hosp_xy<- read_rds(here("output/market-comparisons/01_aha-markets-2017.rds"))  %>% 
  filter(!is.na(longitude) & !is.na(latitude)) %>% 
  select(prvnumgrp, hosp_lon = longitude, hosp_lat = latitude) %>% 
  group_by(prvnumgrp) %>% 
  filter(row_number()==1)
#  st_as_sf(coords=c("longitude","latitude"), crs=4326, agr="constant") 


# Load the hospital-zip service file constructed in "R/read-and-tidy-cms-hospital-service-areas.R")
tmp <- read_rds(here("output/hospital-county-patient-data/2017/hospital-zip-patient-data.rds")) %>% 
  arrange(prvnumgrp,desc(total_cases)) %>% 
  inner_join(zip_xy,"zip_code") %>% 
  inner_join(hosp_xy,"prvnumgrp") %>% 
  group_by(prvnumgrp) %>% 
  mutate_at(vars(zip_lat,zip_lon),as.numeric) %>% 
  nest() 

library(furrr)
plan(multiprocess)

df_distances <- 
  tmp %>% 
    #filter(prvnumgrp=="440039" | prvnumgrp=="440133") %>% 
    mutate(distance = 
      future_map(data,~(
        st_distance(
          .x %>% st_as_sf(coords = c("zip_lon","zip_lat"), crs=4326, agr="constant") ,
          .x %>% select(hosp_lon,hosp_lat) %>% unique() %>% st_as_sf(coords = c("hosp_lon","hosp_lat"), crs = 4326, agr="constant")
        ) %>% tbl_df() %>% 
          rename(distance=value) %>% 
          mutate(miles = distance / 1609.34) 
      ),.progress=TRUE)
    ) 

df_distances %>% 
  unnest() %>% 
  write_rds(here("output/market-comparisons/01_zip-hospital-distances.rds"))

s3saveRDS(df_distances,
          bucket = paste0(project_bucket,"/market-comparisons"), 
          object = "01_zip-hospital-distances.rds")

df_average_dist <- 
  df_distances %>% 
  unnest() %>% 
  group_by(prvnumgrp) %>% 
  summarise(average_miles = weighted.mean(miles,w = total_cases, na.rm=TRUE))

df_average_dist %>% write_rds(here("output/market-comparisons/01_zip-hospital-average-distances.rds"))
s3saveRDS(df_average_dist,
          bucket = paste0(project_bucket,"/market-comparisons"), 
          object = "/01_zip-hospital-average-distances.rds")

