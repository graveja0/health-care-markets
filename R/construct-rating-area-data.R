#' ---
#' output: github_document
#' ---
#' 
#' 

# Note that this file was adapted from code sent to me from the New York Times 
# on 2017-07-03.

# The default geographic rating areas for each state will be the Metropolitan Statistical Areas 
# (MSAs) plus the remainder of the State that is not included in a MSA.States may seek approval 
# from HHS for a number of geographic rating areas that is greater than the number of MSAs in the 
# state plus one (MSAs+1), provided the rating areas are based on counties, three-digit zip codes, 
# or MSAs/non-MSAs. If a state requests geographic rating areas in excess of MSAs+1, then the state 
# must provide actuarial justification, and must demonstrate how they will reflect significant 
# differences in health care unit costs by rating area, lead to stability in rates over time, 
# apply uniformly to all health insurance issuers in a market, are based on one or more geographic 
# boundaries described previously, and will not be unfairly discriminatory.

# Source: https://www.cms.gov/CCIIO/Programs-and-Initiatives/Health-Insurance-Market-Reforms/STATE-gra.html


suppressWarnings(suppressMessages(source(here::here("/R/manifest.R"))))
source(here("R/move-ak-hi.R"))
source(here("R/get-geographic-info.R"))
source(here("R/map-theme.R"))
source(here("R/shared-objects.R"))
source(here("R/get-contiguous-areas.R"))

# The first step is to scrape the county names and 3-Digit ZIP codes associated with 
  # each rating area in each state. This is done from the CCIIO website. It only
  # needs to be done once, and does not appear to change year-on-year (though technically)
if (!file.exists(here("output/geographic-crosswalks/01_rating-areas_counties_2019.rds")) | 
    !file.exists(here("output/geographic-crosswalks/01_rating-areas_zip3_2019.rds"))) {
      source(here("R/construct-rating-area-file-from-cciio-website.R"))
}

df_rating_areas_counties <- 
  read_rds(here("output/geographic-crosswalks/01_rating-areas_counties_2019.rds")) %>% 
  data.frame() %>% 
  unique() 

# (Constructed in "R/construct-rating-area-file-from-cciio-website.R")
fips_to_state <- read_rds(here("output/geographic-crosswalks/01_xw_county-to-fips.rds")) %>% 
  mutate(statefp = str_sub(fips_code,1,2)) %>% 
  select(statefp,state) %>% unique()

ra_county_map <- read_sf(here('public-data/shape-files/county-2017/cb_2017_us_county_5m/cb_2017_us_county_5m.shp')) %>% 
  sf::st_transform(crs ="+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96") %>% 
  janitor::clean_names() %>% 
  left_join(fips_to_state,"statefp") %>% 
  filter(state %in% states) %>% 
  move_ak_hi(state = state) %>% 
  mutate(fips_code = geoid) %>% 
  inner_join(df_rating_areas_counties) %>% 
  group_by(rating_area) %>% 
  summarize() %>% 
  mutate(state = str_sub(rating_area,1,2))

df_rating_areas_zip3 <- 
  read_rds(here("output/geographic-crosswalks/01_rating-areas_zip3_2019.rds")) %>% 
  rename(zip3 = zip_code) %>% 
  select(-state) %>% 
  tbl_df() %>% 
  unique() 

zip3_map <- read_sf(here('public-data/shape-files/zip3-2013/zip3/zip3.shp')) %>% 
  sf::st_transform(crs ="+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96") %>% 
  janitor::clean_names() %>% 
  inner_join(df_rating_areas_zip3,"zip3") %>% 
  filter(state!="CA") %>% 
  group_by(rating_area) %>% 
  summarize() %>% 
  mutate(state = str_sub(rating_area,1,2))

la_zip_map <- read_sf(here('public-data/shape-files/zip3-2013/zip3/zip3.shp')) %>% 
  sf::st_transform(crs ="+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96") %>% 
  janitor::clean_names() %>% 
  inner_join(df_rating_areas_zip3,"zip3") %>% 
  filter(state=="CA")

la_county_map <- read_sf(here('public-data/shape-files/county-2017/cb_2017_us_county_5m/cb_2017_us_county_5m.shp')) %>% 
  sf::st_transform(crs ="+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96") %>% 
  janitor::clean_names() %>% 
  filter(geoid=="06037") %>% 
  st_intersection(la_zip_map) %>% 
  mutate(fips_code = geoid) %>% 
  group_by(rating_area) %>% 
  summarize() %>% 
  mutate(state = str_sub(rating_area,1,2))

rating_area_map <- 
  rbind(rbind(ra_county_map,la_county_map),zip3_map) %>% 
  st_simplify(dTolerance = 300) %>% 
  move_ak_hi(state = state) %>% 
  left_join(get_contiguous(shp = ., id = rating_area), "rating_area") #%>% 
  #rename(cz_id = commuting_zone_id_2010) %>% 
  #left_join(df_cz,"cz_id")

rating_area_map %>% 
  filter(state=="CA") %>% 
  ggplot() + geom_sf() + coord_sf(datum = NA) +
  remove_all_axes

rating_area_map %>% 
  sf::write_sf(here("output/tidy-mapping-files/rating-area/01_rating-area-shape-file.shp"))

shape_types %>% 
  map(~(
    put_object(
      file  = paste0("./output/tidy-mapping-files/rating-area/01_rating-area-shape-file.",.x),
      bucket = paste0(project_bucket, "/tidy-mapping-files/rating-area")
    )
  ))


