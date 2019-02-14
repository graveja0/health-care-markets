#' ---
#' output: github_document
#' ---
#' 
#' The objective of this document is to construct a county to rating area crosswalk.
#' 

suppressWarnings(suppressMessages(source(here::here("/R/manifest.R"))))
source(here("R/map-theme.R"))
source(here("R/shared-objects.R"))
source(here("R/get-market-from-x-y-coordinates.R"))
source(here("R/estimate_hhi.R"))

# For most states the mapping is trivial
county_to_rating_area <- 
  read_rds(here("output/geographic-crosswalks/01_rating-areas_counties_2019.rds")) %>% 
  data.frame() %>% 
  unique() %>% 
  select(fips_code,rating_area) %>% 
  mutate(pct_area = 1)

zip3_to_rating_area <- 
  read_rds(here("output/geographic-crosswalks/01_rating-areas_zip3_2019.rds")) %>% 
  data.frame() %>% 
  unique() %>% 
  select(zip_code,rating_area)

sf_ra <- read_sf(here("output/tidy-mapping-files/rating-area/01_rating-area-shape-file.shp")) %>%
  st_transform(crs = 4326) %>% 
  mutate(rating_area = ratng_r) %>% 
  inner_join(zip3_to_rating_area,"rating_area")

sf_county <- read_sf(here("output/tidy-mapping-files/county/01_county-shape-file.shp")) %>% 
  st_transform(crs = 4326)

foo <- st_intersection(sf_ra,sf_county) %>% 
  mutate(fips_code = paste0(statefp,countyfp)) %>% 
  select(rating_area = ratng_r, fips_code) %>% 
  data.frame() %>% 
  select(-geometry) %>% 
  unique()

# 06037 for CA 

int <- as_tibble(st_intersection(sf_ra,sf_county))
int$area <- st_area(int$geometry)

county_to_rating_area_zip3 <- 
  int %>% select(ratng_r,fips_code,area) %>% 
  unique() %>% 
  arrange(ratng_r,desc(area)) %>% 
  group_by(ratng_r) %>% 
  mutate(area = as.numeric(area)) %>% 
  mutate(pct_area = area / sum(area)) %>% 
  filter(pct_area >.01) %>% 
  #filter(row_number()==1) %>% 
  select(rating_area = ratng_r, fips_code,pct_area) 

df_county_to_rating_area_final <- 
  county_to_rating_area %>% 
  bind_rows(county_to_rating_area_zip3) %>% 
  rename(pct_overlap = pct_area)

df_county_to_rating_area_final %>% 
  saveRDS(here("output/geographic-crosswalks/01_county-to-rating-area-all-states.rds"))

s3saveRDS(df_county_to_rating_area_final,
          bucket = paste0(project_bucket,"/geographic-crosswalks"), 
          object = "/01_county-to-rating-area-all-states.rds")

  
  
  