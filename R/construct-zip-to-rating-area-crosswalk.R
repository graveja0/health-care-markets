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

sf_ra <- read_sf(here("output/tidy-mapping-files/rating-area/01_rating-area-shape-file.shp")) %>%
  st_transform(crs = 4326) %>% 
  mutate(rating_area = ratng_r)

sf_zcta <- read_sf(here("output/tidy-mapping-files/zcta/01_zcta-shape-file.shp")) %>% 
  st_transform(crs = 4326) %>% 
  filter(state_01 %in% states)

# Montana is returning an error, so get for all other states
int <- as_tibble(st_intersection(sf_ra %>% filter(!(state %in% "MT")),
                                 sf_zcta %>% filter(!(state_01 %in% "MT"))))
int$area <- st_area(int$geometry)

zcta_to_rating_area  <- 
  int %>% select(ratng_r,zip_code,area) %>% 
  unique() %>% 
  arrange(ratng_r,desc(area)) %>% 
  group_by(zip_code) %>% 
  mutate(area = as.numeric(area)) %>% 
  mutate(pct_of_zip_in_rating_area = area / sum(area)) %>% 
  filter(pct_of_zip_in_rating_area >.01) %>% 
  select(rating_area = ratng_r, zip_code,pct_of_zip_in_rating_area) 

# Construct for montana
# Montana is returning an error, so get for all other states
library(lwgeom)
int <- as_tibble(st_intersection(
  st_make_valid(sf_ra %>% filter((state %in% "MT"))),
  st_make_valid(sf_zcta %>% filter((state_01 %in% "MT")))))
int$area <- st_area(int$geometry)

MT_zcta_to_rating_area  <- 
  int %>% select(ratng_r,zip_code,area) %>% 
  unique() %>% 
  arrange(ratng_r,desc(area)) %>% 
  group_by(zip_code) %>% 
  mutate(area = as.numeric(area)) %>% 
  mutate(pct_of_zip_in_rating_area = area / sum(area)) %>% 
  filter(pct_of_zip_in_rating_area >.01) %>% 
  select(rating_area = ratng_r, zip_code,pct_of_zip_in_rating_area) 

df_zcta_to_rating_area_final <- 
  zcta_to_rating_area  %>% 
  bind_rows(MT_zcta_to_rating_area)

df_zcta_to_rating_area_final %>% 
  saveRDS(here("output/geographic-crosswalks/01_zcta-to-rating-area-all-states.rds"))

s3saveRDS(df_zcta_to_rating_area_final,
          bucket = paste0(project_bucket,"/geographic-crosswalks"), 
          object = "/01_zcta-to-rating-area-all-states.rds")



