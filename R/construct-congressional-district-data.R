#' ---
#' output: github_document
#' ---
#' 

suppressWarnings(suppressMessages(source(here::here("/R/manifest.R"))))
source(here("R/move-ak-hi.R"))
source(here("R/get-geographic-info.R"))
source(here("R/map-theme.R"))
source(here("R/shared-objects.R"))
source(here("R/get-contiguous-areas.R"))


# STATE FIPS TO STATE ABBREVIATION
fips_to_state <- read_rds(here("output/geographic-crosswalks/01_xw_county-to-fips.rds")) %>% 
  mutate(statefp = str_sub(fips_code,1,2)) %>% 
  select(statefp,state) %>% unique()

sf_cd <- #https://www.census.gov/geo/maps-data/data/cbf/cbf_cds.html
  sf::read_sf(here("public-data/shape-files/congressional-district/cb_2015_us_cd114_500k/cb_2015_us_cd114_500k.shp")) %>% 
  sf::st_transform(crs ="+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96") %>% 
  st_simplify(dTolerance = 500) %>% 
  janitor::clean_names() %>% 
  left_join(fips_to_state,"statefp") %>% 
  move_ak_hi(state = state)

sf_state <- sf::read_sf(here("output/tidy-mapping-files/state/01_state-shape-file.shp")) %>% 
  sf::st_transform(crs ="+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96") %>% 
  st_simplify(dTolerance = 500) %>% 
  move_ak_hi(state = stusps)

sf_cd %>% 
  sf::write_sf(here("output/tidy-mapping-files/congressional-district/01_congressional-district-114-shape-file.shp"))
# shape_types %>% 
#   map(~(
#     put_object(
#       file  = paste0("./output/tidy-mapping-files/hrr/01_hrr-shape-file.",.x),
#       bucket = paste0(project_bucket, "/tidy-mapping-files/hrr")
#     )
#   ))

sf_cd %>% 
  filter(state %in% "TN") %>% 
  ggplot() + geom_sf()

