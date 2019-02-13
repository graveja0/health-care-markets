#' ---
#' output: github_document
#' ---

suppressWarnings(suppressMessages(source(here::here("/R/manifest.R"))))
source(here("R/move-ak-hi.R"))
source(here("R/get-geographic-info.R"))
source(here("R/map-theme.R"))
source(here("R/shared-objects.R"))
source(here("R/get-contiguous-areas.R"))

fips_to_state <- read_rds(here("output/geographic-crosswalks/01_xw_county-to-fips.rds")) %>% 
  mutate(statefp = str_sub(fips_code,1,2)) %>% 
  select(statefp,state) %>% unique()

shp_county <- sf::read_sf(here("public-data/shape-files/county-2017/cb_2017_us_county_5m/cb_2017_us_county_5m.shp")) %>% 
  sf::st_transform(crs ="+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96") %>% 
  janitor::clean_names() %>% 
  left_join(fips_to_state,"statefp") %>% 
  filter(state %in% states) %>% 
  move_ak_hi(state = state) %>% 
  mutate(fips_code = geoid) %>% 
  st_simplify(dTolerance = 100)  %>% 
  left_join(get_contiguous(shp = ., id = fips_code) , "fips_code") 

shp_county %>% 
  sf::write_sf(here("output/tidy-mapping-files/county/01_county-shape-file.shp"))
shape_types %>% 
  map(~(
    put_object(
      file  = paste0("./output/tidy-mapping-files/county/01_county-shape-file.",.x),
      bucket = paste0(project_bucket, "/tidy-mapping-files/county")
    )
  ))

shp_county %>% 
  filter(state=="TN" ) %>% 
  ggplot() + geom_sf(alpha = 0) + theme_bw() + coord_sf(datum=NA) +
  remove_all_axes
