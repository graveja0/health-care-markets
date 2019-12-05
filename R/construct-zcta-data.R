#' ---
#' output: github_document
#' ---

suppressWarnings(suppressMessages(source(here::here("/R/manifest.R"))))
source(here("R/move-ak-hi.R"))
source(here("R/get-geographic-info.R"))
source(here("R/map-theme.R"))
source(here("R/shared-objects.R"))
source(here("R/get-contiguous-areas.R"))

zcta_to_state <- read_csv(here("public-data/zcta-to-fips-county/zcta-to-fips-county.csv")) %>% 
  filter(row_number() !=1) %>% 
  rename(zip5 = zcta5) %>% 
  select(zip5,state = stab) %>% 
  unique() %>% 
  group_by(zip5) %>% 
  mutate(n=paste0("state_",str_pad(row_number(), width=2,pad="0"))) %>% 
  filter(zip5!="99999") %>% 
  spread(n,state)

sf_zcta <-  sf::read_sf(here("public-data/shape-files/zcta-2017/tl_2017_us_zcta510/tl_2017_us_zcta510.shp")) %>% 
  sf::st_transform(crs ="+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96") %>% 
  janitor::clean_names() %>% 
  mutate(zip5 = zcta5ce10) %>% 
  mutate(zip_code =  zcta5ce10) %>% 
  left_join(zcta_to_state,"zip5") %>% 
  filter(state_01 %in% states) %>% 
 #st_simplify(dTolerance = 1000)  %>% 
  move_ak_hi(state = state_01) 

sf_zcta %>% 
  st_simplify(dTolerance = 1000)  %>% 
  filter(state_01 =="KY") %>% 
  ggplot() + geom_sf()

#dir.create(here("output/tidy-mapping-files/zcta/"))
sf_zcta %>% 
  st_simplify(dTolerance = 100)  %>% 
  sf::write_sf(here("output/tidy-mapping-files/zcta/01_zcta-shape-file.shp"))

sf_zcta %>% 
  #st_simplify(dTolerance = 10)  %>% 
  sf::write_sf(here("output/tidy-mapping-files/zcta/01_zcta-shape-file-higher-resolution.shp"))

