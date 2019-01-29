suppressWarnings(suppressMessages(source(here::here("/R/manifest.R"))))
source(here("R/move-ak-hi.R"))
source(here("R/get-geographic-info.R"))
source(here("R/map-theme.R"))
source(here("R/shared-objects.R"))


state_map_shape <- sf::read_sf(here("public-data/shape-files/state-2017/cb_2017_us_state_500k/cb_2017_us_state_500k.shp")) %>% 
  janitor::clean_names() %>% 
  filter(!(stusps %in% c("AS","GU","MP","PR","VI")) ) %>% 
  sf::st_transform(crs = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs") %>% 
  st_simplify(dTolerance=1000) %>% 
  move_ak_hi(state= stusps)

format(object.size(state_map_shape),units="MB")

state_map_shape %>% 
  mutate(test = as.factor(sample(1:10,nrow(.),replace=TRUE))) %>% 
  ggplot()  + 
  geom_sf(aes(fill = test))

if (!dir.exists(here("output/tidy-mapping-files/state"))) dir.create(here("output/tidy-mapping-files/state/"))
state_map_shape %>% 
  sf::write_sf(here("output/tidy-mapping-files/state/01_state-shape-file.shp"))
