#' ---
#' output: github_document
#' ---

suppressWarnings(suppressMessages(source(here::here("/R/manifest.R"))))
library(gganimate)
library(magick)
source(here("R/map-theme.R"))
source(here("R/shared-objects.R"))

hhi_cz <- read_rds(here("output/market-comparisons/01_HHI_genacute_cz.rds")) %>% 
  tbl_df()



sf_cz <- read_sf(here("output/tidy-mapping-files/commuting-zone/01_commuting-zone-shape-file.shp")) %>% 
  st_transform(crs = 4326)
sf_state <- read_sf(here("output/tidy-mapping-files/state/01_state-shape-file.shp")) %>% 
  st_transform(crs = 4326)



# p <- 
#  sf_cz %>% 
#   left_join(hhi_cz,"cz_id") %>% 
#   filter(state_01 %in% states_to_map | state_02 %in% states_to_map  | state_03 %in% states_to_map) %>% 
#   mutate(year = as.integer(paste0(year))) %>% 
#   filter(!is.na(year)) %>% 
#   ggplot() + 
#   geom_sf(aes(fill =hhi_zip_cz)) +
#   scale_fill_gradient2(name = "HHI",low = scales::muted("blue"),mid = "white",high = scales::muted("red"),midpoint = 2500,limits = c(0,10000),
#                        breaks = c(0,2500,5000,10000)) + 
#   #theme(legend.position = "bottom") +
#   geom_sf(data = sf_state %>% filter(stusps %in% states_to_map), alpha = 0,lwd=.7,colour = "black") + 
#   coord_sf(datum=NA) + 
#   remove_all_axes +
#   ggthemes::theme_tufte(base_family = "Gill Sans") + 
#   transition_time(year) +
#   ease_aes('linear') + 
#   labs(title = "Year: {frame_time}")

states_to_map <- census_regions$south_central

hhi_chg <- 
  hhi_cz %>% 
  select(cz_id, year,hhi_zip_cz ) %>% 
  mutate(year = paste0("hhi_",year)) %>% 
  spread(year,hhi_zip_cz) %>% 
  mutate(hhi_2016 = coalesce(hhi_2015,hhi_2016)) %>% 
  mutate(chg_2010 = hhi_2010 - hhi_2010,
         chg_2011 = hhi_2011 - hhi_2010,
         chg_2012 = hhi_2012 - hhi_2010,
         chg_2013 = hhi_2013 - hhi_2010,
         chg_2014 = hhi_2014 - hhi_2010,
         chg_2015 = hhi_2015 - hhi_2010,
         chg_2016 = hhi_2016 - hhi_2010,
         chg_2017 = hhi_2017 - hhi_2010) %>% 
  select(cz_id,contains("chg_")) %>% 
  gather(key,value,-cz_id) %>% 
  mutate(year = as.numeric(paste0(gsub("chg_","",key))))


for (x in names(census_regions[-1])) {
  cat(x) 
  cat("\n\n")
  states_to_map <- census_regions[[x]]
  tmp <- 
    sf_cz %>% 
    left_join(hhi_chg,"cz_id") %>% 
    filter(state_01 %in% states_to_map | state_02 %in% states_to_map  | state_03 %in% states_to_map) %>% 
    mutate(year = as.integer(paste0(year))) %>% 
    filter(!is.na(year)) %>% 
    ggplot() + 
    geom_sf(aes(fill =value)) +
    scale_fill_gradient2(name = "Change in Market\nConcentration Index\n(HHI)",low = scales::muted("blue"),mid = "white",high = scales::muted("red"),midpoint = 0,limits = c(-2000,3000),
                         breaks = c(0,-2000,-1000,0,1000,2000,3000)) + 
    #theme(legend.position = "bottom") +
    geom_sf(data = sf_state %>% filter(stusps %in% states_to_map), alpha = 0,lwd=.7,colour = "black") + 
    coord_sf(datum=NA) + 
    remove_all_axes +
    ggthemes::theme_tufte(base_family = "Gill Sans") + 
    transition_time(year) +
    ease_aes('linear') + 
    labs(title = "Year: {frame_time}") 
    animate(plot = tmp, nframes = 30,end_pause = 3,duration=10, renderer = gifski_renderer(paste0('output/figures/hhi_2010-to-2017-change_',x,'.gif')))
}

