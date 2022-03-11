
# For each state, iterate through all the zipcodes. For each zipcode, create and save a list of 8 dataframes to AWS.
# Each dataframe in this list will contain the geographic coordinates for a given travel time from the given zipcode
# in a given state. 

# the zipcode coordinates are population weighted


# Load required libraries and helper functions
source(here::here("R/manifest.R"))
source(here::here("R/shared-objects.R"))
source("~/Dropbox/setup-mapbox.r")

# Using population weighted zip centroids
load("public-data/weighted_zip_centroids.rda")

zip_centroid_2 <- 
  zip_centroid %>%
  filter(!is.na(wtd_long)) %>%
  rename(x =wtd_long, y = wtd_lat) 
  

#   Given the limitation that we cannot go over 60 minutes isochrones
#  These are all the possible travel times that we can use:

required_travel_times <- list(c("30"), c("60"))

# Parallelize future map code
plan(multiprocess,workers = availableCores()-1)

states_to_run <- c("AK") %>% unique()

# Create objects by state for the first 10 states
for (st in states_to_run) {
  if (!(st %in% c("XXXN","XXXXIA"))) {
  # Create an iterable object by state, with longitude and latitudes
  st_zip_centroids_coords <-
    zip_centroid_2 %>%
    filter(state == st)
  
  print(st)

  # For a given zip in a given state, obtain isochrones for 10,15, 20, and 30 minutes driving times
  travel_time_15_30 <-
    st_zip_centroids_coords %>%
    mutate(test = future_map2(x, y, ~ (c(.x, .y)))) %>%
    pull(test) %>%
    future_map(~ (get_mapbox_isochrone(long = .x[1], lat = .x[2], contours_minutes = c("30"),
                                       mapbox_token = my_mapbox_token)), .progress = TRUE) %>%
    set_names(st_zip_centroids_coords$zip)
  
  #Sys.sleep(10)
  
  # For a given zip in a given state, obtain isochrones for 40, 45, 50, and 60 minutes driving times
  travel_time_40_60 <-
    st_zip_centroids_coords %>%
    mutate(test = future_map2(x, y, ~ (c(.x, .y)))) %>%
    pull(test) %>%
    future_map(~ (get_mapbox_isochrone(long = .x[1], lat = .x[2], contours_minutes = c("60"),
                                       mapbox_token = my_mapbox_token)), .progress = TRUE) %>%
    set_names(st_zip_centroids_coords$zip)
  #  
  # # # Merge the travel times isochrones into one list object
  

  
  all_travel_times <- mapply(c, travel_time_15_30,travel_time_40_60,SIMPLIFY = TRUE)
  
  present <- 
    all_travel_times %>% map_lgl(~(
      !is.null(.x)
    ))
  
  
  minutes_ran <- all_travel_times[present] %>% transpose() %>% names()
  l_minutes_ran <- all_travel_times[present] %>% transpose() %>% map_int(~(.x %>% length()))
  cat("Minutes Ran:\n ",minutes_ran[1]," ",l_minutes_ran[1],"\n ",minutes_ran[2]," ",l_minutes_ran[2],"\n")
  #  
  file_name_st <- paste0(st, "-zip-isochrones.Rdata")
  print(file_name_st)
  s3save(all_travel_times, bucket = "health-care-markets", object = paste0("isochrones-data/state/weighted-zip-centroids/", file_name_st))
  #  
  
  Sys.sleep(15)
  }
}

xy_geography_tmp <-
  all_travel_times %>%
  purrr::transpose() %>%
  future_map(~(.x %>% future_map(~ (.x[[1]])) %>% future_map(~ (as.vector(.x)))))

# Debugging
for (minutes in c("40", "30")) {
  # cat(minutes)
  
  if (!is.null(xy_geography_tmp[minutes] %>% purrr::pluck(1))) {
    print(minutes)
    foo <- xy_geography_tmp[minutes] %>% purrr::pluck(1)
    
    foo2 <-
      #foo %>%
      Filter(Negate(is.null), foo) %>%
      future_map(~ (do.call("rbind", .x) %>% data.frame() %>% set_names(c("longitude", "latitude")) %>% tbl_df())) %>%
      bind_rows(.id = "zip_code") %>%
      group_by(zip_code) %>%
      nest() %>%
      set_names(c("zip_code", paste0("iso_xy_", minutes)))
    
    assign(paste0("df_iso_", minutes), foo2)
    cat("\n ...\n")
  }
}

rm(list = ls(pattern = "df_iso_"))
gc()



get_providers_in_geography <- function(xy_provider, xy_geography) {
  tmp <- 
    xy_geography %>% 
    #purrr::pluck(1) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON")
  
  xy_provider %>%
    filter(st_within(., tmp, sparse = FALSE)) %>%
    data.frame() %>%
    select(-geometry)
}

df_iso <-
  df_iso_30 %>%
  left_join(df_iso_40, "zip_code") 

xy_provider <-
  sf::read_sf(
    paste0("../health-care-markets/provider_location_shp/contig_states/vericred19_aug_npi_",
           st, ".shp")) %>%
  st_transform(crs = 4326)

npi_ri <- 
  df_iso %>% 
  #sample_n(10) %>%
  #filter(zip_code == "46711") %>%
# 30 Minutes
  mutate(isochrone_30 = future_map(iso_xy_30, ~ (
    .x %>%
      st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
      summarise(geometry = st_combine(geometry)) %>%
      st_cast("POLYGON")
  ))) %>%
  mutate(providers_30 = map(iso_xy_30, ~ (get_providers_in_geography(xy_provider = xy_provider, xy_geography = .x)))) %>%
  mutate(providers_30_xy = future_map(providers_30, ~ (
    .x %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  ))) %>%
  # 40 Minutes
  mutate(isochrone_40 = future_map(iso_xy_40, ~ (
    .x %>%
      st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
      summarise(geometry = st_combine(geometry)) %>%
      st_cast("POLYGON")
  ))) %>%
  mutate(providers_40 = future_map(iso_xy_40, ~ (get_providers_in_geography(xy_provider = xy_provider, xy_geography = .x)))) %>%
  mutate(providers_40_xy = future_map(providers_40, ~ (
    .x %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  )))


leaflet(sf_zip %>% filter(state_01 == "IN")) %>%
  addTiles() %>%
  addProviderTiles("CartoDB.Positron", group = "Greyscale") %>%
  addPolygons(fill = T, stroke = T, color = "red", 
              data = npi_ri %>% filter(zip_code == "46711") %>% pull(isochrone_30) %>% purrr::pluck(1),
              weight = 0.5, fillOpacity = 0.3) %>%
  addPolygons(fill = T, stroke = T, color = "blue",
              data = npi_ri %>% filter(zip_code == "46711") %>% pull(isochrone_40) %>% purrr::pluck(1),
              weight = 0.5, fillOpacity = 0.3)


tmp <- 
  npi_ri$iso_xy_30 %>% 
  purrr::pluck(1) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

xy_provider %>%
  filter(st_within(., tmp, sparse = FALSE)) %>%
  data.frame() %>%
  select(-geometry)
