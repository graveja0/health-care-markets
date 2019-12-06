# Extract a mapbox isochrone and test whether a set of x,y coordinates are in or out of the isochrone.

library(httr)
library(tidyverse)
library(jsonlite)
library(sf)
library(ggthemes)

my_mapbox_token <- "<YOUR MAPBOX TOKEN>"

test_xy <- c(-71.122190, 42.373850)
within_xy <- c("-71.065700","42.362130")
outside_xy <- c("-86.808460","36.145780")

get_mapbox_isochrone <- function(long, lat, contours_minutes, base_url = "https://api.mapbox.com/", mapbox_token = Sys.getenv("MAPBOX_API_TOKEN")) {
  request_url <- paste0(
    "isochrone/v1/mapbox/driving/",
    long, ",", lat, "?contours_minutes=", contours_minutes,
    "&polygons=true&access_token=", mapbox_token
  )
  url_to_request <- modify_url(base_url, path = request_url)
  
  tryCatch({
    temp <- suppressWarnings(httr::GET(url_to_request, verbose = T))
    Sys.sleep(1)
  },
  error = function(cond) {
    message(paste("URL does not seem to exist:", url))
    message("Here's the original error message:")
    message(cond)
    # Choose a return value in case of error
    return(NA)
  },
  finally = {
    out <- suppressMessages(jsonlite::fromJSON(content(temp, "text"),
                                               simplifyVector = FALSE, simplifyDataFrame = TRUE, flatten = TRUE
    ))
    
    name_iso <- sort(unlist(str_split(contours_minutes, pattern = ", ", n = 4)), decreasing = T)
    # print(name_iso)
    coords <- out$features$geometry.coordinates
    
    # If isochrone is found, name the the subsets by their corresponding driving-times
    # for example, a 10 minute isochrones is named "10"
    if (!is.null(coords)) {
      names(coords) <- name_iso
    }
    
    # Clean up the JSON object
    df_coords <- 
      mapply(c,coords, SIMPLIFY = TRUE) %>% 
      pluck(1) %>% 
      lapply(.,function(x) as.data.frame(t(x))) %>% 
      bind_rows() %>% 
      tbl_df() %>% 
      rename(long = V1, lat=V2) 
    
    return(df_coords)
  }
  )
}

df_centroids_to_get <- 
  data.frame(long = test_xy[1], lat = test_xy[2]) %>% 
  # Create another copy of x,y to retain the reference centroid as data columns in the shapefile.
  mutate(iso_id = "CAMBRIDGE") %>% 
  group_by(iso_id) %>% 
  nest()

# Get the X,Y coordinates for the 30-minute isochrone
iso_30 <- 
  df_centroids_to_get %>% 
  mutate(iso_30 = map(data,~(
    get_mapbox_isochrone(long = .x$long[1], lat= .x$lat[1], contours_minutes = 30, base_url = "https://api.mapbox.com/",
                         mapbox_token = my_mapbox_token)
  ))) 

# Simplify into a data file
df_iso_30 <- 
  iso_30 %>% 
  select(iso_id,iso_30) %>% 
  unnest() 
  
# Turn into a shapefile (sf)
sf_iso <- 
  df_iso_30  %>% 
  st_as_sf(coords = c("long","lat"), crs = 4326) %>% 
  group_by(iso_id) %>% 
  summarise(geometry = st_combine(geometry)) %>% 
  st_cast("POLYGON") %>% 
  # Bring Back in the centriods
  left_join(
    iso_30 %>% 
      select(iso_id,data) %>% 
      unnest() %>% 
      select(iso_id, centroid_long = long ,centroid_lat = lat) %>% 
      unique(),
    "iso_id")

# Map it to check

# Check if the example x,y coords are in or out of the isochrone.
sf_check <-
  data.frame(provider_id = c("A","B"),long = as.numeric(c(within_xy[1],outside_xy[1])), lat = as.numeric(c(within_xy[2],outside_xy[2]))) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326)

in_isochrone <- 
  st_within(sf_check,sf_iso) %>% data.frame() %>% tbl_df() %>% 
  mutate(provider_id = sf_check[row.id,]$provider_id,
         iso_id = sf_iso[col.id,]$iso_id) %>% 
  mutate(in_isochrone = 1) %>% 
  select(iso_id,provider_id,in_isochrone) %>% 
  full_join(sf_check %>% as.data.frame() %>% mutate(provider_location = geometry),"provider_id")  %>% 
  mutate(in_isochrone = factor(ifelse(is.na(in_isochrone),0,in_isochrone),labels = c("Outside","Within"))) %>% 
  mutate(provider_location_x = st_coordinates(provider_location)[,1],
         provider_location_y = st_coordinates(provider_location)[,2])


sf_iso %>% 
  ggplot() + geom_sf() + theme_map() +
  geom_point(data = in_isochrone %>% filter(in_isochrone=="Within"), aes(x = provider_location_x, y = provider_location_y))

