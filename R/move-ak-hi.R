#' This function moves Alaska and Hawaii to faciliate better mapping. NOote that
#' this function would need to be updated evey time a new geographic unit is added.
#'
#' @param dt Data
#' @param type The level of hte map (e.g., state, county, rating, etc.)
#'
#' @return
#' @export
#'
#'
move_ak_hi <- function(sf, state, elide_hawaii = c(5400000, -1400000), elide_alaska= c(-1870000, -2500000) ) {
  ss <- enquo(state)
  
  rest_of_us <-
    sf %>%
    filter((!!ss %in% states[-grep("HI|AK",states)]))
  
  hi_ak <- 
    sf %>% 
    filter(!(!!ss %in% states[-grep("HI|AK",states)]))
  
  hawaii <- sf  %>%
    filter(!!ss == "HI")
  sp_hawaii <- sf::as_Spatial(hawaii)
  projection = proj4string(sp_hawaii)
  sp_hawaii <- elide(sp_hawaii, rotate = -35)
  sp_hawaii <- elide(sp_hawaii, shift = elide_hawaii)
  proj4string(sp_hawaii) <- CRS(projection)
  new_hawaii <- st_as_sf(sp_hawaii)
  st_crs(new_hawaii)
  
  alaska <- sf  %>%
    filter(!!ss == "AK")
  sp_alaska <- sf::as_Spatial(alaska)
  projection = proj4string(sp_alaska)
  sp_alaska <- elide(sp_alaska, rotate = -50)
  sp_alaska <-
    elide(sp_alaska, scale = max(apply(bbox(sp_alaska), 1, diff)) / 2.3)
  sp_alaska <- elide(sp_alaska, shift = elide_alaska)
  proj4string(sp_alaska) <- CRS(projection)
  new_alaska <- st_as_sf(sp_alaska)
  st_crs(new_alaska)
  

  st_crs(rest_of_us)
  
  out <- rbind(rbind(new_hawaii, rest_of_us), new_alaska)
  out %>%
    ggplot() + geom_sf(aes(fill = test))
  return(out)
}

