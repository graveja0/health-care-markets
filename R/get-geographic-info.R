#' This function extracts various geographic information (e.g., centroid, contiguous geographies from a shapefile.)
#'
#' @param shape Shapefile
#'
#' @return data frame
#' @export
#'


# shape <-  county_map %>% 
#   subset(GEOID != "99")
#gg <- quo(GEOID)

get_geograhic_info <- function(shape,geoid = GEOID, get_contiguous = TRUE) {
  
  gg = enquo(geoid)
  
  df_map <-
    shape@data %>% rownames_to_column()  %>%
    rename(polygon_id = rowname) %>%
    tbl_df() 
  
  centroids <- SpatialPointsDataFrame(gCentroid(shape, byid=TRUE), 
                                      shape@data, match.ID=FALSE) %>% tbl_df() %>% 
    select(!!gg,centroid_x = x, centroid_y = y)
  if (get_contiguous) {
    contiguous <-  gTouches(shape, byid=TRUE, returnDense = FALSE,checkValidity = TRUE) 
    polygon_lut <- names(contiguous) %>% set_names(1:length(contiguous))
    
  
    df_contiguous <- 
      contiguous %>% 
      map(~(data.frame(id = .x))) %>% 
      bind_rows(.id = "polygon_id") %>% 
      filter(!is.na(id)) %>% 
      mutate(contig = polygon_lut[id]) %>% 
      select(polygon_id,contig) %>% 
      arrange(polygon_id,contig) %>% 
      group_by(polygon_id) %>% 
      mutate(n = str_pad(row_number(),width = 2, pad = "0")) %>% 
      mutate(key = paste0("contig_",n)) %>% 
      left_join(df_map %>% select(polygon_id, !!gg),"polygon_id") %>% 
      left_join(df_map %>% select(contig = polygon_id, contig_GEOID = !!gg), "contig") %>% 
      select(polygon_id,!!gg,contig_GEOID,key) %>% 
      spread(key,contig_GEOID) %>% 
      ungroup() %>% 
      select(-polygon_id) 
  
    out <- df_map %>% 
      left_join(centroids, quo_name(gg)) %>% 
      left_join(df_contiguous,quo_name(gg))  %>% 
      janitor::clean_names()
  } else {
    out <- df_map %>% 
      left_join(centroids, quo_name(gg)) %>% 
      janitor::clean_names()
  }
  return(out)
}
