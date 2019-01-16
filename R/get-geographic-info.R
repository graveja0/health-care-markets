get_geograhic_info <- function(shape) {
  df_map <-
    shape@data %>% rownames_to_column()  %>%
    rename(polygon_id = rowname) %>%
    tbl_df() 
  
  centroids <- SpatialPointsDataFrame(gCentroid(shape, byid=TRUE), 
                                      shape@data, match.ID=FALSE) %>% tbl_df() %>% 
    select(GEOID,centroid_x = x, centroid_y = y)
  
  contiguous <-  gTouches(shape, byid=TRUE, returnDense = FALSE) 
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
    left_join(df_map %>% select(polygon_id, GEOID),"polygon_id") %>% 
    left_join(df_map %>% select(contig = polygon_id, contig_GEOID = GEOID), "contig") %>% 
    select(polygon_id,GEOID,contig_GEOID,key) %>% 
    spread(key,contig_GEOID) %>% 
    ungroup() %>% 
    select(-polygon_id) %>% 
    filter(GEOID=="47037")
  
  df_map %>% 
    left_join(centroids, "GEOID") %>% 
    left_join(df_contiguous,"GEOID")  %>% 
    janitor::clean_names()
}
