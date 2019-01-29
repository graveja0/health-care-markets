get_contiguous <- function(shp,id) {
  
  id <- enquo(id)
  
  
  id_names <- shp %>% pull(!!id) 
  
  out <- shp %>% 
    st_intersects() %>% 
    set_names(id_names) %>% 
    map(~(.x %>% data.frame() %>% 
            set_names("contig"))) %>% 
    bind_rows(.id = quo_name(id)) %>% 
    group_by(!!id) %>% 
    filter(!!id != contig) %>% 
    mutate(test = paste0("contig_",str_pad(row_number(),width=2,pad="0"))) %>% 
    spread(test,contig)  %>% 
    ungroup()
  
}