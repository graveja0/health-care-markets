#' Compare geographic area overlap across two market definitions
#'
#' @param shape_1 Shapefile for source market
#' @param shape_2 Shapefile for target maret
#' @param name_1  Name of polygon ID in source market.
#' @param name_2 Name of polygon ID in target market. 
#' @param plot Plot the overlap?
#'
#' @return
#' @export
#'
#' @examples
compare_overlap_in_market_geographies <- function(shape_1, shape_2, name_1, name_2, plot = FALSE) {
  
  #https://github.com/r-spatial/sf/wiki/migrating
  
  area_1 <- enquo(name_1)
  area_2 <- enquo(name_2)
  #browser()
  #before further processing, check the coordinate reference systems of both layers (must be same CRS)
  #note: project to local coordinate system if current CRS in degrees
  #st_crs(shape_1) <- st_crs(shape_2)
  
  shape_2$target_area = st_area(shape_2)
  shape_1$source_area = st_area(shape_1)
  
   # shape_1_orig <- shape_1
   # shape_2_orig <- shape_2
   
  # shape_1 <-  st_simplify(shape_1,dTolerance = 500)
  # shape_2 <-  st_simplify(shape_2,dTolerance = 500)
  
  
  
  #run the intersect function, converting the output to a tibble in the process
  int <- st_intersection(shape_1,shape_2)
  int$intersection_area = st_area(int)
  #union <- st_union(shape_1,shape_2)
  
  # #plot the layers to visually check result of intersect
  # plot(shape_2[quo_name(area_2)])
  # plot(shape_1[quo_name(area_1)])
  # plot(int[quo_name(area_2)])
  
  # tot_area <- 
  #   int %>% 
  #   group_by(!!area_1) %>% 
  #   summarize(total_area = sum(source_area)) %>% 
  #   as.data.frame() %>% 
  #   select(!!area_1,total_area)
  
  df_area <- 
    int %>% 
    select(!!area_1,source_area,!!area_2,target_area,intersection_area) %>% 
    as.data.frame() %>% 
    mutate_at(vars(source_area,target_area,intersection_area),funs(as.numeric)) %>% 
    select(-geometry) %>% 
    filter(intersection_area>0) %>% 
    mutate(fraction_in_target = intersection_area / source_area)  %>% 
    as_tibble()
  
  if (plot) {
    par(mfrow=c(3,1))
    plot(shape_2[quo_name(area_2)])
    plot(shape_1[quo_name(area_1)])
    plot(int[quo_name(area_2)])
  } else {
    df_area
  }
}
