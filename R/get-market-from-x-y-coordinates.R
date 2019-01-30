#' Assign market from x,y coordinates
#'
#' @param df Data frame with x and y coordinates
#' @param x Longitude variable name
#' @param y Latitude variable name
#' @param sf The shape file with market identifiers in it.
#' @param market_id  The variable name for the market identifier. 
#'
#' @return Vector with market identifiersf
#' @export

get_market_from_xy <- function(df,x,y,sf,market_id) {
  
  
  x <- enquo(x)
  y <- enquo(y) 
  market_id <- enquo(market_id)
  
  possible_markets <- sf %>% pull(!!market_id)
  
  lon <- df %>% pull(!!x) 
  lat <- df %>% pull(!!y)
  
  foo <- function(x,y) {
    st_intersects(
      st_point(x = c(x,y)) %>% st_sfc(crs = st_crs(sf)),
      sf) %>% 
      unlist()
  }
  
  market <- map2(lon,lat,~(possible_markets[suppressWarnings(suppressMessages(foo(x=.x,y=.y)))])) 
  
  return(market)
}
