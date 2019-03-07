estimate_hhi <-  function(df, id,  market, weight) {
  ii <- enquo(id)
  mm<- enquo(market)
  ww <- enquo(weight) 
  
  out <- 
    df %>% 
      group_by(!!mm) %>% 
      mutate(denominator = sum(!!ww ,na.rm=TRUE)) %>% 
      group_by(!!mm,!!ii) %>%
      mutate(numerator = sum(!!ww,na.rm=TRUE)) %>%
      # Don't want to multiply count sub-ID units (e.g., two hospitals in the same system would have the same numereator value)
      # So keeping just one observation per system ID
      filter(row_number()==1) %>% 
      select(!!mm,!!ii,!!ww,numerator,denominator) %>%
      unique() %>%
      mutate(market_share = 100 * (numerator / denominator)) %>%
      mutate(market_share_sq = market_share ^ 2)  %>%
      group_by(!!mm) %>%
      mutate(hhi = sum(market_share_sq,na.rm=TRUE)) %>% 
      filter(row_number()==1) %>% 
      select(!!mm,hhi,total_weight = denominator) 
  
  return(out)
}

# Kessler-McClellan Approach
hhi_km <- function(bp) apply(apply(((100*t(apply(bp,1,function(x) x/sum(x))))^2),1,sum)*apply(bp,2,function(x) x/sum(x)),2,sum)
hhi_net <- function(up) apply((100*t(apply(up,1,function(x) x/sum(x))))^2,1,sum)
hhi_net <- function(up) apply((100*t(apply(up,1,function(x) x/sum(x))))^2,1,sum)

estimate_market_share <-  function(df, id,  market, weight) {
  ii <- enquo(id)
  mm<- enquo(market)
  ww <- enquo(weight) 
  
  out <- 
    df %>% 
    group_by(!!mm) %>% 
    mutate(denominator = sum(!!ww ,na.rm=TRUE)) %>% 
    group_by(!!mm,!!ii) %>%
    mutate(numerator = sum(!!ww,na.rm=TRUE)) %>%
  # Don't want to multiply count sub-ID units (e.g., two hospitals in the same system would have the same numereator value)
  # So keeping just one observation per system ID
    filter(row_number()==1) %>% 
    select(!!mm,!!ii,!!ww,numerator,denominator) %>%
    unique() %>%
    mutate(market_share = 100 * (numerator / denominator)) %>%
    mutate(market_share_sq = market_share ^ 2)  %>%
    group_by(!!mm) %>%
    mutate(hhi = sum(market_share_sq,na.rm=TRUE)) 

  return(out)
}
