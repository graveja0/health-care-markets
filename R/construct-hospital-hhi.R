#' ---
#' output: github_document
#' ---

# The objective of this file is to make comparisons of Hirschman-Herfindahl Indexes across alternative 
# geographic market definitions.

# Load Shapefiles

suppressWarnings(suppressMessages(source(here::here("/R/manifest.R"))))
source(here("R/map-theme.R"))
source(here("R/shared-objects.R"))
source(here("R/get-market-from-x-y-coordinates.R"))
source(here("R/estimate_hhi.R"))

rename_in_list <- function(x,from, to) {
  x %>% rename_at(vars(contains(from)), funs(sub(from, to, .)))
}

hhi_years <- c("2010","2011","2012","2013","2014","2015","2016","2017","2018")

hhi_years = "2018"

sf_hrr <- read_sf(here("output/tidy-mapping-files/hrr/01_hrr-shape-file.shp"))  %>% 
  st_transform(crs = 4326)
sf_cz <- read_sf(here("output/tidy-mapping-files/commuting-zone/01_commuting-zone-shape-file.shp")) %>% 
  st_transform(crs = 4326)
sf_cd114 <- read_sf(here("output/tidy-mapping-files/congressional-district/01_congressional-district-114-shape-file.shp")) %>% 
  st_transform(crs = 4326) %>% 
  mutate(cd114fp = paste0(state,cd114fp))
sf_ra <- read_sf(here("output/tidy-mapping-files/rating-area/01_rating-area-shape-file.shp")) %>%
  st_transform(crs = 4326) %>% 
  mutate(rating_area = ratng_r)
sf_state <- read_sf(here("output/tidy-mapping-files/state/01_state-shape-file.shp")) %>% 
  st_transform(crs = 4326)
sf_county <- read_sf(here("output/tidy-mapping-files/county/01_county-shape-file.shp")) %>% 
  st_transform(crs = 4326)

# Map General Actue Care Hospitals to Markets

if (!file.exists(here("output/market-comparisons/01_aha-markets-2018.rds"))) {
  
  aha_files <- c("2018" = "../../box/Research-AHA_Data/data/aha/annual/raw/2018/ASDB FY 2018/COMMA/ASPUB18.CSV",
                 "2017" = "../../box/Research-AHA_Data/data/aha/annual/raw/2017/FY2017 ASDB/COMMA/ASPUB17.CSV",
                 "2016" = "../../box/Research-AHA_Data/data/aha/annual/raw/2016/FY2016 Annual Survey Database/COMMA/ASPUB16.CSV",
                 "2015" = "../../box/Research-AHA_Data/data/aha/annual/raw/2015/FY2015 Annual Survey Database/COMMA/ASPUB15.CSV",
                 "2014" = "../../box/Research-AHA_Data/data/aha/annual/raw/2014/FY2014 ASDB/COMMA/ASPUB14.CSV",
                 "2013" = "../../box/Research-AHA_Data/data/aha/annual/raw/2013/FY2013 ASDB/COMMA/ASPUB13.CSV",
                 "2012" = "../../box/Research-AHA_Data/data/aha/annual/raw/2012/COMMA/ASPUB12.csv",
                 "2011" = "../../box/Research-AHA_Data/data/aha/annual/raw/2011/FY2011 ASDB/COMMA/ASPUB11.csv.csv",
                 "2010" = "../../box/Research-AHA_Data/data/aha/annual/raw/2010/FY2010 ASDB/COMMA/ASPUB10.csv",
                 "2009" = "../../box/Research-AHA_Data/data/aha/annual/raw/2009/FY2009 ASDB/COMMA/ASPUB09.csv",
                 "2008" = "../../box/Research-AHA_Data/data/aha/annual/raw/2008/FY2008 ASDB/COMMA/pubas08.csv",
                 "2007" = "../../box/Research-AHA_Data/data/aha/annual/raw/2007/FY2007 ASDB/COMMA/pubas07.csv",
                 "2006" = "../../box/Research-AHA_Data/data/aha/annual/raw/2006/FY2006 ASDB/COMMA/pubas06.csv"
  )
  aha_files <- aha_files[1]
    
  # Get latitude and longitue of general acute care hospitals in 2017 AHA survey. 
  aha <- 
    aha_files %>% 
      map(~(
        data.table::fread(here(.x)) %>% 
        janitor::clean_names() %>% 
        filter(mstate %in% states) %>% 
        mutate(system_id = ifelse(!is.na(sysid),paste0("SYS_",sysid),id)) %>% 
        filter(serv==10))) %>% 
    map(~rename_in_list(x = .x, from = "hcfaid", to = "mcrnum")) %>% 
    map(~(.x %>% 
        select(mname, id, mcrnum , latitude = lat, longitude = long, hrrnum = hrrcode, hsanum = hsacode, admtot, system_id, mloczip, sysname,fips_code=fcounty) %>% 
        mutate(prvnumgrp = str_pad(mcrnum,width = 6, pad="0")) %>% 
        mutate(hosp_zip_code = str_sub(mloczip,1,5)) %>% 
          mutate(longitude = as.numeric(paste0(longitude))) %>% 
          mutate(latitude = as.numeric(paste0(latitude))) %>% 
          filter(!is.na(longitude) & !is.na(latitude))
      )) %>% 
    set_names(names(aha_files))
      
  # Assign each hospital to its marketplace rating area.  Note I have to do it this way as a handful of 
  # hospitals do not map within a rating area (oddly)
  plan(multiprocess)
  aha_rating_area <- 
    aha %>% 
    future_map(~(
      get_market_from_xy(df = .x, 
                         x = longitude, 
                         y = latitude,
                         sf = sf_ra,
                         market_id = rating_area)
    ),.progress = TRUE)
  
  df_aha_rating_area <- 
    map2(aha_rating_area,aha,~(.x %>% 
      set_names(.y[,"id"]) %>% 
      unlist() %>% 
      data.frame() %>% 
      rownames_to_column() %>% 
      set_names(c("id","rating_area"))
    ))
  
  # Assign each AHA hosptial to its commuting zone. 
  plan(multiprocess)
  aha_cz <- 
    aha %>% 
    future_map(~(
    get_market_from_xy(df = ., 
                       x = longitude, 
                       y = latitude,
                       sf = sf_cz,
                       market_id = cz_id)
    ))
  
  df_cz <- 
    map2(aha_cz,aha,~(.x %>% 
                        set_names(.y[,"id"]) %>% 
                        unlist() %>% 
                        data.frame() %>% 
                        rownames_to_column() %>% 
                        set_names(c("id","cz_id"))
    ))
  
  # Assign each AHA hosptial to its commuting zone. 
  plan(multiprocess)
  aha_cd114 <- 
    aha %>% 
    future_map(~(
      get_market_from_xy(df = ., 
                         x = longitude, 
                         y = latitude,
                         sf = sf_cd114,
                         market_id = cd114fp)
    ))
  
  df_cd114 <- 
    map2(aha_cd114,aha,~(.x %>% 
                        set_names(.y[,"id"]) %>% 
                        unlist() %>% 
                        data.frame() %>% 
                        rownames_to_column() %>% 
                        set_names(c("id","cd114fp"))
    ))
  
  
  aha_markets <- 
    names(aha_files) %>% 
    map(~(
      aha[[.x]] %>% 
      left_join(df_aha_rating_area[[.x]],"id") %>% 
      left_join(df_cz[[.x]],"id") %>% 
      left_join(df_cd114[[.x]],"id")
    )) %>% 
    set_names(names(aha_files))
  

  names(aha_markets) %>%
  walk(~(
    write_rds(aha_markets[[.x]],path=here(paste0("output/market-comparisons/01_aha-markets-",.x,".rds")))
  )) %>% 
  walk(~(
    s3saveRDS(aha_markets[[.x]],
              bucket = paste0(project_bucket,"/market-comparisons"), 
              object = paste0("01_aha-markets-",.x,".rds"))
  ))
}

# Hospital Distances: Average Distance Traveled to Hospital
if (!file.exists(here("output/market-comparisons/01_zip-hospital-distances-2017.rds"))) {
  source(here("R/calculate-zip-hospital-distances.R"))
} else {
  df_hosp_zip_dist <- 
    hhi_years %>% 
    map(~(read_rds(here(paste0("output/market-comparisons/01_zip-hospital-distances-",.x,".rds"))))) %>% 
    set_names(hhi_years)
}

# Load the hospital-zip service file constructed in "R/read-and-tidy-cms-hospital-service-areas.R")
df_hosp_zip <- 
  hhi_years %>% 
  map(~(
    read_rds(here(paste0("output/hospital-county-patient-data/",.x,"/hospital-zip-patient-data.rds"))) %>% 
    left_join(df_hosp_zip_dist[[.x]]  %>%  select(prvnumgrp,zip_code,miles),c("zip_code","prvnumgrp"))
  )) %>% 
  set_names(hhi_years)

# We use an alternative patient count number based on the total_cases variable from the hosptial-zip file 
# in our exploration below. This ensures that the aggregate market HHIs are based
# on the same underlying patient count measure (i.e., not admission totals from AHA)

df_ffs_cases <- 
  df_hosp_zip %>% 
  map(~(.x %>% 
    select(prvnumgrp,total_cases) %>% 
    group_by(prvnumgrp) %>% 
    summarise(ffs_total_cases = sum(total_cases, na.rm=TRUE))
  ))

aha_markets <- hhi_years %>% 
  map(~(
    read_rds(here(paste0("output/market-comparisons/01_aha-markets-",.x,".rds")))  %>% 
    inner_join(df_ffs_cases[[.x]],"prvnumgrp")
  )) %>% 
  set_names(hhi_years)

# Construct Market-Level HHI Measures

hhi_rating_area <-
  hhi_years %>% 
  map(~(
    aha_markets[[.x]] %>%
    estimate_hhi(id = system_id,
                 weight = ffs_total_cases,
                 market = rating_area) %>% 
    rename(hhi_rating_area = hhi,
           total_weight_rating_area = total_weight) %>% 
    left_join(
      aha_markets[[.x]] %>%
        estimate_hhi(id = system_id,
                     weight = admtot,
                     market = rating_area) %>% 
        rename(hhi_rating_area_admtot = hhi,
               total_weight_rating_area_admtot = total_weight) ,
      "rating_area"
    )
  )) %>% 
  set_names(hhi_years)


hhi_hrr <-
  hhi_years %>% 
  map(~(
    aha_markets[[.x]] %>%
    estimate_hhi(id = system_id,
                 weight = ffs_total_cases,
                 market = hrrnum) %>% 
    rename(hhi_hrr = hhi,
           total_weight_hrr= total_weight) %>% 
    left_join(
      aha_markets[[.x]] %>%
        estimate_hhi(id = system_id,
                     weight = admtot,
                     market = hrrnum) %>% 
        rename(hhi_hrr_admtot = hhi,
               total_weight_hrr_admtot = total_weight) ,
      "hrrnum"
    )
  )) %>% 
  set_names(hhi_years)

ms_hrr <-
  hhi_years %>% 
  map(~(
    aha_markets[[.x]] %>%
    estimate_market_share(id = system_id,
                          weight = ffs_total_cases,
                          market = hrrnum) %>% 
    arrange(hrrnum,desc(market_share))%>%
    left_join(aha_markets[[.x]] %>% select(system_id,sysname) %>% unique(), "system_id") %>% 
    left_join(aha_markets[[.x]] %>% filter(sysname=="") %>% select(system_id,mname),"system_id") %>% 
    mutate(sysname = ifelse(sysname=="",NA,sysname)) %>% 
    mutate(name = coalesce(sysname,mname)) %>% 
    select(hrrnum,name,market_share, hhi, everything())
  )) %>% 
  set_names(hhi_years)

ms_hrr %>% write_rds(path = here("output/market-comparisons/01_hrr-market-shares.rds"))


hhi_cz <-
  hhi_years %>% 
  map(~(
    aha_markets[[.x]] %>%
    estimate_hhi(id = system_id,
                 weight = ffs_total_cases,
                 market = cz_id) %>% 
    rename(hhi_cz = hhi,
           total_weight_cz = total_weight) %>% 
    left_join(
      aha_markets[[.x]] %>%
        estimate_hhi(id = system_id,
                     weight = admtot,
                     market = cz_id) %>% 
        rename(hhi_cz_admtot = hhi,
               total_weight_cz_admtot = total_weight) ,
      "cz_id"
    )
  )) %>% 
  set_names(hhi_years)

ms_cz <-
  hhi_years %>% 
  map(~(
    aha_markets[[.x]] %>%
    estimate_market_share(id = system_id,
                 weight = ffs_total_cases,
                 market = cz_id) %>% 
    arrange(cz_id,desc(market_share))%>%
    left_join(aha_markets[[.x]] %>% select(system_id,sysname) %>% unique(), "system_id") %>% 
    left_join(aha_markets[[.x]] %>% filter(sysname=="") %>% select(system_id,mname),"system_id") %>% 
    mutate(sysname = ifelse(sysname=="",NA,sysname)) %>% 
    mutate(name = coalesce(sysname,mname)) %>% 
    select(cz_id,name,market_share, hhi, everything())
  )) %>% 
  set_names(hhi_years) 

ms_cz %>% write_rds(path = here("output/market-comparisons/01_commuting-zone-market-shares.rds"))


hhi_cd <-
  hhi_years %>% 
  map(~(
    aha_markets[[.x]] %>%
      estimate_hhi(id = system_id,
                   weight = ffs_total_cases,
                   market = cd114fp) %>% 
      rename(hhi_cd = hhi,
             total_weight_cd = total_weight) %>% 
      ungroup() %>% 
      mutate(cd114fp = paste0(cd114fp)) %>% 
      left_join(
        aha_markets[[.x]] %>%
          estimate_hhi(id = system_id,
                       weight = admtot,
                       market = cd114fp ) %>% 
          rename(hhi_cz_admtot = hhi,
                 total_weight_cz_admtot = total_weight) %>% 
          ungroup() %>% 
          mutate(cd114fp = paste0(cd114fp)),
        "cd114fp"
      )
  )) %>% 
  set_names(hhi_years)

ms_cd <-
  hhi_years %>% 
  map(~(
    aha_markets[[.x]] %>%
      estimate_market_share(id = system_id,
                            weight = ffs_total_cases,
                            market = cd114fp) %>% 
      arrange(cd114fp,desc(market_share))%>%
      left_join(aha_markets[[.x]] %>% select(system_id,sysname) %>% unique(), "system_id") %>% 
      left_join(aha_markets[[.x]] %>% filter(sysname=="") %>% select(system_id,mname),"system_id") %>% 
      mutate(sysname = ifelse(sysname=="",NA,sysname)) %>% 
      mutate(name = coalesce(sysname,mname)) %>% 
      select(cd114fp,name,market_share, hhi, everything())
  )) %>% 
  set_names(hhi_years) 

ms_cd %>% write_rds(path = here("output/market-comparisons/01_congressional-district-market-shares.rds"))

## ZIP and Hopsital-Level HHIs
plan(multiprocess)
  zip_hhi <-
    hhi_years %>% 
    future_map(~(
      df_hosp_zip[[.x]] %>% 
      inner_join(aha_markets[[.x]],"prvnumgrp") %>% 
      estimate_hhi(id = system_id,
                   weight = total_cases,
                   market = zip_code) %>% 
      rename(hhi_zip = hhi)
    )) %>% 
    set_names(hhi_years)
  
  zip_market_shares <-
    hhi_years %>% 
    future_map(~(
      df_hosp_zip[[.x]] %>% 
      inner_join(aha_markets[[.x]],"prvnumgrp") %>% 
      estimate_market_share(id = system_id,
                            weight = total_cases,
                            market = zip_code) %>% 
      arrange(zip_code,desc(market_share))%>%
      left_join(aha_markets[[.x]] %>% select(system_id,sysname) %>% unique(), "system_id") %>% 
      left_join(aha_markets[[.x]] %>% filter(sysname=="") %>% select(system_id,mname),"system_id") %>% 
      mutate(sysname = ifelse(sysname=="",NA,sysname)) %>% 
      mutate(name = coalesce(sysname,mname)) %>% 
      select(zip_code,name,market_share, hhi, everything())  %>% 
      filter(zip_code!="00000") 
    )) %>% 
    set_names(hhi_years)
    
  zip_market_shares %>% 
    write_rds(path = here("output/market-comparisons/01_ZIP-market-shares.rds"))


  ###############################
  ## Hospital-Level HHI Measures
  ###############################
  
  minimum_market_share_zip <- 0.05
  minimum_market_share_hosp <- 0.05
  
  convert_to_bipartite <- function(df,id) {
    id <- enquo(id)
    nn <- df %>% pull(!!id)
    foo <- df %>% select(-!!id) %>%
      as.matrix()
    
    rownames(foo) <- nn
    foo
  }
  
  
  for (yy in hhi_years) {
    cat(yy)
    cat("\nPreparing Data ... \n")
    df_hosp_zip_yy <- 
      df_hosp_zip[[paste0(yy)]] %>% 
      group_by(prvnumgrp) %>% 
      mutate(market_share_hosp = total_cases/ sum(total_cases,na.rm=TRUE)) %>% 
      group_by(zip_code) %>% 
      mutate(market_share_zip = total_cases / sum(total_cases,na.rm =TRUE))  %>% 
      filter(market_share_zip>minimum_market_share_zip | market_share_hosp>minimum_market_share_hosp ) %>% 
      inner_join(aha_markets[[paste0(yy)]],"prvnumgrp") %>% 
      rename(hosp_x = longitude,
             hosp_y = latitude) %>% 
      group_by(zip_code) %>% 
      mutate(market_share = total_cases / sum(total_cases,na.rm=TRUE)) %>% 
      ungroup()
    
    bp_zip_hosp_weighted <-
      df_hosp_zip_yy %>%
      #left_join(aha_markets[[.x]],"prvnumgrp") %>% 
      select(prvnumgrp,zip_code,total_cases,system_id) %>% 
      select(-system_id) %>% 
      group_by(prvnumgrp,zip_code) %>% 
      summarise_at(vars(total_cases),function(x) sum(x,na.rm=TRUE)) %>% 
      spread(prvnumgrp, total_cases) %>%
      convert_to_bipartite(id = zip_code)
    bp_zip_hosp_weighted[is.na(bp_zip_hosp_weighted)] <- 0
    
    cat("Calculating bipartite matrix ...\n")
    if  (!file.exists(paste0("output/market-comparisons/01_up-weighted-prvnumgrp-",yy,".rds"))) {
      up_prvnumgrp_weighted <- t(bp_zip_hosp_weighted>0) %*% bp_zip_hosp_weighted
      saveRDS(up_prvnumgrp_weighted,file=paste0("output/market-comparisons/01_up-weighted-prvnumgrp-",yy,".rds"))
    } else {
      up_sysid_weighted <- readRDS(paste0("output/market-comparisons/01_up-weighted-prvnumgrp-",yy,".rds"))
    }
    
    hhi_km2 <- function(bp, threshold = 0.005) {
      
      # bp <- bp_zip_hosp_weighted
      # For every ZIP code of patient residence k = 1, ..., K, the 
      # predicted probabilities translate into a predicted share of 
      # patients from zip k going to hospital k.
      
      alpha_jk <- 100*t(apply(bp,1,function(x) x/sum(x)))
      
      # The predicted HHI for patients in ZIP k is 
      
      HHI_k_pat = apply(alpha_jk^2,1,sum)
      
      # beta_kj represents the share of hosptial j's predicted demand coming from zip code k
      
      beta_kj <- apply(bp,2,function(x) x/sum(x))
      
      beta_kj[beta_kj<=threshold] <- 0
      
      HHI_k <- apply(apply(beta_kj,2,function(x) x * HHI_k_pat),2,sum)
      
      return(HHI_k)
    }
    
    
    cat("Calculating HHI ...\n")
    #hhi_kess_mcclellan <-  hhi_km(bp_zip_hosp_weighted)
    hhi_kess_mcclellan <- hhi_km2(bp_zip_hosp_weighted)
    hhi_network <- hhi_net(up_sysid_weighted)
    
    # get_hospital_system_hhi <- function(up) {
    #   market_shares <-  100 * (up / apply(up,1,sum))
    #   hhi <- apply(market_shares,1,function(x) sum(x^2))
    #   out <- list(hhi = hhi, market_share_matrix = market_shares)
    #   return(out)
    # }
    # 
    # hhi_network2 <- get_hospital_system_hhi(up_sysid_weighted)$hhi
    # 
    
    df_hhi_hosp <-
      aha_markets[[yy]] %>% 
      mutate(hhi_km = hhi_kess_mcclellan[prvnumgrp]) %>% 
      mutate(hhi_net = hhi_network[prvnumgrp]) %>% 
      mutate(year = yy)
    
    saveRDS(df_hhi_hosp,file=paste0("output/market-comparisons/01_hospital-level-hhi-",yy,".rds"))
  }
  
  
  
  hhi_hospital <-   
    hhi_years %>% 
    map(~(
      readRDS(df_hhi_hosp,file=paste0("output/market-comparisons/01_hospital-level-hhi-",.x,".rds")) %>% 
        mutate(hrrnum= paste0(hrrnum),
               hsanum = paste0(hsanum)) %>% 
        select(-mcrnum)
    )) %>% 
    set_names(hhi_years) 
  
  hhi_hospital %>% 
    bind_rows() %>% 
    tbl_df() %>% 
    filter(prvnumgrp=="440039") %>% 
    select(mname, prvnumgrp, hhi_net)
  
  hhi_hospital %>% 
    bind_rows() %>% 
    tbl_df() %>% 
    mutate_at(vars(year),as.numeric) %>% 
    ggplot(aes(x = jitter(year), y = hhi_km)) + geom_smooth(se = FALSE) + 
    theme_bw() +
    scale_y_continuous(limits = c(0,10000),breaks = c(0,1500,2500,5000,10000)) + 
    geom_smooth(se = FALSE, aes(y = hhi_net)) 
  
  
  plan(multiprocess)
  zip_hhi_hosp <-
    hhi_years %>% 
    future_map(~(
      df_hosp_zip[[.x]] %>% 
        inner_join(hhi_hospital[[.x]] %>% 
                     select(prvnumgrp, contains("hhi")),"prvnumgrp") %>% 
        group_by(zip_code) %>% 
        mutate(weight = total_cases/sum(total_cases,na.rm=TRUE)) %>% 
        #filter(zip_code=="37023") %>% 
        mutate(hhi_km = hhi_km * weight,
               hhi_net = hhi_net * weight) %>% 
        summarise_at(vars(hhi_km,hhi_net),function(x) sum(x,na.rm=TRUE)) %>% 
        left_join(zip_hhi[[.x]])
    )) %>% 
    set_names(hhi_years)
  
  saveRDS(zip_hhi_hosp,file=paste0("output/market-comparisons/01_hospital-level-hhi-aggregated-to-zip.rds"))
  

# Link Market-Level HHI Measures to "COMMON UNIT" (i.e., county) to facilitate comparison
  
  # Crosswalk from county to HRR
  county_to_hrr <- read_csv(here("public-data/shape-files/dartmouth-hrr-hsa-pcsa/county-to-hrr-hsa.csv")) %>% 
    janitor::clean_names() %>% 
    filter(row_number()!=1)  %>% 
    mutate_at(vars(hrr,pop10,afact,afact2), as.numeric) %>%
    rename(fips_code = county) %>% 
    # Roll up to HRR level
    select(fips_code,hrr,afact) %>% 
    group_by(fips_code,hrr) %>% 
    summarise(afact = sum(afact, na.rm=TRUE)) %>% 
    arrange(fips_code,desc(afact)) %>% 
    group_by(fips_code) %>% 
    # Select the HRR with the largest county area in it. 
    filter(row_number()==1) %>% 
    ungroup() %>% 
    select(fips_code,hrrnum = hrr, hrr_afact = afact)
  
  # Cosswalk from county to commuting zone.
  county_to_cz <- data.table::fread(here("public-data/shape-files/commuting-zones/counties10-zqvz0r.csv")) %>% 
    janitor::clean_names() %>% 
    rename(fips_code = fips) %>% 
    group_by(out10) %>% 
    mutate(commuting_zone_population_2010 = sum(pop10, na.rm=TRUE)) %>% 
    mutate(fips_code = str_pad(paste0(fips_code),width = 5, pad="0")) %>% 
    select(fips_code,
           cz_id = out10)
  
  county_to_rating_area <- 
    read_rds(here("output/geographic-crosswalks/01_county-to-rating-area-all-states.rds")) %>% 
    data.frame() %>% 
    unique() %>% 
    select(fips_code,rating_area,pct_overlap) %>% 
    group_by(fips_code) %>% 
    group_by(fips_code,pct_overlap) %>% 
    # Just keep one per county (use the one with hte largest overlap)
    filter(row_number()==1) %>% 
    ungroup()
  
  
  # Need to aggregate ZIP level data up to county
  
  zip_to_county <- read_csv(here("public-data/zcta-to-fips-county/zcta-to-fips-county.csv")) %>% 
    janitor::clean_names() %>% 
    filter(row_number() !=1) %>% 
    mutate(fips_code = county) %>% 
    select(zip_code = zcta5, fips_code,afact) %>% 
    mutate(afact = as.numeric(paste0(afact))) 
  
  zip_to_hrr <- read_csv(here("public-data/shape-files/dartmouth-hrr-hsa-pcsa/zcta-to-hrr-hsa.csv")) %>% 
    janitor::clean_names() %>% 
    filter(row_number() !=1) %>% 
    select(zip_code = zcta5, hrrnum = hrr, afact) %>% 
    mutate_at(vars(hrrnum,afact),function(x) as.numeric(paste0(x)))
  
  zip_to_cd <- 
    data.table::fread(here("public-data/shape-files/congressional-district/zip-to-114th-congressional-district.csv")) %>% 
    filter(row_number()>1) %>% 
    janitor::clean_names() %>% 
    select(zip_code = zcta5,cd114fp = cd114,state = stab,afact) %>%  
    mutate(afact = as.numeric(paste0(afact))) %>% 
    mutate(cd114fp = paste0(state,cd114fp)) %>% 
    tbl_df()
    
  zip_hhi_aggregated_to_county <-
    names(zip_hhi_hosp) %>% 
     map(~(zip_hhi_hosp[[.x]] %>% 
      inner_join(zip_to_county,"zip_code") %>% 
      mutate(weight = afact * total_weight) %>% 
      group_by(fips_code) %>% 
      summarise(hhi_zip = weighted.mean(hhi_zip,weight,na.rm=TRUE),
                hhi_net = weighted.mean(hhi_net,weight,na.rm=TRUE),
                hhi_km = weighted.mean(hhi_km,weight,na.rm=TRUE))
    )) %>% 
    set_names(names(zip_hhi_hosp))
  
  zip_hhi_aggregated_to_hrr <-
    names(zip_hhi_hosp) %>%  
    map(~( zip_hhi_hosp[[.x]] %>% 
      inner_join(zip_to_hrr,"zip_code") %>% 
      mutate(weight = afact * total_weight) %>% 
      group_by(hrrnum) %>% 
        summarise(hhi_zip = weighted.mean(hhi_zip,weight,na.rm=TRUE),
                  hhi_net = weighted.mean(hhi_net,weight,na.rm=TRUE),
                  hhi_km = weighted.mean(hhi_km,weight,na.rm=TRUE))
    ))  %>% 
    set_names(names(zip_hhi_hosp))
  
  zip_hhi_aggregated_to_cz <- 
    names(zip_hhi_hosp) %>%  
    map(~(zip_hhi_hosp[[.x]]  %>% 
      inner_join(zip_to_county,"zip_code") %>% 
      mutate(weight = afact * total_weight) %>% 
      inner_join(county_to_cz,"fips_code") %>% 
      group_by(cz_id) %>% 
        summarise(hhi_zip = weighted.mean(hhi_zip,weight,na.rm=TRUE),
                  hhi_net = weighted.mean(hhi_net,weight,na.rm=TRUE),
                  hhi_km = weighted.mean(hhi_km,weight,na.rm=TRUE))
    )) %>% 
    set_names(names(zip_hhi_hosp))
  
  zip_hhi_aggregated_to_cd <- 
    names(zip_hhi_hosp) %>%  
    map(~(zip_hhi_hosp[[.x]]  %>% 
            inner_join(zip_to_cd,"zip_code") %>% 
            mutate(weight = afact * total_weight) %>% 
            group_by(cd114fp) %>% 
            summarise(hhi_zip = weighted.mean(hhi_zip,weight,na.rm=TRUE),
                      hhi_net = weighted.mean(hhi_net,weight,na.rm=TRUE),
                      hhi_km = weighted.mean(hhi_km,weight,na.rm=TRUE))
    )) %>% 
    set_names(names(zip_hhi_hosp))
  

  
  
  zip_to_rating_area <- 
    read_rds(here("output/geographic-crosswalks/01_zcta-to-rating-area-all-states.rds"))
  
  zip_hhi_aggregated_to_ra <- 
    names(zip_hhi_hosp) %>%  
    map(~(zip_hhi_hosp[[.x]]  %>% 
            inner_join(zip_to_rating_area,"zip_code") %>% 
            mutate(weight =pct_of_zip_in_rating_area * total_weight) %>% 
            group_by(rating_area) %>% 
            summarise(hhi_zip = weighted.mean(hhi_zip,weight,na.rm=TRUE),
                      hhi_net = weighted.mean(hhi_net,weight,na.rm=TRUE),
                      hhi_km = weighted.mean(hhi_km,weight,na.rm=TRUE))
    )) %>% 
    set_names(names(zip_hhi_hosp))
  
  
  df_county <- 
    hhi_years %>% 
    map(~(
    county_to_cz %>% 
    full_join(county_to_hrr,"fips_code") %>% 
    full_join(county_to_rating_area,"fips_code") %>% 
    left_join(hhi_cz[[.x]]  ,"cz_id") %>% 
    left_join(hhi_rating_area[[.x]] ,"rating_area") %>% 
    mutate(hrrnum = paste0(hrrnum)) %>% 
    left_join(hhi_hrr[[.x]] %>% ungroup() %>%  mutate(hrrnum = paste0(hrrnum)) ,"hrrnum") %>% 
    left_join(zip_hhi_aggregated_to_county[[.x]],"fips_code") %>% 
    select(fips_code,hrrnum,cz_id,rating_area,contains("hhi"))
    )) %>% 
    set_names(hhi_years)
  
  df_county %>% write_rds(here("output/market-comparisons/01_market-comparisons-county.rds"))
  
# Look at Alternative Definitions at the commuting zone level. 
hhi_cz_final <-
  hhi_years %>% 
  map(~(
    hhi_cz[[.x]] %>% 
    left_join(zip_hhi_aggregated_to_cz[[.x]],"cz_id")
  )) %>% 
  set_names(hhi_years) %>% 
  bind_rows(.id="year")
hhi_cz_final %>% 
  write_rds(here("output/market-comparisons/01_HHI_genacute_cz.rds"))
s3saveRDS(hhi_cz_final,
          bucket = paste0(project_bucket,"/market-comparisons"), 
          object = "/01_HHI_genacute_cz.rds")


hhi_cd_final <-
  hhi_years %>% 
  map(~(
    hhi_cd[[.x]] %>% 
      left_join(zip_hhi_aggregated_to_cd[[.x]],"cd114fp")
  )) %>% 
  set_names(hhi_years) %>% 
  bind_rows(.id="year")
hhi_cd_final %>% 
  write_rds(here("output/market-comparisons/01_HHI_genacute_cd.rds"))
s3saveRDS(hhi_cz_final,
          bucket = paste0(project_bucket,"/market-comparisons"), 
          object = "/01_HHI_genacute_cd.rds")

hhi_hrr_final <- 
  hhi_years %>% 
  map(~(
    hhi_hrr[[.x]] %>% 
      ungroup() %>% 
    mutate(hrrnum = paste0(hrrnum)) %>% 
    left_join(zip_hhi_aggregated_to_hrr[[.x]] %>% ungroup() %>%  mutate(hrrnum = paste0(hrrnum)),"hrrnum")
  )) %>% set_names(hhi_years) %>% 
  bind_rows(.id="year")
hhi_hrr_final %>% 
  write_rds(here("output/market-comparisons/01_HHI_genacute_hrr.rds"))
s3saveRDS(hhi_hrr_final,
          bucket = paste0(project_bucket,"/market-comparisons"), 
          object = "/01_HHI_genacute_hrr.rds")

hhi_rating_area_final <- 
  hhi_years %>% 
  map(~(
    hhi_rating_area[[.x]] %>% 
      left_join(zip_hhi_aggregated_to_ra[[.x]],"rating_area")
  )) %>% set_names(hhi_years) %>% 
  bind_rows(.id="year")
hhi_rating_area_final %>% 
  write_rds(here("output/market-comparisons/01_HHI_genacute_rating_area.rds"))
s3saveRDS(hhi_rating_area_final,
          bucket = paste0(project_bucket,"/market-comparisons"), 
          object = "/01_HHI_genacute_rating_area.rds")


####################
#### Construct Maps
####################

states_to_map <- c("KY","TN","VA","NC")
# 
#   sf_hrr %>% 
#   left_join(hhi_hrr,"hrrnum") %>% 
#   filter(hrrstate %in%  states_to_map) %>% 
#   ggplot() + 
#   geom_sf(aes(fill = hhi_hrr)) +
#     scale_fill_gradient2(low = scales::muted("blue"),mid = "white",high = scales::muted("red"),midpoint = 2500,limits = c(0,10000)) + 
#   theme(legend.position = "bottom") +
#   geom_sf(data = sf_state %>% filter(stusps %in% states_to_map), alpha = 0,lwd=.7,colour = "black") + 
#   coord_sf(datum=NA) + 
#   remove_all_axes +
#   ggtitle("Hospital Referral Regions") + 
#   ggthemes::theme_tufte(base_family = "Gill Sans")
# ggsave( filename = here("figs/01_HHI_hrr.png"),dpi = 300, scale =1)
# 
# 
#   sf_cz %>% 
#   left_join(hhi_cz,"cz_id") %>% 
#   filter(state_01 %in% states_to_map | state_02 %in% states_to_map  | state_03 %in% states_to_map) %>% 
#   ggplot() + 
#   geom_sf(aes(fill = hhi_cz)) +
#     scale_fill_gradient2(low = scales::muted("blue"),mid = "white",high = scales::muted("red"),midpoint = 2500,limits = c(0,10000)) + 
#   #theme(legend.position = "bottom") +
#   geom_sf(data = sf_state %>% filter(stusps %in% states_to_map), alpha = 0,lwd=.7,colour = "black") + 
#   coord_sf(datum=NA) + 
#   remove_all_axes +
#   ggtitle("Commuting Zones") + 
#   ggthemes::theme_tufte(base_family = "Gill Sans")
# ggsave(filename = here("figs/01_HHI_commuting-zones.png"),dpi = 300, scale =1)

# ZIP LEVEL MEASURES

p1 =   sf_cz %>% 
  left_join(hhi_cz[["2018"]],"cz_id") %>% 
  filter(state_01 %in% states_to_map | state_02 %in% states_to_map  | state_03 %in% states_to_map) %>% 
  ggplot() + 
  geom_sf(aes(fill = hhi_cz)) +
  scale_fill_gradient2(low = scales::muted("blue"),mid = "white",high = scales::muted("red"),midpoint = 2500,limits = c(0,10000)) + 
  #theme(legend.position = "bottom") +
  geom_sf(data = sf_state %>% filter(stusps %in% states_to_map), alpha = 0,lwd=.7,colour = "black") + 
  coord_sf(datum=NA) + 
  remove_all_axes +
  ggtitle("Commuting Zones\n(Geographic Location Method)") + 
  ggthemes::theme_tufte(base_family = "Gill Sans")

p2 = sf_cz %>% 
  left_join(zip_hhi_aggregated_to_cz[["2018"]] ,"cz_id") %>% 
  filter(state_01 %in% states_to_map | state_02 %in% states_to_map  | state_03 %in% states_to_map) %>% 
  ggplot() + 
  geom_sf(aes(fill = hhi_zip)) +
  scale_fill_gradient2(low = scales::muted("blue"),mid = "white",high = scales::muted("red"),midpoint = 2500,limits = c(0,10000)) + 
  #theme(legend.position = "bottom") +
  geom_sf(data = sf_state %>% filter(stusps %in% states_to_map), alpha = 0,lwd=.7,colour = "black") + 
  coord_sf(datum=NA) + 
  remove_all_axes +
  ggtitle("Commuting Zones\n(Patient Flow Method)") + 
  ggthemes::theme_tufte(base_family = "Gill Sans")

p3 = sf_cz %>% 
  left_join(zip_hhi_aggregated_to_cz[["2018"]] ,"cz_id") %>% 
  filter(state_01 %in% states_to_map | state_02 %in% states_to_map  | state_03 %in% states_to_map) %>% 
  ggplot() + 
  geom_sf(aes(fill = hhi_net)) +
  scale_fill_gradient2(low = scales::muted("blue"),mid = "white",high = scales::muted("red"),midpoint = 2500,limits = c(0,10000)) + 
  #theme(legend.position = "bottom") +
  geom_sf(data = sf_state %>% filter(stusps %in% states_to_map), alpha = 0,lwd=.7,colour = "black") + 
  coord_sf(datum=NA) + 
  remove_all_axes +
  ggtitle("Commuting Zones\n(Joint Competition Method)") + 
  ggthemes::theme_tufte(base_family = "Gill Sans")

p4 = sf_cz %>% 
  left_join(zip_hhi_aggregated_to_cz[["2018"]] ,"cz_id") %>% 
  filter(state_01 %in% states_to_map | state_02 %in% states_to_map  | state_03 %in% states_to_map) %>% 
  ggplot() + 
  geom_sf(aes(fill = hhi_km)) +
  scale_fill_gradient2(low = scales::muted("blue"),mid = "white",high = scales::muted("red"),midpoint = 2500,limits = c(0,10000)) + 
  #theme(legend.position = "bottom") +
  geom_sf(data = sf_state %>% filter(stusps %in% states_to_map), alpha = 0,lwd=.7,colour = "black") + 
  coord_sf(datum=NA) + 
  remove_all_axes +
  ggtitle("Commuting Zones\n(Kessler-McClellan Method)") + 
  ggthemes::theme_tufte(base_family = "Gill Sans")



p1 + p2 + p3 + p4 + plot_layout(ncol=2,nrow=2)
ggsave(filename = here("figs/01_HHI_commuting-zones.png"),dpi = 300, scale =1,width = 12, height=12)

p1_hrr =   sf_hrr %>% 
  left_join(hhi_hrr[["2018"]],"hrrnum") %>% 
  filter(hrrstate %in% states_to_map) %>% 
  ggplot() + 
  geom_sf(aes(fill = hhi_hrr)) +
  scale_fill_gradient2(low = scales::muted("blue"),mid = "white",high = scales::muted("red"),midpoint = 2500,limits = c(0,10000)) + 
  #theme(legend.position = "bottom") +
  geom_sf(data = sf_state %>% filter(stusps %in% states_to_map), alpha = 0,lwd=.7,colour = "black") + 
  coord_sf(datum=NA) + 
  remove_all_axes +
  ggtitle("Hospital Referral Region\n(Geographic Location Method)") + 
  ggthemes::theme_tufte(base_family = "Gill Sans")

p2_hrr = sf_hrr %>% 
  left_join(hhi_hrr_final %>% filter(year==2018) %>% mutate(hrrnum = as.numeric(paste0(hrrnum))),"hrrnum") %>% 
  filter(hrrstate %in% states_to_map) %>% 
  ggplot() + 
  geom_sf(aes(fill = hhi_zip))+
  scale_fill_gradient2(low = scales::muted("blue"),mid = "white",high = scales::muted("red"),midpoint = 2500,limits = c(0,10000)) + 
  #theme(legend.position = "bottom") +
  geom_sf(data = sf_state %>% filter(stusps %in% states_to_map), alpha = 0,lwd=.7,colour = "black") + 
  coord_sf(datum=NA) + 
  remove_all_axes +
  ggtitle("Hospital Referral Region\n(Patient Flow Method)") + 
  ggthemes::theme_tufte(base_family = "Gill Sans")


p3_hrr = sf_hrr %>% 
  left_join(hhi_hrr_final %>% filter(year==2018) %>% mutate(hrrnum = as.numeric(paste0(hrrnum))),"hrrnum") %>% 
  filter(hrrstate %in% states_to_map) %>% 
  ggplot() + 
  geom_sf(aes(fill = hhi_net))+
  scale_fill_gradient2(low = scales::muted("blue"),mid = "white",high = scales::muted("red"),midpoint = 2500,limits = c(0,10000)) + 
  #theme(legend.position = "bottom") +
  geom_sf(data = sf_state %>% filter(stusps %in% states_to_map), alpha = 0,lwd=.7,colour = "black") + 
  coord_sf(datum=NA) + 
  remove_all_axes +
  ggtitle("Commuting Zones\n(Joint Competition Method)") + 
  ggthemes::theme_tufte(base_family = "Gill Sans")


p4_hrr = sf_hrr %>% 
  left_join(hhi_hrr_final %>% filter(year==2018) %>% mutate(hrrnum = as.numeric(paste0(hrrnum))),"hrrnum") %>% 
  filter(hrrstate %in% states_to_map) %>% 
  ggplot() + 
  geom_sf(aes(fill = hhi_km))+
  scale_fill_gradient2(low = scales::muted("blue"),mid = "white",high = scales::muted("red"),midpoint = 2500,limits = c(0,10000)) + 
  #theme(legend.position = "bottom") +
  geom_sf(data = sf_state %>% filter(stusps %in% states_to_map), alpha = 0,lwd=.7,colour = "black") + 
  coord_sf(datum=NA) + 
  remove_all_axes +
  ggtitle("Commuting Zones\n(Kessler-McClellan Method)") + 
  ggthemes::theme_tufte(base_family = "Gill Sans")

p1_hrr + p2_hrr + p3_hrr + p4_hrr + plot_layout(ncol=2,nrow=2)
ggsave(filename = here("figs/01_HHI_hrr.png"),dpi = 300, scale =1,width = 12, height=12)

# p1 + p1_hrr + p2 + p2_hrr + plot_layout(ncol=2,nrow=2)
# ggsave(filename = here("figs/01_HHI_geo-location-vs-pop-flow.png"),dpi = 300, scale =1,width = 12, height=12)


