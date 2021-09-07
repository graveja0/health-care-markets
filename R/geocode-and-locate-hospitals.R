#' ---
#' output: github_document
#' ---

# The objective of this file is to geocode hosptials within their ZIP

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
sf_zip <- read_sf(here("output/tidy-mapping-files/zcta/01_zcta-shape-file.shp")) %>%
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
  
  # Assign each AHA hosptial to its congressional district.
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
  
  
  # Assign each AHA hosptial to its ZIP code
  plan(multiprocess)
  aha_zip <- 
    aha %>% 
    future_map(~(
      get_market_from_xy(df = ., 
                         x = longitude, 
                         y = latitude,
                         sf = sf_zip,
                         market_id = zcta5ce10 )
    ))
  
  df_zip <- 
    map2(aha_cd114,aha,~(.x %>% 
                           set_names(.y[,"id"]) %>% 
                           unlist() %>% 
                           data.frame() %>% 
                           rownames_to_column() %>% 
                           set_names(c("id","zcta5ce10"))
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


# For Becky Staiger and Joe Doyle (2020-03-03)
library(here)
df_aha_geocoded <- 
  list.files(here(paste0("output/market-comparisons/"))) %>% 
  grep("01_aha-markets",.,value =TRUE) %>% 
  map(~(
    read_rds(file.path(here(paste0("output/market-comparisons/"),.x))) %>% 
      mutate(mloczip = str_pad(mloczip,pad = "0",width = 5)) %>% 
      mutate(hrrnum = str_pad(hrrnum,pad="0",width =3)) %>% 
      mutate(hsanum = str_pad(hsanum,pad="0",width =3)) %>% 
      mutate(mcrnum = str_pad(mcrnum,pad="0",width =6)) %>%       
      mutate(hosp_zip_code = str_pad(hosp_zip_code,pad = "0", width = 5)) %>% 
      mutate(year = as.numeric(gsub("01_aha-markets-|.rds","",.x) )) %>% 
      mutate(different_from_aha = as.integer(substr(mloczip,1,5) != hosp_zip_code))
  )) %>% 
  bind_rows() %>% 
  select(year,id,prvnumgrp,mname,latitude,longitude,hrrnum,hsanum,mloczip,geocoded_zip = hosp_zip_code,different_from_aha,fips_code,rating_area,commuting_zone = cz_id) %>% 
  haven::write_dta(here("output/ignore/aha-hospital-geocoding.dta"))


