#' ---
#' output: github_document
#' ---
#' 
#' 

hosp_zip_years <- c("2013","2014","2015","2016",'2017')
hosp_zip_years <- c("2010","2011","2012")
hosp_zip_years <- c("2018")

zip_xy <- read_csv(here("public-data/zcta-to-fips-county/zcta-to-fips-county.csv")) %>% 
  janitor::clean_names() %>% 
  filter(row_number() !=1) %>% 
  mutate(fips_code = county) %>% 
  rename(zip_code = zcta5) %>% 
  arrange(zip_code,fips_code,afact) %>% 
  group_by(zip_code) %>% 
  filter(afact==max(afact)) %>% 
  ungroup() %>% 
  select(zip_code,zip_lon = intptlon, zip_lat = intptlat ) 

hosp_xy<-
  hosp_zip_years %>% 
  map(~(
    read_rds(here(paste0("output/market-comparisons/01_aha-markets-",.x,".rds")))  %>% 
    filter(!is.na(longitude) & !is.na(latitude)) %>% 
    select(prvnumgrp, hosp_lon = longitude, hosp_lat = latitude) %>% 
    group_by(prvnumgrp) %>% 
    filter(row_number()==1) %>% 
    ungroup()# %>% 
   # mutate(prvnumgrp=as.character(paste0(prvnumgrp)))
  )) %>% 
  set_names(hosp_zip_years)

# Load the hospital-zip service file constructed in "R/read-and-tidy-cms-hospital-service-areas.R")
tmp <- 
  hosp_zip_years %>% 
  map(~(
    read_rds(here(paste0("output/hospital-county-patient-data/",.x,"/hospital-zip-patient-data.rds"))) %>% 
     # mutate(prvnumgrp=as.character(paste0(prvnumgrp))) %>% 
    arrange(prvnumgrp,desc(total_cases)) %>% 
    inner_join(zip_xy,"zip_code") %>% 
    inner_join(hosp_xy[[.x]],"prvnumgrp") %>% 
    group_by(prvnumgrp) %>% 
    mutate_at(vars(zip_lat,zip_lon),as.numeric) %>% 
    nest() 
  )) %>% 
  set_names(hosp_zip_years)

library(furrr)
plan(multiprocess)

df_distances <- 
  tmp %>% 
  future_map(~(
    .x %>% 
    mutate(distance = 
      map(data,~(
        st_distance(
          .x %>% st_as_sf(coords = c("zip_lon","zip_lat"), crs=4326, agr="constant") ,
          .x %>% select(hosp_lon,hosp_lat) %>% unique() %>% st_as_sf(coords = c("hosp_lon","hosp_lat"), crs = 4326, agr="constant")
        ) %>% tbl_df() %>% 
          rename(distance=value) %>% 
          mutate(miles = distance / 1609.34) 
      ))
    ) 
  ),.progress=TRUE)

names(df_distances) %>% 
  walk(~(df_distances[[.x]] %>% 
  unnest() %>% 
  write_rds(here(paste0("output/market-comparisons/01_zip-hospital-distances-",.x,".rds"))) 
  )) %>% 
  walk(~(
    s3saveRDS(df_distances[[.x]],
              bucket = paste0(project_bucket,"/market-comparisons"), 
              object = paste0("01_zip-hospital-distances-",.x,".rds"))
  ))

df_average_dist <- 
  df_distances %>% 
    map(~(.x %>% 
    unnest() %>% 
    group_by(prvnumgrp) %>% 
    summarise(average_miles = weighted.mean(miles,w = total_cases, na.rm=TRUE))
  ))

names(df_average_dist) %>% 
  walk(~(df_average_dist[[.x]] %>% 
           unnest() %>% 
           write_rds(here(paste0("output/market-comparisons/01_zip-hospital-average-distances-",.x,".rds"))) 
  )) %>% 
  walk(~(
    s3saveRDS(df_average_dist[[.x]],
              bucket = paste0(project_bucket,"/market-comparisons"), 
              object = paste0("01_zip-hospital-average-distances-",.x,".rds"))
  ))


