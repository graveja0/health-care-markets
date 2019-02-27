suppressWarnings(suppressMessages(source(here::here("/R/manifest.R"))))
source(here("R/map-theme.R"))
source(here("R/shared-objects.R"))
source(here("R/get-market-from-x-y-coordinates.R"))
source(here("R/estimate_hhi.R"))
library(sf)

rename_in_list <- function(x,from, to) {
  x %>% rename_at(vars(contains(from)), funs(sub(from, to, .)))
}

aha_files <- c("2017" = "../../../box/Research-AHA_Data/data/aha/annual/raw/2017/FY2017 ASDB/COMMA/ASPUB17.CSV",
               "2016" = "../../../box/Research-AHA_Data/data/aha/annual/raw/2016/FY2016 Annual Survey Database/COMMA/ASPUB16.CSV",
               "2015" = "../../../box/Research-AHA_Data/data/aha/annual/raw/2015/FY2015 Annual Survey Database/COMMA/ASPUB15.CSV",
               "2014" = "../../../box/Research-AHA_Data/data/aha/annual/raw/2014/FY2014 ASDB/COMMA/ASPUB14.CSV",
               "2013" = "../../../box/Research-AHA_Data/data/aha/annual/raw/2013/FY2013 ASDB/COMMA/ASPUB13.CSV",
               "2012" = "../../../box/Research-AHA_Data/data/aha/annual/raw/2012/COMMA/ASPUB12.csv",
               "2011" = "../../../box/Research-AHA_Data/data/aha/annual/raw/2011/FY2011 ASDB/COMMA/ASPUB11.csv.csv",
               "2010" = "../../../box/Research-AHA_Data/data/aha/annual/raw/2010/FY2010 ASDB/COMMA/ASPUB10.csv",
               "2009" = "../../../box/Research-AHA_Data/data/aha/annual/raw/2009/FY2009 ASDB/COMMA/ASPUB09.csv",
               "2008" = "../../../box/Research-AHA_Data/data/aha/annual/raw/2008/FY2008 ASDB/COMMA/pubas08.csv",
               "2007" = "../../../box/Research-AHA_Data/data/aha/annual/raw/2007/FY2007 ASDB/COMMA/pubas07.csv",
               "2006" = "../../../box/Research-AHA_Data/data/aha/annual/raw/2006/FY2006 ASDB/COMMA/pubas06.csv"
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
          mutate(hrrnum = paste0(hrrnum)) %>% 
          mutate(hsanum = paste0(hsanum)) %>% 
          mutate(prvnumgrp = str_pad(mcrnum,width = 6, pad="0")) %>% 
          mutate(hosp_zip_code = str_sub(mloczip,1,5)) %>% 
          mutate(longitude = as.numeric(paste0(longitude))) %>% 
          mutate(latitude = as.numeric(paste0(latitude))) %>% 
          filter(!is.na(longitude) & !is.na(latitude))
  )) %>% 
  set_names(names(aha_files)) #%>% 
  #bind_rows(.id = "year") %>% 
  #mutate(year = as.numeric(year))

dim(.x)
dim(.y)

.x <- st_as_sf(aha[["2016"]],coords = c("longitude","latitude"), crs = 4326) %>% 
  mutate(name = ifelse(sysname=="",mname,sysname))
.y <- st_as_sf(aha[["2017"]],coords = c("longitude","latitude"), crs = 4326) %>% 
  mutate(name = ifelse(sysname=="",mname,sysname))

distances <- st_distance(.x,.y)
names_y1 <- .x$name
which_is_closest <- .y$name[apply(distances,1,function(x) which(x==min(x))[1]) %>% unlist()]
distance_to_closest <- apply(distances,1,function(x) min(x)[1])

foo <- data.frame(id = .x$id , mname= paste0(.x$mname), sysname = paste0(.x$sysname) , name0 = names_y1, closest = which_is_closest, distance = distance_to_closest) %>% 
  tbl_df() %>% 
  mutate(name0 = paste0(name0),
         closest = paste0(closest))

possible_mergers <- foo %>% filter(distance==0 & name0!=closest) 
possible_closures <- foo %>% filter(distance>100)
