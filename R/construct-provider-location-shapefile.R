# This file creates shapefiles for SK&A (2017/2019) and AHA 2017 NPI location (zip centroid)

# library(data.table)
# library(tidyverse)
# library(rgdal)
# library(sp)
# library(haven)
# library(furrr)
# library(tmaptools)

### import SK&A, AHA ###
ska_clean <- function(x) {
    x  %>% mutate(zip5 = str_sub(zip,1,5),
                  sp = ifelse(spec %in% c("CAR", "PDC"),"cardiology",NA),
                  sp = ifelse(spec %in% c("FMP", "GNP", "INT"),"primcare",sp),
                  sp = ifelse(spec %in% c("ORC", "ORS", "OSN", "OFA"),"orthsurg",sp),
                  sp = ifelse(spec %in% c("PSY","PSC"),"behavioral",sp),
                  sp = ifelse(spec %in% c("PED","IMP"),"pediatrics",sp),
                  sp = ifelse(spec %in% c("ONC","GYO","PHO","RDO","ONL"),"oncology",sp)) %>%
        filter(!is.na(sp),!is.na(npi)) %>% select(npi,spec=sp,zip5,fips)
}
ska17 <- fread("OM359543_5-8-2017_Final.CSV") %>% ska_clean()
ska19 <- fread("OM380190_4-5-2019_Final.CSV") %>% ska_clean()

filename <- paste0("~/Box Sync/Research-AHA_Data/data/aha/annual/aha",2017,".dta")
aha17 <- read_dta(file=filename,encoding = "latin1") %>% rename_all(.funs=tolower) %>% 
    filter(!is.na(npinum)) %>% 
    select(npinum,longitude,lat,serv,mloczip,mstate,fcounty) %>% 
    #flip positive longitude
    mutate(longitude=ifelse(longitude>0,(-1)*longitude,longitude))

### geocode with 2017 zip shape file ###
zip.shp <- readOGR(dsn="./zcta-2017/tl_2017_us_zcta510/tl_2017_us_zcta510.shp",
                   layer = "tl_2017_us_zcta510",verbose = FALSE)
zip.cent <- coordinates(zip.shp) # zip centroids: long,lat
zip_xw <-  data.frame(zip=as.character(zip.shp@data$ZCTA5CE10),stringsAsFactors = F)
zip_xw$longitude <- zip.cent[,1]
zip_xw$lat <- zip.cent[,2]

geoc <- function(x) {
    x %>% left_join(zip_xw,by=c("zip5"="zip"))
}
geocoded <- list(ska17,ska19) %>% map(~geoc(.x)) %>% set_names(c("ska17","ska19"))

# some zips not in the shape file, try to geocode using tmaptools
zips_to_geocode <- map(geocoded, function(x) x %>% filter(is.na(lat)) %>% pull(zip5)) %>% unlist() %>% unique() 
plan(multiprocess)
geocoded_zips <- future_map(paste0(zips_to_geocode,",USA"),
                            safely(function(x) geocode_OSM(x)$coords %>% t() %>% tbl_df())) %>% purrr::transpose()
geocoded.missing <- geocoded_zips$result %>% set_names(zips_to_geocode) %>% 
    bind_rows(.id="zip5") %>% select(zip5,longitude=x,lat=y)

#merge back
merge.back <- function(x) {
    x %>% select(-longitude,-lat) %>% inner_join(geocoded.missing ,by="zip5") %>% 
        bind_rows(x %>% filter(!is.na(lat)))
}
geocoded <- geocoded %>% map(~merge.back(.x))

#verify weird coordinates from AHA
sp <- aha17 %>% filter(longitude>0 | lat<0) %>% mutate(zip5=substr(mloczip,1,5))
sp.zip <- unique(sp$zip5)
sp.coded <- future_map(paste0(sp.zip,",USA"),
                       safely(function(x) geocode_OSM(x)$coords %>% t() %>% tbl_df())) %>% purrr::transpose()
sp <- sp.coded$result %>% set_names(sp.zip) %>% 
    bind_rows(.id="zip5") %>% select(zip5,x,y) %>% right_join(sp,by="zip5"); head(sp)

aha17 <- aha17 %>% mutate(longitude=ifelse(longitude>0,(-1)*longitude,longitude))

### create and export shape file ###
gene_shp <- function(x,folder,layername) {
    pts <- x %>% select(longitude,lat)
    points_spdf <- SpatialPointsDataFrame(coords = pts, data=x,
                                          proj4string = CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))
    writeOGR(obj=points_spdf,dsn=folder,layer=layername, driver="ESRI Shapefile",overwrite_layer=TRUE)
} 
gene_shp(geocoded$ska17,"spdf_shp","ska17_npi")
gene_shp(geocoded$ska19,"spdf_shp","ska19_npi")
gene_shp(aha17,"spdf_shp","aha17_npi")

### add Vericred 2019 data ###
veri19 <- fread("~/Downloads/2019 March Network Package/providers.csv") %>% filter(!is.na(id))

#verify weird coordinates from Vericred 2019: all seem to be US territory
sp2 <- veri19 %>% filter(longitude>0 | latitude<0) %>% mutate(zip5=str_pad(zip, 5, pad = "0")) %>% 
    select(id,city,state,zip5,latitude,longitude,zip)
sp.zip2 <- unique(sp2$zip)
sp.coded2 <- future_map(paste0(sp.zip2,",USA"),
                        safely(function(x) geocode_OSM(x)$coords %>% t() %>% tbl_df())) %>% purrr::transpose()
sp2 <- sp.coded2$result %>% set_names(sp.zip2) %>% 
    bind_rows(.id="zip5") %>% select(zip5,x,y) %>% right_join(sp2,by="zip5"); head(sp2)
sp.corrected2 <- sp2 %>% mutate(longitude=longitude*(x*longitude)/abs(x*longitude),
                                latitude=latitude*(y*latitude)/abs(y*latitude)) %>% 
    select(city,state,id,zip,long=longitude,lat=latitude)

veri19 <- veri19 %>% left_join(sp.corrected2,by=c("id","city","state","zip")) %>% 
    mutate(longitude=ifelse(!is.na(long),long,longitude),
           latitude=ifelse(!is.na(lat),lat,latitude)) %>% 
    select(-lat,-long)

# Version 1: keep providers with name information and identified as one of study specialites based on Vericred data
v1 <- veri19 %>% filter(first_name!="" | last_name!="") %>% 
    mutate(sp=tolower(specialty),
           spec=ifelse(sp=="spec/tech, cardiovascular","cardiology",NA),
           spec=ifelse(sp %in% c("family medicine","internal medicine","general practice"),"primcare",spec),
           spec=ifelse(sp=="orthopaedic surgery" ,"orthsurg",spec),
           spec=ifelse(sp %in% c("psychologist","psychiatry & neurology"),"behavioral",spec),
           spec=ifelse(sp=="pediatrics","pediatrics",spec)
    ) %>% filter(!is.na(spec)) %>% 
    select(npi=id,spec,city,state,zip,latitude,longitude)

# Version 2: merge with SK&A and code speciality based on SK&A data
v2 <- veri19 %>% inner_join(ska19,by=c("id"="npi")) %>% select(npi=id,spec,city,state,zip,latitude,longitude)

# export both versions
gene_shp(v2,"vericred19_npi_ska_spec")
gene_shp(v1,"vericred19_npi_ori_spec")
