# This file reads in and clean the MA provider travel time file
# Export csv file "MA_travel_time.csv" at zip_specialty level 
# (crosswalk ssa to zips 1:m, zips are assigned to fips based on which fips polygon a zip centroild falls in)

# library(tidyverse)
# library(readxl)
# library(rgdal)
# library(rgeos)
# library(sp)

raw <- read_excel("HSD Reference File (01-10-2017).xlsx",sheet = "Provider Time & Distance",skip = 1)

#### 0. clean names & data ####
spec <- str_subset(names(raw)[-c(1:5)],"[0-9]",negate = TRUE) %>% str_replace(" \\(see Notes\\)","")
rnam <- spec %>% map(~paste0(.x,c(":time",":distance"))) %>% unlist()
names(raw) <- c("cnty_name","st","cnty_st","ssa","cnty_type",rnam)

# ssa to fips
# https://www.nber.org/data/ssa-fips-state-county-crosswalk.html
ssa_fips <- read.csv("ssa_fips_state_county2017.csv") %>% mutate(ssa=sprintf("%05d",ssacounty),fips=sprintf("%05d",fipscounty)) %>% 
    select(ssa,fips)
cleaned <- raw[-c(1,2),] %>% select_at(c("cnty_name","st","ssa","cnty_type",rnam)) %>% mutate_at(rnam,as.numeric) %>%
    inner_join(ssa_fips,by="ssa") #some ssa cannot be linked to fips, dropped

#### 1. reshape ####
xx <- cleaned %>% 
    #wide to long
    gather("key","value",-cnty_name,-st,-ssa,-fips,-cnty_type) %>% 
    separate(key,c("spec","type"),":") %>%
    #long to wide
    spread(type,value) %>% 
    mutate(spec2=ifelse(spec=="Cardiology","cardiology",NA),
           spec2=ifelse(spec=="Primary Care","primcare",spec2),
           spec2=ifelse(spec=="Orthopedic Surgery","orthsurg",spec2),
           spec2=ifelse(spec=="Psychiatry","behavioral",spec2),
           spec2=ifelse(spec=="Oncology - Medical, Surgical","oncology",spec2)) %>% 
    filter(!is.na(spec2))
xxs <- xx %>% filter(spec2=="primcare") %>% mutate(spec2="pediatrics")
xx <- xx %>% bind_rows(xxs) %>% select(-spec,-distance) %>% spread(spec2,time)


#### 2. shapefile exercise ####
zip.shp <- readOGR(dsn="public-data/zcta-2017/tl_2017_us_zcta510/tl_2017_us_zcta510.shp",
                   layer = "tl_2017_us_zcta510",verbose = FALSE)
cnty.shp <- readOGR(dsn="public-data/county-2017/cb_2017_us_county_5m/cb_2017_us_county_5m.shp",
                    layer = "cb_2017_us_county_5m",verbose = FALSE)

# extract zip centroids
zip.cent <- coordinates(zip.shp) #long,lat
cnty.cent <- coordinates(cnty.shp)
cleaned_poly <-  1:length(cnty.shp) %>% map(~cnty.shp@polygons[[.x]]@Polygons)

# rank distance between zip/cnty centroids in order to reduce iterations when matching
# https://stackoverflow.com/questions/21977720/r-finding-closest-neighboring-point-and-number-of-neighbors-within-a-given-rad
zz <- SpatialPointsDataFrame(zip.cent,data=data.frame(id=1:length(zip.shp)))
ff <- SpatialPointsDataFrame(cnty.cent,data=data.frame(id=1:length(cnty.shp)))
dd <- gDistance(zz,ff,byid = T)
ranked.d <- 1:length(zip.shp) %>% map(~order(dd[,.x]))

# check 1 zip to 1 fips
check1 <- function(tz,tp) {
    vec <- 1:length(tp) %>% map_dbl(~point.in.polygon(tz[1],tz[2],tp[[.x]]@coords[,1],tp[[.x]]@coords[,2]))
    if(any(vec==1)) 1 else sum(vec)
}

# iterate to find perfect assignment (point inside polygon) for each zip
inside_poly <- function(x) {
    i <- 1
    ed <- 0
    while(ed!=1 & i<=length(cnty.shp)) {
        n <- ranked.d[[x]][i]
        ed <- check1(zip.cent[x,],cleaned_poly[[n]])
        i <- i+1
    }
    return(data.frame(zip_id=x,rank=i-1,result=ed,fips_id=n))
}
match1 <- 1:length(zip.shp) %>% map_df(~inside_poly(.x))

# # try those without a perfect match, i.e. on the edge or vertex (does not help recover any case)
# proxy_poly <- function(x) {
#     i <- 1
#     ed <- 0
#     while(ed==0 & i<=length(cnty.shp)) {
#         n <- ranked.d[[x]][i]
#         ed <- check1(zip.cent[x,],cleaned_poly[[n]])
#         i <- i+1
#     }
#     return(data.frame(zip_id=x,rank=i-1,result=ed,fips_id=n))
# }
# match2 <- match1 %>% filter(result==0) %>% pull(zip_id) %>% map_df(~proxy_poly(.x))

#### 3. link back to fips, zips ####
zips <- data.frame(zip_id=1:length(zip.shp),zip_code=as.character(zip.shp@data$ZCTA5CE10),stringsAsFactors = F)
fips <- cnty.shp@data %>% mutate(fips_code=paste0(STATEFP,COUNTYFP)) %>% distinct(fips_code) %>% 
    bind_cols(data.frame(fips_id=1:length(cnty.shp)))
belonged.poly <- match1 %>% left_join(zips) %>% left_join(fips) %>% select(zip_code,fips_code,result)

cleaned.time <- xx %>% inner_join(belonged.poly,by=c("fips"="fips_code")) %>% select(-result)
write.csv(cleaned.time,file="MA_travel_time.csv",row.names = F)