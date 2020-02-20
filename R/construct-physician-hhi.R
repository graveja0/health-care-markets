#' ---
#' output: github_document
#' ---

# The objective of this file is to construct physician-level HHI measures.

suppressWarnings(suppressMessages(source(here::here("/R/manifest.R"))))
source(here("R/map-theme.R"))
source(here("R/shared-objects.R"))
source(here("R/get-market-from-x-y-coordinates.R"))
source(here("R/estimate_hhi.R"))

Yto1 <- function(x) ifelse(x=="Y" | x=="Yes",1,ifelse(x=="N" | x=="No",0,NA))

specialty_groupings <-
  list(
    cardiology = c("CAR", "PDC","CARDIOVASCULAR DISEASE (CARDIOLOGY)","INTERVENTIONAL CARDIOLOGY","ADVANCED HEART FAILURE AND TRANSPLANT CARDIOLOGY"),
    primcare = c("FMP", "GNP", "INT", "FAMILY MEDICINE", "GENERAL PRACTICE","INTERNAL MEDICINE" ),
    orthsurg = c("ORC", "ORS", "OSN", "OFA","ORTHOPEDIC SURGERY"), 
    behavioral = c("PSY","PSC","PSYCHOLOGIST, CLINICAL", "PSYCHIATRY" ,            "NEUROPSYCHIATRY"   ,     "GERIATRIC PSYCHIATRY" ),
    pediatrics = c("PED","IMP","PEDIATRIC MEDICINE"), 
    oncology = c("ONC","GYO","PHO","RDO","ONL","RADIATION ONCOLOGY"  ,   "HEMATOLOGY/ONCOLOGY" ,   "MEDICAL ONCOLOGY" ,      "SURGICAL ONCOLOGY" ,     "GYNECOLOGICAL ONCOLOGY"), 
    hospital = "GAC",
    emergency = c("EMR","EMERGENCY MEDICINE", "Emergency Medicine"),
    radiology = c("DRD","NER","PDR","RAD","DIAGNOSTIC RADIOLOGY" ,    "INTERVENTIONAL RADIOLOGY","Radiology" ) ,
    anesthes = c("ANS","ANESTHESIOLOGY" ,"Anesthesiology" )
  )

construct_ska_data = FALSE 


# 2017 SK&A Data File
dir.ska <- "../../box/Research-Provider_Networks/data/ska/"


if (construct_ska_data) {
  
  ######
  # 2020-02-03: Commenting this out as these files are now created in Research-Provider_Networks/aim1-breadth/R/construct-ska-data.R
  ######
  
  
  # ska_files <- list.files(file.path(dir.ska),recursive=TRUE, pattern = ".csv|.CSV")
  # 
  # ska_lut <- 
  #   c("ska-2017/OM359543_5-8-2017_Final.CSV" = "2017",
  #     "Raw Files/OM335137_10-9-2015_Final.CSV" = "2015",
  #     "Raw Files/OM335137B_10-9-2015_Final.CSV" = "2013",
  #     "Raw Files/OM335137C_10-9-2015_Final.CSV" = "2011",
  #     "Raw Files/OM335137D_10-9-2015_Final.CSV" = "2009",
  #     "ska-2017/OM359543B_5-8-2017_Final.CSV" = "2007"
  #   ) 
  # 
  # 
  # zcta_to_xy <- read_csv(here("public-data/zcta-to-fips-county/zcta-to-fips-county.csv")) %>% 
  #   filter(row_number()!=1) %>% 
  #   select(zip_code=zcta5, state = stab, longitude = intptlon, latitude = intptlat, fips_code=county, afact) %>% 
  #   group_by(zip_code) %>% 
  #   mutate(n_counties=n())  %>% 
  #   arrange(desc(afact)) %>% 
  #   unique()
  # 
  # df_ska <- 
  #   ska_files  %>% 
  #   map(~(
  #     data.table::fread(file.path(dir.ska,.x)) %>% tbl_df() %>% 
  #       janitor::clean_names() %>%
  #       mutate(file = .x) %>% 
  #       mutate(year = ska_lut[file])
  #   )) %>% 
  #   bind_rows() %>% 
  #   tbl_df() %>% 
  #   mutate_at(vars(do,newpatient,medicare,medicaid,emrsoftwar,emrprescri,emrviewlab,emrcapture,emailavail),funs(Yto1)) %>% 
  #   mutate(change=as.Date(change,"%m/%d/%Y"),
  #          dob=as.Date(dob,"%m/%d/%Y"),
  #          graduate=as.Date(graduate,"%m/%d/%Y"),
  #          timefrno=as.Date(timefrno,"%m/%d/%Y"),
  #          timefryes=as.Date(timefryes,"%m/%d/%Y"),
  #          emrverify=as.Date(emrverify,"%m/%d/%Y")) %>% 
  #   mutate(zip_code = str_pad(str_sub(zip,1,5),width=5,pad="0")) %>% 
  #   mutate(fips_code = str_pad(as.numeric(paste0(fips)),width=5,pad="0")) %>% 
  #   select(-fips) %>% 
  #   left_join(zcta_to_xy,c("fips_code","zip_code")) %>% 
  #   mutate(zip3 = substr(zip,1,3)) %>% 
  #   # 2013 & 2015 Changes (per Michael Richards) 
  #   mutate(fips_code = ifelse(zip3=="190" & year==2013,"42045",fips_code)) %>% 
  #   mutate(fips_code = ifelse(zip3=="503" & year==2013,"19153",fips_code)) %>% 
  #   mutate(fips_code = ifelse(zip3=="544" & year==2013,"55097",fips_code)) %>% 
  #   mutate(fips_code = ifelse(zip3=="841" & year==2013,"49035",fips_code)) %>% 
  #   mutate(fips_code = ifelse(zip3=="850" & year==2013,"04013",fips_code)) %>% 
  #   mutate(fips_code = ifelse(zip3=="851" & year==2013,"04021",fips_code)) %>% 
  #   mutate(fips_code = ifelse(zip3=="852" & year==2013,"04013",fips_code)) %>% 
  #   mutate(zip3 = ifelse(fips_code=="31157" & year==2015,"693",zip3)) 
  # 
  #   df_ska <- df_ska %>% 
  #   mutate(in_group_practice = as.integer(!is.na(code3) & code3 != ""),
  #          in_health_system = as.integer(!is.na(code4) & code4 != ""),
  #          in_hospital_system = as.integer(!is.na(code5) & code5 != ""),
  #          has_hospital_affiliation = as.integer(!is.na(code7) & code7 !="")) %>% 
  #   mutate(system_id = ifelse(in_health_system==1, paste0("HLTHSYS_",code4), 
  #                             ifelse(in_hospital_system==1, paste0("HOSPSYS_",code5),
  #                                    ifelse(in_group_practice ==1 , paste0("GRP_",code3),paste0("IND_",id))))) 
  # 
  # # Add specialty indicators
  # df_ska <- 
  #   specialty_groupings %>% 
  #   map(
  #     ~(df_ska %>% mutate(tmp = as.integer(spec %in% .x))%>% pull(tmp))
  #   ) %>% 
  #   set_names(names(specialty_groupings)) %>% 
  #   bind_cols() %>% 
  #   bind_cols(df_ska,.) 
  # 
  # # Geocode Locations - Validate XY data in SK&A data. 
  #   set.seed(123)
  #   zips_to_geocode <- df_ska %>% mutate(zip5 = str_sub(zip,1,5)) %>% pull(zip) %>% unique()  %>% sample(100)
  #   library(furrr)
  #   library(tmaptools)
  #   plan(multiprocess)
  #   geocoded_zips <- 
  #     zips_to_geocode %>%
  #     future_map(~geocode_OSM(.x)$coords %>% t() %>% tbl_df() ) %>% 
  #     set_names(zips_to_geocode) %>% 
  #     bind_rows(.id = "zip")
  #   df_ska %>% filter(zip %in% zips_to_geocode) %>% select(zip,longitude,latitude) %>% unique() %>% inner_join(geocoded_zips,"zip")
  # 
  # ska_name <- paste0("ska-",paste0(range(df_ska$year),collapse="-"),".rds")
  # 
  # latest_ska_year <- max(df_ska$year)
  # ska_name_latest <- paste0("ska-",paste0(latest_ska_year,collapse="-"),".rds")
  # write_rds(df_ska %>% filter(year == latest_ska_year), path = file.path(here("output/ska"),ska_name_latest))
} else {
  # df_ska <- read_rds(here("output/ska/ska-2017.rds"))
  # 
 # df_ska <- read_rds(here("output/ska/ska-2019.rds"))
  df_ska <- 
    get_aws_files(prefix = "data/data-aim1-breadth/ska") %>% 
    filter(grepl("ska-2019.rds",value)) %>% 
    pull(value) %>% 
    s3readRDS(., bucket = "vumc.graves.networks.proj") %>% 
    filter(!is.na(npi))
}

pcsa_info <- foreign::read.dbf(here("public-data/shape-files/dartmouth-hrr-hsa-pcsa/ct_pcsav31.dbf")) %>% 
  janitor::clean_names() %>% 
  select(pcsa,pcsa_st, pcsa_l) %>% 
  mutate_at(vars(pcsa,pcsa_st,pcsa_l),funs(paste0)) %>% 
  unique()

zcta_to_pcsa <- foreign::read.dbf(here("public-data/shape-files/dartmouth-hrr-hsa-pcsa/zip5_pcsav31.dbf")) %>% 
  janitor::clean_names() %>% 
  mutate(pcsa = paste0(pcsa)) %>% 
  left_join(pcsa_info,"pcsa") %>% 
  rename(zip_code = zip5)

# ZCTA to HRR and HSA Crosswalk
zcta_to_hrr_hsa <- read_csv(here("public-data/shape-files/nber-hrr-hsa-pcsa/ziphsahrr2014.csv")) %>% 
  janitor::clean_names() %>% 
  rename(zip_code = zipcode ) %>% 
  select(-year)

# Source: https://sites.psu.edu/psucz/data/
county_to_cz <- data.table::fread(here("public-data/shape-files/commuting-zones/counties10-zqvz0r.csv")) %>% 
  janitor::clean_names() %>% 
  rename(fips_code = fips) %>% 
  group_by(out10) %>% 
  mutate(commuting_zone_population_2010 = sum(pop10, na.rm=TRUE)) %>% 
  mutate(fips_code = str_pad(paste0(fips_code),width = 5, pad="0")) %>% 
  select(fips_code,
         cz_id= out10,
         commuting_zone_population_2010 ) 

# Crosswalk from county to rating area 
# !!!!!! TK NOTE THIS WILL MISS LA COUNTY AS WELL AS ALL COUNTIES IN NE, AK, AND MA. 
# Need to use geographic mapping code to assign those rating areas to counties. 

county_to_rating_area <- 
  read_rds(here("output/geographic-crosswalks/01_rating-areas_counties_2019.rds")) %>% 
  data.frame() %>% 
  unique() %>% 
  select(fips_code,rating_area)

ndf_md_hhi <-
  specialty_groupings %>%
  map(~(
    df_ska %>% 
      filter(spec %in% .x) 
  )) %>% 
  set_names(names(specialty_groupings)) %>% 
  bind_rows(.id = "specialty") %>% 
  select(year,specialty,system_id,id,uid,npi,everything())  %>% 
  left_join(county_to_cz,"fips_code") %>% 
  mutate(zip_code = substr(zip,1,5)) %>% 
  left_join(zcta_to_pcsa,"zip_code") %>%
  left_join(zcta_to_hrr_hsa,"zip_code") %>% 
  left_join(county_to_rating_area,"fips_code") %>% 
  ###################################################
  # Could merge in some other panel size estimate here. 
  mutate(panel_size = 1) %>% 
  ###################################################
  group_by(year,specialty) %>% 
  nest()  %>% 
  mutate(hhi_pcsa = map(data,~(
    .x %>% 
      estimate_hhi(id = system_id,
                   weight = panel_size,
                   market = pcsa) 
  ))) %>% 
  mutate(hhi_hrr = map(data,~(
    .x %>% 
      estimate_hhi(id = system_id,
                   weight = panel_size,
                   market = hrrnum) 
  ))) %>% 
  mutate(hhi_hsa = map(data,~(
    .x %>% 
      estimate_hhi(id = system_id,
                   weight = panel_size,
                   market = hsanum) 
  ))) %>% 
  mutate(hhi_cz = map(data,~(
    .x %>% 
      estimate_hhi(id = system_id,
                   weight = panel_size,
                   market = cz_id) 
  ))) %>% 
  mutate(hhi_ra = map(data,~(
    .x %>% 
      estimate_hhi(id = system_id,
                   weight = panel_size,
                   market = rating_area) 
  ))) %>% 
  mutate(hhi_fips = map(data,~(
    .x %>% 
      estimate_hhi(id = system_id,
                   weight = panel_size,
                   market = fips_code) 
  )))


df_md_hhi_hrr <- 
  ndf_md_hhi %>% 
  select(year,specialty,hhi_hrr) %>% 
  unnest()  %>% 
  gather(var,value,-hrrnum,-specialty,-year) %>% 
  unite(specialty,var,specialty) %>% 
  spread(specialty,value)

df_md_hhi_hsa <- 
  ndf_md_hhi %>% 
  select(year,specialty,hhi_hsa) %>% 
  unnest()  %>% 
  gather(var,value,-hsanum,-specialty,-year) %>% 
  unite(specialty,var,specialty) %>% 
  spread(specialty,value)

df_md_hhi_pcsa <- 
  ndf_md_hhi %>% 
  select(year,specialty,hhi_pcsa) %>% 
  unnest()  %>% 
  gather(var,value,-pcsa,-specialty,-year) %>% 
  unite(specialty,var,specialty) %>% 
  spread(specialty,value)

df_md_hhi_cz <- 
  ndf_md_hhi %>% 
  select(year,specialty,hhi_cz) %>% 
  unnest()  %>% 
  gather(var,value,-cz_id,-specialty,-year) %>% 
  unite(specialty,var,specialty) %>% 
  spread(specialty,value)

df_md_hhi_cz  %>% 
  write_rds(here("output/market-comparisons/01_HHI_md_cz-2019.rds"))

df_md_hhi_county <- 
  ndf_md_hhi %>% 
  select(year,specialty,hhi_fips) %>% 
  unnest()  %>% 
  gather(var,value,-fips_code,-specialty,-year) %>% 
  unite(specialty,var,specialty) %>% 
  spread(specialty,value)

df_md_hhi_ra <- 
  ndf_md_hhi %>% 
  select(year,specialty,hhi_ra) %>% 
  unnest()  %>% 
  gather(var,value,-rating_area,-specialty,-year) %>% 
  unite(specialty,var,specialty) %>% 
  spread(specialty,value)

# Load Shapefiles
sf_pcsa <- read_sf(here("output/tidy-mapping-files/pcsa/01_pcsa-shape-file.shp"))  %>% 
  st_transform(crs = 4326)
sf_hrr <- read_sf(here("output/tidy-mapping-files/hrr/01_hrr-shape-file.shp"))  %>% 
  st_transform(crs = 4326)
sf_cz <- read_sf(here("output/tidy-mapping-files/commuting-zone/01_commuting-zone-shape-file.shp")) %>% 
  st_transform(crs = 4326)
sf_ra <- read_sf(here("output/tidy-mapping-files/rating-area/01_rating-area-shape-file.shp")) %>%
  st_transform(crs = 4326) %>% 
  mutate(rating_area = ratng_r)
sf_state <- read_sf(here("output/tidy-mapping-files/state/01_state-shape-file.shp")) %>% 
  st_transform(crs = 4326)
sf_county <- read_sf(here("output/tidy-mapping-files/county/01_county-shape-file.shp")) %>% 
  st_transform(crs = 4326)




states_to_map <- c("AL","GA","KY","TN","VA","NC","SC")


p1 <- sf_county %>% 
  left_join(df_md_hhi_county %>% filter(year==2019),"fips_code") %>% 
  filter(state %in% states_to_map ) %>% 
  ggplot() + 
  geom_sf(aes(fill =hhi_primcare)) +
  scale_fill_gradient2(low = scales::muted("blue"),mid = "white",high = scales::muted("red"),midpoint = 2500,limits = c(0,10000)) + 
  #theme(legend.position = "bottom") +
  geom_sf(data = sf_state %>% filter(stusps %in% states_to_map), alpha = 0,lwd=.7,colour = "black") + 
  coord_sf(datum=NA) + 
  remove_all_axes +
  ggtitle("Primary Care\nCounty") + 
  ggthemes::theme_tufte(base_family = "Gill Sans")


p2 <- sf_cz %>% 
  left_join(df_md_hhi_cz %>% filter(year==2019),"cz_id") %>% 
  filter(state_01 %in% states_to_map | state_02 %in% states_to_map  | state_03 %in% states_to_map) %>% 
  ggplot() + 
  geom_sf(aes(fill = hhi_primcare)) +
  scale_fill_gradient2(low = scales::muted("blue"),mid = "white",high = scales::muted("red"),midpoint = 2500,limits = c(0,10000)) + 
  #theme(legend.position = "bottom") +
  geom_sf(data = sf_state %>% filter(stusps %in% states_to_map), alpha = 0,lwd=.7,colour = "black") + 
  coord_sf(datum=NA) + 
  remove_all_axes +
  ggtitle("Primary Care\nCommuting Zones") + 
  ggthemes::theme_tufte(base_family = "Gill Sans")


p3 <- sf_hrr %>% 
  left_join(df_md_hhi_hrr %>% filter(year==2019),"hrrnum") %>% 
  filter(hrrstate %in% states_to_map) %>% 
  ggplot() + 
  geom_sf(aes(fill = hhi_primcare)) +
  scale_fill_gradient2(low = scales::muted("blue"),mid = "white",high = scales::muted("red"),midpoint = 2500,limits = c(0,10000)) + 
  #theme(legend.position = "bottom") +
  geom_sf(data = sf_state %>% filter(stusps %in% states_to_map), alpha = 0,lwd=.7,colour = "black") + 
  coord_sf(datum=NA) + 
  remove_all_axes +
  ggtitle("Primary Care\nHospital Referral Region") + 
  ggthemes::theme_tufte(base_family = "Gill Sans")

p4 <- sf_ra %>% 
  left_join(df_md_hhi_ra %>% filter(year==2019),"rating_area") %>% 
  filter(state %in% states_to_map) %>% 
  ggplot() + 
  geom_sf(aes(fill = hhi_primcare)) +
  scale_fill_gradient2(low = scales::muted("blue"),mid = "white",high = scales::muted("red"),midpoint = 2500,limits = c(0,10000)) + 
  #theme(legend.position = "bottom") +
  geom_sf(data = sf_state %>% filter(stusps %in% states_to_map), alpha = 0,lwd=.7,colour = "black") + 
  coord_sf(datum=NA) + 
  remove_all_axes +
  ggtitle("Primary Care\nRating Area") + 
  ggthemes::theme_tufte(base_family = "Gill Sans")

p5 <- sf_pcsa %>% 
  left_join(df_md_hhi_pcsa %>% filter(year==2019),"pcsa") %>% 
  filter(pcsa_st %in% states_to_map) %>% 
  ggplot() + 
  geom_sf(aes(fill = hhi_primcare)) +
  scale_fill_gradient2(low = scales::muted("blue"),mid = "white",high = scales::muted("red"),midpoint = 2500,limits = c(0,10000)) + 
  #theme(legend.position = "bottom") +
  geom_sf(data = sf_state %>% filter(stusps %in% states_to_map), alpha = 0,lwd=.7,colour = "black") + 
  coord_sf(datum=NA) + 
  remove_all_axes +
  ggtitle("Primary Care\nPrimary Care Service Area") + 
  ggthemes::theme_tufte(base_family = "Gill Sans")


p1 + p2 + p3 + p4  + p5 +  plot_layout(ncol=2, nrow=3)
ggsave(filename = here("figs/01_HHI_primary-care.png"),dpi = 300, scale =1,width = 12, height=16)


