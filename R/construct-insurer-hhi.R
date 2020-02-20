#' ---
#' output: github_document
#' ---

# Note do we need to split CZs into CZ-states? Otherwise HHI measures from one state will be informed by those from another.

suppressWarnings(suppressMessages(source(here::here("/R/manifest.R"))))
source(here("R/map-theme.R"))
source(here("R/shared-objects.R"))
source(here("R/get-market-from-x-y-coordinates.R"))
source(here("R/estimate_hhi.R"))
library(readxl)

threshold_for_inclusion = 0.01

# This file constructs the insurer HHI measures using DRG data.


df_drg_16 <-data.table::fread(file.path(fs::path_home(),"box/Research-Provider_Networks/aim1-breadth/input/drg/drg-mms-2016-2017.csv"))  %>% 
  filter(month==7) %>% 
  filter(company_name=="") %>% 
  select(year,fips_code,mco_name,parent_id,total_book,commercial,comm_si, comm_fi,medicare,medicaid,phxind,phxshop) %>% 
  mutate(fips_code = str_pad(fips_code,width=5,pad="0")) %>% 
  mutate_at(vars(total_book,commercial,comm_si, comm_fi,medicare,medicaid,phxind,phxshop),
            function(x) ifelse(is.na(x),0,x))  %>% 
  gather(market,enrollment,-fips_code,-mco_name,-parent_id,-year) %>%
  unique() %>% 
  group_by(year,fips_code,market) %>% 
  mutate(enrollment = suppressWarnings(as.numeric(paste0(enrollment)))) %>% 
  mutate(pct = enrollment / sum(enrollment,na.rm=TRUE)) %>% 
  filter(pct > threshold_for_inclusion)  %>% 
  group_by(year,market) %>% 
  nest()  %>% 
  mutate(hhi = map(data,~(
    .x %>% 
      group_by(fips_code) %>% 
      estimate_hhi(id = parent_id,
                   weight = enrollment,
                   market=fips_code) 
  ))) %>% 
  select(year,market,hhi) %>% 
  unnest()  %>% 
  gather(var,value,-fips_code,-market,-year) %>% 
  unite(market,var,market) %>% 
  spread(market,value)

df_drg_19 <- 
  get_aws_files(prefix = "data/drg/") %>% 
  filter(grepl("mms_county_201901_medical_145088_1568828477.xlsx",value)) %>% 
  pull(value) %>% 
  aws.s3::s3read_using(object = ., readxl::read_xlsx,bucket =  "vumc.graves.networks.proj",skip = 14,sheet = 2) %>% 
  janitor::clean_names() %>% 
  mutate(year = 2019) %>% 
  rename(company_name = payer) %>%  
  rename(mco_name = parent) %>% 
  rename(total_book = overall) %>% 
  mutate(fips_code = str_pad(fips_county_code,width = 5, pad="0")) %>% 
  filter(company_name=="" | is.na(company_name)) %>% 
  select(year,fips_code,mco_name,parent_id,total_book,commercial,comm_si=commercial_si, 
         comm_fi = commercial_fi,medicare  = medicare_advantage_part_c,medicaid = payer_managed_medicaid,phxind = public_hix_individual,
         phxshop = public_hix_shop) %>% 
  mutate_at(vars(total_book,commercial,comm_si, comm_fi,medicare,medicaid,phxind,phxshop),
            function(x) ifelse(is.na(x),0,x))  %>% 
  gather(market,enrollment,-fips_code,-mco_name,-parent_id,-year) %>%
  unique() %>% 
  group_by(year,fips_code,market) %>% 
  mutate(enrollment = suppressWarnings(as.numeric(paste0(enrollment)))) %>% 
  mutate(pct = enrollment / sum(enrollment,na.rm=TRUE)) %>% 
  filter(pct > threshold_for_inclusion)  %>% 
  group_by(year,market) %>% 
  nest()  %>% 
  mutate(hhi = map(data,~(
    .x %>% 
      group_by(fips_code) %>% 
      estimate_hhi(id = parent_id,
                   weight = enrollment,
                   market=fips_code) 
  ))) %>% 
  select(year,market,hhi) %>% 
  unnest()  %>% 
  gather(var,value,-fips_code,-market,-year) %>% 
  unite(market,var,market) %>% 
  spread(market,value)
  
df_drg  <- 
  df_drg_16 %>% 
  bind_rows(df_drg_19)
  
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

# ms_county <- 
#   df_drg %>% 
#   group_by(fips_code) %>% 
#   mutate(total = sum(comm_si,na.rm=TRUE)) %>% 
#   mutate(pct = comm_si / total) %>% 
#   filter(pct > threshold_for_inclusion) %>% 
#   estimate_market_share(id = parent_id,
#                weight = comm_si,
#                market=fips_code) %>% 
#   left_join(df_drg %>% select(parent_id,mco_name) %>% unique(), "parent_id") %>% 
#   select(fips_code,mco_name,parent_id,market_share,hhi,everything())
# 
# ms_county %>% write_rds(path = here("output/market-comparisons/01_2017_drg-county-market-shares.rds"))

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
  #filter(row_number()==1) %>% 
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

# Crosswalk from county to rating area 
# !!!!!! TK NOTE THIS WILL MISS LA COUNTY AS WELL AS ALL COUNTIES IN NE, AK, AND MA. 
# Need to use geographic mapping code to assign those rating areas to counties. 

county_to_rating_area <- 
  read_rds(here("output/geographic-crosswalks/01_rating-areas_counties_2019.rds")) %>% 
  data.frame() %>% 
  unique() %>% 
  select(fips_code,rating_area)

drg_hhi_aggregated <-
  df_drg %>% 
  gather(variable,value,-fips_code,-year) %>% 
  mutate(market = gsub("hhi_|total_weight_","",variable)) %>% 
  mutate(measure = ifelse(grepl("hhi_",variable),"hhi","total_weight")) %>% 
  select(-variable) %>% 
  spread(measure,value) %>% 
  filter(!is.na(hhi)) %>% 
  group_by(market,year) %>% 
  nest() %>% 
  mutate(hhi_county = map(data,~(
    .x %>% rename(hhi_county = hhi) %>% 
      select(fips_code,hhi_county)
  ))) %>% 
  mutate(hhi_hrr = map(data,~(
      .x %>%  inner_join(county_to_hrr,"fips_code") %>% 
        mutate(weight = hrr_afact * total_weight) %>% 
        group_by(hrrnum) %>% 
        summarise(hhi_hrr = weighted.mean(hhi,weight,na.rm=TRUE))
    ))
  ) %>% 
  mutate(hhi_cz = map(data,~(
    .x %>%  
      inner_join(county_to_cz,"fips_code") %>% 
      mutate(weight = total_weight) %>% 
      group_by(cz_id) %>% 
      summarise(hhi_cz = weighted.mean(hhi,weight,na.rm=TRUE))
  ))) %>% 
  mutate(hhi_rating_area = map(data,~(
    .x %>%  
      inner_join(county_to_rating_area,"fips_code") %>% 
      mutate(weight = total_weight) %>% 
      group_by(rating_area) %>% 
      summarise(hhi_rating_area = weighted.mean(hhi,weight,na.rm=TRUE))
  )))

drg_hhi_county <- 
  drg_hhi_aggregated %>% 
  select(year,market,hhi_county) %>% 
  unnest() %>% 
  mutate(market = paste0("hhi_",market)) %>% 
  spread(market,hhi_county)

drg_hhi_hrr <- 
  drg_hhi_aggregated %>% 
  select(year,market,hhi_hrr) %>% 
  unnest() %>% 
  mutate(market = paste0("hhi_",market)) %>% 
  spread(market,hhi_hrr)

drg_hhi_hrr %>% 
  write_rds(here("output/market-comparisons/01_HHI_insurer_hrr.rds"))


drg_hhi_cz <- 
  drg_hhi_aggregated %>% 
  select(year,market,hhi_cz) %>% 
  unnest() %>% 
  mutate(market = paste0("hhi_",market)) %>% 
  spread(market,hhi_cz)

drg_hhi_cz %>% 
  write_rds(here("output/market-comparisons/01_HHI_insurer_cz.rds"))



drg_hhi_rating_area <- 
  drg_hhi_aggregated %>% 
  select(year,market,hhi_rating_area) %>% 
  unnest() %>% 
  mutate(market = paste0("hhi_",market)) %>% 
  spread(market,hhi_rating_area)


states_to_map <- c("AL","GA","KY","TN","VA","NC","SC")


p1 <- sf_county %>% 
  left_join(drg_hhi_county %>% filter(year==2019),"fips_code") %>% 
  filter(state %in% states_to_map ) %>% 
  ggplot() + 
  geom_sf(aes(fill =hhi_comm_si)) +
  scale_fill_gradient2(low = scales::muted("blue"),mid = "white",high = scales::muted("red"),midpoint = 2500,limits = c(0,10000)) + 
  #theme(legend.position = "bottom") +
  geom_sf(data = sf_state %>% filter(stusps %in% states_to_map), alpha = 0,lwd=.7,colour = "black") + 
  coord_sf(datum=NA) + 
  remove_all_axes +
  ggtitle("Commercial Self Insured\nCounties") + 
  ggthemes::theme_tufte(base_family = "Gill Sans")


p2 <- sf_cz %>% 
  left_join(drg_hhi_cz %>% filter(year==2019) ,"cz_id") %>% 
  filter(state_01 %in% states_to_map | state_02 %in% states_to_map  | state_03 %in% states_to_map) %>% 
  ggplot() + 
  geom_sf(aes(fill = hhi_comm_si)) +
  scale_fill_gradient2(low = scales::muted("blue"),mid = "white",high = scales::muted("red"),midpoint = 2500,limits = c(0,10000)) + 
  #theme(legend.position = "bottom") +
  geom_sf(data = sf_state %>% filter(stusps %in% states_to_map), alpha = 0,lwd=.7,colour = "black") + 
  coord_sf(datum=NA) + 
  remove_all_axes +
  ggtitle("Commercial Self Insured\nCommuting Zones") + 
  ggthemes::theme_tufte(base_family = "Gill Sans")


p3 <- sf_hrr %>% 
  left_join(drg_hhi_hrr %>% filter(year==2019) ,"hrrnum") %>% 
  filter(hrrstate %in% states_to_map) %>% 
  ggplot() + 
  geom_sf(aes(fill = hhi_comm_si)) +
  scale_fill_gradient2(low = scales::muted("blue"),mid = "white",high = scales::muted("red"),midpoint = 2500,limits = c(0,10000)) + 
  #theme(legend.position = "bottom") +
  geom_sf(data = sf_state %>% filter(stusps %in% states_to_map), alpha = 0,lwd=.7,colour = "black") + 
  coord_sf(datum=NA) + 
  remove_all_axes +
  ggtitle("Commercial Self Insured\nHospital Referral Region") + 
  ggthemes::theme_tufte(base_family = "Gill Sans")

p4 <- sf_ra %>% 
  left_join(drg_hhi_rating_area %>% filter(year==2019) ,"rating_area") %>% 
  filter(state %in% states_to_map) %>% 
  ggplot() + 
  geom_sf(aes(fill = hhi_comm_si)) +
  scale_fill_gradient2(low = scales::muted("blue"),mid = "white",high = scales::muted("red"),midpoint = 2500,limits = c(0,10000)) + 
  #theme(legend.position = "bottom") +
  geom_sf(data = sf_state %>% filter(stusps %in% states_to_map), alpha = 0,lwd=.7,colour = "black") + 
  coord_sf(datum=NA) + 
  remove_all_axes +
  ggtitle("Commercial Self Insured\nRating Area") + 
  ggthemes::theme_tufte(base_family = "Gill Sans")


p1 + p2 + p3 + p4  + plot_layout(ncol=2, nrow=2)
ggsave(filename = here("figs/01_HHI_commercial-self-insured.png"),dpi = 300, scale =1,width = 12, height=12)


p_commercial <- 
  sf_county %>% 
  left_join(drg_hhi_county  %>% filter(year==2019),"fips_code") %>% 
  filter(state %in% states_to_map ) %>% 
  ggplot() + 
  geom_sf(aes(fill =hhi_commercial)) +
  scale_fill_gradient2(low = scales::muted("blue"),mid = "white",high = scales::muted("red"),midpoint = 2500,limits = c(0,10000)) + 
  #theme(legend.position = "bottom") +
  geom_sf(data = sf_state %>% filter(stusps %in% states_to_map), alpha = 0,lwd=.7,colour = "black") + 
  coord_sf(datum=NA) + 
  remove_all_axes +
  ggtitle("Commercial Self Insured") + 
  ggthemes::theme_tufte(base_family = "Gill Sans")

p_comm_si <- 
  sf_county %>% 
  left_join(drg_hhi_county %>% filter(year==2019) ,"fips_code") %>% 
  filter(state %in% states_to_map ) %>% 
  ggplot() + 
  geom_sf(aes(fill =hhi_comm_si)) +
  scale_fill_gradient2(low = scales::muted("blue"),mid = "white",high = scales::muted("red"),midpoint = 2500,limits = c(0,10000)) + 
  #theme(legend.position = "bottom") +
  geom_sf(data = sf_state %>% filter(stusps %in% states_to_map), alpha = 0,lwd=.7,colour = "black") + 
  coord_sf(datum=NA) + 
  remove_all_axes +
  ggtitle("Commercial\n(Self and Fully Insured)") + 
  ggthemes::theme_tufte(base_family = "Gill Sans")
  
p_comm_fi <- 
  sf_county %>% 
    left_join(drg_hhi_county %>% filter(year==2019) ,"fips_code") %>% 
    filter(state %in% states_to_map ) %>% 
    ggplot() + 
    geom_sf(aes(fill =hhi_comm_fi)) +
    scale_fill_gradient2(low = scales::muted("blue"),mid = "white",high = scales::muted("red"),midpoint = 2500,limits = c(0,10000)) + 
    #theme(legend.position = "bottom") +
    geom_sf(data = sf_state %>% filter(stusps %in% states_to_map), alpha = 0,lwd=.7,colour = "black") + 
    coord_sf(datum=NA) + 
    remove_all_axes +
    ggtitle("Commercial Fully Insured") + 
    ggthemes::theme_tufte(base_family = "Gill Sans")

p_phxind <- 
  sf_county %>% 
  left_join(drg_hhi_county %>% filter(year==2019) ,"fips_code") %>% 
  filter(state %in% states_to_map ) %>% 
  ggplot() + 
  geom_sf(aes(fill =hhi_phxind)) +
  scale_fill_gradient2(low = scales::muted("blue"),mid = "white",high = scales::muted("red"),midpoint = 2500,limits = c(0,10000)) + 
  #theme(legend.position = "bottom") +
  geom_sf(data = sf_state %>% filter(stusps %in% states_to_map), alpha = 0,lwd=.7,colour = "black") + 
  coord_sf(datum=NA) + 
  remove_all_axes +
  ggtitle("Individual Market") + 
  ggthemes::theme_tufte(base_family = "Gill Sans") 

p_medicaid <- 
  sf_county %>% 
  left_join(drg_hhi_county  %>% filter(year==2019),"fips_code") %>% 
  filter(state %in% states_to_map ) %>% 
  ggplot() + 
  geom_sf(aes(fill =hhi_medicaid)) +
  scale_fill_gradient2(low = scales::muted("blue"),mid = "white",high = scales::muted("red"),midpoint = 2500,limits = c(0,10000)) + 
  #theme(legend.position = "bottom") +
  geom_sf(data = sf_state %>% filter(stusps %in% states_to_map), alpha = 0,lwd=.7,colour = "black") + 
  coord_sf(datum=NA) + 
  remove_all_axes +
  ggtitle("Medicaid Managed Care") + 
  ggthemes::theme_tufte(base_family = "Gill Sans") 


p_medicare <- 
  sf_county %>% 
  left_join(drg_hhi_county  %>% filter(year==2019),"fips_code") %>% 
  filter(state %in% states_to_map ) %>% 
  ggplot() + 
  geom_sf(aes(fill =hhi_medicare)) +
  scale_fill_gradient2(low = scales::muted("blue"),mid = "white",high = scales::muted("red"),midpoint = 2500,limits = c(0,10000)) + 
  #theme(legend.position = "bottom") +
  geom_sf(data = sf_state %>% filter(stusps %in% states_to_map), alpha = 0,lwd=.7,colour = "black") + 
  coord_sf(datum=NA) + 
  remove_all_axes +
  ggtitle("Medicare Advantage") + 
  ggthemes::theme_tufte(base_family = "Gill Sans") 


  
p_commercial + p_phxind + p_medicaid + p_medicare  + plot_layout(nrow=2,ncol=2)
ggsave(filename = here("figs/01_HHI_insurer-by-market-type.png"),dpi = 300, scale =1,width = 12, height=12)


sf_cz %>% 
  left_join(drg_hhi_cz %>% filter(year==2019) ,"cz_id") %>% 
  filter(state_01 %in% states ) %>% 
  filter(!(state_01 %in% c("HI","AK"))) %>% 
  ggplot() + 
  geom_sf(aes(fill =hhi_total_book)) +
  scale_fill_gradient2(low = scales::muted("blue"),mid = "white",high = scales::muted("red"),midpoint = 2500,limits = c(0,10000)) + 
  #theme(legend.position = "bottom") +
  geom_sf(data = sf_state %>% filter(stusps %in% states) %>% filter(!stusps %in% c("HI","AK")), alpha = 0,lwd=.7,colour = "black") + 
  coord_sf(datum=NA) + 
  remove_all_axes +
  ggtitle("Total Insured") + 
  ggthemes::theme_tufte(base_family = "Gill Sans")
ggsave(filename = here("figs/01_HHI_insurer-total_insured.png"),dpi = 300, scale =1,width = 12, height=12)


