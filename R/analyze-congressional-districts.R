#' ---
#' output: github_document
#' ---

suppressWarnings(suppressMessages(source(here::here("/R/manifest.R"))))
library(gganimate)
library(magick)
source(here("R/map-theme.R"))
source(here("R/shared-objects.R"))

hhi_cz <- read_rds(here("output/market-comparisons/01_HHI_genacute_cz.rds")) %>% 
  tbl_df()

hhi_cd <- read_rds(here("output/market-comparisons/01_HHI_genacute_cd.rds")) %>% 
  tbl_df()

sf_state <- read_sf(here("output/tidy-mapping-files/state/01_state-shape-file.shp")) %>% 
  st_transform(crs = 4326)
fips_to_state <- read_rds(here("output/geographic-crosswalks/01_xw_county-to-fips.rds")) %>% 
  mutate(statefp = str_sub(fips_code,1,2)) %>% 
  select(statefp,state) %>% unique()

sf_cd <-  #https://www.census.gov/geo/maps-data/data/cbf/cbf_cds.html
  sf::read_sf(here("public-data/shape-files/congressional-district/cb_2015_us_cd114_500k/cb_2015_us_cd114_500k.shp")) %>% 
  sf::st_transform(crs ="+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96") %>% 
  st_simplify(dTolerance = 500) %>% 
  st_transform(crs = 4326) %>% 
  janitor::clean_names() %>% 
  left_join(fips_to_state,"statefp")  %>% 
  mutate(cd114fp = paste0(state,cd114fp))

hh <- quo(hhi_net)
ii <- quo(cd114fp)
hhi_chg_cd <- 
  hhi_cd %>% 
  select(!!ii, year,!!hh ) %>% 
  mutate(year = paste0("hhi_",year)) %>% 
  spread(year,!!hh) %>% 
  mutate(hhi_2016 = coalesce(hhi_2015,hhi_2016)) %>% 
  mutate(chg_2010 = hhi_2010 - hhi_2010,
         chg_2011 = hhi_2011 - hhi_2010,
         chg_2012 = hhi_2012 - hhi_2010,
         chg_2013 = hhi_2013 - hhi_2010,
         chg_2014 = hhi_2014 - hhi_2010,
         chg_2015 = hhi_2015 - hhi_2010,
         chg_2016 = hhi_2016 - hhi_2010,
         chg_2017 = hhi_2017 - hhi_2010) %>% 
  select(!!ii,contains("chg_")) %>% 
  gather(key,value,-!!ii) %>% 
  mutate(year = as.numeric(paste0(gsub("chg_","",key))))

for (x in names(census_regions[-1])) {
  cat(x) 
  cat("\n\n")
  states_to_map <- census_regions[[x]]
  tmp <- 
    sf_cd %>% 
    left_join(hhi_chg_cd,"cd114fp") %>% 
    filter(state %in% states_to_map ) %>% 
    mutate(year = as.integer(paste0(year))) %>% 
    filter(!is.na(year)) %>% 
    ggplot() + 
    geom_sf(aes(fill =value)) +
    scale_fill_gradient2(name = "Change in Hospital Market\nConcentration Index\n(HHI)",low = scales::muted("blue"),mid = "white",high = scales::muted("red"),midpoint = 0,limits = c(-3500,3500),
                         breaks = c(-3500,0,3500),
                         labels = c("More Competitive -3,500","No Change 0","More Concentrated +3,500")) + 
    #theme(legend.position = "bottom") +
    geom_sf(data = sf_state %>% filter(stusps %in% states_to_map), alpha = 0,lwd=.7,colour = "black") + 
    coord_sf(datum=NA) + 
    remove_all_axes +
    ggthemes::theme_tufte(base_family = "Gill Sans") + 
    transition_time(year) +
    ease_aes('linear') + 
    labs(title = "Year: {frame_time}") 
  animate(plot = tmp, nframes = 30,end_pause = 3,duration=10, renderer = gifski_renderer(paste0('output/figures/hhi_2010-to-2017-change_congressional-district_',x,'.gif')))
}


hhi_chg_cd %>% filter(year==2017)  %>% arrange(desc(value))
# TN01 (Phil Roe-R) Ballad Health (Wellmont and Mountain States Merger)
# NY25 (Joe Morelle-D) Rochester Regional Health System (Merger b/t General Hospital System and Unity Health System)
# VA09 (Morgan Griffith-R) Ballad Health (Wellmont and Mountain States Merger)
# OH10 (Mike Turner-R) 

hhi_chg_cd %>% filter(year==2017)  %>% arrange(value)

hhi_cd %>% filter(year==2017) %>% arrange(desc(hhi_zip))
up2010 <-  readRDS(paste0("output/market-comparisons/01_up-weighted-sysid-","2010",".rds"))
up2017 <-  readRDS(paste0("output/market-comparisons/01_up-weighted-sysid-","2017",".rds"))

foo <- up2010[which(rownames(up2010)==more_conc$system_id_2010[6]),]; foo[foo>5]
foo2 <- up2017[which(rownames(up2017)==more_conc$system_id_2017[6]),]; foo2[foo2>5]





county_fips_to_state <- read_rds(here("output/geographic-crosswalks/01_xw_county-to-fips.rds")) %>% 
  mutate(statefp = str_sub(fips_code,1,2))  %>% 
  select(fips_code,state)

test <- read_rds(paste0("output/market-comparisons/01_hospital-level-hhi-aggregated-to-zip.rds")) 

hhi_years <- paste0(2010:2017)
df_hhi <-   
  hhi_years %>% 
  map(~(
    readRDS(file=paste0("output/market-comparisons/01_hospital-level-hhi-",.x,".rds")) %>% 
      mutate(hrrnum= paste0(hrrnum),
             hsanum = paste0(hsanum)) %>% 
      select(-mcrnum) %>% 
      select(prvnumgrp,mname,sysname,hhi_net = hhi_km,fips_code,system_id) 
  )) %>% 
  set_names(hhi_years) %>% 
  bind_rows(.id = "year") %>% 
  mutate(year = as.numeric(year)) %>% 
  gather(measure,value,-prvnumgrp,-mname,-year,-fips_code) %>% 
  group_by(prvnumgrp,year,measure) %>% 
  filter(row_number()==1) %>% 
  ungroup() %>% 
  unite(hhi,measure,year) %>% 
  tbl_df() %>% 
  spread(hhi,value) %>% 
  mutate_at(vars(contains("hhi_net")),funs(as.numeric)) %>% 
  mutate(change = hhi_net_2017 - hhi_net_2010) %>% 
  mutate(fips_code = str_pad(fips_code,width = 5, pad="0")) %>% 
  left_join(county_fips_to_state,"fips_code") %>% 
  mutate(name = paste0(mname," (",state,")")) %>% 
  filter(!is.na(change)) %>% 
  ungroup() %>% 
  arrange(desc(change))  %>% 
  mutate_at(vars(change,contains("hhi")), function(x) round(x,0)) 

more_conc <- df_hhi %>% select(name,prvnumgrp,sysname_2017,sysname_2010,system_id_2017,system_id_2010,hhi_net_2010,hhi_net_2017,change) %>% head(n=20) %>% data.frame()
less_conc <- df_hhi %>% select(name,prvnumgrp,sysname_2017,sysname_2010,system_id_2017,system_id_2010,hhi_net_2010,hhi_net_2017,change) %>% tail(n=10) %>% data.frame()

df_hhi %>% filter(state =="TN") %>% select(name,sysname,hhi_net_2010,hhi_net_2017,change)
  
# Lallie Kemp Medical Center (191321)
# https://www.beckershospitalreview.com/hospital-management-administration/lsu-approves-closure-of-huey-p-long-medical-center.html

# Illini Community Hospital (IL)  (#4) : Blessing Hospital started drawing patients from its catchment area. 
# Tyrone Hospital (PA) (#5): UPMC expanded its presence into its catchment area. 

up2010 <-  readRDS(paste0("output/market-comparisons/01_up-weighted-sysid-","2010",".rds"))
up2017 <-  readRDS(paste0("output/market-comparisons/01_up-weighted-sysid-","2017",".rds"))

foo <- up2010[which(rownames(up2010)==more_conc$system_id_2010[6]),]; foo[foo>5]
foo2 <- up2017[which(rownames(up2017)==more_conc$system_id_2017[6]),]; foo2[foo2>5]


df_hhi %>% filter(system_id_2010 %in% setdiff(names(foo[foo>5]),names(foo2[foo2>5]))) %>% select(sysname_2010,mname) %>% unique()
df_hhi %>% filter(system_id_2010 %in% "6390640") %>% select(sysname_2010,mname) %>% unique() 
sum((100*(foo/sum(foo)))^2)
sum((100*(foo2/sum(foo2)))^2)

foo[foo>5]-foo2[names(foo[foo>5])]