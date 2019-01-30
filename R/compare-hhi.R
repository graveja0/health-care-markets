#' ---
#' output: github_document
#' ---

# The objective of this file is to make comparisons of Hirschman-Herfindahl Indexes across alternative 
# geographic market definitions.

# We will consider the following geographic markets for hospitals:

# 1. HRR
# 2. Commuting Zone
# 3. Rating Area


suppressWarnings(suppressMessages(source(here::here("/R/manifest.R"))))
source(here("R/map-theme.R"))
source(here("R/shared-objects.R"))
source(here("R/get-market-from-x-y-coordinates.R"))

sf_hrr <- read_sf(here("output/tidy-mapping-files/hrr/01_hrr-shape-file.shp"))  %>% 
  st_transform(crs = 4326)
sf_cz <- read_sf(here("output/tidy-mapping-files/commuting-zone/01_commuting-zone-shape-file.shp")) %>% 
  st_transform(crs = 4326)
sf_ra <- read_sf(here("output/tidy-mapping-files/rating-area/01_rating-area-shape-file.shp")) %>%
  st_transform(crs = 4326) %>% 
  mutate(rating_area = ratng_r)
sf_state <- read_sf(here("output/tidy-mapping-files/state/01_state-shape-file.shp")) %>% 
  st_transform(crs = 4326)

if (!file.exists(here("output/market-comparisons/01_aha-markets-2017.rds"))) {
  # Get latitude and longitue of general acute care hospitals in 2017 AHA survey. 
  aha <- data.table::fread(here("../../../box/Research-AHA_Data/data/aha/annual/raw/2017/FY2017 ASDB/COMMA/ASPUB17.CSV")) %>% 
    janitor::clean_names() %>% 
    filter(mstate %in% states) %>% 
    mutate(system_id = ifelse(!is.na(sysid),paste0("SYS_",sysid),id)) %>% 
    filter(serv==10) %>% 
    select(mname, id, mcrnum , latitude = lat, longitude = long, hrrnum = hrrcode, hsanum = hsacode, admtot, system_id) 
  
  # Assign each hospital to its marketplace rating area.  Note I have to do it this way as a handful of 
  # hospitals do not map within a rating area (oddly)
  
  aha_rating_area <- 
    aha %>% 
    get_market_from_xy(df = ., 
                       x = longitude, 
                       y = latitude,
                       sf = sf_ra,
                       market_id = rating_area)
  df_aha_rating_area <- 
    aha_rating_area %>% 
    set_names(aha$id) %>% 
    unlist() %>% 
    data.frame() %>% 
    rownames_to_column() %>% 
    set_names(c("id","rating_area"))
  
  # Assign each AHA hosptial to its commuting zone. 
  aha_cz <- 
    aha %>% 
    get_market_from_xy(df = ., 
                       x = longitude, 
                       y = latitude,
                       sf = sf_cz,
                       market_id = cz_id)
  df_cz <- 
    aha_cz %>% 
    set_names(aha$id) %>% 
    unlist() %>% 
    data.frame() %>% 
    rownames_to_column() %>% 
    set_names(c("id","cz_id"))
  
  aha_markets <- 
    aha %>% 
    left_join(df_aha_rating_area,"id") %>% 
    left_join(df_cz,"id")
  
  write_rds(aha_markets,path=here("output/market-comparisons/01_aha-markets-2017.rds"))

}

estimate_hhi <-  function(df, id,  market, weight) {
  ii <- enquo(id)
  mm<- enquo(market)
  ww <- enquo(weight) 
  
  
  df %>% 
    group_by(!!mm) %>% 
    mutate(denominator = sum(!!ww ,na.rm=TRUE)) %>% 
    group_by(!!mm,!!ii) %>%
    mutate(numerator = sum(!!ww,na.rm=TRUE)) %>%
    select(!!mm,!!ii,numerator,denominator) %>%
    unique() %>%
    mutate(market_share = 100 * (numerator / denominator)) %>%
    mutate(market_share_sq = market_share ^ 2)  %>%
    group_by(!!mm) %>%
    summarize(hhi = sum(market_share_sq,na.rm=TRUE))
}

hhi_rating_area <-
  aha_markets %>%
  mutate(weight = admtot) %>%
  estimate_hhi(id = system_id,
               weight = weight,
               market = rating_area)

hhi_hrr <-
  aha_markets %>%
  mutate(weight = admtot) %>%
  estimate_hhi(id = system_id,
               weight = weight,
               market = hrrnum)

hhi_cz <-
  aha_markets %>%
  mutate(weight = admtot) %>%
  estimate_hhi(id = system_id,
               weight = weight,
               market = cz_id)


states_to_map <- c("TN","VA","NC")
p_hrr <- 
  sf_hrr %>% 
  left_join(hhi_hrr,"hrrnum") %>% 
  filter(hrrstate %in%  states_to_map) %>% 
  ggplot() + 
  geom_sf(aes(fill = hhi)) +
  scale_fill_gradient2(low = scales::muted("blue"),mid = "white",high = scales::muted("red"),midpoint = 2500) + 
  theme(legend.position = "bottom") +
  geom_sf(data = sf_state %>% filter(stusps %in% states_to_map), alpha = 0,lwd=.7,colour = "black") + 
  coord_sf(datum=NA) + 
  remove_all_axes +
  ggtitle("Hospital Referral Regions") + 
  ggthemes::theme_tufte(base_family = "Gill Sans")
ggsave(p_hrr, filename = here("figs/01_HHI_hrr.png"),dpi = 300, scale =1)


p_rating_area <- 
  sf_ra %>% 
  left_join(hhi_rating_area,"rating_area") %>% 
  filter(state %in%  states_to_map) %>% 
  ggplot() + 
  geom_sf(aes(fill = hhi)) +
  scale_fill_gradient2(low = scales::muted("blue"),mid = "white",high = scales::muted("red"),midpoint = 2500) + 
  theme(legend.position = "bottom") +
  geom_sf(data = sf_state %>% filter(stusps %in% states_to_map), alpha = 0,lwd=.7,colour = "black") + 
  coord_sf(datum=NA) + 
  remove_all_axes +
  ggtitle("Rating Areas") + 
  ggthemes::theme_tufte(base_family = "Gill Sans")
ggsave(p_rating_area, filename = here("figs/01_HHI_rating-area.png"),dpi = 300, scale =1)


p_cz <- 
  sf_cz %>% 
  left_join(hhi_cz,"cz_id") %>% 
  filter(state_01 %in% states_to_map | state_02 %in% states_to_map  | state_03 %in% states_to_map) %>% 
  ggplot() + 
  geom_sf(aes(fill = hhi)) +
  scale_fill_gradient2(low = scales::muted("blue"),mid = "white",high = scales::muted("red"),midpoint = 2500) + 
  #theme(legend.position = "bottom") +
  geom_sf(data = sf_state %>% filter(stusps %in% states_to_map), alpha = 0,lwd=.7,colour = "black") + 
  coord_sf(datum=NA) + 
  remove_all_axes +
  ggtitle("Commuting Zones") + 
  ggthemes::theme_tufte(base_family = "Gill Sans")
ggsave(p_cz, filename = here("figs/01_HHI_commuting-zones.png"),dpi = 300, scale =1)

# p_final <- p_hrr + p_rating_area + p_cz + plot_layout(nrow=2,ncol=2)
# 
# ggsave(p_final, filename = here("figs/compare-hhi-by-market-definition.png"),dpi = 300, scale =1)


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

county_to_cz <- data.table::fread(here("public-data/shape-files/commuting-zones/counties10-zqvz0r.csv")) %>% 
  janitor::clean_names() %>% 
  rename(fips_code = fips) %>% 
  group_by(out10) %>% 
  mutate(commuting_zone_population_2010 = sum(pop10, na.rm=TRUE)) %>% 
  mutate(fips_code = str_pad(paste0(fips_code),width = 5, pad="0")) %>% 
  select(fips_code,
         cz_id = out10)

county_to_rating_area <- 
  read_rds(here("output/geographic-crosswalks/01_rating-areas_counties_2019.rds")) %>% 
  data.frame() %>% 
  unique() %>% 
  select(fips_code,rating_area)

df_county <- 
  county_to_cz %>% 
  full_join(county_to_hrr,"fips_code") %>% 
  full_join(county_to_rating_area,"fips_code") %>% 
  left_join(hhi_cz %>% rename(hhi_cz = hhi) ,"cz_id") %>% 
  left_join(hhi_rating_area %>% rename(hhi_rating_area = hhi),"rating_area") %>% 
  left_join(hhi_hrr %>% rename(hhi_hrr = hhi),"hrrnum")

sf_county <- read_sf(here("output/tidy-mapping-files/county/01_county-shape-file.shp")) %>% 
  st_transform(crs = 4326) %>% 
  left_join(df_county,"fips_code") 

sf_county %>% 
  mutate(diff_hrr_cz = hhi_hrr - hhi_cz) %>% 
  filter(state %in% states_to_map) %>% 
  ggplot() + 
  geom_sf(aes(fill = diff_hrr_cz)) + 
  scale_fill_gradient2(low = scales::muted("blue"),mid = "white",high = scales::muted("red"), 
                       midpoint = 0,limits = c(-10000,10000), breaks = c(-10000,0,10000), 
                       name = "",
                       labels = c("-10,000\nMore Competitive\nUsing HRR","0\nNo Difference","+10,000\nLess Competitive\nUsing HRR")) + 
  geom_sf(data = sf_state %>% filter(stusps %in% states_to_map), alpha = 0,lwd=.7,colour = "black") + 
  coord_sf(datum=NA) + 
  remove_all_axes +
  ggtitle("HRRs vs. Commuting Zones") + 
  ggthemes::theme_tufte(base_family = "Gill Sans")
ggsave(filename = here("figs/01_HHI-HRR-vs-commuting-zones.png"),dpi = 300, scale =1)
  

sf_county %>% 
  mutate(diff_hrr_cz = hhi_hrr - hhi_rating_area) %>% 
  filter(state %in% states_to_map) %>% 
  ggplot() + 
  geom_sf(aes(fill = diff_hrr_cz)) + 
  scale_fill_gradient2(low = scales::muted("blue"),mid = "white",high = scales::muted("red"), 
                       midpoint = 0,limits = c(-10000,10000), breaks = c(-10000,0,10000), 
                       name = "",
                       labels = c("-10,000\nMore Competitive\nUsing HRR","0\nNo Difference","+10,000\nLess Competitive\nUsing HRR")) + 
  geom_sf(data = sf_state %>% filter(stusps %in% states_to_map), alpha = 0,lwd=.7,colour = "black") + 
  coord_sf(datum=NA) + 
  remove_all_axes +
  ggtitle("HRRs vs. Rating Areas") + 
  ggthemes::theme_tufte(base_family = "Gill Sans")
ggsave(filename = here("figs/01_HHI-HRR-vs-rating-areas.png"),dpi = 300, scale =1)


sf_county %>% 
  mutate(diff_hrr_cz = hhi_cz - hhi_rating_area) %>% 
  filter(state %in% states_to_map) %>% 
  ggplot() + 
  geom_sf(aes(fill = diff_hrr_cz)) + 
  scale_fill_gradient2(low = scales::muted("blue"),mid = "white",high = scales::muted("red"), 
                       midpoint = 0,limits = c(-10000,10000), breaks = c(-10000,0,10000), 
                       name = "",
                       labels = c("-10,000\nMore Competitive\nUsing Commuting Zones","0\nNo Difference","+10,000\nLess Competitive\nUsing Commuting Zones")) + 
  geom_sf(data = sf_state %>% filter(stusps %in% states_to_map), alpha = 0,lwd=.7,colour = "black") + 
  coord_sf(datum=NA) + 
  remove_all_axes +
  ggtitle("Commuting Zones vs. Rating Areas") + 
  ggthemes::theme_tufte(base_family = "Gill Sans")
ggsave(filename = here("figs/01_HHI-commuting-zones-vs-rating-areas.png"),dpi = 300, scale =1)

