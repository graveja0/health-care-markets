#' ---
#' output: github_document
#' ---

# NEED TO MAKE SURE TO JUST USE GENERAL ACUTE CARE

suppressWarnings(suppressMessages(source(here::here("/R/manifest.R"))))
source(here("R/map-theme.R"))
source(here("R/move-ak-hi.R"))
source(here("R/get-geographic-info.R"))
source(here("R/map-theme.R"))
source(here("R/shared-objects.R"))
source(here("R/get-contiguous-areas.R"))

# STAT
library(rlang)

# STATE FIPS TO STATE ABBREVIATION
fips_to_state <- read_rds(here("output/geographic-crosswalks/01_xw_county-to-fips.rds")) %>% 
  mutate(statefp = str_sub(fips_code,1,2)) %>% 
  select(statefp,state) %>% unique()

fips_xw <- data.table::fread(file.path("~/box/Research-Provider_Networks/data/","xw","county-fips-crosswalk.txt")) %>% 
  set_names(c("state","state_fips","county_fips","county","fips_class")) %>% 
  mutate(
    state_fips = stringr::str_pad(state_fips, 2, pad = "0"),
    county_fips = stringr::str_pad(county_fips, 3, pad = "0")
  ) %>%
  mutate(county_fips = as.character(county_fips)) %>% tbl_df() %>%
  mutate(county = gsub(" County", "", county)) %>%
  mutate(fips_code = paste0(state_fips, county_fips)) %>%
  select(county, state, fips_code,state_fips,county_fips) %>% data.frame() #%>% 
 # filter(str_sub(fips_code,1,2)=="06")

# Load the hospital-county patient sharing file
df_hosp_fips <- read_rds(here("output/hospital-county-patient-data/2017/hospital-county-patient-data.rds")) #%>% 
  #filter(str_sub(fips_code,1,2)=="06")

# Create function to convert dataframe to bipartite matrix
convert_to_bipartite <- function(df,id) {
  id <- enquo(id)
  nn <- df %>% pull(!!id)
  foo <- df %>% select(-!!id) %>%
    as.matrix()
  
  rownames(foo) <- nn
  foo
}

# Need the contiguous county bipartite data frame to restrcit CMS data only
# the fips_codes in the map.

bp_contig_1 <- read_rds(here("output/tidy-mapping-files/county/df_county_info.rds")) %>% 
  tbl_df() %>% 
  mutate(fips_code = str_pad(paste0(geoid),width=5,pad="0")) %>% 
  select(fips_code, starts_with("contig_")) %>% 
  #filter(str_sub(fips_code,1,2)=="06") %>% 
  #filter(!(fips_code %in% setdiff(.$fips_code,rownames(up_fips)))) %>% 
  gather(key,fips_contig,-fips_code) %>% 
  #filter(!(fips_contig %in% setdiff(.$fips_contig,rownames(up_fips)))) %>% 
  filter(!is.na(fips_contig) & !is.na(fips_code)) %>% 
  select(fips_code,fips_contig) %>% 
  mutate(contig = 1) %>% 
  spread(fips_contig,contig)

bipartite_contig_1 <-
  read_rds(path = file.path("~/box/Research-Provider_Networks/aim1-breadth/input/geographic-crosswalks/2016","contiguous-counties-2016.rds")) %>%
  janitor::clean_names() %>% 
  tbl_df() %>%
  #filter(state == "TN") %>%
  #filter(id %in% tn_fips) %>%
  #filter(contig %in% tn_fips) %>%
  select(id, contig) %>%
  #mutate(id = as.integer(id), contig = as.integer(contig)) %>%
  rename(fips_code = id) %>%
  #filter(str_sub(fips_code,1,2)=="06") %>% 
  unique() %>%
  mutate(contiguous = 1) %>%
  spread(contig, contiguous) 
  # mutate_at(vars(-1), function(x) ifelse(is.na(x), 0, x)) %>%
  # convert_to_bipartite(id = fips_code)


minimum_share = 0.10
minimum_number = 10

bp_fips_hosp <-
  df_hosp_fips %>%
  group_by(fips_code) %>%
  mutate(share_of_patients = total_cases / sum(total_cases, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(connected = as.integer(share_of_patients >= minimum_share))  %>%
  mutate(share = ifelse(connected==1,share_of_patients,0)) %>% 
  select(fips_code, prvnumgrp, connected) %>%
  inner_join(bp_contig_1 %>% select(fips_code),"fips_code") %>% 
  spread(prvnumgrp, connected) %>%
  convert_to_bipartite(id = fips_code)
bp_fips_hosp[is.na(bp_fips_hosp)] <- 0
bp_fips_hosp[1:10,1:10]

up_fips <-bp_fips_hosp %*% t(bp_fips_hosp)

# Now create the contiguous county unipartite matrix.
bp_contig <- 
  bp_contig_1 %>% 
  convert_to_bipartite(id = fips_code)
bp_contig[is.na(bp_contig)] <- 0
up_contig <- bp_contig %*% t(bp_contig)
up_contig <- up_contig[rownames(up_fips),colnames(up_fips)]
up_contig[up_contig>0] <- 1

unipartite_final <- up_fips 

# unipartite_final[grep("^06",rownames(unipartite_final)),grep("^06",colnames(unipartite_final))]
# up_contig[grep("^06",rownames(up_contig)),grep("^06",colnames(up_contig))]


tn_contig_graph_adj_noloops <- 
  graph_from_adjacency_matrix(unipartite_final, weighted = TRUE) %>%
  simplify(., remove.loops = TRUE)

# Run cluster_walktrap on this network. 
initial_communities <-
  walktrap.community(tn_contig_graph_adj_noloops,
                     steps = 2,
                     merges = TRUE,
                     modularity = TRUE,
                     membership = TRUE) 

market <- membership(initial_communities)
df_walktrap <- bind_cols(fips_code = names(market), mkt = market) %>% 
  mutate(statefp = str_sub(fips_code,1,2))
df_walktrap %>% filter(grepl("^06",fips_code)) %>% select(mkt) %>% unique() %>% 
  dim()

sf_walktrap <- 
  sf::read_sf(here("public-data/shape-files/county-2017/cb_2017_us_county_5m/cb_2017_us_county_5m.shp")) %>% 
  sf::st_transform(crs ="+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96") %>% 
  janitor::clean_names() %>% 
  left_join(fips_to_state,"statefp") %>% 
  filter(state %in% states) %>% 
  move_ak_hi(state = state) %>% 
  mutate(fips_code = geoid) %>% 
  inner_join(df_walktrap, "fips_code") %>% 
  group_by(mkt) %>% 
  summarise() %>% 
  ungroup() %>% 
  st_simplify(dTolerance = 100)  %>% 
 # left_join(get_contiguous(shp = ., id = commuting_zone_id_2010) %>% 
              #mutate(commuting_zone_id_2010 = as.integer(commuting_zone_id_2010)), "commuting_zone_id_2010") %>% 
  left_join(
    df_walktrap %>% 
      select(mkt,statefp) %>% 
      left_join(fips_to_state,"statefp") %>% 
      select(-statefp) %>% 
      unique() %>% 
      group_by(mkt) %>% 
      mutate(n = paste0("state_",str_pad(row_number(),width = 2, pad="0"))) %>% 
      spread(n,state)
      ,"mkt") %>% 
  rename(walktrap_id = mkt) %>% 
  st_transform(crs = 4326)

sf_state <- read_sf(here("output/tidy-mapping-files/state/01_state-shape-file.shp")) %>% 
  st_transform(crs = 4326)
  
states_to_map = states[-grep("HI|AK",states)] #c("TN","AL","GA","NC","VA","AR","MS","KY")
sf_walktrap %>% 
  filter(state_01 %in% states_to_map  | state_02 %in% states_to_map | state_03 %in% states_to_map | state_04 %in% states_to_map |
           state_05 %in% states_to_map | state_06 %in% states_to_map | state_07 %in% states_to_map) %>% 
  mutate(test = as.factor(sample(1:100,replace=TRUE,nrow(.)))) %>% 
  ggplot() + geom_sf(aes(fill=test)) + theme_bw() + coord_sf(datum=NA) +
  remove_all_axes + 
  theme(legend.position = "none") + 
  geom_sf(data = sf_state %>% filter(stusps %in% states_to_map), alpha = 0,lwd=.7,colour = "black") 


