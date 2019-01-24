#' ---
#' output: github_document
#' ---

# NEED TO MAKE SURE TO JUST USE GENERAL ACUTE CARE

suppressWarnings(suppressMessages(source(here::here("/R/manifest.R"))))
source(here("R/map-theme.R"))
library(rlang)

df_county_map <- read_rds(here("output/tidy-mapping-files/county/df_county.rds"))

fips_xw <- data.table::fread(file.path("~/box/Research-Provider_Networks/data/","xw","county-fips-crosswalk.txt")) %>% 
  set_names(c("state","state_fips","county_fips","county","fips_class")) %>% 
  mutate(
    state_fips = stringr::str_pad(state_fips, 2, pad = "0"),
    county_fips = stringr::str_pad(county_fips, 3, pad = "0")
  ) %>%
  mutate(county_fips = as.character(county_fips)) %>% tbl_df() %>%
  mutate(county = gsub(" County", "", county)) %>%
  mutate(fips_code = paste0(state_fips, county_fips)) %>%
  select(county, state, fips_code,state_fips,county_fips) %>% data.frame()

# Load the hospital-county patient sharing file
df_hosp_fips <- read_rds(here("output/hospital-county-patient-data/2017/hospital-county-patient-data.rds"))

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
  unique() %>%
  mutate(contiguous = 1) %>%
  spread(contig, contiguous)# %>%
  # mutate_at(vars(-1), function(x) ifelse(is.na(x), 0, x)) %>%
  # convert_to_bipartite(id = fips_code)


minimum_share = 0.01

bp_fips_hosp <-
  df_hosp_fips %>%
  group_by(prvnumgrp) %>%
  mutate(share_of_patients = total_cases / sum(total_cases, na.rm = TRUE)) %>%
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

unipartite_final[grep("^06",rownames(unipartite_final)),grep("^06",colnames(unipartite_final))]
up_contig[grep("^06",rownames(up_contig)),grep("^06",colnames(up_contig))]


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
df_market <- bind_cols(fips_code = names(market), mkt = market)
df_market %>% filter(grepl("^06",fips_code)) %>% select(mkt) %>% unique() %>% 
  dim()

df_county_map %>%
  left_join(df_market,"fips_code") %>% 
  mutate(market = factor(mkt)) %>% 
  mutate(state_fips = str_sub(fips_code,1,2)) %>% 
  filter(!state_fips %in% c("02","15")) %>% 
  filter(grepl("^06",fips_code)) %>%
  tbl_df() %>%
  mutate(test = factor(sample(1:10,nrow(.),replace=TRUE))) %>%
  ggplot() +
  aes(long,lat,group=group) +
  geom_polygon(aes(fill = market)) +
  geom_path(color="black") +
  coord_equal() +
  ggthemes::theme_tufte() +
  theme(legend.position = "none") + 
  remove_all_axes


# 
# 
# 
# g <- tn_contig_graph_adj_noloops
# steps <- seq(1, 10, 1)
# w <- list()
# ccount <- NULL
# 
# for(s in steps){
#   # cat(paste('Running walktrap with steps =', s, '\n'))
#   w0 <- walktrap.community(g, steps = s)
#   ccount <- c(ccount, length(levels(as.factor(w0$membership))))
#   w[[s]] <- w0
# }
# 
# plot(ccount ~ steps,
#      ylim = c(1, 10),
#      xlab = 'Number of steps',
#      ylab = 'Number of communities',
#      main = 'Walktrap with increasing number of steps')
# 
