#' ---
#' output: github_document
#' ---

# The objective of this file is to construct physician-level HHI measures.

suppressWarnings(suppressMessages(source(here::here("/R/manifest.R"))))
source(here("R/map-theme.R"))
source(here("R/shared-objects.R"))
source(here("R/get-market-from-x-y-coordinates.R"))
source(here("R/estimate_hhi.R"))
source(here("../../../box/Research-Provider_Networks/networks/R/construct-network-object.R"))

pcsa_info <- foreign::read.dbf(here("public-data/shape-files/dartmouth-hrr-hsa-pcsa/ct_pcsav31.dbf")) %>% 
  janitor::clean_names() %>% 
  select(pcsa,pcsa_st, pcsa_l) %>% 
  mutate_at(vars(pcsa,pcsa_st,pcsa_l),funs(paste0)) %>% 
  unique()

# ZCTA to PCSA Crosswalk (merges on PCSA state from above)
zcta_to_pcsa <- foreign::read.dbf(here("public-data/shape-files/dartmouth-hrr-hsa-pcsa/zip5_pcsav31.dbf")) %>% 
  janitor::clean_names() %>% 
  mutate(pcsa = paste0(pcsa)) %>% 
  left_join(pcsa_info,"pcsa") %>% 
  rename(zip_code=zip5)


df_hosp_npi <- read_rds(here("../../../box/Research-Provider_Networks/data/penn-xw-npi/general-acute-care-hospital-npi-crosswalk.rds")) 

df_aha  <- read_rds(here("output/market-comparisons/01_aha-markets-2017.rds")) %>%  # Constructed in R/construct-hosptial-hhi.R 
  rename(aha_id=id) %>% 
  select(aha_id,prvnumgrp,sysname,mname,everything()) %>% 
  left_join(df_hosp_npi,c("aha_id","prvnumgrp")) %>% 
  filter(!is.na(npi)) %>% 
  mutate(genacute = 1) %>% 
  rename(zip_code = hosp_zip_code) %>% 
  select(npi,genacute,zip_code ) %>% 
  mutate(npi = paste0(npi)) %>% 
  tbl_df() #%>% 
  #mutate(fips_code = paste0(fips_code,width=5,pad="0"))

# Provider Denominator File from SK&A
df_ska <- read_rds(here("output/ska/ska-2017.rds")) %>% 
  #mutate(primcare = cardiology) %>% 
  filter(primcare ==1 ) %>% 
  mutate(npi = paste0(npi))  %>% 
  select(npi,primcare,zip_code)

df <- 
  df_ska %>% 
  bind_rows(df_aha) %>% 
  mutate(primcare = ifelse(is.na(primcare),0,primcare)) %>% 
  mutate(genacute = ifelse(is.na(genacute),0,genacute)) %>% 
  filter(npi!="NA" & !is.na(npi)) %>% 
  left_join(zcta_to_pcsa,"zip_code")

ff <- quo(genacute) #enquo(source)
tt <- quo(primcare) #enquo(target)
kk <- sym("pcsa") #syms(keep)
cc <- quo(referral_count) #enquo(referral_count)
gg <- quo(pcsa) #enquo(geog)
window = "60"

tmp <-
  df %>%
  select(npi, !!ff, !!tt,!!!kk) %>%
  filter(!!ff == 1 | !!tt == 1) %>%
  group_by(npi) %>%
  filter(!is.na(npi) & npi != "NA") %>%
  ungroup() %>%
  group_by(npi) %>% 
  filter(row_number()==1) %>% 
  ungroup() %>% 
  mutate(npi = paste0(npi))

# Docgraph Data

VV <- readRDS(here("../../../box/Research-Provider_Networks/data/careset/node-list-2015.rds")) %>%
  mutate(npi = paste0(npi)) %>%
  rename(id = node_id) %>%
  inner_join(tmp,"npi") %>% data.frame() %>%
  mutate(id = as.numeric(paste0(id)))

from_ids <- VV %>% filter(!!ff == 1) %>% pull(id) %>% unique()
to_ids <- VV %>% filter(!!tt == 1) %>% pull(id) %>% unique()

EE <- readRDS(here("../../../box/Research-Provider_Networks/data/careset/edge-list-2015.rds"))  %>%
  filter(from %in% from_ids & to %in% to_ids) %>% data.frame() %>%
  mutate(from = as.numeric(paste0(from)),
         to = as.numeric(paste0(to)))

referrals_by_pcsa <- 
  EE %>% 
  left_join(VV %>% select(from = id, npi = npi),"from") %>% 
  left_join(VV %>% select(to = id, to_npi = npi, pcsa=pcsa),"to") %>% 
  left_join(df_hosp_npi %>% mutate(npi = paste0(npi)),"npi") %>% 
  group_by(aha_id,prvnumgrp,pcsa) %>% 
  summarise(primary_care_referrals = sum(pair_count,na.rm=TRUE))
  



total_referrals <- 
  referrals_by_pcsa %>% 
  group_by(aha_id,prvnumgrp) %>%  
  summarise(total_primary_care_referrals = sum(primary_care_referrals,na.rm=TRUE)) 

df_aha_market  <- read_rds(here("output/market-comparisons/01_aha-markets-2017.rds")) %>%  # Constructed in R/construct-hosptial-hhi.R 
  rename(aha_id=id) %>% 
  select(aha_id,prvnumgrp,sysname,mname,everything()) %>% 
  inner_join(total_referrals,c("aha_id","prvnumgrp"))

df_aha_market %>% 
  ggplot(aes(x = rank(admtot),y=rank(total_primary_care_referrals))) + geom_point(alpha=0.1) + 
  geom_smooth(se=FALSE, colour="black") +
  theme_tufte() +
  geom_abline(slope=1,intercept=0,lty=2)

with(df_aha_market,lm(rank(admtot)~rank(total_primary_care_referrals)))
  
hhi_genacute_pcsa_referral <- 
  df_aha_market %>% 
  inner_join(referrals_by_pcsa,c("aha_id","prvnumgrp")) %>% 
  ungroup() %>% 
  select(system_id,total_primary_care_referrals,pcsa) %>% 
  estimate_hhi(id = system_id,
               weight = total_primary_care_referrals,
               market= pcsa) %>% 
  rename(hhi_referral = hhi,
         total_weight = total_weight) 

pcsa_to_county <- read_csv(here("public-data/zcta-to-fips-county/zcta-to-fips-county.csv")) %>% 
  janitor::clean_names() %>% 
  filter(row_number() !=1) %>% 
  mutate(fips_code = county) %>% 
  select(zip_code = zcta5, fips_code,afact) %>% 
  mutate(afact = as.numeric(paste0(afact))) %>% 
  left_join(zcta_to_pcsa,"zip_code") %>% 
  filter(afact==1) %>% 
  group_by(pcsa,fips_code) %>% 
  filter(row_number()==1) %>% 
  select(pcsa,fips_code) %>% 
  unique()

pcsa_hhi_aggregated_to_county <-
  hhi_genacute_pcsa_referral  %>% 
  inner_join(pcsa_to_county,"pcsa") %>% 
  mutate(weight = total_weight) %>% 
  group_by(fips_code) %>% 
  summarise(hhi_referral = weighted.mean(hhi_referral,weight,na.rm=TRUE))

hhi_foo <- 
  read_rds(here("output/market-comparisons/01_market-comparisons-county.rds"))  %>%
  bind_rows() %>%
  left_join(pcsa_hhi_aggregated_to_county, "fips_code")

hhi_foo %>% 
  select(fips_code,hhi_referral,hhi_zip) %>% 
  ggplot(aes(x=hhi_referral,y=hhi_zip)) + 
  geom_point(alpha=0.1) +
  geom_smooth(se=FALSE)

with(hhi_foo,lm(hhi_referral~hhi_zip))

ms_by_zip <- read_rds(here("output/market-comparisons/01_2017_ZIP-market-shares.rds")) %>% 
  filter(zip_code=="37203") %>% 
  mutate(name = ifelse(sysname==""|is.na(sysname),mname,sysname)) %>% 
  select(zip_code,name,total_cases,market_share,hhi_zip=hhi) %>% 
  group_by(name) %>% 
  summarise_at(vars(total_cases),function(x) sum(x,na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate_at(vars(total_cases),function(x) x /sum(x))

referrals_by_zip_code %>% filter(zip_code==37203) %>% arrange(desc(primary_care_referrals)) %>% 
  left_join(df_aha_market %>% select(aha_id,prvnumgrp,sysname,mname),c("aha_id","prvnumgrp")) %>% 
  mutate(name = ifelse(sysname=="",mname,sysname)) %>% 
  group_by(name) %>% 
  summarise_at(vars(primary_care_referrals),function(x) sum(x,na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate_at(vars(primary_care_referrals),function(x) x /sum(x)) %>% 
  left_join(ms_by_zip,c("name")) %>% 
  arrange(desc(primary_care_referrals))
