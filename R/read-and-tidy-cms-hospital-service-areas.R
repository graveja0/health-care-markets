#' ---
#' output: github_document
#' ---

suppressWarnings(suppressMessages(source(here::here("/R/manifest.R"))))

# Get the CMS Hospital Service Area Data
# Note the source of these data are downloaded csv files from the interactive 
# CMS data explorer available at the links at 
# https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Hospital-Service-Area-File/index.html

hosp_serv_file <- "2017/Hospital_Service_Area_File_-_2017.csv"
df_hosp_serv17_zip <- 
  data.table::fread(here("public-data/cms-hospital-service-area/",
  hosp_serv_file)) %>%
  tbl_df() %>% 
  janitor::clean_names() %>% 
  rename(prvnumgrp = medicare_provider_number, 
         zip_code = zip_code_of_residence)  %>% 
  mutate(zip_code = str_pad(zip_code, pad = "0",width = 5)) %>% 
  mutate_at(vars(total_days_of_care,total_charges, total_cases), function(x) as.numeric(paste0(x)))

# We now need to roll these ZIP level data up to the county level. We
# will do this by allocating each patient count / charge / days measure
# using the fraction of the ZIP code in each county. 
# Thus, if 100% of the ZIP is in a county, then 100% of the total_days_of_care
# variable will be attributed to the hospital-county pair. If only 50% is, then
# we only attribute 50%.

df_zip_to_fips <- 
  read_rds(here("output/geographic-crosswalks/zcta-to-fips-county.rds"))

df_hosp_serv17_fips <- 
  df_hosp_serv17_zip %>% 
  left_join(df_zip_to_fips,"zip_code") %>% 
  mutate_at(vars(total_days_of_care,total_charges, total_cases), function(x) x * .$pct_of_zip_in_fips) %>% 
  group_by(prvnumgrp,fips_code) %>% 
  summarise_at(vars(total_days_of_care,total_charges, total_cases),function(x) sum(x,na.rm=TRUE))

write_rds(df_hosp_serv17_fips,path = here("output/hospital-county-patient-data/2017/hospital-county-patient-data.rds"))


  










