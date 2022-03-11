#' ---
#' output: github_document
#' ---
rename_in_list <- function(x,from, to) {
  x %>% rename_at(vars(contains(from)), funs(sub(from, to, .)))
}


suppressWarnings(suppressMessages(source(here::here("R/manifest.R"))))

# Get the CMS Hospital Service Area Data
# Note the source of these data are downloaded csv files from the interactive 
# CMS data explorer available at the links at 
# https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Hospital-Service-Area-File/index.html

hosp_serv_files <- c(
    "2020" = "public-data/cms-hospital-service-area/2020/HSAF_2020_SUPPRESS.csv",
  "2019" = "public-data/cms-hospital-service-area/2019/HSAF_2019_SUPPRESS.csv",
  "2018" = "public-data/cms-hospital-service-area/2018/Hospital_Service_Area_File_-_2018.csv",
                     "2017" = "public-data/cms-hospital-service-area/2017/Hospital_Service_Area_File_-_2017.csv",
                     "2016" = "public-data/cms-hospital-service-area/2016/2016_Hospital_Service_Area_File__HSAF_.csv",
                     "2015" = "public-data/cms-hospital-service-area/2015/Hospital_Service_Area_File_-_2015.csv",
                     "2014" = "public-data/cms-hospital-service-area/2014/Hospital_Service_Area_File_-_2014.csv",
                     "2013" = "public-data/cms-hospital-service-area/2013/Hospital_Service_Area_File_-_2013.csv",
                     "2012" = "public-data/cms-hospital-service-area/2012/Hospital_Service_Area_File_-_2012.csv",
                     "2011" = "public-data/cms-hospital-service-area/2011/Hospital_Service_Area_File_-_2011.csv",
                     "2010" = "public-data/cms-hospital-service-area/2010/Hospital_Service_Area_File_-_2010.csv")
hosp_serv_files <- hosp_serv_files[1:2]
df_hosp_serv_zip <- 
  hosp_serv_files %>% 
  map(~(
    data.table::fread(here(.x)) %>%
    tbl_df() %>% 
    janitor::clean_names()))  %>% 
  set_names(names(hosp_serv_files)) %>% 
  map(~rename_in_list(x = .x, from = "medicare_provider_number", to = "prvnumgrp")) %>% 
  map(~rename_in_list(x = .x, from = "medicare_prov_num", to = "prvnumgrp")) %>% 
  map(~rename_in_list(x = .x, from = "zip_code_of_residence", to = "zip_code"))  %>% 
  map(~rename_in_list(x = .x, from = "zip_cd_of_residence", to = "zip_code"))  %>% 
  map(~(.x %>% 
    mutate(zip_code = str_pad(zip_code, pad = "0",width = 5)) %>% 
    mutate_at(vars(total_days_of_care,total_charges, total_cases), function(x) as.numeric(paste0(x)))
  ))
 
names(df_hosp_serv_zip) %>% 
  walk(
    ~write_rds(df_hosp_serv_zip[[.x]],path = here(paste0("output/hospital-county-patient-data/",.x,"/hospital-zip-patient-data.rds")))
  )


# We now need to roll these ZIP level data up to the county level. We
# will do this by allocating each patient count / charge / days measure
# using the fraction of the ZIP code in each county. 
# Thus, if 100% of the ZIP is in a county, then 100% of the total_days_of_care
# variable will be attributed to the hospital-county pair. If only 50% is, then
# we only attribute 50%.

df_zip_to_fips <-
  read_rds(here("output/geographic-crosswalks/zcta-to-fips-county.rds"))

df_hosp_serv18_fips <-
  df_hosp_serv_zip[[1]] %>%
  left_join(df_zip_to_fips,"zip_code") %>%
  mutate_at(vars(total_days_of_care,total_charges, total_cases), function(x) x * .$pct_of_zip_in_fips) %>%
  group_by(prvnumgrp,fips_code) %>%
  summarise_at(vars(total_days_of_care,total_charges, total_cases),function(x) sum(x,na.rm=TRUE))

write_rds(df_hosp_serv18_fips,path = here("output/hospital-county-patient-data/2018/hospital-county-patient-data.rds"))


  










