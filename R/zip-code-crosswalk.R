# This file reads in and tidys the ZIP code crosswalk
# file, which is created by mapping ZCTAs to all 
# possible target geographies in the tool at 
# http://mcdc.missouri.edu/applications/geocorr2014.html

tmp <- data.table::fread("public-data/zcta-to-fips-county/zcta-to-fips-county.csv",header=TRUE) %>% 
  filter(row_number() != 1) %>% 
  janitor::clean_names() %>% 
  mutate(zip_code = str_pad(as.numeric(paste0(zcta5)),width = 5, pad="0")) %>% 
  mutate(fips_code = str_pad(as.numeric(paste0(county)), width = 5, pad = "0")) %>% 
  mutate(pct_of_zip_in_fips = as.numeric(paste0(afact))) %>% 
  tbl_df() %>% 
  select(zip_code,fips_code,pct_of_zip_in_fips) %>% 
  write_rds(path = here("output/geographic-crosswalks/zcta-to-fips-county.rds"))
