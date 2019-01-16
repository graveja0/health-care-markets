read-and-tidy-cms-hospital-service-areas.R
================
johngraves
Wed Jan 16 10:49:55 2019

``` r
suppressWarnings(suppressMessages(source(here::here("/R/manifest.R"))))

# Community Detection 


# Get the CMS Hospital Service Area Data
# Note the source of these data are downloaded csv files from the interactive 
# CMS data explorer available at the links at 
# https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Hospital-Service-Area-File/index.html

df_hosp_serv17 <- data.table::fread(here("public-data/cms-hospital-service-area/2017/Hospital_Service_Area_File_-_2017.csv")) %>%
  tbl_df() %>% 
  janitor::clean_names() %>% 
  rename(prvnumgrp = medicare_provider_number, 
         zip_code = zip_code_of_residence)  %>% 
  mutate(zip_code = str_pad(zip_code, pad = "0",width = 5))


convert_to_bipartite <- function(df,id) {
  id <- enquo(id)
  nn <- df %>% pull(!!id)
  foo <- df %>% select(-!!id) %>%
    as.matrix()
  
  rownames(foo) <- nn
  foo
}
```
