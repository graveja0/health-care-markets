#' ---
#' output: github_document
#' ---

suppressWarnings(suppressMessages(source(here::here("/R/manifest.R"))))

# Community Detection 


# Get the CMS Hospital Service Area Data
# Note the source of these data are downloaded csv files from the interactive 
# CMS data explorer available at the links at 
# https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Hospital-Service-Area-File/index.html

hosp_serv_file <- "2017/Hospital_Service_Area_File_-_2017.csv"
df_hosp_serv17 <- 
  data.table::fread(here("public-data/cms-hospital-service-area/",
  hosp_serv_file)) %>%
  tbl_df() %>% 
  janitor::clean_names() %>% 
  rename(prvnumgrp = medicare_provider_number, 
         zip_code = zip_code_of_residence)  %>% 
  mutate(zip_code = str_pad(zip_code, pad = "0",width = 5))

# We now need to roll these ZIP level data up to the county level. 



convert_to_bipartite <- function(df,id) {
  id <- enquo(id)
  nn <- df %>% pull(!!id)
  foo <- df %>% select(-!!id) %>%
    as.matrix()
  
  rownames(foo) <- nn
  foo
}


  










