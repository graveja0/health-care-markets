project_bucket <- "health-care-markets"
aws_files <- get_bucket(project_bucket) %>% 
  transpose() %>% 
  pluck("Key") %>% 
  unlist() %>% 
  tbl_df() 
# put_folder("geographic-crosswalks",project_bucket)
# put_folder("hosptial-county-patient-data",project_bucket)
# put_folder("market-comparisons",project_bucket)
# put_folder("ska",project_bucket)
# put_folder("tidy-mapping-files",project_bucket)
#put_folder("tidy-mapping-files/commuting-zone",project_bucket)

states <- c(
  "AK",
  "AL",
  "AR",
  "AZ",
  "CA",
  "CO",
  "CT",
  "DC",
  "DE",
  "FL",
  "GA",
  "HI",
  "IA",
  "ID",
  "IL",
  "IN",
  "KS",
  "KY",
  "LA",
  "MA",
  "MD",
  "ME",
  "MI",
  "MN",
  "MO",
  "MS",
  "MT",
  "NC",
  "ND",
  "NE",
  "NH",
  "NJ",
  "NM",
  "NV",
  "NY",
  "OH",
  "OK",
  "OR",
  "PA",
  "RI",
  "SC",
  "SD",
  "TN",
  "TX",
  "UT",
  "VA",
  "VT",
  "WA",
  "WI",
  "WV",
  "WY"
)

shape_types <- c("dbf","prj","shp","shx")

