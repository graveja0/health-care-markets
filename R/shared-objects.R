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


census_regions <- 
  list(
    "ak_hi" = c("AK","HI"),
    "west_pacific" = c("CA","OR","WA"),
    "west_mountain" = c("MT","ID","WY","UT","CO","NM","AZ","NV"), 
    "midwest_west" = c("ND","SD","MN","NE","IA","KS","MO"),
    "midwest_east" = c("WI","MI","IL","IN","OH"),
    "south_west" = c("TX","OK","AR","LA"),
    "south_central" = c("KY","TN","MS","AL"),
    "south_atlantic" = c("MD","DE","DC","WV","VA","NC","SC","GA","FL"), 
    "northeast_middle"= c("PA","NJ","NY"),
    "northeast_newengland" = c("CT","RI","MA","NH","VT","ME")
    
  )

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

