if (Sys.getenv("LOGNAME") == "gravesj") source("~/auth-aws.r")
project_bucket <- "health-care-markets"
aws_files <- get_bucket(project_bucket) %>% 
  transpose() %>% 
  pluck("Key") %>% 
  unlist() %>% 
  tbl_df() 

get_aws_files <- function(project_bucket = "vumc.graves.networks.proj", prefix = "") {
  get_bucket(project_bucket, prefix = prefix) %>%
    transpose() %>%
    purrr::pluck("Key") %>%
    unlist() %>%
    tbl_df()
}

# put_folder("geographic-crosswalks",project_bucket)
# put_folder("hosptial-county-patient-data",project_bucket)
# put_folder("market-comparisons",project_bucket)
# put_folder("ska",project_bucket)
# put_folder("tidy-mapping-files",project_bucket)
#put_folder("tidy-mapping-files/commuting-zone",project_bucket)

rename_in_list <- function(x,from, to) {
  x %>% rename_at(vars(contains(from)), funs(sub(from, to, .)))
}

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


#' Title: Function to Obtain Mapbox Isochrones
#'
#' @param long longitute for a given zipcode
#' @param lat latitude for a given zipcode
#' @param contours_minutes a comma-separated string of travel times. The maximum allowed is 4 travel times of no-more than 60 minutes.
#' @param base_url 
#' @param mapbox_token The given private token provided by MAPBOX on registration
#'
#' @return a named list of coordinates consisting of isochrones with travel time equivalent to those specified in contour_minutes


get_mapbox_isochrone <- function(long, lat, contours_minutes, base_url = "https://api.mapbox.com/", mapbox_token = Sys.getenv("MAPBOX_API_TOKEN")) {
  request_url <- paste0(
    "isochrone/v1/mapbox/driving/",
    long, ",", lat, "?contours_minutes=", contours_minutes,
    "&polygons=true&access_token=", mapbox_token
  )
  url <- modify_url(base_url, path = request_url)
  
  tryCatch({
    temp <- suppressWarnings(httr::GET(url, verbose = T))
    Sys.sleep(1)
  },
  error = function(cond) {
    message(paste("URL does not seem to exist:", url))
    message("Here's the original error message:")
    message(cond)
    # Choose a return value in case of error
    return(NA)
  },
  finally = {
    out <- suppressMessages(jsonlite::fromJSON(content(temp, "text"),
                                               simplifyVector = FALSE, simplifyDataFrame = TRUE, flatten = TRUE
    ))
    
    name_iso <- sort(unlist(str_split(contours_minutes, pattern = ", ", n = 4)), decreasing = T)
    # print(name_iso)
    coords <- out$features$geometry.coordinates
    
    # If isochrone is found, name the the subsets by their corresponding driving-times
    # for example, a 10 minute isochrones is named "10"
    if (!is.null(coords)) {
      names(coords) <- name_iso
    }
    return(coords)
  }
  )
}
