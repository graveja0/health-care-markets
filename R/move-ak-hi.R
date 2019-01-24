#' This function moves Alaska and Hawaii to faciliate better mapping. NOote that 
#' this function would need to be updated evey time a new geographic unit is added.
#'
#' @param dt Data
#' @param type The level of hte map (e.g., state, county, rating, etc.)
#'
#' @return
#' @export
#'
move_ak_hi <- function(dt,type) {
  
  # convert it to Albers equal area
  us_aea <- spTransform(dt, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
  #us_aea <- dt
  us_aea@data$id <- rownames(us_aea@data)

  ###rules to filter states
  if(type=="rating" ) {
    alaska <- us_aea[grepl("AK_*",us_aea$name),]
    hawaii <- us_aea[grepl("HI_*",us_aea$name),]
    alaska <- elide(alaska, rotate=-50)
    alaska <- elide(alaska, scale=max(apply(bbox(alaska), 1, diff)) / 2.3)
    alaska <- elide(alaska, shift=c(-1870000, -2500000))
  } else if (type=="county") {
    alaska <- us_aea[us_aea$STATEFP=="02",]
    hawaii <- us_aea[us_aea$STATEFP=="15",]
    alaska <- elide(alaska, rotate=-50)
    alaska <- elide(alaska, scale=max(apply(bbox(alaska), 1, diff)) / 2.3)
    alaska <- elide(alaska, shift=c(-2100000, -2500000))
  } else if (type=="state") {
      alaska <- us_aea[us_aea$name=="02",]
      hawaii <- us_aea[us_aea$name=="15",]
      alaska <- elide(alaska, rotate=-50)
      alaska <- elide(alaska, scale=max(apply(bbox(alaska), 1, diff)) / 2.3)
      alaska <- elide(alaska, shift=c(-2100000, -2500000))
  } else if (type == "hrrnum") {
    alaska <- us_aea[us_aea$hrrstate=="AK",]
    hawaii <- us_aea[us_aea$hrrstate=="HI",]
    alaska <- elide(alaska, rotate=-50)
    alaska <- elide(alaska, scale=max(apply(bbox(alaska), 1, diff)) / 2.3)
    alaska <- elide(alaska, shift=c(-2100000, -2500000))
  } else if (type == "hsanum") {
    alaska <- us_aea[us_aea$hsastate=="AK",]
    hawaii <- us_aea[us_aea$hsastate=="HI",]
    alaska <- elide(alaska, rotate=-50)
    alaska <- elide(alaska, scale=max(apply(bbox(alaska), 1, diff)) / 2.3)
    alaska <- elide(alaska, shift=c(-2100000, -2500000))
  } else if (type == "pcsa") {
    alaska <- us_aea[us_aea$pcsa_st=="AK",]
    hawaii <- us_aea[us_aea$pcsa_st=="HI",]
    alaska <- elide(alaska, rotate=-50)
    alaska <- elide(alaska, scale=max(apply(bbox(alaska), 1, diff)) / 2.3)
    alaska <- elide(alaska, shift=c(-2100000, -2500000))
  } else if (type == "cz") {
    alaska <- us_aea[us_aea$commuting_zone_id_2000 %in% 
                       c("660", "171", "103" ,"692" ,"639" ,"378" ,"701", "378" ,"638" ,"638",
                         "103" ,"649", "667", "668" ,"103", "706" ,"901" ,"901" ,"649","638",
                         "638", "679" ,"645" ,"707" ,"696","638", "378"),]
    hawaii <- us_aea[us_aea$commuting_zone_id_2000 %in% c("290" ,"290","290", "702", "290"),]
    alaska <- elide(alaska, rotate=-50)
    alaska <- elide(alaska, scale=max(apply(bbox(alaska), 1, diff)) / 2.3)
    alaska <- elide(alaska, shift=c(-2100000, -2500000))
  } else stop("type error")

  # extract, then rotate, shrink & move alaska (and reset projection)
  proj4string(alaska) <- proj4string(us_aea)

  # extract, then rotate & shift hawaii
  hawaii <- elide(hawaii, rotate=-35)
  hawaii <- elide(hawaii, shift=c(5400000, -1400000))
  proj4string(hawaii) <- proj4string(us_aea)

  if(type=="rating") {
    us_aea <- us_aea[!grepl("AK_*",us_aea$name) & !grepl("HI_*",us_aea$name),]
  } else if (type=="county") {
    us_aea <- us_aea[!us_aea$STATEFP %in% c("02", "15", "72"),]
  } else if (type=="state") {
    us_aea <- us_aea[!us_aea$name %in% c("02","15","72"),]
  } else if (type=="hrrnum") {
    us_aea <- us_aea[!us_aea$hrrstate %in% c("AK","HI"),]
  } else if (type=="hsanum") {
    us_aea <- us_aea[!us_aea$hsastate %in% c("AK","HI"),]
  } else if (type=="pcsa") {
    us_aea <- us_aea[!us_aea$pcsa_st %in% c("AK","HI"),]
  } else if (type=="cz") {
    us_aea <- us_aea[!us_aea$commuting_zone_id_2000 %in% c("660","171" ,"103", "692", "639" ,"378", "701", "378" ,"638" ,
                                                           "638" ,"103", "649", "667", "668" ,"103", "706" ,"901" ,"901" ,"649", "638", "638" ,
                                                           "679" ,"645", "707" ,"696" ,"638","378" ,"290", "290", "290" ,"702" ,"290"),]
  } else stop("type error")

  us_aea <- rbind(us_aea, alaska, hawaii)
  # remove old states and put new ones back in; note the different order
  # we're also removing puerto rico in this example but you can move it
  # between texas and florida via similar methods to the ones we just used

  us_aea2 <- spTransform(us_aea, proj4string(dt))

  return(us_aea2)
}
