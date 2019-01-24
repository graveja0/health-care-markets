#' ---
#' output: github_document
#' ---
#' 

####################################################
####################################################
####################################################
# Create HRR and HSA Boundary and Data Files
####################################################
####################################################
####################################################

# The objective of this document is to construct the HRR, HSA, and PCSA 
# boundary and data files from the underlying ZCTA data. 
# The reason we do this over using the boundary files constructed by Dartmouth
# is that those files lack sufficient information to obtain contiguous 
# market area data. 

# Also, this file serves as a blueprint more generally for obtaining 
# polygon data frames and contiguous areas data for any clustering of
# a smaller geographic unit (e.g., counties to markets). 

# The first step is to read in the ZCTA boundary file. 
suppressWarnings(suppressMessages(source(here::here("/R/manifest.R"))))
source(here("R/move-ak-hi.R"))
source(here("R/get-geographic-info.R"))
source(here("R/map-theme.R"))

# Crosswalk from ZCTA to HRR and HSA
zcta_to_hrr_hsa <- read_csv(here("public-data/shape-files/nber-hrr-hsa-pcsa/ziphsahrr2014.csv")) %>% 
  janitor::clean_names() %>% 
  rename(ZCTA5CE10 = zipcode )
# Crosswalk for ZCTA to PSCS
pcsa_info <- foreign::read.dbf(here("public-data/shape-files/dartmouth-hrr-hsa-pcsa/ct_pcsav31.dbf")) %>% 
  janitor::clean_names() %>% 
  select(pcsa,pcsa_st, pcsa_l) %>% 
  mutate_at(vars(pcsa,pcsa_st,pcsa_l),funs(paste0)) %>% 
  unique()
zcta_to_pcsa <- foreign::read.dbf(here("public-data/shape-files/dartmouth-hrr-hsa-pcsa/zip5_pcsav31.dbf")) %>% 
  janitor::clean_names() %>% 
  mutate(pcsa = paste0(pcsa)) %>% 
  left_join(pcsa_info,"pcsa")


# ARCGIS Shape File for ZCTAs (source shape file)
zcta_map_shape <- suppressMessages(suppressWarnings(
  readShapeSpatial(here("public-data/shape-files/zcta-2017/tl_2017_us_zcta510/tl_2017_us_zcta510"))
  ))

# Reproject the map. 
proj4string(zcta_map_shape) <- CRS("+proj=longlat +datum=NAD27") # Update the projection
zcta_map_reproj <- spTransform(zcta_map_shape,CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96"))

# Buffer the shape file to clean up some polygons. 
suppressWarnings({
  zcta_map <- zcta_map_reproj %>% 
 gBuffer(byid=TRUE,width=0)
})

# Assign the ZIP code as the polygon identifier. 
zcta_map_a <- 
  spChFIDs(zcta_map,paste0(zcta_map$ZCTA5CE10))

# Extract the polygon objects
zcta_map_pl <- slot(zcta_map_a,"polygons")

# Check the polygons for completeness (note: this step takes a while).
zcta_map_pl_a <- lapply(zcta_map_pl,checkPolygonsHoles)

# Get the coordinate system
zcta_map_crs <- CRS(proj4string(zcta_map_a))

# Get the spatial polygons
zctaSP <- SpatialPolygons(zcta_map_pl_a, proj4string = zcta_map_crs ) 

# Create the final ZCTA map object
zcta_map_final <- SpatialPolygonsDataFrame(zctaSP, data = as(zcta_map_a, "data.frame")) 

# An issue now comes up because some of the ZCTAs mapped do not show up in the Dartmouth data, and 
# vice versa. We need to clean up both objects so that they share only a common set of ZCTAs. 

# These are the ZCTAs in the map object.
zcta_in_map <- row.names(zcta_map_final)

# HRR Data Frame (contains information on HRR ID, City, State, etc.)
  df_hrr <- zcta_to_hrr_hsa %>% 
    select(ZCTA5CE10, hrrnum,hrrcity,hrrstate,hrr_year = year) %>% 
    data.frame() 
# Must have ZCTAs as row names to merge onto map objects
  rownames(df_hrr) <- df_hrr$ZCTA5CE10
  df_hrr <- df_hrr %>% select(-ZCTA5CE10) 

 
# HSA Data Frame (contains information on HSA ID, City, State, etc.)
  df_hsa<- zcta_to_hrr_hsa %>% 
    select(ZCTA5CE10, hsanum,hsacity,hsastate,hsa_year = year) %>% 
    data.frame() 
  # Must have ZCTAs as row names to merge onto map objects
  rownames(df_hsa) <- df_hsa$ZCTA5CE10
  df_hsa <- df_hsa %>% select(-ZCTA5CE10) 
 

ztas_in_map_but_not_hrr <- setdiff(row.names(zcta_map_final),row.names(df_hrr))
  
zcta_map_final2 <- 
  subset(zcta_map_final, !(ZCTA5CE10 %in% setdiff(row.names(zcta_map_final),row.names(df_hrr)) )) 
  
#########################
# Construct HRR Map Data
#########################

  # Edit down the HRR Data to only include matched ZCTAs
  df_hrr <- df_hrr[match(row.names(zcta_map_final),row.names(df_hrr)),]
  df_hrr2 <- df_hrr[-which(row.names(df_hrr) %in% setdiff(row.names(df_hrr),row.names(zcta_map_final2))),]
  
  # Spatial merge the ZCTA Map with the HRR Data
  # Note: this takes a few minutes. 
  zcta_map_with_hrr <- spCbind(zcta_map_final2, df_hrr2)
  hrr_map_merged <- unionSpatialPolygons(zcta_map_with_hrr,zcta_map_with_hrr$hrrnum) 
  
  # Construct a data frame object that will get appended onto the map (needed to get contiguous HRR data, 
  # and to merge in other HRR attributes later). 
  hrr_map_merged_df <- 
    slot(hrr_map_merged, "polygons") %>% 
    map_chr(~(slot(.x,"ID"))) %>% 
    as.data.frame() %>% 
    set_names("hrrnum") %>% 
    tbl_df() %>% 
    mutate(hrrnum = paste0(hrrnum)) %>% 
    data.frame() %>% 
    left_join(df_hrr %>% select(hrrnum,everything()) %>% unique() %>% mutate(hrrnum = paste0(hrrnum)),"hrrnum")
  rownames(hrr_map_merged_df) <- hrr_map_merged_df$hrrnum
  
  hrr_map <- SpatialPolygonsDataFrame(hrr_map_merged, data = hrr_map_merged_df)
  hrr_map <- move_ak_hi(hrr_map,type="hrrnum")
  hrr_map <- gBuffer(hrr_map, byid=TRUE, width=0)
  
  df_hrr_info <- 
    hrr_map %>% 
    get_geograhic_info(hrrnum, get_contiguous = TRUE) 
  
  hrr_map_simple <- gSimplify(hrr_map, tol = 300)
  
  df_hrr_map = fortify(hrr_map_simple,region = "hrrnum") %>%
    rename(hrrnum = id) %>%
    dplyr::select(hrrnum,everything()) %>% 
    tbl_df()
  dim(df_hrr_map)
  
  df_hrr_map %>%
    left_join(df_hrr_info, "hrrnum") %>% 
    filter(hrrstate %in% c("TN")) %>% 
    tbl_df() %>%
    mutate(test = factor(sample(1:10,nrow(.),replace=TRUE))) %>%
    ggplot() +
    aes(long,lat,group=group) +
    geom_polygon(aes(fill = test)) +
    geom_path(color="black") +
    coord_equal() +
    ggthemes::theme_tufte() +
    theme(legend.position = "none") +
    remove_all_axes +
    ggtitle("tol = 200")

  # Write the final output.
  if (!dir.exists(here("output/tidy-mapping-files/hrr"))) dir.create(here("output/tidy-mapping-files/hrr"))
  write_rds(df_hrr_info,here("output/tidy-mapping-files/hrr/","df_hrr_info.rds"))
  write_rds(df_hrr_map %>% left_join(df_hrr_info, "hrrnum"),here("output/tidy-mapping-files/hrr/","df_hrr.rds"))
  

  #########################
  # Construct HSA Map Data
  #########################
  
  # Edit down the HRR Data to only include matched ZCTAs
  df_hsa <- df_hsa[match(row.names(zcta_map_final),row.names(df_hsa)),]
  df_hsa2 <- df_hsa[-which(row.names(df_hsa) %in% setdiff(row.names(df_hsa),row.names(zcta_map_final2))),]
  
  # Spatial merge the ZCTA Map with the HRR Data
  # Note: this takes a few minutes. 
  zcta_map_with_hsa <- spCbind(zcta_map_final2, df_hsa2)
  hsa_map_merged <- unionSpatialPolygons(zcta_map_with_hsa,zcta_map_with_hsa$hsanum) 
  
  # Construct a data frame object that will get appended onto the map (needed to get contiguous HRR data, 
  # and to merge in other HRR attributes later). 
  hsa_map_merged_df <- 
    slot(hsa_map_merged, "polygons") %>% 
    map_chr(~(slot(.x,"ID"))) %>% 
    as.data.frame() %>% 
    set_names("hsanum") %>% 
    tbl_df() %>% 
    mutate(hsanum = paste0(hsanum)) %>% 
    data.frame() %>% 
    left_join(df_hsa %>% select(hsanum,everything()) %>% unique() %>% mutate(hsanum = paste0(hsanum)),"hsanum")
  rownames(hsa_map_merged_df) <- hsa_map_merged_df$hsanum
  
  hsa_map <- SpatialPolygonsDataFrame(hsa_map_merged, data = hsa_map_merged_df)
  hsa_map <- move_ak_hi(hsa_map,type="hsanum")
  hsa_map <- gBuffer(hsa_map, byid=TRUE, width=0)
  
  df_hsa_info <- 
    hsa_map %>% 
    get_geograhic_info(hsanum, get_contiguous = TRUE) 
  
  # Simplify the polygon
  hsa_map_simple <- gSimplify(hsa_map, tol = 300)
  
  # Convert to a regular data frame object for ggplot mapping. 
  df_hsa_map = fortify(hsa_map_simple,region = "hsanum") %>%
    rename(hsanum = id) %>%
    dplyr::select(hsanum,everything()) %>% 
    tbl_df()
  dim(df_hsa_map)
  
  df_hsa_map %>%
    left_join(df_hsa_info, "hsanum") %>% 
    filter(hsastate %in% c("TN")) %>% 
    tbl_df() %>%
    mutate(test = factor(sample(1:10,nrow(.),replace=TRUE))) %>%
    ggplot() +
    aes(long,lat,group=group) +
    geom_polygon(aes(fill = test)) +
    geom_path(color="black") +
    coord_equal() +
    ggthemes::theme_tufte() +
    theme(legend.position = "none") +
    remove_all_axes
  
  # Write the final output.
  if (!dir.exists(here("output/tidy-mapping-files/hsa"))) dir.create(here("output/tidy-mapping-files/hsa"))
  write_rds(df_hsa_info,here("output/tidy-mapping-files/hsa/","df_hsa_info.rds"))
  write_rds(df_hsa_map %>% left_join(df_hsa_info, "hsanum"),here("output/tidy-mapping-files/hsa/","df_hsa.rds"))
  
  
  #########################
  # Construct PCSA Map Data
  #########################
  # PCSA Data Frame (contains information on HSA ID, City, State, etc.)
  df_pcsa <- zcta_to_pcsa %>%
    data.frame() 
  rownames(df_pcsa) <- df_pcsa$zip5
  df_pcsa <- df_pcsa %>% select(-zip5)
  
  ztas_in_map_but_not_pcsa <- setdiff(row.names(zcta_map_final),row.names(df_pcsa))
  
  zcta_map_final_pcsa <- 
    subset(zcta_map_final, !(ZCTA5CE10 %in% setdiff(row.names(zcta_map_final),row.names(df_pcsa)) )) 
  
  # Edit down the HRR Data to only include matched ZCTAs
  df_pcsa2 <- df_pcsa[-which(row.names(df_pcsa) %in% setdiff(row.names(df_pcsa),row.names(zcta_map_final_pcsa))),]
  df_pcsa <- df_pcsa2[match(row.names(zcta_map_final_pcsa),row.names(df_pcsa2)),]
  
  # Spatial merge the ZCTA Map with the HRR Data
  # Note: this takes a few minutes. 
  zcta_map_with_pcsa <- spCbind(zcta_map_final_pcsa, df_pcsa)
  pcsa_map_merged <- unionSpatialPolygons(zcta_map_with_pcsa,zcta_map_with_pcsa$pcsa) 
  
  # Construct a data frame object that will get appended onto the map (needed to get contiguous HRR data, 
  # and to merge in other HRR attributes later). 
  pcsa_map_merged_df <- 
    slot(pcsa_map_merged, "polygons") %>% 
    map_chr(~(slot(.x,"ID"))) %>% 
    as.data.frame() %>% 
    set_names("pcsa") %>% 
    tbl_df() %>% 
    mutate(pcsa = paste0(pcsa)) %>% 
    data.frame() %>% 
    left_join(df_pcsa %>% select(pcsa,everything()) %>% unique() %>% mutate(pcsa = paste0(pcsa)),"pcsa")
  rownames(pcsa_map_merged_df) <- pcsa_map_merged_df$pcsa
  
  pcsa_map <- SpatialPolygonsDataFrame(pcsa_map_merged, data = pcsa_map_merged_df)
  pcsa_map <- move_ak_hi(pcsa_map,type="pcsa")
  pcsa_map <- gBuffer(pcsa_map, byid=TRUE, width=0)
  
  #foo <- subset(pcsa_map,pcsa_st=="TN") %>% gSimplify(tol = 200); plot(foo)
  
  df_pcsa_info <- 
    pcsa_map %>% 
    get_geograhic_info(pcsa, get_contiguous = TRUE)  %>% 
    filter(pcsa_st != "NA")
  
  pcsa_map_simple <- gSimplify(pcsa_map, tol = 300)
  
  df_pcsa_map = fortify(pcsa_map_simple,region = "pcsa") %>%
    rename(pcsa = id) %>%
    dplyr::select(pcsa,everything()) %>% 
    tbl_df() #%>% 
    #filter(pcsa != "-80")
  dim(df_hrr_map)
  
  df_pcsa_map %>%
    left_join(df_pcsa_info, "pcsa") %>% 
    filter(pcsa_st %in% c("TN")) %>% 
    filter(id != "" & !is.na(id)) %>% 
    tbl_df() %>%
    mutate(test = factor(sample(1:10,nrow(.),replace=TRUE))) %>%
    ggplot() +
    aes(long,lat,group=group) +
    geom_polygon(aes(fill = test)) +
    geom_path(color="black") +
    coord_equal() +
    ggthemes::theme_tufte() +
    theme(legend.position = "none") +
    #geom_text(aes(x= centroid_x, y = centroid_y, label = id)) +
    remove_all_axes
  
  # Write the final output.
  if (!dir.exists(here("output/tidy-mapping-files/pcsa"))) dir.create(here("output/tidy-mapping-files/pcsa"))
  write_rds(df_pcsa_info,here("output/tidy-mapping-files/pcsa/","df_pcsa_info.rds"))
  write_rds(df_pcsa_map %>% left_join(df_pcsa_info, "pcsa"),here("output/tidy-mapping-files/pcsa/","df_pcsa.rds"))
  
  