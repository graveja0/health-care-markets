#' ---
#' output: github_document
#' ---


# The basis of the COUNTY to COMMUTING ZONE conversion is the crosswalk file downloaded from
# https://www.ers.usda.gov/data-products/commuting-zones-and-labor-market-areas/ on 2019-01-24.

suppressWarnings(suppressMessages(source(here::here("R/manifest.R"))))
source(here("R/move-ak-hi.R"))
source(here("R/get-geographic-info.R"))
source(here("R/map-theme.R"))
source(here("R/shared-objects.R"))
source(here("R/get-contiguous-areas.R"))

# STATE FIPS TO STATE ABBREVIATION
fips_to_state <- read_rds(here("output/geographic-crosswalks/01_xw_county-to-fips.rds")) %>% 
  mutate(statefp = str_sub(fips_code,1,2)) %>% 
  select(statefp,state) %>% unique()

county_fips_to_state <- read_rds(here("output/geographic-crosswalks/01_xw_county-to-fips.rds")) %>% 
  mutate(statefp = str_sub(fips_code,1,2))  %>% 
  select(fips_code,state)

# Source: https://sites.psu.edu/psucz/data/
county_to_cz <- data.table::fread(here("public-data/shape-files/commuting-zones/counties10-zqvz0r.csv")) %>% 
  janitor::clean_names() %>% 
  rename(fips_code = fips) %>% 
  group_by(out10) %>% 
  mutate(commuting_zone_population_2010 = sum(pop10, na.rm=TRUE)) %>% 
  mutate(fips_code = str_pad(paste0(fips_code),width = 5, pad="0")) %>% 
  select(fips_code,
         commuting_zone_id_2010 = out10,
         commuting_zone_population_2010 ) 

df_cz <- data.table::fread(here("public-data/shape-files/commuting-zones/counties10-zqvz0r.csv")) %>% 
  janitor::clean_names() %>% 
  rename(fips_code = fips) %>% 
  group_by(out10) %>% 
  mutate(commuting_zone_population_2010 = sum(pop10, na.rm=TRUE)) %>% 
  mutate(fips_code = str_pad(paste0(fips_code),width = 5, pad="0")) %>% 
  select(fips_code,
         cz_id = out10 ) %>% 
  left_join(county_fips_to_state,"fips_code") %>% 
  select(cz_id,state) %>% 
  group_by(cz_id) %>% 
  unique() %>% 
  mutate(foo = paste0("state_",str_pad(paste0(row_number()),width=2,pad="0"))) %>% 
  spread(foo,state)

cz_info <- county_to_cz %>% 
  select(contains("commuting_zone")) %>% 
  unique()

# County Shape File
shp_cz <- sf::read_sf(here("public-data/shape-files/county-2017/cb_2017_us_county_5m/cb_2017_us_county_5m.shp")) %>% 
  #sf::st_transform(crs = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs")  %>% 
  sf::st_transform(crs ="+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96") %>% 
  janitor::clean_names() %>% 
  left_join(fips_to_state,"statefp") %>% 
  filter(state %in% states) %>% 
  move_ak_hi(state = state) %>% 
  mutate(fips_code = geoid) %>% 
  left_join(county_to_cz, "fips_code") %>% 
  group_by(commuting_zone_id_2010) %>% 
  summarise() %>% 
  ungroup() %>% 
  st_simplify(dTolerance = 100)  %>% 
  left_join(get_contiguous(shp = ., id = commuting_zone_id_2010) %>% 
              mutate(commuting_zone_id_2010 = as.integer(commuting_zone_id_2010)), "commuting_zone_id_2010") %>% 
  rename(cz_id = commuting_zone_id_2010) %>% 
  left_join(df_cz,"cz_id")

shp_cz %>% 
  sf::write_sf(here("output/tidy-mapping-files/commuting-zone/01_commuting-zone-shape-file.shp"))

shp_cz %>% 
  filter(state_01=="TN" | state_02=="TN") %>% 
  ggplot() + geom_sf() + theme_bw() + coord_sf(datum=NA) +
  remove_all_axes

shape_types %>% 
 map(~(
    put_object(
      file  = paste0("./output/tidy-mapping-files/commuting-zone/01_commuting-zone-shape-file.",.x),
      bucket = paste0(project_bucket, "/tidy-mapping-files/commuting-zone")
    )
))

# FIPS to Commuting Zone Crosswalk Data
df_cz_final <- 
  county_to_cz %>% 
  rename(cz_id = commuting_zone_id_2010, 
         cz_pop = commuting_zone_population_2010) %>% 
  left_join(df_cz,"cz_id")
write_rds(df_cz_final, "output/geographic-crosswalks/01_fips_to_cz.rds")
s3saveRDS(df_cz_final,
          bucket = paste0(project_bucket,"/geographic-crosswalks"), 
                          object = "01_fips_to_commuting_zone.rds")
