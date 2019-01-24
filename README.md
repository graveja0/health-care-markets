
The objective of this document is to lay out some thoughts, analytics,
and data for defining geographic markets for health care services in the
U.S.

``` r
df_hrr_map <- read_rds(here("output/tidy-mapping-files/hrr/","df_hrr.rds"))
p_hrr <- 
  df_hrr_map %>%
    filter(hrrstate %in% c("TN")) %>% 
    tbl_df() %>%
    mutate(test = factor(sample(1:10,nrow(.),replace=TRUE))) %>%
    ggplot() +
    aes(long,lat,group=group) +
    geom_polygon(aes(fill = test)) +
    geom_path(color="black") +
    coord_equal() +
  ggthemes::theme_tufte(base_family = "Gill Sans") +
  theme(legend.position = "none") +
  remove_all_axes +
  ggtitle("Hospital Referral Region\n(HRR)" ) 


df_hsa_map <- 
  read_rds(here("output/tidy-mapping-files/hsa/","df_hsa.rds")) 
p_hsa <- 
  df_hsa_map %>%
    filter(hsastate %in% c("TN")) %>% 
    tbl_df() %>%
    mutate(test = factor(sample(1:10,nrow(.),replace=TRUE))) %>%
    ggplot() +
    aes(long,lat,group=group) +
    geom_polygon(aes(fill = test)) +
    geom_path(color="black") +
    coord_equal() +
  ggthemes::theme_tufte(base_family = "Gill Sans") +
  theme(legend.position = "none") +
  remove_all_axes +
  ggtitle("Hospital Service Area\n(HSA)") 

df_pcsa_map <- read_rds(here("output/tidy-mapping-files/pcsa/","df_pcsa.rds"))
p_pcsa <- 
  df_pcsa_map %>%
    filter(pcsa_st %in% c("TN")) %>% 
    filter(id != "" & !is.na(id)) %>% 
    tbl_df() %>%
    mutate(test = factor(sample(1:10,nrow(.),replace=TRUE))) %>%
    ggplot() +
    aes(long,lat,group=group) +
    geom_polygon(aes(fill = test)) +
    geom_path(color="black") +
    coord_equal() +
  ggthemes::theme_tufte(base_family = "Gill Sans") +
  theme(legend.position = "none") +
  remove_all_axes +
  ggtitle("Primary Care Service Area\n(PCSA)") 

df_cz_map <- read_rds(here("output/tidy-mapping-files/commuting-zone/","df_cz.rds"))


county_to_cz <- readxl::read_xls(here("public-data/shape-files/commuting-zones/cz00_eqv_v1.xls")) %>% 
  janitor::clean_names()  %>% 
  rename(fips_code = fips) 

tn_czs <- 
  county_to_cz %>% mutate(commuting_zone_id_2000 = paste0(commuting_zone_id_2000)) %>% filter(str_sub(fips_code,1,2) %in% c("47")) %>% pull(commuting_zone_id_2000)


p_cz <- 
  df_cz_map %>%
  filter(commuting_zone_id_2000 %in% tn_czs) %>% 
  tbl_df() %>%
  mutate(test = factor(sample(1:10,nrow(.),replace=TRUE))) %>%
  ggplot() +
  aes(long,lat,group=group) +
  geom_polygon(aes(fill = test)) +
  geom_path(color="black") +
  coord_equal() +
  ggthemes::theme_tufte(base_family = "Gill Sans") +
  theme(legend.position = "none") +
  remove_all_axes +
  ggtitle("Commuting Zone\n(CZ-2000)") 
  

p_hrr + p_hsa + p_pcsa + p_cz + plot_layout(nrow=2)
```

![](README_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

# File Descriptions

## Geographic Mapping Files

  - The file
    [R/construct-county-map-data.R](R/construct-county-map-data.md)
    constructs ggplot-friendly mapping data for U.S. counties
    (`output/tidy-mapping-files/county/df_county.rds`). It also extracts
    contiguous counties and estimates county centroids
    (`output/tidy-mapping-files/county/df_county_info.rds`).

  - The file
    [R/construct-dartmouth-geography-data.R](R/construct-dartmouth-geography-data.R)
    constructs ggplot-friendly mapping data for Dartmouth Atlas
    geographies including Hospital Referral Region (HRR), Hospital
    Service Region (HSA) and Primary Care Service Region (PCSA).

## Patient Sharing Files

  - The file
    [R/read-and-tidy-cms-hospital-service-areas.R](R/read-and-tidy-cms-hospital-service-areas.R)
    reads in the CMS Hospital Service Area file for 2017. Note the
    source of these data are downloaded csv files from the interactive
    CMS data explorer available at the links at [this
    link](https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Hospital-Service-Area-File/index.html).
    The final file is rolled up to the FIPS county level and is storeed
    in
    `output/hospital-county-patient-data/2017/hospital-county-patient-data.rds`.
