#' ---
#' output: github_document
#' ---
#' 
# Note that most of the files in this document are construted in R/compare-hhi.R

suppressWarnings(suppressMessages(source(here::here("/R/manifest.R"))))
source(here("R/map-theme.R"))
source(here("R/shared-objects.R"))

# df_county <- read_rds(here("output/market-comparisons/01_market-comparisons-county.rds"))
# sf_county <- read_sf(here("output/tidy-mapping-files/county/01_county-shape-file.shp")) %>% 
#   st_transform(crs = 4326) %>% 
#   left_join(df_county,"fips_code")

cz_pop <- read_csv("public-data/shape-files/commuting-zones/counties10-zqvz0r.csv") %>% 
  janitor::clean_names() %>% 
  select(cz_id = out10, 
         cz_pop = pop10) %>% 
  unique() %>% 
  group_by(cz_id) %>% 
  summarise(cz_pop = sum(cz_pop,na.rm=TRUE))

df_cz <- read_rds(here("output/market-comparisons/01_HHI_commuting-zone.rds")) %>% 
  left_join(cz_pop,"cz_id")
sf_cz <- read_sf(here("output/tidy-mapping-files/commuting-zone/01_commuting-zone-shape-file.shp")) %>% 
  st_transform(crs = 4326) %>% 
  left_join(df_cz,"cz_id") 

tmp_cz <- 
  df_cz %>% 
  filter(!is.na(cz_id)) %>% 
  select(hhi_cz,cz_pop)  %>% 
  arrange(hhi_cz) %>% 
  tbl_df() %>% 
  ungroup() %>% 
  mutate(cumpct = cumsum(hhi_cz * cz_pop) / sum(hhi_cz * cz_pop)) %>% 
  mutate(measure = "CZ") %>% 
  select(measure,hhi = hhi_cz, cumpct) 

fn_cz <- approxfun(tmp_cz$hhi,tmp_cz$cumpct)

tmp_zip_cz <- 
  df_cz %>% 
  filter(!is.na(cz_id)) %>% 
  select(hhi_zip_cz,cz_pop)  %>% 
  arrange(hhi_zip_cz) %>% 
  tbl_df() %>% 
  ungroup() %>% 
  mutate(cumpct = cumsum(hhi_zip_cz * cz_pop) / sum(hhi_zip_cz * cz_pop)) %>% 
  mutate(measure = "ZIP") %>% 
  select(measure,hhi = hhi_zip_cz, cumpct)

fn_zip_cz <- approxfun(tmp_zip_cz$hhi,tmp_zip_cz$cumpct)

mydata = data.frame(x=seq(0,10000,10),
                    hhi_zip_cz = sapply(seq(0,10000,10), FUN = function(x){fn_zip_cz(x)}),
                    hhi_cz = sapply(seq(0,10000,10), FUN = function(x){fn_cz(x)}))


ggplot(mydata, aes(x=x)) +
  geom_line(aes(y = hhi_zip_cz ),colour = "black",lwd=1) + 
  geom_line(aes(y = hhi_cz ), colour = scales::muted("red"),lwd=1) + 
  geom_ribbon(data = subset(mydata,hhi_cz<=hhi_zip_cz), aes(ymin = hhi_zip_cz , ymax = hhi_cz ), fill = "blue", alpha = .25) +
  geom_ribbon(data = subset(mydata,hhi_cz>=hhi_zip_cz), aes(ymin = hhi_zip_cz , ymax = hhi_cz ), fill = "red", alpha = .25) +
  theme_bw(base_family = "Gill Sans") + 
  ggtitle("Empirical CDF of Hospital HHI\n(weighted by population)") + 
  annotate('text',x = 1900, y= .12,label = "Overall\nMeasure\nUnderstates\nConcentration",cex=1.5,hjust=0,color = scales::muted("red"),fontface="bold") + 
  annotate('text',x = 6250, y= .95,label = "Overall Measure\nOverstates Concentration",cex=1.5,hjust=0,color = scales::muted("blue"),fontface="bold") + 
  annotate('text',x=1000,y=.5,label = "Overall Commuting\nZone Measure", color = scales::muted("red"), cex= 2,hjust=0,fontface="bold") +
  annotate('text',x=4000,y=.35,label = "Aggregated\nCommuting\nZone Measure\n(from ZIPs)", color = "black", cex= 2,hjust=0,fontface="bold") 
ggsave(here("figs/01_HHI_ECDF-commuting-zones.png"))


df_cz  %>% 
  ungroup() %>% 
  summarise_at(vars(hhi_cz,hhi_zip_cz), function(x) quantile(x,0.5,na.rm=TRUE))
# 
# 
# # National Map
# states_to_map <- states
# sf_cz %>% 
#   mutate(diff = hhi_cz -  hhi_zip_cz) %>% 
#   filter(state_01 %in% states_to_map) %>% 
#   filter(state_01 != "HI" & state_01 !="AK") %>% 
#   ggplot() + 
#   geom_sf(aes(fill = diff)) + 
#   scale_fill_gradient2(low = scales::muted("blue"),mid = "white",high = scales::muted("red"), 
#                        midpoint = 0,limits = c(-10000,10000), breaks = c(-10000,0,10000), 
#                        name = "",
#                        labels = c("-10,000\nMore Competitive\nin Aggregate","0\nNo Difference","+10,000\nLess Competitive\nin Aggregate")) + 
#   #geom_sf(data = sf_state %>% filter(stusps %in% states_to_map), alpha = 0,lwd=.7,colour = "black") + 
#   coord_sf(datum=NA) + 
#   remove_all_axes 
#   ggthemes::theme_tufte(base_family = "Gill Sans")
