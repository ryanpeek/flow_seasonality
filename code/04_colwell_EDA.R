# rerun Colwell with revised flows from REF

library(hydrostats)
library(tidyverse)
library(purrr)
library(lubridate)
library(glue)
library(tidylog)
library(ggdark)
library(sf)
library(mapview)
mapviewOptions(fgb=FALSE)

# 01: Import Flow/GAGE Data -----------------------------------------------

# flow data
load("data/usgs_Q_daily_alt_gages_trim.rda") # nrow=8977068
load("data/usgs_Q_daily_ref_gages_trim.rda") # nrow=2442511

# get gage metadata:
gage_alt_meta <- usgs_flows_alt_trim %>% distinct(site_no, .keep_all=TRUE) %>% 
  select(site_no,station_nm:flowcnt) # n=517
gage_ref_meta <- usgs_flows_ref_trim %>% distinct(site_no, .keep_all=TRUE) %>% 
  select(site_no,station_nm, usgs_lat, usgs_lon, huc8, parm_cd, stat_cd, 
         ref_yr_start, ref_yr_end, ref_por_range, yr_begin, yr_end, yr_total, 
         gagetype, parm_cd:flowcnt) # n=221

# bind rows
gage_metadata <- bind_rows(gage_alt_meta, gage_ref_meta)

# 02: CALCULATE COLWELL -------------------------------------------------------

# quick colwell analysis of seasonality:
## From Tonkin et al 2017: M (Contingency) as metric of seasonality
## To standardize the role of seasonality in relation to overall predictability,
## we divided (M) by overall predictability (the sum of (M) and constancy (C)

# standardize
df_ref <- usgs_flows_ref_trim %>% 
  ungroup() %>% 
  rename("Q"=Flow, Date=date) %>% 
  select(Date, Q, site_no, flowcnt) %>% 
  group_by(site_no) %>% 
  group_split() %>%
  #.[1:30] %>% # extract 30 to test
  map(., ~as.data.frame(.x))

# calc colwells and add ID
df_colwell_ref <- df_ref %>% 
  map(., ~hydrostats::Colwells(.x)) %>% 
  map(., ~tibble(MP_metric=c(.x[["MP"]]))) %>%
  bind_rows() %>% 
  mutate(site_no = gage_ref_meta$site_no,
         gagetype = "REF")

# standardize
df_alt <- usgs_flows_alt_trim %>%   
  rename("Q"=Flow, Date=date) %>% 
  select(Date, Q, site_no) %>% 
  group_by(site_no) %>% 
  group_split() %>%
  #.[1:30] %>% # extract 30 to test
  map(., ~as.data.frame(.x)) 

# calc colwells and add ID
df_colwell_alt <- df_alt %>% 
  map(., ~hydrostats::Colwells(.x)) %>% 
  map(., ~tibble(MP_metric=c(.x[["MP"]]))) %>%
  bind_rows() %>% 
  mutate(site_no = gage_alt_meta$site_no,
         gagetype = "ALT")

summary(df_colwell_alt)
summary(df_colwell_ref)

# BIND: 
df_colwell_all <- bind_rows(df_colwell_alt, df_colwell_ref)
table(df_colwell_all$gagetype)

# ALT REF 
# 517 221

# 03: JOIN WITH META ----------------------------------------------------------

df_colwell_all_meta <- df_colwell_all %>% 
  left_join(., gage_metadata %>% select(-gagetype), by=c("site_no"))

table(df_colwell_all$gagetype, useNA = "ifany")

# 04: SAVE ------------------------------------------------------------------

# save
write_rds(df_colwell_all_meta, file = "output/usgs_gages_colwells_metric.rds")

# 05: READ IN DATA ---------------------------------------------------------

df_colwell_all_meta <- read_rds("output/usgs_gages_colwells_metric.rds")

# 06: PLOTS -----------------------------------------------------------------

# Histogram
df_colwell_all %>% ggplot() + geom_histogram(aes(y=MP_metric, fill=gagetype))

# Boxplot of ref/alt
df_colwell_all_meta %>% 
  ggplot() + 
  geom_jitter(aes(y=MP_metric, x=gagetype), color="gray20", alpha=0.5) +
  geom_boxplot(aes(y=MP_metric, x=gagetype, fill=gagetype), lwd=0.75, alpha=0.85, color="black") +
  theme_classic(base_family = "Roboto Condensed")+
  scale_fill_viridis_d(option = "A", "Gage Type") +
  labs(y="Intra-annual Seasonality (Colwell's M/P)", x="",
       caption = "Standardized seasonality in relation to overall predictability \nby dividing seasonality (M) by overall predictability \n(the sum of (M) and constancy (C)), as per Tonkin et al. (2017)")

ggsave(filename = "figures/boxplot_colwells_ref_alt.png", width = 10, height = 8, dpi = 300, units = "in")

# Notched Boxplot of ref/alt
df_colwell_all_meta %>% 
  ggplot() + 
  geom_jitter(aes(y=MP_metric, x=gagetype), color="gray20", alpha=0.5) +
  geom_boxplot(aes(y=MP_metric, x=gagetype, fill=gagetype), lwd=0.75, alpha=0.85, color="black", notch = TRUE) +
  theme_classic(base_family = "Roboto Condensed")+
  scale_fill_viridis_d(option = "A", "Gage Type") +
  labs(y="Intra-annual Seasonality (Colwell's M/P)", x="",
       caption = "Standardized seasonality in relation to overall predictability \nby dividing seasonality (M) by overall predictability \n(the sum of (M) and constancy (C)), as per Tonkin et al. (2017)")

ggsave(filename = "figures/boxplot_notched_colwells_ref_alt.png", width = 10, height = 8, dpi = 300, units = "in")

# 07A: IMPORT CSCI -------------------------------------------------------

# bring in data:
csci_trim <- read_rds("data/02c_selected_final_bmi_csci_dat_trim.rds")
csci_trim %>% st_drop_geometry() %>% 
  #distinct(StationCode, site_id, .keep_all = TRUE) %>% nrow() # 431
  #distinct(StationCode, .keep_all = TRUE) %>% nrow() # 246 CSCI sites
  distinct(site_id, .keep_all = TRUE) %>% nrow() # 209 CSCI sites
table(csci_trim$CEFF_type, useNA = "ifany") # n=527 alt, n=193 REF

# older dataset w alt and ref gages
csci_por <- read_rds("data/04_selected_csci_ffm_por_trim.rds") %>% 
  select(StationCode:HUC_12, site_id, comid_gage:comid_ffc) %>% 
  distinct(StationCode, SampleID, site_id, .keep_all=TRUE)
table(csci_por$CEFF_type, useNA = "ifany") # n=521 alt, n=193 REF


# 07B: IMPORT ASCI ------------------------------------------------------------

asci_trim <- read_rds("https://github.com/ksirving/asci_ffm_2019/blob/master/output_data/06_asci_por_trim_final_dataset.rds?raw=true") %>% st_drop_geometry() %>% 
  select(StationCode:HUC_12, CEFF_type, 
         site_id, comid_gage, site_id_nm = station_nm,
         SampleID:YYYY) %>%  
  distinct(StationCode, SampleID, site_id, .keep_all=TRUE)
table(asci_trim$CEFF_type)

# 08: JOIN WITH COLWELL AND ASCI/CSCI -----------------------------------------------

# join w seasonality:
csci_trim_colwell <- left_join(csci_trim, df_colwell_all_meta, by=c("site_id"="site_no")) %>%
  distinct(StationCode, SampleID, site_id, .keep_all=TRUE) %>% 
  filter(!is.na(gagetype))
  
table(csci_trim_colwell$gagetype, useNA = "ifany") # alt 430, ref 193

csci_por_colwell <- left_join(csci_por, df_colwell_all_meta, by=c("site_id"="site_no")) %>%
  distinct(StationCode, SampleID, site_id, .keep_all=TRUE) %>% 
  filter(!is.na(gagetype))

table(csci_por_colwell$gagetype, useNA = "ifany") # alt 430, ref 193

# ASCI
asci_colwell <- left_join(asci_trim, df_colwell_all_meta, by=c("site_id"="site_no")) %>%
  distinct(StationCode, SampleID, site_id, .keep_all=TRUE) %>% 
  # drop crap gages (canals etc)
  filter(!is.na(gagetype))

  
table(asci_colwell$gagetype, useNA = "ifany") # n=260

# 09: GAM PLOT ---------------------------------------------------------------

# CSCI vs GAGETYPE
csci_por_colwell %>% 
  ggplot() + 
  geom_point(aes(y=MP_metric, x=csci, fill=gagetype), pch=21, size=2.7, alpha=0.9) +
  stat_smooth(aes(y=MP_metric, x=csci, color=gagetype), 
              method = "gam") +
  theme_classic(base_family = "Roboto Condensed") +
  scale_color_viridis_d(option = "B", "Gage Type") +
  scale_fill_viridis_d(option = "A", "Gage Type") +
  labs(y="Intra-annual Seasonality \n(Colwell's M/P)", x="CSCI",
       caption = "Standardized seasonality in relation to overall predictability \nby dividing seasonality (M) by overall predictability \n(the sum of (M) and constancy (C)), as per Tonkin et al. (2017)")

ggsave(filename = "figures/colwells_ref_alt_vs_csci_gam_trend.png", width = 10, height = 8, dpi = 300, units = "in")

# ASCI vs GAGETYPE
asci_colwell %>% 
  ggplot() + 
  geom_point(aes(y=MP_metric, x=H_ASCI, fill=gagetype), pch=21, size=2.7, alpha=0.9) +
  stat_smooth(aes(y=MP_metric, x=H_ASCI, color=gagetype), 
              method = "gam") +
  theme_classic(base_family = "Roboto Condensed") +
  scale_color_viridis_d(option = "B", "Gage Type") +
  scale_fill_viridis_d(option = "A", "Gage Type") +
  labs(y="Intra-annual Seasonality \n(Colwell's M/P)", x="ASCI",
       caption = "Standardized seasonality in relation to overall predictability \nby dividing seasonality (M) by overall predictability \n(the sum of (M) and constancy (C)), as per Tonkin et al. (2017)")

ggsave(filename = "figures/colwells_alt_vs_asci_gam_trend.png", width = 10, height = 8, dpi = 300, units = "in")


# 10: ADD STREAM CLASSES -----------------------------------------------

# get stream classes: 
load("data/eflows_final_classification_9CLASS/ca_stream_class3-9_final.rda") # strm_class_final

# CSCI
csci_final <- left_join(csci_por_colwell, strm_class_final %>% st_drop_geometry(),
                      by=c("comid_gage"="COMID")) %>%
  distinct(.keep_all=TRUE)

table(csci_final$class3_name, useNA = "ifany")

csci_final <- csci_final %>% 
  mutate(class3_name = case_when(
    is.na(class3_name) ~ "SNOWMELT",
    TRUE ~ class3_name
  ))

# ASCI
asci_final <- left_join(asci_colwell, 
                        strm_class_final %>% st_drop_geometry(),
                        by=c("comid_gage"="COMID")) %>%
  distinct(.keep_all=TRUE)

table(asci_final$class3_name, useNA = "ifany")

asci_final %>% st_as_sf(coords=c("usgs_lon", "usgs_lat"), crs=4269, remove=F) %>% mapview(zcol="class3_name", burst=TRUE)

mapview::mapview(asci_final)


asci_final <- asci_final %>% 
  mutate(class3_name = case_when(
    is.na(class3_name) ~ "RAIN",
    TRUE ~ class3_name
  ))

table(asci_final$gagetype, useNA = "always")


# 11: PLOT GAGETYPE BY STREAMCLASS ------------------------------------------

# CSCI vs GAGETYPE
csci_final %>% 
  ggplot() + 
  geom_point(aes(y=MP_metric, x=csci, fill=gagetype), pch=21, size=2.7, alpha=0.9) +
  stat_smooth(aes(y=MP_metric, x=csci, color=gagetype),
              method = "gam", se = F) +
  theme_classic(base_family = "Roboto Condensed") +
  ggthemes::scale_color_few(palette = "Medium", "Gage Type") +
  ggthemes::scale_fill_few(palette = "Medium", "Gage Type") +
  #scale_fill_viridis_d(option = "A", "StreamClass") +
  labs(y="Seasonality (Colwell's M/P)", x="CSCI",
       caption = "Standardized seasonality in relation to overall predictability \nby dividing seasonality (M) by overall predictability \n(the sum of (M) and constancy (C)), as per Tonkin et al. (2017)") +
  facet_wrap(.~class3_name)

ggsave(filename = "figures/seasonality_colwell_by_streamclass3_alt_glm.png",
       width=11, height = 8, dpi=300)
ggsave(filename = "figures/seasonality_colwell_by_streamclass3_alt_gam.png",
       width=11, height = 8, dpi=300)


# ASCI vs GAGETYPE
asci_final %>% 
  ggplot() + 
  geom_point(aes(y=MP_metric, x=H_ASCI, fill=gagetype), pch=21, size=2.7, alpha=0.9) +
  stat_smooth(aes(y=MP_metric, x=H_ASCI, color=gagetype),
              method = "gam", se = F) +
  theme_classic(base_family = "Roboto Condensed") +
  ggthemes::scale_color_few(palette = "Medium", "Gage Type") +
  ggthemes::scale_fill_few(palette = "Medium", "Gage Type") +
  #scale_fill_viridis_d(option = "A", "StreamClass") +
  labs(y="Seasonality (Colwell's M/P)", x="ASCI",
       caption = "Standardized seasonality in relation to overall predictability \nby dividing seasonality (M) by overall predictability \n(the sum of (M) and constancy (C)), as per Tonkin et al. (2017)") +
  facet_wrap(.~class3_name)

ggsave(filename = "figures/seasonality_colwell_by_streamclass3_alt_asci_glm.png",
       width=11, height = 8, dpi=300)
ggsave(filename = "figures/seasonality_colwell_by_streamclass3_alt_asci_gam.png",
       width=11, height = 8, dpi=300)

# 12: Save Out ----------------------------------------------------------------

write_rds(csci_final, file = "output/04_gages_csci_colwells_w_streamclass_metric.rds")

write_rds(asci_final, file = "output/04_gages_asci_colwells_w_streamclass_metric.rds")


