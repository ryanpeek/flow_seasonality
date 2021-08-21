# rerun Colwell with revised flows from REF

library(hydrostats)
library(tidyverse)
library(purrr)
library(lubridate)
library(glue)
library(tidylog)
library(ggdark)
library(sf)


# 01: Import Flow/GAGE Data -----------------------------------------------

# flow data
load("data/usgs_Q_daily_alt_gages_trim.rda") # nrow=8977068
load("data/usgs_Q_daily_ref_gages_trim.rda") # nrow=2442511

# get gage metadata:
gage_alt_meta <- usgs_flows_alt_trim %>% distinct(site_no, .keep_all=TRUE) %>% 
  select(site_no,station_nm:flowcnt) # n=517
gage_ref_meta <- usgs_flows_ref_trim %>% distinct(site_no, .keep_all=TRUE) %>% 
  select(site_no,station_nm:flowcnt) # n=221

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
  mutate(site_no = gage_alt_meta$site_no,
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

# 07: BRING IN CSCI -------------------------------------------------------

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

# 08: JOIN COLWELL AND CSCI -----------------------------------------------

# join w seasonality:
csci_trim_colwell <- left_join(csci_trim, df_colwell_all_meta, by=c("site_id"="site_no")) %>%
  distinct(StationCode, SampleID, site_id, .keep_all=TRUE) %>% 
  filter(!is.na(gagetype))
  
table(csci_trim_colwell$gagetype, useNA = "ifany") # alt 430, ref 193

csci_por_colwell <- left_join(csci_por, df_colwell_all_meta, by=c("site_id"="site_no")) %>%
  distinct(StationCode, SampleID, site_id, .keep_all=TRUE) %>% 
  filter(!is.na(gagetype))

table(csci_por_colwell$gagetype, useNA = "ifany") # alt 430, ref 193

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

# 10: ADD STREAM CLASSES -----------------------------------------------

# get stream class and combine into 9 class and 3 class Xwalks 
# see "3_color_classes_gages_legend"
ceff_strmclass <- st_read("data/eflows_final_classification_9CLASS/Final_Classification_9CLASS.shp")

# crosswalk
strmclass_xwalk <- tibble(
  "CLASS"=c(1,2,3,4,5,6,7,8,9), 
  "CLASS_NAME"=c("snowmelt", # 3 class: 1=SNOWMELT
                 "high-volume snowmelt and rain", # 3 class: 2=MIXED
                 "low-volume snowmelt and rain", # 3 class: 2=MIXED,
                 "winter storms", # 3 class: 3=RAIN
                 "groundwater", # 3 class: 2=MIXED
                 "perennial groundwater and rain", # 3 class: 3=RAIN
                 "flashy, ephemeral rain", # 3 class: 3=RAIN
                 "rain and seasonal groundwater", # 3 class: 3=RAIN
                 "high elevation low precipitation"), # 3 class: 1=SNOWMELT
  "class3_name" = c("SNOWMELT",
                    "MIXED","MIXED","RAIN","MIXED",
                    "RAIN","RAIN","RAIN",
                    "SNOWMELT"),
  "class3_id" = c(1,
                  2,2,3,2,
                  3,3,3,
                  1))

# join with class names
ceff_strmclass <- left_join(ceff_strmclass, strmclass_xwalk)

# add streamclass name
df_final <- left_join(csci_por_colwell, ceff_strmclass, by=c("comid_gage"="COMID")) %>% 
  distinct(.keep_all=TRUE) %>% 
  #most NAs are likely SNOWMelt driven, lump for now?
  mutate(class3_name = case_when(
    is.na(class3_name) ~ "SNOWMELT",
    TRUE ~ class3_name
  ))
table(df_final$class3_name, useNA = "ifany")


# 11: PLOT GAGETYPE BY STREAMCLASS ------------------------------------------


# CSCI vs GAGETYPE
df_final %>% 
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

# 12: Save Out ----------------------------------------------------------------

write_rds(df_final, file = "output/usgs_gages_colwells_w_streamclass_metric.rds")
save(ceff_strmclass, file="output/eflows_streamclasses.rda", compress = "gz")
write_csv(strmclass_xwalk, file="output/eflows_streamclasses_xwalk_3class.csv")
