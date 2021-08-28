# Add Wavelets with Colwell with CSCI

# Libraries -------------------------------------------------------------

library(tidyverse)
library(fs)
library(glue)
library(viridis)
library(lubridate)
library(sf)
library(patchwork)
library(mapview)
mapviewOptions(fgb = FALSE)

# Get Colwell Data ------------------------------------------------------

# Get Colwell
df_csci <- read_rds("output/04b_gages_csci_colwells_w_streamclass_metric.rds")
df_asci <- read_rds("output/04b_gages_asci_colwells_w_streamclass_metric.rds")

df_csci %>%
  distinct(site_id, .keep_all = TRUE) %>% 
  select(gagetype) %>% table() # ALT=125, REF=53

df_asci %>% 
  distinct(site_id, .keep_all = TRUE) %>% 
  select(gagetype) %>% table() # ALT=126, REF=48

# check streamclass
table(df_asci$class3_name, useNA="ifany")
#MIXED     RAIN SNOWMELT 
# 88      238       42 
table(df_csci$class3_name, useNA="ifany")
#MIXED     RAIN SNOWMELT 
# 126      405      95 

# Get Wavelet Data --------------------------------------------------------

# Get Wavelet
load("output/06_wavelet_combined_period_power_outputs.rda")
# df_wav_max: max power no matter the period
# df_wav: raw data, all points
# df_wav_12: max power at 12 months
# df_wav_6: max power at 6 months


# Get Gage Metadata -------------------------------------------------------

# get list of potential gages (n=232)
gagelist <- unique(c(df_csci$site_id, df_asci$site_id))

# Get GAGE metadata
usgs_daily_alt <- read_csv("data/usgs_metadata_alt_gages.csv")
usgs_daily_ref <- read_csv("data/usgs_metadata_ref_gages.csv") %>% 
  mutate(site_id = as.character(site_id))
gages_meta <- bind_rows(usgs_daily_alt, usgs_daily_ref) %>% 
  filter(site_id %in% gagelist) # filter to just the bio sites
rm(usgs_daily_alt, usgs_daily_ref) # rm temp files

# how many total unique gages?
table(gages_meta$gagetype) # ALT 164, REF 68

# Join Colwell with Wavelet -----------------------------------------------

# join with 12 month value
df_csci_final <- left_join(df_csci, df_wav_12, by="site_id") %>%
  ungroup() %>% 
  select(-gagetype.x) %>% 
  rename(gagetype=gagetype.y)

# double check 
df_csci_final %>% 
  distinct(site_id, .keep_all=TRUE) %>% 
  select(gagetype) %>% table(useNA = "always")
# matches!!

# join with 12 month value
df_asci_final <- left_join(df_asci, df_wav_12, by="site_id") %>%
  ungroup() %>% 
  select(-gagetype.x) %>% 
  rename(gagetype=gagetype.y) %>% 
  filter(!is.na(gagetype))

# double check 
df_asci_final %>% 
  distinct(site_id, .keep_all=TRUE) %>% 
  select(gagetype) %>% table(useNA = "always")
# drop one (126, 47 ref)


# COMBINE DATASETS --------------------------------------------------------

janitor::compare_df_cols(df_asci_final, df_csci_final)

# only a few diff cols: csci, csci_percentile, H_ASCI, mmi, 

df_final <- bind_rows(df_asci_final %>% 
                        select(-c(class3_id, CEFF_type, flowdays, flowcnt, Period_rnd)) %>% 
                        rename(asci=H_ASCI) %>% 
                        mutate(bioindicator="ASCI"), 
                      df_csci_final %>%  
                        select(-c(class3_id, CEFF_type, flowdays, flowcnt, Period_rnd)) %>% 
                        mutate(bioindicator="CSCI"))

# combine csci/asci col:
df_final <- df_final %>% 
  mutate(biovalue=coalesce(asci, csci),
         class3_name = as.factor(class3_name),
         gagetype = as.factor(gagetype),
         bioindicator = as.factor(bioindicator)) %>% 
  select(StationCode:comid_gage, gagetype, station_nm:class3_name, SampleID, sampledate, bioindicator, biovalue, asci, csci, csci_percentile, COMID_bio, MP_metric, Power.avg:Period)

# check
summary(df_final)

## Seasonality vs. Predict by StreamClass for -------

df_final %>%
  ggplot() + 
  geom_point(aes(x=MP_metric, y=Power.avg, color=gagetype, 
                 shape=bioindicator), alpha=0.8)+
  geom_smooth(aes(x=MP_metric, y=Power.avg, color=gagetype), 
              method = "glm", 
              se = FALSE) +
  #scale_shape_manual("GageType", values=c(21,22)) +
  #ggthemes::scale_fill_colorblind("GageType") +
  ggthemes::scale_linetype_stata("Bioindicator") +
  ggthemes::scale_color_colorblind("Gagetype") +
  theme_classic(base_family = "Roboto Condensed") +
  facet_wrap(bioindicator~class3_name)

ggsave(filename = "figures/wavelet_vs_colwell_by_streamclass_glm.png", 
       width = 11, height = 8, dpi=300, units = "in")

# plot 2: colwell vs. csci
df_final %>% filter(gagetype=="ALT") %>% 
  ggplot() + 
  geom_point(aes(x=MP_metric, y=biovalue, fill=bioindicator, shape=bioindicator),
             alpha=0.8, size=4, show.legend = TRUE) + 
  scale_shape_manual("Bioindicator", values=c(21,22)) +
  geom_smooth(method = "gam",se = FALSE,
              aes(x=MP_metric, y=biovalue,
                  color=bioindicator, group=bioindicator), alpha=0.5) +
  ggthemes::scale_color_colorblind("Bioindicator") +
  #scale_color_viridis_d("Bioindicator") +
  scale_fill_grey("Bioindicator") +
  #guides(fill="none") +
  labs(x="Intra-annual Seasonality (Colwell's M/P)", 
       subtitle = "Altered Gages",
       y="Condition Score (ASCI or CSCI)") +
  theme_classic(base_family = "Roboto Condensed") +
  facet_wrap(.~class3_name)

ggsave(filename = "figures/colwell_vs_asci_csci_by_streamclass_w_trendline_gam.png", 
       width = 11, height = 8, dpi=300, units = "in")

# plot 3: wavelet vs. csci
df_final %>%  filter(gagetype=="ALT") %>% 
  ggplot() + 
  geom_point(aes(x=Power.avg, y=biovalue, fill=bioindicator, shape=bioindicator),
             alpha=0.8, size=4, show.legend = FALSE) + 
  scale_shape_manual("GageType", values=c(21,22)) +
  geom_smooth(method = "gam",se = FALSE,
              aes(x=Power.avg, y=biovalue, color=bioindicator, 
                  group=bioindicator), alpha=0.5) +
  ggthemes::scale_color_colorblind("Bioindicator") +
  scale_x_log10() +
  #scale_color_viridis_d("Bioindicator") +
  scale_fill_grey("Bioindicator") +
  #guides(fill="none") +
  labs(x="Inter-annual Seasonality (Wavelet Power)", 
       subtitle = "Altered Gages",
       y="Condition Score (ASCI or CSCI)") +
  theme_classic(base_family = "Roboto Condensed") +
  facet_wrap(.~class3_name)

ggsave(filename = "figures/wavelet_vs_asci_csci_by_streamclass_w_trendline_gam.png", 
       width = 11, height = 8, dpi=300, units = "in")

# Save Data Out -----------------------------------------------------------

write_rds(df_final, file = "output/07_wavelet_asci_csci_colwell_final.rds")


