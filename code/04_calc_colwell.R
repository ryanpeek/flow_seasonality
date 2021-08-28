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
write_rds(df_colwell_all_meta, file = "output/04_usgs_gages_colwells_metric.rds")

rm(df_alt, df_ref, df_colwell_all, df_colwell_alt, df_colwell_ref, 
   gage_alt_meta, gage_ref_meta, usgs_flows_alt_trim, usgs_flows_ref_trim)

# 05: READ IN DATA ---------------------------------------------------------

df_colwell_all_meta <- read_rds("output/04_usgs_gages_colwells_metric.rds")

# 06: PLOTS -----------------------------------------------------------------

# Histogram
df_colwell_all_meta %>% ggplot() + geom_histogram(aes(y=MP_metric, fill=gagetype))

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
