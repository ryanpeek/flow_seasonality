# 11_Get Representative sites and Look at hydrographs

# Libraries -------------------------------------------------------------

library(tidyverse)
library(glue)
library(viridis)
library(lubridate)
library(sf)
library(plotly)
library(patchwork)
library(mapview)
mapviewOptions(fgb = FALSE)

# Data --------------------------------------------------------------------

# get combined data w FFC p50
df_ffc <- read_rds("output/ffc_filtered_final_combined.rds")

# get all wavelet data
load("output/wavelet_combined_period_power_outputs.rda")

# csci
df_csci <- read_rds("output/wavelet_csci_colwell_final.rds")

# get 12 mon wavelet
df_wav_12 <- df_wav_12 %>% 
  rename(Period12 = Period, Power.avg12 = Power.avg)
df_csci <- df_csci %>% left_join(., df_wav_12[,c("site_id","Period12","Power.avg12", "gagetype")])

# Pull Actual Flow Data & Filter ------------------------------------------

load("data/usgs_Q_daily_ref_gages_trim.rda") # usgs_flows_ref
load("data/usgs_Q_daily_alt_gages_trim.rda") # usgs_flows_alt

# filter to filtered CSCI Paired sites
refdat <- usgs_flows_ref_trim %>% 
  filter(site_no %in% unique(df_ffc$gageid)) 

altdat <- usgs_flows_alt_trim %>% 
  filter(site_no %in% unique(df_ffc$gageid)) 

length(unique(altdat$site_no)) # n=138
length(unique(refdat$site_no)) # n=55

# ALT: Mean Daily Flow Plots ---------------------------------------------------

# plot altered
altdat %>% 
  group_by(site_no, DOWY) %>% 
  summarize(meanFlow = mean(Flow, na.rm=TRUE)) %>%
  left_join(., df_csci[,c("site_id", "csci", "mmi", "MP_metric")], by=c("site_no"="site_id")) %>%
  left_join(., df_wav_12, by=c("site_no"="site_id")) %>% 
  #filter(site_no %in% unique(altdat$site_no)[1:12]) %>% 
  #filter(site_no %in% unique(altdat$site_no)[13:24]) %>%
  #filter(site_no %in% unique(altdat$site_no)[25:36]) %>%
  #filter(site_no %in% unique(altdat$site_no)[37:48]) %>%
  #filter(site_no %in% unique(altdat$site_no)[49:60]) %>%
  #filter(site_no %in% unique(altdat$site_no)[61:72]) %>%
  #filter(site_no %in% unique(altdat$site_no)[73:84]) %>%
  #filter(site_no %in% unique(altdat$site_no)[85:96]) %>%
  filter(site_no %in% unique(altdat$site_no)[109:138]) %>%
  ggplot() + 
  theme_bw() +
  # switch color: Power.avg or MP_Metric
  #geom_line(aes(x=DOWY, y=log(meanFlow), group=site_no, color=MP_metric)) + 
  geom_line(aes(x=DOWY, y=meanFlow, group=site_no, color=MP_metric)) +
  #scale_color_viridis(limits=c(0,10)) + # for Power.avg
  scale_color_viridis(limits=c(0,1)) + # for MP
  facet_wrap(.~site_no, scales = "free")

#ggsave(filename = "figures/hydro_alt_mean_daily_flow_1_12.png", width = 11, height = 8.5, dpi=300)
#ggsave(filename = "figures/hydro_alt_mean_daily_flow_13_24.png", width = 11, height = 8.5, dpi=300)
#ggsave(filename = "figures/hydro_alt_mean_daily_flow_25_36.png", width = 11, height = 8.5, dpi=300)
#ggsave(filename = "figures/hydro_alt_mean_daily_flow_37_48.png", width = 11, height = 8.5, dpi=300)
#ggsave(filename = "figures/hydro_alt_mean_daily_flow_49_60.png", width = 11, height = 8.5, dpi=300)
#ggsave(filename = "figures/hydro_alt_mean_daily_flow_61_72.png", width = 11, height = 8.5, dpi=300)
#ggsave(filename = "figures/hydro_alt_mean_daily_flow_73_84.png", width = 11, height = 8.5, dpi=300)
#ggsave(filename = "figures/hydro_alt_mean_daily_flow_85_96.png", width = 11, height = 8.5, dpi=300)
ggsave(filename = "figures/hydro_alt_mean_daily_flow_109_138.png", width = 11, height = 8.5, dpi=300)


# identify gages to drop: (tunnels)
gages_to_drop_alt <- c("11216400", "11404100")

# REF: Mean Daily Flow Plots ---------------------------------------------------

### REFERENCE sites
# plot REF
refdat %>% 
  group_by(site_no, DOWY) %>% 
  summarize(meanFlow = mean(Flow, na.rm=TRUE)) %>%
  left_join(., df_csci[,c("site_id", "csci", "mmi", "MP_metric")], by=c("site_no"="site_id")) %>%
  left_join(., df_wav_12, by=c("site_no"="site_id")) %>%
  #filter(site_no %in% unique(refdat$site_no)[1:20]) %>%
  #filter(site_no %in% unique(refdat$site_no)[21:40]) %>%
  filter(site_no %in% unique(refdat$site_no)[41:55]) %>%
  ggplot() + 
  theme_bw() +
  # switch color: Power.avg or MP_Metric
  #geom_line(aes(x=DOWY, y=log(meanFlow), group=site_no, color=MP_metric)) + 
  geom_line(aes(x=DOWY, y=meanFlow, group=site_no, color=MP_metric)) +
  #scale_color_viridis(limits=c(0,10)) + # for Power.avg
  scale_color_viridis(limits=c(0,1)) + # for MP
  facet_wrap(.~site_no, scales = "free")

#ggsave(filename = "figures/hydro_ref_mean_daily_flow_1_20.png", width = 11, height = 8.5, dpi=300)
#ggsave(filename = "figures/hydro_ref_mean_daily_flow_21-40.png", width = 11, height = 8.5, dpi=300)
ggsave(filename = "figures/hydro_ref_mean_daily_flow_41-55.png", width = 11, height = 8.5, dpi=300)


# STREAM CLASS SNOWMELT: Filter and Plot ------------------------------------------

# get just ref SNOW
ref_snow <- df_csci %>% distinct(site_id, .keep_all=TRUE) %>% 
  filter(class3_name=="SNOWMELT", gagetype=="REF")

refdat %>%
  filter(site_no %in% ref_snow$site_id) %>% 
  group_by(site_no, DOWY) %>% 
  summarize(meanFlow = mean(Flow, na.rm=TRUE)) %>%
  left_join(., df_csci[,c("site_id", "csci", "mmi", "MP_metric")], by=c("site_no"="site_id")) %>%
  left_join(., df_wav_12, by=c("site_no"="site_id")) %>%
  ggplot() + 
  theme_bw() +
  geom_line(aes(x=DOWY, y=log(meanFlow), group=site_no, color=MP_metric)) +
  scale_color_viridis(limits=c(0,1)) + 
  facet_wrap(gagetype~site_no, scales="free")

# look at a single one:
df_csci %>% 
  #filter(site_id == "10336580") %>% 
  select(site_id, station_nm, MP_metric, Power.avg12, Period, csci, csci_percentile, gagetype) %>% 
  mutate(seas_ratio = log(Power.avg12/(1-MP_metric))) %>% # arbitrary scaling?
  ggplot() + 
  geom_point(aes(x=seas_ratio, y=csci_percentile, shape=gagetype, color=gagetype), size=3)+
  ggthemes::scale_color_colorblind() +
  geom_smooth(aes(x=seas_ratio, y=csci_percentile, color=gagetype), method="glm")
  #scale_x_continuous(limits = c(0, 150))


# get just alt SNOW
alt_snow <- df_csci %>% distinct(site_id, .keep_all=TRUE) %>% 
  filter(class3_name=="SNOWMELT", gagetype=="ALT")

altdat %>%
  filter(site_no %in% alt_snow$site_id) %>% 
  group_by(site_no, DOWY) %>% 
  summarize(meanFlow = mean(Flow, na.rm=TRUE)) %>%
  left_join(., df_csci[,c("site_id", "csci", "mmi", "MP_metric")], by=c("site_no"="site_id")) %>%
  left_join(., df_wav_12, by=c("site_no"="site_id")) %>%
  ggplot() + 
  theme_bw() +
  geom_line(aes(x=DOWY, y=log(meanFlow), group=site_no, color=MP_metric)) +
  scale_color_viridis(limits=c(0,1)) + 
  facet_wrap(gagetype~site_no, scales="free")

# look at a single one:
df_csci %>% 
  filter(site_id == "10336610") %>% 
  select(site_id, station_nm, MP_metric, Power.avg12, Period, csci, csci_percentile, gagetype) %>% 
  mutate(seas_ratio = log(Power.avg12/(1-MP_metric))) %>% # arbitrary scaling?
  #View()
  ggplot() + 
  geom_point(aes(x=seas_ratio, y=csci, shape=gagetype, color=gagetype), size=3)+
  ggthemes::scale_color_colorblind() +
  geom_smooth(aes(x=seas_ratio, y=csci, color=gagetype), method="glm")
#scale_x_continuous(limits = c(0, 150))


# STREAM CLASS MIXED: Filter and Plot ------------------------------------------

  # get just ref SNOW
  ref_mixed <- df_csci %>% distinct(site_id, .keep_all=TRUE) %>% 
    filter(class3_name=="MIXED", gagetype=="REF")
  
  refdat %>%
    filter(site_no %in% ref_mixed$site_id) %>% 
    group_by(site_no, DOWY) %>% 
    summarize(meanFlow = mean(Flow, na.rm=TRUE)) %>%
    left_join(., df_csci[,c("site_id", "csci", "mmi", "MP_metric")], by=c("site_no"="site_id")) %>%
    left_join(., df_wav_12, by=c("site_no"="site_id")) %>%
    ggplot() + 
    theme_bw() +
    geom_line(aes(x=DOWY, y=log(meanFlow), group=site_no, color=MP_metric)) +
    scale_color_viridis(limits=c(0,1)) + 
    facet_wrap(gagetype~site_no, scales="free")
  
  # look at a single one:
  df_csci %>% 
    filter(site_id == "11113000") %>% 
    select(site_id, station_nm, MP_metric, Power.avg12, Period12, csci, csci_percentile, gagetype) %>% 
    mutate(seas_ratio = log(Power.avg12/(1-MP_metric))) %>% # arbitrary scaling?
    ggplot() + 
    geom_point(aes(x=seas_ratio, y=csci, shape=gagetype, color=gagetype), size=3)+
    ggthemes::scale_color_colorblind() +
    geom_smooth(aes(x=seas_ratio, y=csci, color=gagetype), method="glm")
  #scale_x_continuous(limits = c(0, 150))
  
  # get just alt SNOW
  alt_mixed <- df_csci %>% distinct(site_id, .keep_all=TRUE) %>% 
    filter(class3_name=="MIXED", gagetype=="ALT")
  
  altdat %>%
    filter(site_no %in% alt_snow$site_id) %>% 
    group_by(site_no, DOWY) %>% 
    summarize(meanFlow = mean(Flow, na.rm=TRUE)) %>%
    left_join(., df_csci[,c("site_id", "csci", "mmi", "MP_metric")], by=c("site_no"="site_id")) %>%
    left_join(., df_wav_12, by=c("site_no"="site_id")) %>%
    ggplot() + 
    theme_bw() +
    geom_line(aes(x=DOWY, y=log(meanFlow), group=site_no, color=MP_metric)) +
    scale_color_viridis(limits=c(0,1)) + 
    facet_wrap(gagetype~site_no, scales="free")
  
  # look at a single one:
  df_csci %>% 
    filter(site_id == "10336610") %>% 
    select(site_id, station_nm, MP_metric, Power.avg12, Period, csci, csci_percentile, gagetype) %>% 
    mutate(seas_ratio = log(Power.avg12/(1-MP_metric))) %>% # arbitrary scaling?
    #View()
  ggplot() + 
    geom_point(aes(x=seas_ratio, y=csci, shape=gagetype, color=gagetype), size=3)+
    ggthemes::scale_color_colorblind() +
    geom_smooth(aes(x=seas_ratio, y=csci, color=gagetype), method="glm")
  #scale_x_continuous(limits = c(0, 150))
  
  