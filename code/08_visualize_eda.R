# Visualize and EDA of data

# Libraries -------------------------------------------------------------

library(tidyverse)
library(fs)
library(glue)
library(viridis)
library(lubridate)
library(sf)
library(plotly)
library(patchwork)
library(mapview)
mapviewOptions(fgb = FALSE)

# Data --------------------------------------------------------------------

df_final <- read_rds("output/wavelet_csci_colwell_final.rds")
df_final %>% distinct(.keep_all = TRUE) %>% 
  group_by(gagetype) %>% tally() # ALT = 305, REF=117

# Get Wavelets
load("output/wavelet_combined_period_power_outputs.rda")
# df_wav_max: max power no matter the period
# df_wav: raw data, all points
# df_wav_12: max power at 12 months
# df_wav_6: max power at 6 months

# Filter Wav above 75% percentile  ----------------------------------

# Filtered to top 25 percent of all sites 
df_final %>%
  filter(Power.avg >= round(quantile(df_wav_max$Power.avg, probs = c(0.75)), 2)) %>%
  filter(site_id %in% unique(df_final$site_id)) -> df_wav_75_csci
# n=29 sites with strong 6 month pattern (in top 25th percentile)
table(df_wav_75_csci$gagetype) # ALT=35, REF=20

# Pull Actual Flow Data ---------------------------------------------------

#devtools::install_github("ryanpeek/wateRshedTools")

load("data/usgs_Q_daily_ref_gages_trim.rda") # usgs_flows_ref
load("data/usgs_Q_daily_alt_gages_trim.rda") # usgs_flows_alt

usgs_flows_ref %>% ungroup() %>% distinct(site_no) %>% tally()
usgs_flows_alt %>% ungroup() %>% distinct(site_no) %>% tally()

# now filter to data that had a 6 month peak ()
refdat6 <- usgs_flows_ref_trim %>% 
  filter(site_no %in% unique(df_wav_75_csci$site_id))
altdat6 <- usgs_flows_alt_trim %>% 
  filter(site_no %in% unique(df_wav_75_csci$site_id)) 

# filter to filtered CSCI Paired sites
refdat <- usgs_flows_ref_trim %>% 
  filter(site_no %in% unique(df_final$site_id)) 

altdat <- usgs_flows_alt_trim %>% 
  filter(site_no %in% unique(df_final$site_id)) 

length(unique(altdat$site_no))
length(unique(refdat$site_no))

# MEAN ANN PLOT 6MON by Colwells -------------------------------------------------------

# mean annual
altdat %>% distinct(site_no) %>% nrow() # unfilt: n=13

altdat %>% 
  group_by(site_no, DOWY) %>% 
  summarize(meanFlow = mean(Flow, na.rm=TRUE)) %>%
  left_join(., df_final[,c("site_id", "csci", "MP_metric")], by=c("site_no"="site_id")) %>%
  left_join(., df_wav_max, by=c("site_no"="site_id")) %>%
  ggplot() + 
  theme_bw() +
  # switch color: Power.avg or MP_Metric
  geom_line(aes(x=DOWY, y=log(meanFlow), group=site_no, color=MP_metric)) + 
  #geom_line(aes(x=DOWY, y=meanFlow, group=site_no, color=MP_metric)) +
  #scale_color_viridis(limits=c(0,10)) + # for Power.avg
  scale_color_viridis(limits=c(0,1)) + # for MP
  labs(subtitle = "Alt: Mean Annual Discharge (cfs) [n=140], 6 month peak") -> g6_a
g6_a

#ggplotly(g6_a)

refdat %>% distinct(site_no) %>% nrow()# n=6
refdat %>% 
  group_by(site_no, DOWY) %>% 
  summarize(meanFlow = mean(Flow, na.rm=TRUE)) %>%
  left_join(., df_final[,c("site_id", "csci", "MP_metric")], by=c("site_no"="site_id")) %>%
  left_join(., df_wav_6, by=c("site_no"="site_id")) %>% 
  ggplot() + 
  theme_bw() +
  # switch color: Power.avg or MP_Metric
  geom_line(aes(x=DOWY, y=log(meanFlow), group=site_no, color=MP_metric)) + 
  #geom_line(aes(x=DOWY, y=meanFlow, group=site_no, color=MP_metric)) +
  #scale_color_viridis(limits=c(0,10)) + # for Power.avg
  scale_color_viridis(limits=c(0,1)) + # for MP
  labs(subtitle = "Ref: Mean Annual Discharge (cfs) [n=9], 6 month peak") -> g6_b
g6_b

#ggplotly(g6_b)

# plot together
g6_a / g6_b

ggsave(filename = "figures/mean_ann_flow_alt_ref_6mon_filt_75_colwells.png", 
       width = 11, height = 8, dpi=300)

ggsave(filename = "figures/mean_ann_logflow_alt_ref_6mon_filt_75_colwells.png", 
       width = 11, height = 8, dpi=300)


## now by wavelet -------------------------------------------
# mean annual
altdat6 %>% distinct(site_no) %>% nrow() # unfilt: n=20
altdat6 %>% 
  group_by(site_no, DOWY) %>% 
  summarize(meanFlow = mean(Flow, na.rm=TRUE)) %>%
  left_join(., df_final[,c("site_id", "csci", "MP_metric")], by=c("site_no"="site_id")) %>%
  left_join(., df_wav_6, by=c("site_no"="site_id")) %>% 
  ggplot() + 
  theme_bw() +
  # switch color: Power.avg or MP_Metric
  #geom_line(aes(x=DOWY, y=log(meanFlow), group=site_no, color=Power.avg)) + 
  geom_line(aes(x=DOWY, y=meanFlow, group=site_no, color=Power.avg)) +
  scale_color_viridis(limits=c(0,10)) + # for Power.avg
  #scale_color_viridis(limits=c(0,1)) + # for MP
  labs(subtitle = "Alt: Mean Annual Discharge (cfs) [n=20], 6 month peak") -> g6_a
g6_a

ggplotly(g6_a)

refdat6 %>% distinct(site_no) %>% nrow()# n=9
refdat6 %>% 
  group_by(site_no, DOWY) %>% 
  summarize(meanFlow = mean(Flow, na.rm=TRUE)) %>%
  left_join(., df_final[,c("site_id", "csci", "MP_metric")], by=c("site_no"="site_id")) %>%
  left_join(., df_wav_6, by=c("site_no"="site_id")) %>% 
  ggplot() + 
  theme_bw() +
  # switch color: Power.avg or MP_Metric
  #geom_line(aes(x=DOWY, y=log(meanFlow), group=site_no, color=Power.avg)) + 
  geom_line(aes(x=DOWY, y=meanFlow, group=site_no, color=Power.avg)) +
  scale_color_viridis(limits=c(0,10)) + # for Power.avg
  labs(subtitle = "Ref: Mean Annual Discharge (cfs) [n=9], 6 month peak") -> g6_b
g6_b

ggplotly(g6_b)

# plot together
g6_a / g6_b

ggsave(filename = "figures/mean_ann_flow_alt_ref_6mon_filt_75_wav.png", 
       width = 11, height = 8, dpi=300)

ggsave(filename = "figures/mean_ann_logflow_alt_ref_6mon_filt_75_wav.png", 
       width = 11, height = 8, dpi=300)




# MEAN ANN Max Wavelet Score for All CSCI Sites ---------------------------

# mean annual
altdat %>% distinct(site_no) %>% nrow() #n=137
altdat %>% 
  group_by(site_no, DOWY) %>% 
  summarize(meanFlow = mean(Flow, na.rm=TRUE)) %>%
  left_join(., df_final[,c("site_id", "csci", "MP_metric", "Power.avg")], by=c("site_no"="site_id")) %>%
  #left_join(., df_wav_6, by=c("site_no"="site_id")) %>% 
  ggplot() + 
  theme_bw() +
  geom_line(aes(x=DOWY, y=log(meanFlow), group=site_no, color=Power.avg)) +
  #geom_line(aes(x=DOWY, y=meanFlow, group=site_no, color=Power.avg)) +
  scale_color_viridis(limits=c(0,10)) +
  labs(subtitle = "Alt: Mean Annual Discharge (cfs) [n=137]") -> g12_a
g12_a

refdat %>% distinct(site_no) %>% nrow() #n=55 
refdat %>% 
  group_by(site_no, DOWY) %>% 
  summarize(meanFlow = mean(Flow, na.rm=TRUE)) %>%
  left_join(., df_wav_max, by=c("site_no"="site_id")) %>% #View()
  ggplot() + 
  theme_bw() +
  geom_line(aes(x=DOWY, y=log(meanFlow), group=site_no, color=Power.avg)) +
  #geom_line(aes(x=DOWY, y=meanFlow, group=site_no, color=Power.avg)) +
  #scale_color_viridis(limits=c(6, 17)) +
  scale_color_viridis() +
  labs(subtitle = "Ref: Mean Annual Discharge (cfs) [n=55]") -> g12_b
g12_b

# plot together
g12_a / g12_b

ggsave(filename = "figures/mean_ann_flow_alt_ref_12mon_filt_.png", 
       width = 11, height = 8, dpi=300)

ggsave(filename = "figures/mean_ann_logflow_alt_ref_12mon_period_filt.png", 
       width = 11, height = 8, dpi=300)

## Now by Colwell's -------------------------------------------------------

# mean annual
altdat %>% distinct(site_no) %>% nrow() #n=137
altdat %>% 
  group_by(site_no, DOWY) %>% 
  summarize(meanFlow = mean(Flow, na.rm=TRUE)) %>%
  left_join(., df_final[,c("site_id", "csci", "MP_metric")], by=c("site_no"="site_id")) %>%
  left_join(., df_wav_max, by=c("site_no"="site_id")) %>% 
  ggplot() + 
  theme_bw() +
  #geom_line(aes(x=DOWY, y=log(meanFlow), group=site_no, color=MP_metric)) +
  geom_line(aes(x=DOWY, y=meanFlow, group=site_no, color=MP_metric)) +
  scale_color_viridis(limits=c(0, 1)) +
  labs(subtitle = "Alt: Mean Annual Discharge (cfs) [n=137] 12 mon") -> g12_a
g12_a

refdat %>% distinct(site_no) %>% nrow() #n=55 
refdat %>% 
  group_by(site_no, DOWY) %>% 
  summarize(meanFlow = mean(Flow, na.rm=TRUE)) %>%
  left_join(., df_final[,c("site_id", "csci", "MP_metric")], by=c("site_no"="site_id")) %>%
  left_join(., df_wav_max, by=c("site_no"="site_id")) %>% 
  ggplot() + 
  theme_bw() +
  #geom_line(aes(x=DOWY, y=log(meanFlow), group=site_no, color=MP_metric)) +
  geom_line(aes(x=DOWY, y=meanFlow, group=site_no, color=MP_metric)) +
  #scale_color_viridis(limits=c(6, 17)) +
  scale_color_viridis(limits=c(0, 1)) +
  labs(subtitle = "Ref: Mean Annual Discharge (cfs) [n=55] 12 mon") -> g12_b
g12_b

# plot together
g12_a / g12_b

ggsave(filename = "figures/mean_ann_flow_alt_ref_12mon_filt_colwell.png", 
       width = 11, height = 8, dpi=300)

ggsave(filename = "figures/mean_ann_logflow_alt_ref_12mon_filt_colwell.png", 
       width = 11, height = 8, dpi=300)



# More Plots? By CSCI? -------------------------------------------------------------

# mean annual
altdat %>% distinct(site_no) %>% nrow() #n=137
altdat %>% 
  group_by(site_no, DOWY) %>% 
  summarize(meanFlow = mean(Flow, na.rm=TRUE)) %>%
  left_join(., df_final[,c("site_id", "csci", "MP_metric")], by=c("site_no"="site_id")) %>%
  left_join(., df_wav_max, by=c("site_no"="site_id")) %>% 
  ggplot() + 
  theme_bw() +
  #geom_line(aes(x=DOWY, y=log(meanFlow), group=site_no, color=MP_metric)) +
  geom_line(aes(x=DOWY, y=meanFlow, group=site_no, color=csci)) +
  scale_color_viridis(limits=c(0, 1)) +
  labs(subtitle = "Alt: Mean Annual Discharge (cfs) [n=137] 12 mon") -> g12_a
g12_a

refdat %>% distinct(site_no) %>% nrow() #n=55 
refdat %>% 
  group_by(site_no, DOWY) %>% 
  summarize(meanFlow = mean(Flow, na.rm=TRUE)) %>%
  left_join(., df_final[,c("site_id", "csci", "MP_metric")], by=c("site_no"="site_id")) %>%
  left_join(., df_wav_max, by=c("site_no"="site_id")) %>% 
  ggplot() + 
  theme_bw() +
  #geom_line(aes(x=DOWY, y=log(meanFlow), group=site_no, color=MP_metric)) +
  geom_line(aes(x=DOWY, y=meanFlow, group=site_no, color=csci)) +
  #scale_color_viridis(limits=c(6, 17)) +
  scale_color_viridis(limits=c(0, 1.1)) +
  labs(subtitle = "Ref: Mean Annual Discharge (cfs) [n=55] 12 mon") -> g12_b
g12_b

# plot together
g12_a / g12_b

ggsave(filename = "figures/mean_ann_flow_alt_ref_12mon_filt_colwell.png", 
       width = 11, height = 8, dpi=300)

ggsave(filename = "figures/mean_ann_logflow_alt_ref_12mon_filt_colwell.png", 
       width = 11, height = 8, dpi=300)



# Boxplots ----------------------------------------------------------------

# # POWER
(p6_1 <- df_wav_75_csci %>%
  ggplot() + geom_boxplot(aes(x=gagetype, y=Power.avg, fill=gagetype), show.legend = FALSE) + labs(y="Interannual Seasonality (Power Avg)"))

(p6_2 <- df_wav_75_csci %>% 
  ggplot() + geom_boxplot(aes(x=gagetype, y=MP_metric, fill=gagetype), show.legend = FALSE))

(p6_3 <- df_wav_75_csci %>% 
  ggplot() + geom_boxplot(aes(x=gagetype, y=csci, fill=gagetype), show.legend = FALSE))

p6_1 + p6_2 + p6_3

# ggsave(filename = "figures/boxplots_of_6mon_sites.png", width = 11, height = 8, dpi=300)

# now same plots but with full dataset
(p_1 <- df_final %>% 
  ggplot() + geom_boxplot(aes(x=gagetype, y=Power.avg, fill=gagetype), show.legend = FALSE) + labs(y="Interannual Seasonality (Power Avg)"))

(p_2 <- df_final %>% 
  ggplot() + geom_boxplot(aes(x=gagetype, y=MP_metric, fill=gagetype)) +
    labs(y="Intrannual Seasonality (Colwell MP)"))

(p_3 <- df_final %>% 
  ggplot() + geom_boxplot(aes(x=gagetype, y=csci, fill=gagetype)) +
    labs(y="CSCI"))

p_1 + p_2 + p_3

ggsave(filename = "figures/boxplots_of_csci_sites_max_wav.png", width = 11, height = 8, dpi=300)

# CSCI vs. Seasonality GAM Plots ------------------------------------------

df_final %>% 
  ggplot() + geom_point(aes(x=csci, y=Power.avg, fill=gagetype), pch=21, size=3) +
  geom_smooth(aes(x=csci, y=Power.avg, color=gagetype), method = "gam") +
  scale_fill_viridis_d() + scale_color_viridis_d() +
  labs(y="Interannual Seasonality \n(Wavelet Power Avg)") -> gg_gam_poweravg

(df_final %>% filter(Period < 20) %>% 
    ggplot() + geom_point(aes(y=csci, x=Period, fill=gagetype), pch=21, size=3.5) +
    geom_smooth(aes(y=csci, x=Power.avg, color=gagetype), method = "gam") +
    scale_fill_viridis_d() + scale_color_viridis_d() -> gg_gam_period)

df_final %>% 
  ggplot() + geom_point(aes(x=csci, y=MP_metric, fill=gagetype), pch=21) +
  geom_smooth(aes(x=csci, y=MP_metric, color=gagetype), method = "gam") +
  scale_fill_viridis_d() + scale_color_viridis_d() +
  labs(y="Intrannual Seasonality (Colwell MP)") -> gg_gam_mp

gg_gam_poweravg / gg_gam_period / gg_gam_mp
