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

df_filt <- read_rds("output/csci_wavelet_col_finalsites_filtered.rds")
df_meta_filt <- read_rds("output/wavelet_colwell_all_sites_filtered.rds")

# Get Wavelets
load("output/wavelet_combined_period_power_outputs.rda")
# df_wav_max: max power no matter the period
# df_wav: raw data, all points
# df_wav_12: max power at 12 months
# df_wav_6: max power at 6 months


df_wav_max_filt <- 
  df_wav_max %>% 
  filter(site_id %in% unique(df_filt$site_id))
table(df_wav_max_filt$gagetype) # 137 ALT, 55 REF


# Filter the 6 month Gages ------------------------------------------------

# filter to just csci paired sites
df_wav_6 %>% 
  filter(site_id %in% unique(df_filt$site_id)) -> df_wav_6_csci
table(df_wav_6_csci$gagetype) # ALT=137, REF=55

# all sites (n=722) filtered to top 25 percent of all sites,
# then filter to just csci sites
df_wav_6 %>% ungroup() %>% 
  filter(Power.avg >= 
           quantile(round(quantile(df_wav_6$Power.avg, probs = c(0.75)), 2))) %>%
  filter(site_id %in% unique(df_filt$site_id)) -> df_wav_6_75_csci
# n=29 sites with strong 6 month pattern (in top 25th percentile)
table(df_wav_6_75_csci$gagetype) # ALT=20, REF=9

# Pull Actual Flow Data ---------------------------------------------------

#devtools::install_github("ryanpeek/wateRshedTools")

load("data/usgs_Q_daily_ref_gages_rev.rda") # usgs_flows_ref
load("data/usgs_Q_daily_alt_gages_rev.rda") # usgs_flows_alt

usgs_flows_ref %>% ungroup() %>% distinct(site_no) %>% tally()
usgs_flows_alt %>% ungroup() %>% distinct(site_no) %>% tally()

# now filter to data that had a 6 month peak ()
refdat6 <- usgs_flows_ref %>% 
  filter(site_no %in% unique(df_wav_6_75_csci$site_id)) %>%
  wateRshedTools::add_WYD("date")
altdat6 <- usgs_flows_alt %>% 
  filter(site_no %in% unique(df_wav_6_75_csci$site_id)) %>% 
  wateRshedTools::add_WYD("date")

# filter to filtered CSCI Paired sites
refdat <- usgs_flows_ref %>% 
  filter(site_no %in% unique(df_filt$site_id)) %>% 
  #left_join(., df_filt[,c("site_id", "csci", "MP_metric")], by=c("site_no"="site_id")) %>% 
  wateRshedTools::add_WYD("date")
altdat <- usgs_flows_alt %>% 
  filter(site_no %in% unique(df_filt$site_id)) %>% 
  #left_join(., df_filt[,c("site_id", "csci", "MP_metric")], by=c("site_no"="site_id")) %>% 
  wateRshedTools::add_WYD("date")


# PLOT 6MON by Colwells -------------------------------------------------------

# mean annual
altdat6 %>% distinct(site_no) %>% nrow() # unfilt: n=20
altdat6 %>% 
  group_by(site_no, DOWY) %>% 
  summarize(meanFlow = mean(Flow, na.rm=TRUE)) %>%
  left_join(., df_filt[,c("site_id", "csci", "MP_metric")], by=c("site_no"="site_id")) %>%
  left_join(., df_wav_6, by=c("site_no"="site_id")) %>% 
  ggplot() + 
  theme_bw() +
  # switch color: Power.avg or MP_Metric
  geom_line(aes(x=DOWY, y=log(meanFlow), group=site_no, color=MP_metric)) + 
  #geom_line(aes(x=DOWY, y=meanFlow, group=site_no, color=MP_metric)) +
  #scale_color_viridis(limits=c(0,10)) + # for Power.avg
  scale_color_viridis(limits=c(0,1)) + # for MP
  labs(subtitle = "Alt: Mean Annual Discharge (cfs) [n=20], 6 month peak") -> g6_a
g6_a

ggplotly(g6_a)

refdat6 %>% distinct(site_no) %>% nrow()# n=9
refdat6 %>% 
  group_by(site_no, DOWY) %>% 
  summarize(meanFlow = mean(Flow, na.rm=TRUE)) %>%
  left_join(., df_filt[,c("site_id", "csci", "MP_metric")], by=c("site_no"="site_id")) %>%
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

ggplotly(g6_b)

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
  left_join(., df_filt[,c("site_id", "csci", "MP_metric")], by=c("site_no"="site_id")) %>%
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
  left_join(., df_filt[,c("site_id", "csci", "MP_metric")], by=c("site_no"="site_id")) %>%
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




# Now Plot Max Wavelet Score for All CSCI Sites ---------------------------

# mean annual
altdat %>% distinct(site_no) %>% nrow() #n=137
altdat %>% 
  group_by(site_no, DOWY) %>% 
  summarize(meanFlow = mean(Flow, na.rm=TRUE)) %>%
  left_join(., df_filt[,c("site_id", "csci", "MP_metric")], by=c("site_no"="site_id")) %>%
  left_join(., df_wav_6_csci, by=c("site_no"="site_id")) %>% 
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
  left_join(., df_wav_6_csci, by=c("site_no"="site_id")) %>% #View()
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
  left_join(., df_filt[,c("site_id", "csci", "MP_metric")], by=c("site_no"="site_id")) %>%
  left_join(., df_wav_6_csci, by=c("site_no"="site_id")) %>% 
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
  left_join(., df_filt[,c("site_id", "csci", "MP_metric")], by=c("site_no"="site_id")) %>%
  left_join(., df_wav_6_csci, by=c("site_no"="site_id")) %>% 
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
  left_join(., df_filt[,c("site_id", "csci", "MP_metric")], by=c("site_no"="site_id")) %>%
  left_join(., df_wav_6_csci, by=c("site_no"="site_id")) %>% 
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
  left_join(., df_filt[,c("site_id", "csci", "MP_metric")], by=c("site_no"="site_id")) %>%
  left_join(., df_wav_6_csci, by=c("site_no"="site_id")) %>% 
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

# 6 month peaks only (N=29) that were in top 25%
p6_1 <- df_wav_6_75_csci %>% 
  left_join(., df_filt[,c("site_id", "csci", "MP_metric")], by=c("site_id")) %>%
  ggplot() + geom_boxplot(aes(x=gagetype, y=Power.avg, fill=gagetype))
p6_2 <- df_wav_6_75_csci %>% 
  left_join(., df_filt[,c("site_id", "csci", "MP_metric")], by=c("site_id")) %>%
  ggplot() + geom_boxplot(aes(x=gagetype, y=MP_metric, fill=gagetype))
p6_3 <- df_wav_6_75_csci %>% 
  left_join(., df_filt[,c("site_id", "csci", "MP_metric")], by=c("site_id")) %>%
  ggplot() + geom_boxplot(aes(x=gagetype, y=csci, fill=gagetype))

p6_1 + p6_2 + p6_3
ggsave(filename = "figures/boxplots_of_6mon_sites.png", width = 11, height = 8, dpi=300)

# now same plots but with full dataset
p_1 <- df_wav_max_filt %>% 
  left_join(., df_filt[,c("site_id", "csci", "MP_metric")], by=c("site_id")) %>%
  ggplot() + geom_boxplot(aes(x=gagetype, y=Power.avg, fill=gagetype))
p_2 <- df_wav_max_filt %>% 
  left_join(., df_filt[,c("site_id", "csci", "MP_metric")], by=c("site_id")) %>%
  ggplot() + geom_boxplot(aes(x=gagetype, y=MP_metric, fill=gagetype))
p_3 <- df_wav_max_filt %>% 
  left_join(., df_filt[,c("site_id", "csci", "MP_metric")], by=c("site_id")) %>%
  ggplot() + geom_boxplot(aes(x=gagetype, y=csci, fill=gagetype))

p_1 + p_2 + p_3
ggsave(filename = "figures/boxplots_of_csci_sites_max_wav.png", width = 11, height = 8, dpi=300)

df_wav_max_filt %>% 
  left_join(., df_filt[,c("site_id", "csci", "MP_metric")], by=c("site_id")) %>%
  ggplot() + geom_point(aes(x=csci, y=Power.avg, fill=gagetype), pch=21) +
  geom_smooth(aes(x=csci, y=Power.avg, fill=gagetype), method = "gam")


df_wav_max_filt %>% 
  left_join(., df_filt[,c("site_id", "csci", "MP_metric")], by=c("site_id")) %>%
  ggplot() + geom_point(aes(x=csci, y=Period, fill=gagetype), pch=21) +
  geom_smooth(aes(x=csci, y=Period, fill=gagetype), method = "gam") +
  scale_y_continuous(limits=c(0,32))


df_wav_max_filt %>% 
  left_join(., df_filt[,c("site_id", "csci", "MP_metric")], by=c("site_id")) %>%
  ggplot() + geom_point(aes(x=csci, y=MP_metric, fill=gagetype), pch=21) +
  geom_smooth(aes(x=csci, y=MP_metric, fill=gagetype), method = "gam") 


