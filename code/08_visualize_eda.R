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
  group_by(gagetype) %>% tally() # ALT = 294, REF=116

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
table(df_wav_75_csci$gagetype) # ALT=35, REF=24

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

# MEAN ANN PLOT 12MON by Colwells -------------------------------------------------------

# mean annual
altdat %>% distinct(site_no) %>% nrow() # unfilt: n=140

altdat %>% 
  group_by(site_no, DOWY) %>% 
  summarize(meanFlow = mean(Flow, na.rm=TRUE)) %>%
  left_join(., df_final[,c("site_id", "csci", "MP_metric")], by=c("site_no"="site_id")) %>%
  left_join(., df_wav_12, by=c("site_no"="site_id")) %>%
  ggplot() + 
  theme_bw() +
  # switch color: Power.avg or MP_Metric
  #geom_line(aes(x=DOWY, y=log(meanFlow), group=site_no, color=MP_metric)) + 
  geom_line(aes(x=DOWY, y=meanFlow, group=site_no, color=MP_metric)) +
  #scale_color_viridis(limits=c(0,10)) + # for Power.avg
  scale_color_viridis("Colwell (MP)",limits=c(0,1)) + # for MP
  labs(subtitle = "Alt: Mean Annual Discharge (cfs) [n=140], 12 month peak") -> g6_a
g6_a


refdat %>% distinct(site_no) %>% nrow()# n=55
refdat %>% 
  group_by(site_no, DOWY) %>% 
  summarize(meanFlow = mean(Flow, na.rm=TRUE)) %>%
  left_join(., df_final[,c("site_id", "csci", "MP_metric")], by=c("site_no"="site_id")) %>%
  left_join(., df_wav_12, by=c("site_no"="site_id")) %>% 
  ggplot() + 
  theme_bw() +
  # switch color: Power.avg or MP_Metric
  #geom_line(aes(x=DOWY, y=log(meanFlow), group=site_no, color=MP_metric)) + 
  geom_line(aes(x=DOWY, y=meanFlow, group=site_no, color=MP_metric)) +
  #scale_color_viridis(limits=c(0,10)) + # for Power.avg
  scale_color_viridis("Colwell (MP)", limits=c(0,1)) + # for MP
  labs(subtitle = "Ref: Mean Annual Discharge (cfs) [n=55], 12 month peak") -> g6_b
g6_b

#ggplotly(g6_b)

# plot together
g6_a / g6_b

ggsave(filename = "figures/mean_ann_flow_alt_ref_12mon_colwells.png", 
       width = 11, height = 8, dpi=300)

ggsave(filename = "figures/mean_ann_logflow_alt_ref_12mon_colwells.png", 
       width = 11, height = 8, dpi=300)


## now by wavelet -------------------------------------------
# mean annual
altdat %>% 
  group_by(site_no, DOWY) %>% 
  summarize(meanFlow = mean(Flow, na.rm=TRUE)) %>%
  left_join(., df_final[,c("site_id", "csci", "MP_metric")], by=c("site_no"="site_id")) %>%
  left_join(., df_wav_12, by=c("site_no"="site_id")) %>% 
  ggplot() + 
  theme_bw() +
  # switch color: Power.avg or MP_Metric
  geom_line(aes(x=DOWY, y=log(meanFlow), group=site_no, color=Power.avg)) + 
  #geom_line(aes(x=DOWY, y=meanFlow, group=site_no, color=Power.avg)) +
  scale_color_viridis("Wavelet \nPower", limits=c(0,15)) + # for Power.avg
  #scale_color_viridis(limits=c(0,1)) + # for MP
  labs(subtitle = "Alt: Mean Annual Discharge (cfs) [n=155], 12 month peak") -> g6_a
g6_a


refdat %>% 
  group_by(site_no, DOWY) %>% 
  summarize(meanFlow = mean(Flow, na.rm=TRUE)) %>%
  left_join(., df_final[,c("site_id", "csci", "MP_metric")], by=c("site_no"="site_id")) %>%
  left_join(., df_wav_12, by=c("site_no"="site_id")) %>% 
  ggplot() + 
  theme_bw() +
  # switch color: Power.avg or MP_Metric
  geom_line(aes(x=DOWY, y=log(meanFlow), group=site_no, color=Power.avg)) + 
  #geom_line(aes(x=DOWY, y=meanFlow, group=site_no, color=Power.avg)) +
  scale_color_viridis("Wavelet \nPower",limits=c(0,15)) + # for Power.avg
  labs(subtitle = "Ref: Mean Annual Discharge (cfs) [n=55], 12 month peak") -> g6_b
g6_b

# plot together
g6_a / g6_b

ggsave(filename = "figures/mean_ann_flow_alt_ref_12mon_wav.png", 
       width = 11, height = 8, dpi=300)

ggsave(filename = "figures/mean_ann_logflow_alt_ref_12mon_wav.png", 
       width = 11, height = 8, dpi=300)


# More Plots? By CSCI? -------------------------------------------------------------

# mean annual
altdat %>% distinct(site_no) %>% nrow() #n=137
altdat %>% 
  group_by(site_no, DOWY) %>% 
  summarize(meanFlow = mean(Flow, na.rm=TRUE)) %>%
  left_join(., df_final[,c("site_id", "csci", "MP_metric")], by=c("site_no"="site_id")) %>%
  left_join(., df_wav_12, by=c("site_no"="site_id")) %>% 
  ggplot() + 
  theme_bw() +
  geom_line(aes(x=DOWY, y=log(meanFlow), group=site_no, color=csci)) +
  #geom_line(aes(x=DOWY, y=meanFlow, group=site_no, color=csci)) +
  scale_color_viridis(limits=c(0, 1.2)) +
  labs(subtitle = "Alt: Mean Annual Discharge (cfs) [n=140] 12 mon") -> g12_a
g12_a

refdat %>% distinct(site_no) %>% nrow() #n=55 
refdat %>% 
  group_by(site_no, DOWY) %>% 
  summarize(meanFlow = mean(Flow, na.rm=TRUE)) %>%
  left_join(., df_final[,c("site_id", "csci", "MP_metric")], by=c("site_no"="site_id")) %>%
  left_join(., df_wav_12, by=c("site_no"="site_id")) %>% 
  ggplot() + 
  theme_bw() +
  geom_line(aes(x=DOWY, y=log(meanFlow), group=site_no, color=csci)) +
  #geom_line(aes(x=DOWY, y=meanFlow, group=site_no, color=csci)) +
  #scale_color_viridis(limits=c(6, 17)) +
  scale_color_viridis(limits=c(0, 1.2)) +
  labs(subtitle = "Ref: Mean Annual Discharge (cfs) [n=55] 12 mon") -> g12_b
g12_b

# plot together
g12_a / g12_b

ggsave(filename = "figures/mean_ann_flow_alt_ref_12mon_csci.png", 
       width = 11, height = 8, dpi=300)

ggsave(filename = "figures/mean_ann_logflow_alt_ref_12mon_csci.png", 
       width = 11, height = 8, dpi=300)


# Boxplots ----------------------------------------------------------------

# # POWER
(p12_1 <- ggplot(data=df_final, aes(x=gagetype, y=Power.avg)) + 
   geom_jitter(data=df_final, aes(x=gagetype, y=Power.avg), 
               pch=16, size=2, color="gray40", 
               show.legend = FALSE, alpha=0.5) +
   geom_boxplot(data=df_final, aes(x=gagetype, y=Power.avg, fill=gagetype), 
                show.legend = FALSE, alpha=0.8) + 
   labs(y="Interannual Seasonality (Power Avg)", x="") +
   scale_y_log10() +
   theme_classic() +
   scale_fill_viridis_d(option = "E", direction = -1) +
   geom_signif(comparisons = list(c("REF", "ALT")),
               map_signif_level=TRUE))

(p12_2 <- ggplot(data=df_final, aes(x=gagetype, y=MP_metric)) + 
    geom_jitter(data=df_final, aes(x=gagetype, y=MP_metric), 
                pch=16, size=2, color="gray40", 
                show.legend = FALSE, alpha=0.5) +
    geom_boxplot(data=df_final, aes(x=gagetype, y=MP_metric, fill=gagetype), 
                 show.legend = FALSE, alpha=0.8) +
    labs(y="Intra-annual Seasonality (Colwell's M/P)", x="") +
    theme_classic() +
    scale_y_log10() + 
    scale_fill_viridis_d(option = "E", direction = -1) +
    geom_signif(comparisons = list(c("REF", "ALT")),
                map_signif_level=TRUE))

(p12_3 <- ggplot(data=df_final, aes(x=gagetype, y=csci)) + 
    geom_jitter(data=df_final, aes(x=gagetype, y=csci), 
                pch=16, size=2, color="gray40", 
                show.legend = FALSE, alpha=0.5) +
    geom_boxplot(data=df_final, aes(x=gagetype, y=csci, fill=gagetype), 
                 show.legend = FALSE, alpha=0.8) +
    labs(y="CSCI", x="") +
    theme_classic() +
    scale_y_log10() + 
    scale_fill_viridis_d(option = "E", direction = -1) +
    geom_signif(comparisons = list(c("REF", "ALT")),
                map_signif_level=TRUE))

p12_1 + p12_2 + p12_3

ggsave(filename = "figures/boxplots_of_12mon_wav_col_csci.png", width = 11, height = 8, dpi=300)

# add sig: install.packages("ggpubr")
# library(ggpubr)
# ggboxplot(data=df_final, x = "gagetype", y="Power.avg", 
#           fill="gagetype", add="jitter", alpha=0.7,
#           palette = viridis(n=2, direction = -1, option="E")) +
#   stat_compare_means(comparisons = list(c("ALT","REF"))) 


# CSCI vs. Seasonality GAM Plots ------------------------------------------

df_final %>% 
  ggplot() + geom_point(aes(x=csci, y=Power.avg, fill=gagetype), pch=21, size=3) +
  geom_smooth(aes(x=csci, y=Power.avg, color=gagetype), method = "gam") +
  scale_fill_viridis_d("GageType", option="E", direction = -1) + 
  scale_y_log10() +
  scale_color_viridis_d("GageType",option="E", direction = -1) +
  theme_classic()+
  labs(y="Interannual Seasonality \n(Wavelet Power Avg)") -> gg_gam_poweravg
gg_gam_poweravg

df_final %>% 
  ggplot() + geom_point(aes(x=csci, y=MP_metric, fill=gagetype), pch=21, size=3, show.legend = FALSE) +
  geom_smooth(aes(x=csci, y=MP_metric, color=gagetype), method = "gam", show.legend = FALSE) +
  scale_fill_viridis_d(option="E", direction = -1) + 
  #scale_y_log10() +
  scale_color_viridis_d(option="E", direction = -1) +
  theme_classic()+
  labs(y="Intrannual Seasonality (Colwell MP)") -> gg_gam_mp
gg_gam_mp

gg_gam_mp / gg_gam_poweravg
gg_gam_mp + gg_gam_poweravg

ggsave(filename = "figures/trendfit_gam_col_wav_vs_csci.png", width = 11, height = 8.5, 
       dpi=300)
