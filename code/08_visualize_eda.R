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

df_final <- read_rds("output/07_wavelet_asci_csci_colwell_final.rds")
df_final %>% distinct(bioindicator, site_id, .keep_all = TRUE) %>% 
  group_by(bioindicator, gagetype) %>% tally() 
# CSCI ALT = 125, REF=53
# ASCI ALT = 122, REF=47

# Get Wavelets
# load("output/06_wavelet_combined_period_power_outputs.rda")
# df_wav_max: max power no matter the period
# df_wav: raw data, all points
# df_wav_12: max power at 12 months
# df_wav_6: max power at 6 months

# Filter Wav above 75% percentile  ----------------------------------

# Filtered to top 25 percent of all sites 
# df_final %>%
#   filter(Power.avg >= round(quantile(df_wav_max$Power.avg, probs = c(0.75)), 2)) %>%
#   filter(site_id %in% unique(df_final$site_id)) -> df_wav_75_csci
# # n=29 sites with strong 6 month pattern (in top 25th percentile)
# table(df_wav_75_csci$gagetype) # ALT=35, REF=24

# Pull Actual Flow Data ---------------------------------------------------

#devtools::install_github("ryanpeek/wateRshedTools")

load("data/usgs_Q_daily_ref_gages_trim.rda") # usgs_flows_ref
load("data/usgs_Q_daily_alt_gages_trim.rda") # usgs_flows_alt

usgs_flows_ref_trim %>% ungroup() %>% distinct(site_no) %>% tally()
usgs_flows_alt_trim %>% ungroup() %>% distinct(site_no) %>% tally()

# filter to filtered CSCI Paired sites
refdat <- usgs_flows_ref_trim %>% 
  filter(site_no %in% unique(df_final$site_id)) 

altdat <- usgs_flows_alt_trim %>% 
  filter(site_no %in% unique(df_final$site_id)) 

length(unique(altdat$site_no)) # n=164
length(unique(refdat$site_no)) # n=67

# MEAN ANN PLOT 12MON by Colwells -------------------------------------------------------

# mean annual
altdat %>% distinct(site_no) %>% nrow() # unfilt: n=164

# Colwell
(altdat %>% 
    group_by(site_no, DOWY) %>% 
    summarize(meanFlow = mean(Flow, na.rm=TRUE)) %>%
    left_join(., df_final[,c("site_id", "bioindicator","biovalue", 
                             "MP_metric", "Power.avg", "class3_name","gagetype")],
              by=c("site_no"="site_id")) %>%
    ggplot() + 
    theme_classic() +
    # switch color: Power.avg or MP_Metric
    #geom_line(aes(x=DOWY, y=log(meanFlow), group=site_no, color=MP_metric)) + 
    geom_line(aes(x=DOWY, y=log(meanFlow), group=site_no, color=MP_metric)) +
    #scale_color_viridis(limits=c(0,10)) + # for Power.avg
    scale_color_viridis("Colwell (MP)",limits=c(0,1)) + # for MP
    labs(y="Mean Annual Discharge (cfs)", x="Day of Water Year",
         subtitle = "Altered Gages [n=164]") +
    facet_grid(rows = vars(bioindicator), scales = "free_y") -> g1_a)

ggsave(filename = "figures/mean_ann_logflow_alt_csci_asci_12mon_colwells.png", 
       width = 11, height = 8, dpi=300)
ggsave(filename = "figures/mean_ann_flow_alt_csci_asci_12mon_colwells.png", 
       width = 11, height = 8, dpi=300)

# Wavelet
refdat %>% distinct(site_no) %>% nrow()# n=67
(refdat %>% 
    group_by(site_no, DOWY) %>% 
    summarize(meanFlow = mean(Flow, na.rm=TRUE)) %>%
    left_join(., df_final[,c("site_id", "bioindicator","biovalue", 
                             "MP_metric", "Power.avg", "class3_name","gagetype")],
              by=c("site_no"="site_id")) %>%
    ggplot() + 
    theme_classic() +
    # switch color: Power.avg or MP_Metric
    geom_line(aes(x=DOWY, y=log(meanFlow), group=site_no, color=MP_metric)) + 
    #geom_line(aes(x=DOWY, y=meanFlow, group=site_no, color=MP_metric)) +
    #scale_color_viridis(limits=c(0,10)) + # for Power.avg
    scale_color_viridis("Colwell (MP)", limits=c(0,1)) + # for MP
    labs(y="Mean Annual Discharge (cfs)", x="Day of Water Year",
         subtitle = "Ref Gages [n=67]") +
    facet_grid(rows = vars(bioindicator)) -> g1_b)


# plot together
g1_a / g1_b

ggsave(filename = "figures/mean_ann_flow_alt_ref_12mon_colwells.png", 
       width = 11, height = 8, dpi=300)

ggsave(filename = "figures/mean_ann_logflow_alt_ref_12mon_colwells.png", 
       width = 11, height = 8, dpi=300)





## now by wavelet -------------------------------------------
# mean annual
# Colwell
(altdat %>% 
   group_by(site_no, DOWY) %>% 
   summarize(meanFlow = mean(Flow, na.rm=TRUE)) %>%
   left_join(., df_final[,c("site_id", "bioindicator","biovalue", 
                            "MP_metric", "Power.avg", "class3_name","gagetype")],
             by=c("site_no"="site_id")) %>%
   ggplot() + 
   theme_classic() +
   # switch color: Power.avg or MP_Metric
   #geom_line(aes(x=DOWY, y=log(meanFlow), group=site_no, color=Power.avg)) + 
   geom_line(aes(x=DOWY, y=log(meanFlow), group=site_no, color=Power.avg)) +
   scale_color_viridis("Wavelet \nPower",limits=c(0,15)) + # for Power.avg
   labs(y="Mean Annual Discharge (cfs)", x="Day of Water Year",
        subtitle = "Altered Gages [n=160], 12 month peak") +
   facet_grid(rows = vars(bioindicator), scales = "free_y") -> g2_a)

ggsave(filename = "figures/mean_ann_logflow_alt_csci_asci_12mon_wav.png", 
       width = 11, height = 8, dpi=300)
ggsave(filename = "figures/mean_ann_flow_alt_csci_asci_12mon_wav.png", 
       width = 11, height = 8, dpi=300)


# More Plots? By CSCI? -------------------------------------------------------------

# mean annual
(altdat %>% 
  group_by(site_no, DOWY) %>% 
  summarize(meanFlow = mean(Flow, na.rm=TRUE)) %>%
  left_join(., df_final[,c("site_id", "bioindicator","biovalue", 
                           "MP_metric", "Power.avg", "class3_name","gagetype")],
            by=c("site_no"="site_id")) %>%
  ggplot() + 
  theme_bw() +
  #geom_line(aes(x=DOWY, y=log(meanFlow), group=site_no, color=biovalue)) +
  geom_line(aes(x=DOWY, y=meanFlow, group=site_no, color=biovalue)) +
  scale_color_viridis(limits=c(0, 1.2)) +
  labs(y="Mean Annual Discharge (cfs)", x="Day of Water Year",
       subtitle = "Altered Gages [n=160], 12 month peak") +
  facet_grid(rows = vars(bioindicator), scales = "free_y") -> g3_a)
g3_a

# plot together

ggsave(filename = "figures/mean_ann_flow_alt_ref_12mon_asci_csci.png", 
       width = 11, height = 8, dpi=300)

ggsave(filename = "figures/mean_ann_logflow_alt_ref_12mon_asci_csci.png", 
       width = 11, height = 8, dpi=300)


# Boxplots ----------------------------------------------------------------

library(ggpubr)


## Wavelet: CSCI v Gagetype ------------------------------------------------

(p12_1_csci <- ggplot(data=df_final %>% filter(bioindicator=="CSCI"), 
                      aes(x=gagetype, y=Power.avg)) + 
   geom_jitter(data=df_final %>% filter(bioindicator=="CSCI"), 
               aes(x=gagetype, y=Power.avg), 
               pch=16, size=2, color="gray40", 
               show.legend = FALSE, alpha=0.5) +
   geom_boxplot(data=df_final %>% filter(bioindicator=="CSCI"), 
                aes(x=gagetype, y=Power.avg, fill=gagetype), 
                show.legend = FALSE, alpha=0.8, notch = TRUE) + 
   labs(y="Interannual Seasonality (Power Avg)", x="",
        subtitle="CSCI") +
   scale_y_log10() +
   theme_classic() +
   scale_fill_viridis_d(option = "E", direction = -1) +
   geom_signif(data=df_final %>% filter(bioindicator=="CSCI"),
               comparisons = list(c("REF", "ALT")),
               map_signif_level=TRUE))

## Wavelet: ASCI v Gagetype ------------------------------------------------

(p12_1_asci <- ggplot(data=df_final %>% filter(bioindicator=="ASCI"), 
                      aes(x=gagetype, y=Power.avg)) + 
   geom_jitter(data=df_final %>% filter(bioindicator=="ASCI"), 
               aes(x=gagetype, y=Power.avg), 
               pch=16, size=2, color="gray40", 
               show.legend = FALSE, alpha=0.5) +
   geom_boxplot(data=df_final %>% filter(bioindicator=="ASCI"), 
                aes(x=gagetype, y=Power.avg, fill=gagetype), 
                show.legend = FALSE, alpha=0.8, notch = TRUE) + 
   labs(y="Interannual Seasonality (Power Avg)", x="",
        subtitle="ASCI") +
   scale_y_log10() +
   theme_classic() +
   scale_fill_viridis_d(option = "E", direction = -1) +
   geom_signif(data=df_final %>% filter(bioindicator=="ASCI"),
               comparisons = list(c("REF", "ALT")),
               map_signif_level=TRUE))



## Wavelet: ASCI vs CSCI ---------------------------------------------------

(p12_1 <- ggplot(data=df_final, #%>% filter(gagetype=="ALT"), 
                 aes(x=bioindicator, y=Power.avg)) + 
    geom_jitter(data=df_final, #%>% filter(gagetype=="ALT"), 
                aes(x=bioindicator, y=Power.avg), 
                pch=16, size=2, color="gray40", 
                show.legend = FALSE, alpha=0.5) +
    geom_boxplot(data=df_final, #%>% filter(gagetype=="ALT"), 
                 aes(x=bioindicator, y=Power.avg, fill=bioindicator), 
                 show.legend = FALSE, alpha=0.8, notch = TRUE) + 
    labs(y="Interannual Seasonality (Power Avg)", x="",
         subtitle="ASCI vs. CSCI") +
    scale_y_log10() +
    theme_classic() +
    scale_fill_viridis_d(option = "E", direction = -1) +
    geom_signif(data = df_final, #%>% filter(gagetype=="ALT"),
                comparisons = list(c("ASCI", "CSCI")),
                map_signif_level=TRUE) +
   facet_grid(.~gagetype))


## Colwell: CSCI vs Gagetype -----------------------------------------------

# CSCI v gagetype
(p12_2_csci <- ggplot(data=df_final %>% filter(bioindicator=="CSCI"), 
                      aes(x=gagetype, y=MP_metric)) + 
    geom_jitter(data=df_final %>% filter(bioindicator=="CSCI"), 
                aes(x=gagetype, y=MP_metric), 
                pch=16, size=2, color="gray40", 
                show.legend = FALSE, alpha=0.5) +
    geom_boxplot(data=df_final %>% filter(bioindicator=="CSCI"), 
                 aes(x=gagetype, y=MP_metric, fill=gagetype), 
                 show.legend = FALSE, alpha=0.8, notch = TRUE) +
    labs(y="Intra-annual Seasonality (Colwell's M/P)", x="",
         subtitle="CSCI") +
    theme_classic() +
    scale_y_log10() + 
    scale_fill_viridis_d(option = "E", direction = -1) +
    geom_signif(data = df_final %>% filter(bioindicator=="CSCI"),
                comparisons = list(c("REF", "ALT")),
                map_signif_level=TRUE))


## Colwell: ASCI vs Gagetype -----------------------------------------------

# ASCI v gagetype
(p12_2_asci <- ggplot(data=df_final %>% filter(bioindicator=="ASCI"), 
                      aes(x=gagetype, y=MP_metric)) + 
   geom_jitter(data=df_final %>% filter(bioindicator=="ASCI"), 
               aes(x=gagetype, y=MP_metric), 
               pch=16, size=2, color="gray40", 
               show.legend = FALSE, alpha=0.5) +
   geom_boxplot(data=df_final %>% filter(bioindicator=="ASCI"), 
                aes(x=gagetype, y=MP_metric, fill=gagetype), 
                show.legend = FALSE, alpha=0.8) +
   labs(y="Intra-annual Seasonality (Colwell's M/P)", x="",
        subtitle="ASCI") +
   theme_classic() +
   scale_y_log10() + 
   scale_fill_viridis_d(option = "E", direction = -1) +
   geom_signif(data = df_final %>% filter(bioindicator=="ASCI"),
               comparisons = list(c("REF", "ALT")),
               map_signif_level=TRUE))


## Colwell: CSCI vs ASCI -----------------------------------------------

# ASCI vs CSCI Colwell
(p12_2 <- ggplot(data=df_final, #%>% filter(gagetype=="ALT"), 
                 aes(x=bioindicator, y=MP_metric)) + 
    geom_jitter(data=df_final, aes(x=bioindicator, y=MP_metric), 
                pch=16, size=2, color="gray40", 
                show.legend = FALSE, alpha=0.5) +
    geom_boxplot(data=df_final, aes(x=bioindicator, y=MP_metric, fill=bioindicator), 
                 show.legend = FALSE, alpha=0.8) +
    labs(y="Intra-annual Seasonality (Colwell's M/P)", x="",
         subtitle="ASCI vs. CSCI") +
    theme_classic() +
    scale_y_log10() + 
    scale_fill_viridis_d(option = "E", direction = -1) +
    geom_signif(data = df_final, 
                comparisons = list(c("ASCI", "CSCI")),
                map_signif_level=TRUE) +
  facet_grid(.~gagetype))


## Gagetype: CSCI  ------------------------------------------------------

(p12_3_csci <- ggplot(data=df_final %>% filter(bioindicator=="CSCI"), 
                      aes(x=gagetype, y=biovalue)) + 
    geom_jitter(data=df_final %>% filter(bioindicator=="CSCI"), 
                aes(x=gagetype, y=biovalue), 
                pch=16, size=2, color="gray40", 
                show.legend = FALSE, alpha=0.5) +
    geom_boxplot(data=df_final %>% filter(bioindicator=="CSCI"), 
                 aes(x=gagetype, y=biovalue, fill=gagetype), 
                 show.legend = FALSE, alpha=0.8) +
    labs(y="CSCI", x="", subtitle="CSCI") +
    theme_classic() +
    scale_y_log10() + 
    scale_fill_viridis_d(option = "E", direction = -1) +
    geom_signif(data = df_final %>% filter(bioindicator=="CSCI"), 
                comparisons = list(c("REF", "ALT")),
                map_signif_level=TRUE))


## Gagetype: ASCI  ------------------------------------------------------

(p12_3_asci <- ggplot(data=df_final %>% filter(bioindicator=="ASCI"), 
                      aes(x=gagetype, y=biovalue)) + 
   geom_jitter(data=df_final %>% filter(bioindicator=="ASCI"), 
               aes(x=gagetype, y=biovalue), 
               pch=16, size=2, color="gray40", 
               show.legend = FALSE, alpha=0.5) +
   geom_boxplot(data=df_final %>% filter(bioindicator=="ASCI"), 
                aes(x=gagetype, y=biovalue, fill=gagetype), 
                show.legend = FALSE, alpha=0.8) +
   labs(y="ASCI", x="", subtitle="ASCI") +
   theme_classic() +
   scale_y_log10() + 
   scale_fill_viridis_d(option = "E", direction = -1) +
   geom_signif(data = df_final %>% filter(bioindicator=="ASCI"), 
               comparisons = list(c("REF", "ALT")),
               map_signif_level=TRUE))



## Save Boxplots -----------------------------------------------------------

p12_1_csci + p12_2_csci + p12_3_csci
ggsave(filename = "figures/boxplots_of_12mon_wav_col_csci.png", width = 11, height = 8, dpi=300)

p12_1_asci + p12_2_asci + p12_3_asci
ggsave(filename = "figures/boxplots_of_12mon_wav_col_asci.png", width = 11, height = 8, dpi=300)


p12_1 + p12_2
ggsave(filename = "figures/boxplots_of_12mon_wav_col_asci_csci.png", width = 11, height = 8, dpi=300)


# add sig: install.packages("ggpubr")
# library(ggpubr)
# ggboxplot(data=df_final, x = "gagetype", y="Power.avg", 
#           fill="gagetype", add="jitter", alpha=0.7,
#           palette = viridis(n=2, direction = -1, option="E")) +
#   stat_compare_means(comparisons = list(c("ALT","REF"))) 

# Bioindicator vs. Seasonality GAM Plots ------------------------------------------

df_final %>% filter(gagetype=="ALT") %>% 
  ggplot() + geom_point(aes(x=biovalue, y=Power.avg, fill=bioindicator), 
                        pch=21, size=3) +
  geom_smooth(aes(x=biovalue, y=Power.avg, fill=bioindicator), color="gray70", 
              method = "gam", alpha=0.3) +
  scale_fill_viridis_d("Bioindicator", option="E", direction = -1) + 
  scale_y_log10() +
  scale_color_viridis_d("Bioindicator",option="E", direction = -1) +
  theme_classic()+
  labs(y="Interannual Seasonality \n(Wavelet Power Avg)") -> gg1_gam_poweravg
gg1_gam_poweravg

df_final %>% filter(gagetype=="ALT") %>% 
  ggplot() + geom_point(aes(x=biovalue, y=MP_metric, fill=bioindicator), 
                        pch=21, size=3) +
  geom_smooth(aes(x=biovalue, y=MP_metric, fill=bioindicator), color="gray70", 
              method = "gam", alpha=0.3) +
  scale_fill_viridis_d("Bioindicator", option="E", direction = -1) + 
  scale_y_log10() +
  scale_color_viridis_d("Bioindicator",option="E", direction = -1) +
  theme_classic()+
  labs(y="Intrannual Seasonality \n(Colwell MP)") -> gg1_gam_mp
gg1_gam_mp

gg1_gam_mp / gg1_gam_poweravg
gg1_gam_mp + gg1_gam_poweravg

ggsave(filename = "figures/trendfit_gam_col_wav_vs_asci_csci.png", 
       width = 11, height = 8.5, dpi=300)


# CSCI vs. Seasonality GAM Plots ------------------------------------------

df_final %>% filter(bioindicator=="CSCI") %>% 
  ggplot() + geom_point(aes(x=csci, y=Power.avg, fill=gagetype), pch=21, size=3) +
  geom_smooth(aes(x=csci, y=Power.avg, color=gagetype), method = "gam") +
  scale_fill_viridis_d("GageType", option="E", direction = -1) + 
  scale_y_log10() +
  scale_color_viridis_d("GageType",option="E", direction = -1) +
  theme_classic()+
  labs(y="Interannual Seasonality \n(Wavelet Power Avg)",
       x="CSCI") -> gg_gam_poweravg
gg_gam_poweravg

df_final %>% filter(bioindicator=="CSCI") %>% 
  ggplot() + geom_point(aes(x=csci, y=MP_metric, fill=gagetype), pch=21, size=3, show.legend = FALSE) +
  geom_smooth(aes(x=csci, y=MP_metric, color=gagetype), method = "gam", show.legend = FALSE) +
  scale_fill_viridis_d(option="E", direction = -1) + 
  #scale_y_log10() +
  scale_color_viridis_d(option="E", direction = -1) +
  theme_classic()+
  labs(y="Intrannual Seasonality (Colwell MP)",
       x="CSCI") -> gg_gam_mp
gg_gam_mp

gg_gam_mp / gg_gam_poweravg
gg_gam_mp + gg_gam_poweravg

ggsave(filename = "figures/trendfit_gam_col_wav_vs_csci.png", width = 11, height = 8.5, 
       dpi=300)
