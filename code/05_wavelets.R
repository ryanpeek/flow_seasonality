# Wavelet Calc

# LIBRARIES ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(WaveletComp) # for wavelet analysis
library(hydrostats) # for seasonality colwell analysis
library(viridis)
library(ggforce)
library(sf)
library(glue)
library(fs)

# Get Revised Flow Data ---------------------------------------------------

# flow data
load("data/usgs_Q_daily_alt_gages_trim.rda") # nrow=8977068
load("data/usgs_Q_daily_ref_gages_trim.rda") # nrow=2442511

# get gage metadata:
gage_alt_meta <- usgs_flows_alt_trim %>% distinct(site_no, .keep_all=TRUE) %>%   select(site_no,station_nm:flowcnt) # n=517
gage_ref_meta <- usgs_flows_ref_trim %>% distinct(site_no, .keep_all=TRUE) %>%
  select(site_no,station_nm:flowcnt) # n=221

# bind rows
gage_metadata <- bind_rows(gage_alt_meta, gage_ref_meta)

# Test with Subset --------------------------------------------------------

# test with a few gages first
GAGE_tst <- usgs_flows_ref_trim %>% ungroup() %>%
  distinct(site_no) %>%
  arrange() %>%
  slice(10:20)

# filter to a multiple
flowdat_mult <- usgs_flows_ref_trim %>%
  ungroup() %>%
  filter(site_no %in% GAGE_tst$site_no)

# plot mult
flowdat_mult %>%
  ggplot() +
  geom_line(aes(x=date, y=Flow, color=site_no), show.legend = FALSE) +
  facet_wrap(~site_no, scales = "free")

# now select one gage of interest
flowdat_single <- usgs_flows_ref_trim %>%
  ungroup() %>%
  filter(site_no %in% GAGE_tst$site_no) %>%
  split(.$site_no) %>%
  pluck(10) # select a single dataframe

class(flowdat_single)

# plot one
flowdat_single %>%
  ggplot() +
  geom_line(aes(x=date, y=Flow, color=site_no), show.legend = FALSE) +
  facet_wrap(~site_no, scales = "free")

# Get Gages of Interest ---------------------------------------------------

# get list of gages
GAGE <- usgs_flows_alt_trim %>% ungroup() %>% 
  distinct(site_no) %>% 
  dplyr::arrange(site_no)

# Get List of Dataframes --------------------------------------------------

# get list of dataframes
datalist <- usgs_flows_alt_trim %>% 
  filter(site_no %in% GAGE$site_no) %>% 
  group_by(site_no) %>% 
  group_split() 

# Load & Apply Function --------------------------------------------------

source("code/f_run_wavelet.R")

# check dir exist/create:
fs::dir_create(path = "figures/wavelet")
fs::dir_create(path = "output/wavelets")


# RUN WAVELETS ------------------------------------------------------------

# This takes substantial time (multiple hours) and
# significant disk space (100-140 GB)

# apply function over list
# map(datalist, ~run_wavelet(.x, 
#                            gage = unique(.x$site_no), 
#                            figout ="figures/wavelet",
#                            datout = "output/wavelets"))
  

# Z: WAVELET: TEST SINGLE ---------------------------------------------------------

# wavelet: give Flow col for "my.series" arg
w1 <- analyze.wavelet(flowdat_single, my.series = 4, dt = 1/30, 
                      date.format = "%Y-%m-%d", 
                      date.tz ="America/Los_Angeles")  
# usgs flow col = 4, 1/30 is monthly time frame
# w1$date.tz # check time zone present
# w1$date.format # check date format

# set gage for plotting purposes
(GAGE_sel <- flowdat_single$site_no[1])

# plot image
#png(filename = glue("figures/wavelet/wt_{GAGE_sel}_plot.png"), 
#    width = 11, height = 8.5, units = "in", res = 300)

wt.image(w1, label.time.axis = T, show.date=TRUE, 
         date.format = "%Y-%m-%d", 
         col.ridge = "maroon",
         date.tz = "America/Los_Angeles",
         lwd=1.3,
         color.palette = "viridis(n.levels, option='B')",
         main = glue("{GAGE_sel} : Seasonality of Daily Flow"),
         legend.params = list(
           lab = "cross-wavelet power levels", 
           width=1.2,
           mar=5,
           shrink=0.8),
         timelab = "Time (years)", periodlab = "period (months)")
#dev.off()

wt.avg(w1, show.siglvl = T, siglvl = c(.001, 0.01, 0.05))
w1_df <- w1[c("Power.avg","Period","Power.avg.pval")] %>% as.data.frame

ggplot() + 
  geom_line(data=w1_df, aes(x=Period, y=Power.avg)) +
  geom_point(data=w1_df, aes(x=Period,y=Power.avg), 
             col=ifelse(w1_df$Power.avg.pval<0.05, "maroon", "steelblue")) +
  theme_bw(base_family = "Roboto Condensed") + 
  labs(x="Period (months)", y="Avg Predictability Power") +
  scale_x_continuous(breaks=seq(0,120, 6), limits = c(0,120)) +
  scale_y_continuous(breaks=seq(0,36,4)) #+
#coord_flip()

# save out
# write_csv(w1_df, file = glue("output/wavelet_predictability_{GAGE_sel}.csv"))
# save(w1, file = glue("output/wavelet_model_{GAGE_sel}_gz.rda"), compress = "gzip")

# library(tictoc)
# tic(msg = "Done!")
#write_rds(w1, file = glue("output/wavelet_model_{GAGE_sel}.rds"), compress = "bz")
# 0.5 sec, no compression, ~250MB
# bz = 19.4 sec, 177MB 
# gz = 65 sec, ~157MB
# xz = forever (66 sec), ~157MB

#save(w1, file = glue("output/wavelet_model_{GAGE_sel}.rda")) 
# 6 sec w no compression (177MB)
# 6 sec w gzip (177MB)
# 19.3 sec w bzip2 (~179MB)
# 92 sec w xz (~159MB)

#toc()

## Z: Archive/Tar -------------------------------------------------------------

# library(archive)
# library(here)
# 
# # full dir
# archive_write_dir(glue("{here()}/output/wavelets_zipped2.tgz"), dir = glue("{here()}/output/wavelets/"), format = "tar")
# 
# # specific files
# archive_write_files("output/wavelets_zipped.tgz", c("output/wavelets/wavelet_model_10281800.rda"), format = "tar")
# 
# # unzip     
# archive_extract(glue("{here()}/output/wavelets_zipped2.tgz"), dir = glue("{here()}/output/tst"))


