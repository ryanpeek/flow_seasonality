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

load("data/usgs_Q_daily_ref_gages_rev.rda")
load("data/usgs_Q_daily_alt_gages_rev.rda")

# Filter to Subset --------------------------------------------------------

# test with a few gages first
GAGE_tst <- usgs_flows_ref %>% ungroup() %>% 
  distinct(site_no) %>%
  arrange() %>% 
  slice(14:20)

# filter to a multiple
flowdat_mult <- usgs_flows_ref %>%
  ungroup() %>% 
  filter(site_no %in% GAGE_tst$site_no) 

# plot mult
flowdat_mult %>% 
  ggplot() + 
  geom_line(aes(x=date, y=Flow, color=site_no), show.legend = FALSE) +
  facet_wrap(~site_no, scales = "free")

# now select one gage of interest
# filter to a single df
flowdat_single <- usgs_flows_ref %>% 
  ungroup() %>% 
  filter(site_no %in% GAGE_tst$site_no) %>% 
  split(.$site_no) %>% 
  pluck(2) # select a single dataframe

# select one
flowdat_single %>% 
  ggplot() + 
  geom_line(aes(x=date, y=Flow, color=site_no), show.legend = FALSE) +
  facet_wrap(~site_no, scales = "free")

# Check Periods: Start and End --------------------------------------------

# figure out how to split or take the longest sequence of numbers
# find the longest seq row number from the flowcnt field
# then subtract that many rows back to get the starting row
# flowdat_single %>% 
#   # get the max value and max row and select first and last
#   slice( (which.max(flowcnt) - # max value row
#             # now 1 minus the max **value** to get first row 
#             (flowdat_single %>% slice_max(flowcnt) %>% pluck("flowcnt") - 1)):which.max(flowcnt), 
#          .preserve = TRUE) %>% 
#   View()

# Get Gages of Interest ---------------------------------------------------

# get list of gages
GAGE <- usgs_flows_alt %>% ungroup() %>% 
  distinct(site_no) %>% 
  dplyr::arrange(site_no)

# Get List of Dataframes --------------------------------------------------

# get list of dataframes
datalist <- usgs_flows_alt %>% 
  filter(site_no %in% GAGE$site_no) %>% 
  group_by(site_no) %>% 
  group_split() 


# Load & Apply Function --------------------------------------------------

source("code/f_run_wavelet.R")

# check dir exist/create:
fs::dir_create(path = "figures/wavelet")
fs::dir_create(path = "output/wavelets")

# apply function over list
map(datalist, ~run_wavelet(.x, 
                           gage = unique(.x$site_no), 
                           figout ="figures/wavelet",
                           datout = "output/wavelets"))
  

# Z: WAVELET: SINGLE ---------------------------------------------------------

# wavelet: give Flow col for "my.series" arg
w1 <- analyze.wavelet(flowdat, my.series = 4, dt = 1/30, 
                      date.format = "%Y-%m-%d", date.tz ="America/Los_Angeles")  
# usgs flow col = 4, 1/30 is monthly time frame
# w1$date.tz # check time zone present
# w1$date.format # check date format

# set gage for plotting purposes
(GAGE_sel <- GAGE$site_no[1])

# plot image
png(filename = glue("figures/wavelet/wt_{GAGE_sel}_plot.png"), 
    width = 11, height = 8.5, units = "in", res = 300)
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
dev.off()

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
write_csv(w1_df, file = glue("output/wavelet_predictability_{GAGE_sel}.csv"))
save(w1, file = glue("output/wavelet_model_{GAGE_sel}_gz.rda"), compress = "gzip")

library(tictoc)
tic(msg = "Done!")
#write_rds(w1, file = glue("output/wavelet_model_{GAGE_sel}.rds"), compress = "bz")
# 0.5 sec, no compression, ~250MB
# bz = 19.4 sec, 177MB 
# gz = 65 sec, ~157MB
# xz = forever (66 sec), ~157MB

save(w1, file = glue("output/wavelet_model_{GAGE_sel}.rda")) 
# 6 sec w no compression (177MB)
# 6 sec w gzip (177MB)
# 19.3 sec w bzip2 (~179MB)
# 92 sec w xz (~159MB)

toc()

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


