# Wavelet Calc

# LIBRARIES ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(WaveletComp) # for wavelet analysis
library(hydrostats) # for seasonality colwell analysis
library(viridis)
library(ggforce)
library(ggrepel)
library(sf)
library(glue)

# Data --------------------------------------------------------------------

# flow data
load("data/usgs_Q_daily_alt_gages.rda")
load("data/usgs_Q_daily_ref_gages.rda")


# Fix Dates ---------------------------------------------------------------

# check dates:
tst <- usgs_flows_ref %>% group_by(site_no) %>% 
  complete(Date = seq.Date(min(Date), max(Date), by = "day")) %>% 
  group_by(site_no) %>%
  mutate(diff = Date - lag(Date))


# Prep --------------------------------------------------------------------

# fix date cols
usgs_flows_ref <- usgs_flows_ref %>% 
  mutate(date = ymd(as.character(Date), 
                    tz="America/Los_Angeles"), .after="Date") %>% 
  select(-agency_cd, -Date)

usgs_flows_alt <- usgs_flows_alt %>% 
  mutate(date = ymd(as.character(Date), 
                    tz="America/Los_Angeles"), .after="Date") %>% 
  select(-agency_cd, -Date)



# Get Gages of Interest ---------------------------------------------------

# get list of gages
GAGE <- usgs_flows_ref %>% distinct(site_no) %>% slice(12:nrow(.))
GAGE <- usgs_flows_ref %>% distinct(site_no) %>% slice(14:20)

# filter!
flowdat <- usgs_flows_ref %>% 
  filter(site_no %in% GAGE$site_no)

# test dates
# check dates:
tst <- flowdat %>% group_by(site_no) %>% 
  complete(Date = seq.Date(min(Date), max(Date), by = "day")) %>% 
  count(Date, sort = TRUE)

# plot
flowdat %>% ggplot() + geom_line(aes(x=date, y=Flow, color=site_no), show.legend = FALSE) +
  facet_wrap(~site_no, scales = "free")

# get list of dataframes
datalist <- usgs_flows_ref %>% 
  filter(site_no %in% GAGE$site_no) %>% 
  group_by(site_no) %>% 
  group_split() 

# FUNCTION TO DO THIS STUFF -----------------------------------------------

# function to run wavelet
run_wavelet <- function(data, gage, figout, datout){
  gage <- gage
  wv_out <- analyze.wavelet(data, my.series = 4, 
                            dt = 1/30,
                            date.format = "%Y-%m-%d", 
                            date.tz ="America/Los_Angeles")
  # plot
  png(filename = glue("{figout}/wt_{gage}_plot.png"), 
      width = 11, height = 8.5, units = "in", res = 300)
  wt.image(wv_out, 
           label.time.axis = T, show.date=TRUE, 
           date.format = "%Y-%m-%d", 
           col.ridge = "maroon",
           date.tz = "America/Los_Angeles",
           lwd=1.3,
           color.palette = "viridis(n.levels, option='B')",
           main = glue("{gage} : Seasonality of Daily Flow"),
           legend.params = list(
             lab = "cross-wavelet power levels", 
             width=1,
             mar=4.5,
             shrink=0.8),
           timelab = "Time (years)", periodlab = "period (months)")
  dev.off()
  
  # write df
  wv_df <- wv_out[c("Power.avg","Period","Power.avg.pval")] %>% as.data.frame
  
  # save out
  write_csv(wv_df, file = glue("{datout}/wavelet_predictability_{gage}.csv"))
  save(wv_out, file = glue("{datout}/wavelet_model_{gage}.rda"), compress = "gzip")
  
}

# apply?
map(datalist, ~purrr::possibly(run_wavelet(.x, unique(.x$site_no), "figures/wavelet", "output/wavelets"), otherwise = "NULL"))
  

# WAVELET: SINGLE ---------------------------------------------------------

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

## Archive/Tar -------------------------------------------------------------

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


# WAVELET PURRR -----------------------------------------------------------

# now apply above to a list
wavlist <- map(datalist, ~analyze.wavelet(.x, my.series = 4, dt = 1/30,
                                  date.format = "%Y-%m-%d", 
                                  date.tz = "America/Los_Angeles"))

wavlist <- wavlist %>% set_names(., GAGE$site_no)
wavdat <- map_df(wavlist, ~.x[c("Power.avg","Period","Power.avg.pval")], .id = "site_no")
table(wavdat$site_no)

# write it out!!
gagesNo <- "1_5"

write_csv(wavdat, file = glue("output/wavelet_outputs_gages_{gagesNo}.csv"))
save(wavlist, file = glue("output/wavelet_outputs_gages_{gagesNo}.rda"), compress = "xz")

# COLWELL -----------------------------------------------------------------

# quick colwell analysis of seasonality:
## From Tonkin et al 2017: M (Contingency) as metric of seasonality
## To standardize the role of seasonality in relation to overall predictability,
## we divided (M) by overall predictability (the sum of (M) and constancy (C)

# standardize
df_flowdat <- flowdat %>% 
  rename(Q=Flow) %>% 
  select(Date, Q)

# analyze
df.Col <- hydrostats::Colwells(df_flowdat)
(df.season <- tibble(site_no=c(GAGE$site_no), MP_metric=c(df.Col$MP)))

# compare?
df_final <- read_rds("output/usgs_gages_colwells_w_streamclass_metric.rds")
# revised flow data
alt_revised <- read_rds("output/alt_revised_gages.rds") %>% select(-gagetype)
ref_revised <- read_rds("output/ref_revised_gages.rds")

