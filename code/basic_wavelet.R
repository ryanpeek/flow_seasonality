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

# Prep --------------------------------------------------------------------

# get list of gages
GAGE <- usgs_flows_ref %>% distinct(site_no) %>% slice(1:5)

# filter!
flowdat <- usgs_flows_ref %>% 
  filter(site_no %in% GAGE$site_no) %>% 
  mutate(date = ymd(as.character(Date), 
                    tz="America/Los_Angeles"), .after="Date") %>% 
  select(-agency_cd, -Date)

# plot
flowdat %>% ggplot() + geom_line(aes(x=date, y=Flow, color=site_no)) 

# get list of dataframes
datalist <- usgs_flows_ref %>% 
  filter(site_no %in% GAGE$site_no) %>% 
  mutate(date = ymd(as.character(Date), 
                    tz="America/Los_Angeles"), .after="Date") %>% 
  select(-agency_cd, -Date) %>% 
  group_by(site_no) %>% 
  group_split() 

# WAVELET -----------------------------------------------------------------

# wavelet
w1 <- analyze.wavelet(flowdat, my.series = 5, dt = 1/30, 
                      date.format = "%Y-%m-%d", date.tz ="America/Los_Angeles")  
# usgs flow is in col 5, 1/30 is monthly time frame
w1$date.tz
w1$date.format

wt.image(w1, label.time.axis = T, show.date=TRUE, 
         date.format = "%Y-%m-%d", 
         date.tz = "America/Los_Angeles",
         color.palette = "viridis(n.levels, option='B')",
         main = glue("{GAGE} : Seasonality of Daily Flow"),
         legend.params = list(lab = "cross-wavelet power levels"),
         timelab = "Time (years)", periodlab = "period (months)")

wt.avg(w1, show.siglvl = T, siglvl = c(.001, 0.01, 0.05))

plot_w1 <- w1[c("Power.avg","Period","Power.avg.pval")] %>% as.data.frame

ggplot() + 
  geom_line(data=plot_w1, aes(x=Period, y=Power.avg)) +
  geom_point(data=plot_w1, aes(x=Period,y=Power.avg), 
             col=ifelse(plot_w1$Power.avg.pval<0.05, "maroon", "steelblue")) +
  theme_bw(base_family = "Roboto Condensed") +
  scale_x_continuous(breaks=seq(0,100, 4), limits = c(0,100))

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





# WAVELET LOOP ------------------------------------------------------------

# pre allocate a list to use:
wt.list <- list()
for(i in seq_along(mf[[1]])){
  print(paste0("Processing: ",mf$filename[i]))
  #wt.list[[i]] <- tibble(file=mf$filename[i], site=mf$site[i]) # for testing
  wt.list[[i]] <- analyze.wavelet(mf[[3]][[i]], my.series = 4, dt = 1/30)
  print(paste0("Completed: ", mf$filename[i]))
}

# set names in list
wt.list <- wt.list %>% set_names(df$site)

for(i in seq_along(wt.list)){
  
  pdf(file = paste0("figs/",mf[[2]][i],".pdf"))
  
  wt.image(wt.list[[i]], main = paste0(names(wt.list[i]),
                                       ": Seasonality of Daily Flow"),
           legend.params = list(lab = "cross-wavelet power levels"),
           timelab = "Time (days)", periodlab = "period (months)")
  
  dev.off()
}

# pre allocate data_frame
predict.mf <- list()

for(i in seq_along(wt.list)){
  print(paste0("Processing: ",names(wt.list[i])))
  predict.mf[[i]]<-wt.list[[i]][c("Power.avg","Period","Power.avg.pval")] %>% 
    as.data.frame %>% 
    mutate(site=df$site[i])
}

# flatten all dataframes into one df
predict.mf <- map_df(predict.mf, bind_rows)



# SEASONALITY LOOP --------------------------------------------------------

## From Tonkin et al 2017: M (Contingency) as metric of seasonality
## To standardize the role of seasonality in relation to overall predictability,
## we divided (M) by overall predictability (the sum of (M) and constancy (C)
# 
# s.df <- mf[[3]][3] %>% as.data.frame %>%
# rename(Date=date, Q=flow_cfs)
# s.df <- RUB_dv %>% as.data.frame %>%
# rename(Date=date, Q=flow_avg_cfs)

# analyze
# s.col <- hydrostats::Colwells(s.df)
# (s.mp <- tibble(site=c("RUB_PCWA_2009_2016"), MP_metric=c(s.col$MP)))

# map over the list of data and calc colwell info
season.list <- data_frame(
  site=mf$site,
  MPdat = map(mf[[3]], rename, Date=date, Q=flow_cfs) %>% 
    map(., hydrostats::Colwells)) #%>% 

season.df <- list()
for(i in seq_along(season.list[[2]])){
  print(paste0("Processing: ",season.list$site[i]))
  season.df[[i]] <- tibble(site=season.list$site[i],
                           MP_metric=season.list[[2]][[i]][7]) %>% flatten
  print(paste0("Completed: ", season.list$site[i]))
}

(season.df <- season.df %>% bind_rows) # flatten it out into df

# SAVE --------------------------------------------------------------------

# see the results:
predict.mf %>% group_by(site) %>% 
  summarize(maxPower=max(Power.avg, na.rm = T),
            maxPeriod=Period[which.max(Power.avg)])

season.df

# save model outputs
save(predict.mf, season.df,  file = paste0("models/", river,"_wavelet_colwell.rda"))

# save singles:
# save(p.mf, s.mp, file = paste0("models/RUB_dv_PCWA_wavelet_colwell.rda"))
