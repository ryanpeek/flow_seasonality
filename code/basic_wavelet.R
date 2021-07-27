# Wavelet Calc

# LIBRARIES ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(WaveletComp) # for wavelet analysis
library(hydrostats) # for seasonality colwell analysis
library(viridis)
library(ggforce)
library(ggrepel)
library(dataRetrieval)
library(sf)

# FUNCTIONS ---------------------------------------------------------------

source("scripts/f_dv_zoom.R")


# Test --------------------------------------------------------------------




# USGS: NFA

# at Shirttail
# 11426500 (1911-1941)
get_usgs_dv(gage = 11426500, site = "NFA", facetYr = 1934, filterYrs = F, savedat = T, saveplot = F)

# at NF Clementine
# 11427000 (1941-2017)
get_usgs_dv(gage = 11427000, site = "NFA", facetYr = 1974, filterYrs = F, savedat = T, saveplot = F)

# quick wavelet analysis:
load("data/flows/NFA_dv_USGS_11426500_1911_1941.rda")
nfa <- filename

site <- "NFA"

nfa.w <- analyze.wavelet(nfa, my.series = 4, dt = 1/30) # usgs flow is in col 4

wt.image(nfa.w, label.time.axis = T, show.date=TRUE, 
         date.format = "%Y",
         main = paste0(site, ": Seasonality of Daily Flow"),
         legend.params = list(lab = "cross-wavelet power levels"),
         timelab = "Time (years)", periodlab = "period (months)")

wt.avg(nfa.w, show.siglvl = T, siglvl = c(.001, 0.01, 0.05))

plotNFA<-nfa.w[c("Power.avg","Period","Power.avg.pval")] %>% as.data.frame

ggplot() + 
  geom_line(data=plotNFA, aes(x=Period, y=Power.avg)) +
  geom_point(data=plotNFA, aes(x=Period,y=Power.avg), 
             col=ifelse(plotNFA$Power.avg.pval<0.05, "red", "blue")) + theme_bw() +
  scale_x_continuous(breaks=seq(0,64, 2), limits = c(0,64))

# quick colwell analysis of seasonality:
## From Tonkin et al 2017: M (Contingency) as metric of seasonality
## To standardize the role of seasonality in relation to overall predictability,
## we divided (M) by overall predictability (the sum of (M) and constancy (C)

# standardize
df.nfa <- nfa %>%  
  rename(Date=date, Q=flow_cfs)

# analyze
nfa.Col <- hydrostats::Colwells(df.nfa)
(nfa.season <- tibble(site=c(site), MP_metric=c(nfa.Col$MP)))

# save models
save(plotNFA, nfa.Col, nfa.season,  file = paste0("models/",site,"_usgs_wavelet_colwell.rda"))


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
