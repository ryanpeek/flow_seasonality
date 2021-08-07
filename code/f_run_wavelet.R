library(dplyr)
library(readr)
library(WaveletComp)
library(glue)


# function to run wavelet
run_wavelet <- function(data, gage, figout, datout){
  data_df <- data %>% 
    slice( (which.max(flowcnt) - # max value row
              # now 1 minus the max **value** to get first row 
              (data %>% slice_max(flowcnt) %>% 
                 pluck("flowcnt") - 1)):which.max(flowcnt), .preserve = TRUE) %>% 
    as.data.frame()
  
  wv_out <- analyze.wavelet(data_df, my.series = 4, 
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
