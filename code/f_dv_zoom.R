# f_zoom

dv_zoom <- function(dat_dv, sitename, scalelog=T, zoomYr=1970){
  dat_dv <- dat_dv # load data
  
  if(scalelog){
    ggplot() + 
      geom_line(data=dat_dv, aes(x=date, y=flow_cfs)) +
      scale_y_log10(labels = scales::comma) + 
      theme_bw(base_family = "Roboto Condensed")+
      ylab("Discharge (cfs)") + xlab("") +
      ggtitle(paste0(sitename,": USGS"," (", 
                     length(unique(lubridate::year(dat_dv$date)))," years)")) +
      ggforce::facet_zoom(x = lubridate::year(dat_dv$date) == zoomYr, shrink=T)}
  else{
    ggplot() + 
      geom_line(data=dat_dv, aes(x=date, y=flow_cfs)) +
      theme_bw(base_family = "Roboto Condensed")+
      ylab("Discharge (cfs)") + xlab("") +
      ggtitle(paste0(sitename,": USGS"," (", 
                     length(unique(lubridate::year(dat_dv$date)))," years)")) +
      ggforce::facet_zoom(x = lubridate::year(dat_dv$date) == zoomYr, shrink=T)
  }
}
