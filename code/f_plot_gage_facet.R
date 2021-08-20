
# plot gage faceted
plot_gage_facet <- function(data, x, y, facetid, logged=FALSE, plotly=FALSE){
  if(plotly == TRUE & logged == FALSE){
    p1 <- ggplot() +
      geom_line(data=data,
                aes(x=.data[[x]], y=.data[[y]],
                    group=.data[[facetid]], color=.data[[facetid]]),
                show.legend = F) +
      theme_classic(base_family = "Roboto Condensed", base_size = 9) +
      scale_color_viridis_d() + labs(y="Flow (cfs)", x="") +
      theme(axis.text.x = element_text(angle=90, hjust = 1)) +
      facet_wrap(.~.data[[facetid]], scales= "free_x")
    cat("Plotly it is!")
    return(plotly::ggplotly(p1))
  }
  if(plotly == TRUE & logged==TRUE){
    p2 <- ggplot() +
      geom_line(data=data,
                aes(x=.data[[x]], y=log(.data[[y]]),
                    group=.data[[facetid]], color=.data[[facetid]]),
                show.legend = F) +
      theme_classic(base_family = "Roboto Condensed", base_size = 9) +
      scale_color_viridis_d() + labs(y="log(Flow) (cfs)", x="") +
      theme(axis.text.x = element_text(angle=90, hjust = 1)) +
      facet_wrap(.~.data[[facetid]], scales= "free_x")
    cat("Plotly it is!")
    return(plotly::ggplotly(p2))
  }
  if(logged==TRUE & plotly == FALSE){
    p3 <- ggplot() +
      geom_line(data=data,
                aes(x=.data[[x]], y=log(.data[[y]]),
                    group=.data[[facetid]], color=.data[[facetid]]),
                show.legend = F) +
      theme_classic(base_family = "Roboto Condensed", base_size = 9) +
      scale_color_viridis_d() + labs(y="log(Flow) (cfs)", x="") +
      theme(axis.text.x = element_text(angle=90, hjust = 1)) +
      facet_wrap(.~.data[[facetid]], scales= "free")
    cat("printing static ggplots...")
    return(print(p3))
  }
  p4 <- ggplot() +
    geom_line(data=data,
              aes(x=.data[[x]], y=.data[[y]],
                  group=.data[[facetid]], color=.data[[facetid]]),
              show.legend = F) +
    theme_classic(base_family = "Roboto Condensed", base_size = 9) +
    scale_color_viridis_d() + labs(y="Flow (cfs)", x="") +
    theme(axis.text.x = element_text(angle=90, hjust = 1)) +
    facet_wrap(.~.data[[facetid]], scales= "free")
  cat("printing static ggplots...")
  return(print(p4))
}
