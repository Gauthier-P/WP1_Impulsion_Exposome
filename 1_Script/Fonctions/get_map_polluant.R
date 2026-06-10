f.polluant <- function(plot.data, polluant, p.title){
  
  ggplot(plot.data) +
    geom_sf(aes(fill = .data[[polluant]]), color = NA) +
    coord_sf(
      expand = FALSE) + 
    scale_fill_viridis_c(
      option = "turbo",
      na.value = "grey90",
      name ="**Concentration**<br><span style='color:grey'>µg/m<sup>3</sup></span>",
      guide = guide_colorbar(
        title.position = "top",   
        title.hjust = 0,  
        label.theme = element_text(size = 6), 
        direction = "horizontal",
        title.theme = element_markdown(size = 8))
    ) +
    labs(title = p.title) +
    theme_void() +
    theme(
      plot.background = element_rect(fill="#fbf9f4",color=NA),
      legend.position = "bottom",
      legend.text = element_text(),
      legend.key.height = unit(0.15, "cm"),
      legend.key.width  = unit(0.8, "cm"),
      plot.title = element_markdown(hjust=0.5, face="bold"),
      plot.subtitle = element_text(hjust=0.5,color="grey40"),
      plot.caption = element_markdown(color="grey20",hjust=0.5)
    )
}