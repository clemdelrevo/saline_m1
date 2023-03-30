map_echantillonnage <- function(points_transect, reunion_map){
  
  #targets::tar_load(reunion_map)
  #targets::tar_load(points_transect)
  points_transect <- points_transect[1:9,]
  
  saline <- ggplot2::ggplot(data = reunion_map)+
    ggplot2::geom_sf(ggplot2::aes(fill = l1_attrib), 
                     col = "grey", )+
    ggplot2::theme_classic()+
    ggplot2::geom_sf(data = points_transect, fill = "white", shape = 16,
                     col = "red")+
    #ggplot2::scale_fill_manual(values = c("white", "sienna", "turquoise3", "steelblue3" ))+
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "light blue")) +
    ggplot2::coord_sf(xlim = c(55.22, 55.26), ylim = c(-21.09, -21.119),
                      expand = FALSE)+
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90),
                   legend.title = ggplot2::element_blank())
  
  reunion <- ggplot2::ggplot(data = reunion_map)+
    ggplot2::geom_sf(fill = "grey", )+
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "light blue"))+
    ggspatial::annotation_scale(location = "br", line_width = .5) +
    ggspatial::annotation_north_arrow(location = "bl", height = ggplot2::unit(0.7, "cm"), width = ggplot2::unit(0.7, "cm"))+
    ggplot2::geom_rect(
      xmin = 55.22,
      ymin = -21.09,
      xmax = 55.26,
      ymax = -21.119,
      fill = NA, 
      colour = "black",
      linewidth = 0.6
    )
  
  map_saline <- cowplot::plot_grid(reunion, saline, rel_widths = c(1.2, 1))
  
  #dir.create("outputs/graphique/cartes")
  ggplot2::ggsave("outputs/graphique/cartes/map_saline.png", dpi = 500,
                  plot = ggplot2::last_plot())
  
}
