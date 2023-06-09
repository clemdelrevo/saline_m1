# create map of study area -----------------------------------------------------

map_echantillonnage <- function(points_transect, reunion_map){
  
  #targets::tar_load(reunion_map)
  #targets::tar_load(points_transect)
  
  # extract map of the world in rnaturalearth package
  world <- rnaturalearth::ne_countries(scale='medium',returnclass = 'sf')
  world <- sf::st_transform(world, crs = "EPSG:4326")
  
  # plot map of the world
  map_world <- ggplot2::ggplot(world)+
    ggplot2::geom_sf(fill = "white")+
    ggplot2::coord_sf(crs = sf::st_crs(4326), xlim = c(28, 70), ylim = c(0, -30))+
    ggplot2::geom_rect(
      xmin = 54.5,
      ymin = -20,
      xmax = 56.7,
      ymax = -22,
      fill = NA, 
      colour = "black",
      linewidth = 0.3
    )+
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank())
  
  # sumplify map of Reunion
  saline_map_simple <- sf::st_simplify(reunion_map, dTolerance = 33)
  
  # filter points data
  points_transect <- points_transect[1:9,]
  
  # create map of la Saline reef
  saline <- ggplot2::ggplot(data = saline_map_simple)+
    ggplot2::geom_sf(ggplot2::aes(fill = l1_attrib), 
                     col = "grey", )+
    ggplot2::theme_classic()+
    ggplot2::geom_sf(data = points_transect, fill = "white", shape = 16,
                     col = "red")+
    ggplot2::scale_fill_manual(values = c("grey", "white" ))+
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "light blue")) +
    ggplot2::coord_sf(xlim = c(55.22, 55.26), ylim = c(-21.09, -21.119),
                      expand = FALSE)+
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   legend.title = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   legend.position = "none")+
    ggspatial::annotation_scale(location = "br", line_width = .5) +
    ggspatial::annotation_north_arrow(location = "bl", height = ggplot2::unit(0.7, "cm"), width = ggplot2::unit(0.7, "cm"))
  
  # plot map of Reunion
  reunion <- ggplot2::ggplot(data = reunion_map)+
    ggplot2::geom_sf(fill = "grey", )+
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "light blue"))+
    ggplot2::coord_sf(xlim = c(54.8, 55.8), ylim = c(-20.6, -21.4))+
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
  
  # assemble all plot
  map_reunion <- cowplot::ggdraw(reunion)+
    cowplot::draw_plot(map_world, width = 0.38, height = 0.38, 
                       x = 0.16, y = 0.42)
  
  
  map_final <- cowplot::ggdraw(xlim = c(0, 40), ylim = c(0, 20)) +
    cowplot::draw_plot(map_reunion, x = 0, y = 0, width = 20, height = 20)+
    cowplot::draw_plot(saline, x = 20, y = 0, width =20, height = 20)+
    ggplot2::geom_segment(ggplot2::aes(x = 10.6, xend = 20.7, y = 9.5, yend = 13.75))+
    ggplot2::geom_segment(ggplot2::aes(x = 10.6, xend = 20.7, y = 9.3, yend = 6.4))+
    cowplot::draw_plot_label(c("A", "B", "C"), c(9.5, 18, 38), c(13.7, 13.8, 13.8), size = 11)+
    cowplot::draw_label("M1 BEST ALI
2022/2023
R 4.2.2
WG84
sources : Millénium Coral Reef Mapping Project pour l'Outre-Mer Français ", x = 34.4, y = 5.5, size = 4)
  
  #dir.create("outputs/graphique/cartes")
  ggplot2::ggsave("outputs/graphique/cartes/map_saline.png", dpi = 500,
                  plot = ggplot2::last_plot())
  
}


