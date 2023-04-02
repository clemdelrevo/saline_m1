graphique_recouvrement_substrat_station <- function(recouvrement_substrat){
  
  #targets::tar_load(recouvrement_substrat)
  barplot_substrat_station <- recouvrement_substrat |>
    dplyr::group_by(station, type_substrat) |>
    dplyr::summarise(moyenne_recouvrement = mean(recouvrement),
                     sd_recouvrement = sd(recouvrement),
                     erreur_st = plotrix::std.error(recouvrement))
  
  
  barplot_substrat_station <- ggplot2::ggplot(barplot_substrat_station, 
                                              ggplot2::aes(y = moyenne_recouvrement, 
                                                      x = type_substrat,
                                                      fill = station))+
    ggplot2::scale_fill_brewer(palette = "Set1")+
    ggplot2::theme_bw()+
    ggplot2::geom_col(position = "dodge")+
    ggplot2::geom_errorbar(ggplot2::aes(ymin = moyenne_recouvrement,
                                        ymax = moyenne_recouvrement + erreur_st),
                           position = ggplot2::position_dodge(0.9), width = 0.2)+
    ggplot2::ylab("mean % cover ± SE")+
    ggplot2::theme(axis.title.x = ggplot2::element_blank(), legend.position = "none")+
    ggplot2::ylim(c(0,90))+
    ggplot2::annotate("text", x = 1.7, y = 25, label = "***")+
    ggplot2::annotate("text", x =2.7 , y = 11, label = "***")+
    ggplot2::annotate("text", x = 3, y = 17, label = "***")+
    ggplot2::annotate("text", x = 3.3, y = 27, label = "***")+
    ggplot2::annotate("text", x = 3.70, y = 79, label = "***")
    
  #dir.create("outputs/graphique")
  #dir.create("outputs/graphique/recouvrement_station")
  ggplot2::ggsave("outputs/graphique/recouvrement_station/recouvrement_susbtrat_station.png",
                  plot = ggplot2::last_plot(), dpi = 500)
  
  return(barplot_substrat_station)
  
}

graphique_recouvrement_organismes_corals_station <- function(recouvrement_organismes_in_substrat){
  
  #targets::tar_load(recouvrement_organismes_in_substrat)
  corals <- c("C_B", "C_D", "C_E", "C_F", "C_L", "C_M", "C_SM")
  organismes_corals_station <- recouvrement_organismes_in_substrat |>
    dplyr::group_by(station, type_substrat, organismes_benthiques) |>
    dplyr::summarise(moyenne_recouvrement = mean(recouvrement),
                     sd_recouvrement = sd(recouvrement),
                     erreur_st = plotrix::std.error(recouvrement)) |>
    dplyr::filter(type_substrat == "CV") |>
    dplyr::filter(organismes_benthiques %in% corals)
  
  barplot_organismes_corals_station <- ggplot2::ggplot(organismes_corals_station, 
                                                       ggplot2::aes(y = moyenne_recouvrement, 
                                                        x = organismes_benthiques,
                                                        fill = station ))+
    ggplot2::scale_fill_brewer(palette = "Set1")+
    ggplot2::theme_bw()+
    ggplot2::geom_col(position = "dodge")+
    ggplot2::geom_errorbar(ggplot2::aes(ymin = moyenne_recouvrement,
                                        ymax = moyenne_recouvrement + erreur_st),
                           position = ggplot2::position_dodge(0.9), width = 0.2)+
    ggplot2::ylab("mean % cover ± SE")+
    ggplot2::theme(axis.title.x = ggplot2::element_blank(), 
                   legend.position = "none")
   
  
  ggplot2::ggsave("outputs/graphique/recouvrement_station/recouvrement_organismes_corals_station.png",
                  plot = ggplot2::last_plot(), dpi = 500)
  
  return(barplot_organismes_corals_station)
  
}

graphique_recouvrement_organismes_others_station <- function(recouvrement_organismes){

  #targets::tar_load(recouvrement_organismes)
  others <- c("COR", "CYA", "GA", "INV", "MAC", "NU" )
  organismes_others_station <- recouvrement_organismes |>
    dplyr::group_by(station, organismes_benthiques) |>
    dplyr::summarise(moyenne_recouvrement = mean(recouvrement),
                     sd_recouvrement = sd(recouvrement),
                     erreur_st = plotrix::std.error(recouvrement)) |>
    dplyr::filter(organismes_benthiques %in% others)
  
  barplot_organismes_others_station <- ggplot2::ggplot(organismes_others_station, 
                                                       ggplot2::aes(y = moyenne_recouvrement, 
                                                                    x = organismes_benthiques,
                                                                    fill = station ))+
    ggplot2::scale_fill_brewer(palette = "Set1")+
    ggplot2::theme_bw()+
    ggplot2::geom_col(position = "dodge")+
    ggplot2::geom_errorbar(ggplot2::aes(ymin = moyenne_recouvrement,
                                        ymax = moyenne_recouvrement + erreur_st),
                           position = ggplot2::position_dodge(0.9), width = 0.2)+
    ggplot2::ylab("mean % cover ± SE")+
    ggplot2::theme(axis.title.x = ggplot2::element_blank())+
    ggplot2::annotate("text", x = 2.7, y = 40, label = "***")+
    ggplot2::annotate("text", x =3 , y = 35, label = "***")+
    ggplot2::annotate("text", x = 3.3, y = 54, label = "***")+
    ggplot2::annotate("text", x = 5.7, y = 48, label = "***")+
    ggplot2::annotate("text", x = 6, y = 43, label = "***")+
    ggplot2::annotate("text", x = 6.3, y = 32, label = "***")
  
  ggplot2::ggsave("outputs/graphique/recouvrement_station/recouvrement_organismes_others_station.png",
                  plot = ggplot2::last_plot(), dpi = 500)
  
  return(barplot_organismes_others_station)
  
}

graphique_recouvrement_organismes_others_in_substrat_station <- function(recouvrement_organismes_in_substrat){
  
  #targets::tar_load(recouvrement_organismes_in_substrat)
  others <- c("COR", "CYA", "GA", "INV", "MAC", "NU" )
  organismes_others_in_substrat_station <- recouvrement_organismes_in_substrat |>
    dplyr::group_by(station, type_substrat, organismes_benthiques) |>
    dplyr::summarise(moyenne_recouvrement = mean(recouvrement),
                     sd_recouvrement = sd(recouvrement),
                     erreur_st = plotrix::std.error(recouvrement)) |>
    dplyr::filter(type_substrat != "CV") |>
    dplyr::filter(organismes_benthiques %in% others)
  
  barplot_organismes_others_in_substrat_station <- ggplot2::ggplot(organismes_others_in_substrat_station, 
                                                       ggplot2::aes(y = moyenne_recouvrement, 
                                                                    x = organismes_benthiques,
                                                                    fill = station ))+
    ggplot2::scale_fill_brewer(palette = "Set1")+
    ggplot2::theme_bw()+
    ggplot2::geom_col(position = "dodge")+
    ggplot2::geom_errorbar(ggplot2::aes(ymin = moyenne_recouvrement,
                                        ymax = moyenne_recouvrement + erreur_st),
                           position = ggplot2::position_dodge(0.9), width = 0.2)+
    ggplot2::ylab("% cover ± SE")+
    ggplot2::theme(axis.title.x = ggplot2::element_blank())+
    ggplot2::facet_grid(~type_substrat)+
    ggplot2::theme(panel.spacing = grid::unit(0.3, "cm"))+
    ggplot2::geom_text(data = subset(organismes_others_in_substrat_station, 
                                     type_substrat == "SD"), 
                       ggplot2::aes(x = 6.3, y = 29, label = "***"), size = 3)+
    ggplot2::geom_text(data = subset(organismes_others_in_substrat_station, 
                                     type_substrat == "SD"), 
                       ggplot2::aes(x = 6, y = 37.5, label = "***"),  size = 3)+
    ggplot2::geom_text(data = subset(organismes_others_in_substrat_station, 
                                     type_substrat == "SD"), 
                       ggplot2::aes(x = 5.7, y = 44, label = "***"),  size = 3)+
    ggplot2::geom_segment(x=3.7, xend=3.7, y=5.5, yend=6.5, col="black", 
                          data = subset(organismes_others_in_substrat_station, 
                                        type_substrat == "SD")) + 
    ggplot2::geom_segment(x=4, xend=4, y=5.5, yend=6.5, col="black", 
                          data = subset(organismes_others_in_substrat_station, 
                                        type_substrat == "SD")) + 
    ggplot2::geom_segment(x=3.7, xend=4, y=6.5, yend=6.5, col="black", 
                          data = subset(organismes_others_in_substrat_station, 
                                        type_substrat == "SD"))+
    ggplot2::geom_text(data = subset(organismes_others_in_substrat_station, 
                                     type_substrat == "SD"), 
                       ggplot2::aes(x = 3.85, y = 7, label = "**"),  size = 3)+
    ggplot2::geom_text(data = subset(organismes_others_in_substrat_station, 
                                     type_substrat == "D"), 
                       ggplot2::aes(x = 2.7, y = 6.5, label = "***"),  size = 3)+
    ggplot2::geom_text(data = subset(organismes_others_in_substrat_station, 
                                     type_substrat == "D"), 
                       ggplot2::aes(x = 3, y = 11.5, label = "***"),  size = 3)+
    ggplot2::geom_text(data = subset(organismes_others_in_substrat_station, 
                                     type_substrat == "D"), 
                       ggplot2::aes(x = 3.3, y = 23.5, label = "***"),  size = 3)
  
  ggplot2::ggsave("outputs/graphique/recouvrement_station/recouvrement_organismes_others_in_substrat_station.png",
                  plot = ggplot2::last_plot(), dpi = 500)
  
  return(barplot_organismes_others_in_substrat_station)
  
}

assemblage_graph_station <- function(barplot_substrat_station,
                             barplot_organismes_corals_station, 
                             barplot_organismes_others_station){
  
  final_graph_station <- cowplot::ggdraw()+
    cowplot::draw_plot(barplot_substrat_station, 0, 0.5, 0.5, 0.5)+
    cowplot::draw_plot(barplot_organismes_corals_station, 0.5, 0.5, 0.5, 0.5)+
    cowplot::draw_plot(barplot_organismes_others_station, 0, 0, 1, 0.5)+
    cowplot::draw_plot_label(c("A", "B", "C"), c(0, 0.5, 0), c(1, 1, 0.5), size = 15)
  
  ggplot2::ggsave("outputs/graphique/recouvrement_station/final_graph_station.png",
                  plot = ggplot2::last_plot(), dpi = 500, width = 8, height = 5)
  
  return(final_graph_station) 
  
}
  
