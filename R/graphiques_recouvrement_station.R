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
    ggplot2::ylab("% cover ± SE")+
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

graphique_recouvrement_organismes_corals_station <- function(recouvrement_organismes){
  
  #targets::tar_load(recouvrement_organismes)
  corals <- c("C_B", "C_D", "C_E", "C_F", "C_L", "C_M", "C_SM")
  organismes_corals_station <- recouvrement_organismes |>
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
    ggplot2::ylab("% cover ± SE")+
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
    dplyr::group_by(station, type_substrat, organismes_benthiques) |>
    dplyr::summarise(moyenne_recouvrement = mean(recouvrement),
                     sd_recouvrement = sd(recouvrement),
                     erreur_st = plotrix::std.error(recouvrement)) |>
    dplyr::filter(type_substrat != "CV") |>
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
    ggplot2::ylab("% cover ± SE")+
    ggplot2::theme(axis.title.x = ggplot2::element_blank())+
    ggplot2::facet_wrap(~ type_substrat)+
    ggplot2::theme(panel.spacing = grid::unit(0.3, "cm"))+
    ggplot2::geom_text(data = subset(organismes_others_station, 
                                     type_substrat == "SD"), 
                       ggplot2::aes(x = 6.3, y = 29, label = "***"), size = 3)+
    ggplot2::geom_text(data = subset(organismes_others_station, 
                                     type_substrat == "SD"), 
                       ggplot2::aes(x = 6, y = 37.5, label = "***"),  size = 3)+
    ggplot2::geom_text(data = subset(organismes_others_station, 
                                     type_substrat == "SD"), 
                       ggplot2::aes(x = 5.7, y = 44, label = "***"),  size = 3)+
    ggplot2::geom_segment(x=3.7, xend=3.7, y=5.5, yend=6.5, col="black", 
                          data = subset(organismes_others_station, 
                                        type_substrat == "SD")) + 
    ggplot2::geom_segment(x=4, xend=4, y=5.5, yend=6.5, col="black", 
                          data = subset(organismes_others_station, 
                                        type_substrat == "SD")) + 
    ggplot2::geom_segment(x=3.7, xend=4, y=6.5, yend=6.5, col="black", 
                          data = subset(organismes_others_station, 
                                        type_substrat == "SD"))+
    ggplot2::geom_text(data = subset(organismes_others_station, 
                                     type_substrat == "SD"), 
                       ggplot2::aes(x = 3.85, y = 7, label = "**"),  size = 3)+
    ggplot2::geom_text(data = subset(organismes_others_station, 
                                     type_substrat == "D"), 
                       ggplot2::aes(x = 2.7, y = 6.5, label = "***"),  size = 3)+
    ggplot2::geom_text(data = subset(organismes_others_station, 
                                     type_substrat == "D"), 
                       ggplot2::aes(x = 3, y = 11.5, label = "***"),  size = 3)+
    ggplot2::geom_text(data = subset(organismes_others_station, 
                                     type_substrat == "D"), 
                       ggplot2::aes(x = 3.3, y = 23.5, label = "***"),  size = 3)
  
  ggplot2::ggsave("outputs/graphique/recouvrement_station/recouvrement_organismes_others_station.png",
                  plot = ggplot2::last_plot(), dpi = 500)
  
  return(barplot_organismes_others_station)
  
}

graphique_assemble_station <- function(barplot_substrat_station,
                                barplot_organismes_corals_station, 
                                barplot_organismes_others_station){
  
    #targets::tar_load(barplot_substrat_station)
    #targets::tar_load(barplot_organismes_corals_station)
    #targets::tar_load(barplot_organismes_others_station)
    png(filename = "outputs/graphique/recouvrement_station/assemble_graphique_organismes_station.png"
        , width = 1080, height = 720, res = 100)
    grid::grid.newpage() 
    # Créer la mise en page : nrow = 2, ncol = 2
    grid::pushViewport(grid::viewport(layout = grid::grid.layout(2, 2)))
    # Une fonction pour definir une region dans la mise en page
    define_region <- function(row, col){
      grid::viewport(layout.pos.row = row, layout.pos.col = col)
    } 
    # Arranger les graphiques
      print(barplot_substrat_station, vp=define_region(1, 1))
      print(barplot_organismes_corals_station, vp = define_region(1, 2))
      print(barplot_organismes_others_station, vp = define_region(2, 1:2))
  
      dev.off()
}
