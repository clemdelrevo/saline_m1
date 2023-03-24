graphique_recouvrement_substrat <- function(recouvrement_substrat){
  
  #targets::tar_load(recouvrement_substrat)
  barplot_substrat_station <- recouvrement_substrat |>
    dplyr::group_by(station, type_substrat) |>
    dplyr::summarise(moyenne_recouvrement = mean(recouvrement),
                     sd_recouvrement = sd(recouvrement),
                     erreur_st = plotrix::std.error(recouvrement))
  
  
  barplot_substrat_station <-ggplot2::ggplot(barplot_substrat_station, ggplot2::aes(y = moyenne_recouvrement, 
                                                      x = type_substrat,
                                                      fill = station))+
    ggplot2::scale_fill_brewer(palette = "Set1")+
    ggplot2::theme_bw()+
    ggplot2::geom_col(position = "dodge")+
    ggplot2::geom_errorbar(ggplot2::aes(ymin = moyenne_recouvrement,
                                        ymax = moyenne_recouvrement + erreur_st),
                           position = ggplot2::position_dodge(0.9), width = 0.2)+
    ggplot2::ylab("% cover ± SE")+
    ggplot2::theme(axis.title.x = ggplot2::element_blank(), legend.position = "none")
  #dir.create("outputs/graphique")
  #dir.create("outputs/graphique/recouvrement_station")
  ggplot2::ggsave("outputs/graphique/recouvrement_station/recouvrement_susbtrat_station.png",
                  plot = ggplot2::last_plot(), dpi = 500)
  
}

graphique_recouvrement_organismes_corals <- function(recouvrement_organismes){
  
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
  
}

graphique_recouvrement_organismes_others <- function(recouvrement_organismes){
  
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
    ggplot2::facet_grid(~ type_substrat)
  
  
  ggplot2::ggsave("outputs/graphique/recouvrement_station/recouvrement_organismes_others_station.png",
                  plot = ggplot2::last_plot(), dpi = 500)
  
}

assemble_organismes <- function(barplot_substrat_station,
                                barplot_organismes_corals_station, 
                                barplot_organismes_others_station ){
  
  grid::grid.newpage() 
  # Créer la mise en page : nrow = 2, ncol = 2
  grid::pushViewport(viewport(layout = grid.layout(2, 2)))
  # Une fonction pour definir une region dans la mise en page
  define_region <- function(row, col){
    grid::viewport(layout.pos.row = row, layout.pos.col = col)
  } 
  # Arranger les graphiques
  print(barplot_substrat_station, vp=define_region(1, 1))
  print(barplot_organismes_corals_station, vp = define_region(1, 2))
  print(barplot_organismes_others_station, vp = define_region(2, 1:2))
}
