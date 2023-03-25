graphique_recouvrement_substrat_radiale <- function(recouvrement_substrat){
  
  #targets::tar_load(recouvrement_substrat)
  barplot_substrat_radiale <- recouvrement_substrat |>
    dplyr::group_by(radiale, type_substrat) |>
    dplyr::summarise(moyenne_recouvrement = mean(recouvrement),
                     sd_recouvrement = sd(recouvrement),
                     erreur_st = plotrix::std.error(recouvrement))
  
  
  barplot_substrat_radiale <- ggplot2::ggplot(barplot_substrat_radiale, 
                                              ggplot2::aes(y = moyenne_recouvrement, 
                                                           x = type_substrat,
                                                           fill = radiale))+
    ggplot2::scale_fill_brewer(palette = "Set2")+
    ggplot2::theme_bw()+
    ggplot2::geom_col(position = "dodge")+
    ggplot2::geom_errorbar(ggplot2::aes(ymin = moyenne_recouvrement,
                                        ymax = moyenne_recouvrement + erreur_st),
                           position = ggplot2::position_dodge(0.9), width = 0.2)+
    ggplot2::ylab("% cover ± SE")+
    ggplot2::theme(axis.title.x = ggplot2::element_blank(), legend.position = "none")

  #dir.create("outputs/graphique")
  #dir.create("outputs/graphique/recouvrement_radiale")
  ggplot2::ggsave("outputs/graphique/recouvrement_radiale/recouvrement_substrat_radiale.png",
                  plot = ggplot2::last_plot(), dpi = 500)
  
  return(barplot_substrat_radiale)
  
}

graphique_recouvrement_organismes_corals_radiale <- function(recouvrement_organismes){
  
  #targets::tar_load(recouvrement_organismes)
  corals <- c("C_B", "C_D", "C_E", "C_F", "C_L", "C_M", "C_SM")
  organismes_corals_radiale <- recouvrement_organismes |>
    dplyr::group_by(radiale, type_substrat, organismes_benthiques) |>
    dplyr::summarise(moyenne_recouvrement = mean(recouvrement),
                     sd_recouvrement = sd(recouvrement),
                     erreur_st = plotrix::std.error(recouvrement)) |>
    dplyr::filter(type_substrat == "CV") |>
    dplyr::filter(organismes_benthiques %in% corals)
  
  barplot_organismes_corals_radiale <- ggplot2::ggplot(organismes_corals_radiale, 
                                                       ggplot2::aes(y = moyenne_recouvrement, 
                                                                    x = organismes_benthiques,
                                                                    fill = radiale ))+
    ggplot2::scale_fill_brewer(palette = "Set2")+
    ggplot2::theme_bw()+
    ggplot2::geom_col(position = "dodge")+
    ggplot2::geom_errorbar(ggplot2::aes(ymin = moyenne_recouvrement,
                                        ymax = moyenne_recouvrement + erreur_st),
                           position = ggplot2::position_dodge(0.9), width = 0.2)+
    ggplot2::ylab("% cover ± SE")+
    ggplot2::theme(axis.title.x = ggplot2::element_blank(), 
                   legend.position = "none")
  
  
  ggplot2::ggsave("outputs/graphique/recouvrement_radiale/recouvrement_organismes_corals_radiale.png",
                  plot = ggplot2::last_plot(), dpi = 500)
  
  return(barplot_organismes_corals_radiale)
  
}

graphique_recouvrement_organismes_others_radiale <- function(recouvrement_organismes){
  
  #targets::tar_load(recouvrement_organismes)
  others <- c("COR", "CYA", "GA", "INV", "MAC", "NU" )
  organismes_others_radiale <- recouvrement_organismes |>
    dplyr::group_by(radiale, type_substrat, organismes_benthiques) |>
    dplyr::summarise(moyenne_recouvrement = mean(recouvrement),
                     sd_recouvrement = sd(recouvrement),
                     erreur_st = plotrix::std.error(recouvrement)) |>
    dplyr::filter(type_substrat != "CV") |>
    dplyr::filter(organismes_benthiques %in% others)
  
  barplot_organismes_others_radiale <- ggplot2::ggplot(organismes_others_radiale, 
                                                       ggplot2::aes(y = moyenne_recouvrement, 
                                                                    x = organismes_benthiques,
                                                                    fill = radiale ))+
    ggplot2::scale_fill_brewer(palette = "Set2")+
    ggplot2::theme_bw()+
    ggplot2::geom_col(position = "dodge")+
    ggplot2::geom_errorbar(ggplot2::aes(ymin = moyenne_recouvrement,
                                        ymax = moyenne_recouvrement + erreur_st),
                           position = ggplot2::position_dodge(0.9), width = 0.2)+
    ggplot2::ylab("% cover ± SE")+
    ggplot2::theme(axis.title.x = ggplot2::element_blank())+
    ggplot2::facet_wrap(~ type_substrat)+
    ggplot2::theme(panel.spacing = grid::unit(0.3, "cm"))
    
  ggplot2::ggsave("outputs/graphique/recouvrement_radiale/recouvrement_organismes_others_radiale.png",
                  plot = ggplot2::last_plot(), dpi = 500)
  
  return(barplot_organismes_others_radiale)
  
}

graphique_assemble_radiale <- function(barplot_substrat_radiale,
                                          barplot_organismes_corals_radiale, 
                                          barplot_organismes_others_radiale){
  
  #targets::tar_load(barplot_substrat_radiale)
  #targets::tar_load(barplot_organismes_corals_radiale)
  #targets::tar_load(barplot_organismes_others_radiale)
  png(filename = "outputs/graphique/recouvrement_radiale/assemble_graphique_organismes_radiale.png"
      , width = 1080, height = 720, res = 100)
  grid::grid.newpage() 
  # Créer la mise en page : nrow = 2, ncol = 2
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(2, 2)))
  # Une fonction pour definir une region dans la mise en page
  define_region <- function(row, col){
    grid::viewport(layout.pos.row = row, layout.pos.col = col)
  } 
  # Arranger les graphiques
  print(barplot_substrat_radiale, vp=define_region(1, 1))
  print(barplot_organismes_corals_radiale, vp = define_region(1, 2))
  print(barplot_organismes_others_radiale, vp = define_region(2, 1:2))
  
  dev.off()
}
