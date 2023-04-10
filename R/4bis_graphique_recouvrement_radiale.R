# barplot of mean % cover ± SE of substrat -------------------------------------

graphique_recouvrement_substrat_radiale <- function(recouvrement_substrat, overwrite = TRUE){
  
  #targets::tar_load(recouvrement_substrat)
  
  barplot_substrat_radiale <- recouvrement_substrat |>
    dplyr::group_by(radiale, type_substrat) |>
    dplyr::summarise(moyenne_recouvrement = mean(recouvrement),   # mean calcul
                     sd_recouvrement = sd(recouvrement),         # SD calcul
                     erreur_st = plotrix::std.error(recouvrement))  # SE calcul
  
  barplot_substrat_radiale$radiale <- forcats::fct_relevel(
    barplot_substrat_radiale$radiale, c("N", "C", "S"))
  
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
    ggplot2::ylab("mean % cover ± SE")+
    ggplot2::theme(axis.title.x = ggplot2::element_blank(), legend.position = "none")+
    ggplot2::annotate("text", x = 0.7, y = 30, label = "***")+
    ggplot2::annotate("text", x =1 , y = 18, label = "*")+
    ggplot2::annotate("text", x = 1.3, y = 15, label = "*")+
    ggplot2::annotate("text", x = 1.7, y = 38.5, label = "***")+
    ggplot2::annotate("text", x = 2, y = 23, label = "***")+
    ggplot2::annotate("text", x = 2.3, y = 32, label = "***")+
    ggplot2::annotate("text", x = 2.7, y = 11.5, label = "***")+
    ggplot2::annotate("text", x = 3.7, y = 55.5, label = "***")+
    ggplot2::annotate("text", x = 4, y = 72.5, label = "**")+
    ggplot2::annotate("text", x = 4.3, y = 66.5, label = "**")

  #dir.create("outputs/graphique")
  #dir.create("outputs/graphique/recouvrement_radiale")
  ggplot2::ggsave("outputs/graphique/recouvrement_radiale/recouvrement_substrat_radiale.png",
                  plot = ggplot2::last_plot(), dpi = 500)
   
  
  return(barplot_substrat_radiale)
  
}

# barplot of mean % cover ± SE of corals morphotypes ---------------------------

graphique_recouvrement_organismes_corals_radiale <- function(recouvrement_organismes_in_substrat, overwrite = TRUE){
  
  #targets::tar_load(recouvrement_organismes_in_substrat)
  
  corals <- c("C_B", "C_D", "C_E", "C_F", "C_L", "C_M", "C_SM")
  
  organismes_corals_radiale <- recouvrement_organismes_in_substrat |>
    dplyr::group_by(radiale, type_substrat, organismes_benthiques) |>
    dplyr::summarise(moyenne_recouvrement = mean(recouvrement),    # mean calcul
                     sd_recouvrement = sd(recouvrement),         # SD calcul
                     erreur_st = plotrix::std.error(recouvrement)) |> #SE calcul
    dplyr::filter(type_substrat == "CV") |>
    dplyr::filter(organismes_benthiques %in% corals)
  
  organismes_corals_radiale$radiale <- forcats::fct_relevel(
    organismes_corals_radiale$radiale, c("N", "C", "S"))
  
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
    ggplot2::ylab("mean % cover ± SE")+
    ggplot2::theme(axis.title.x = ggplot2::element_blank(), 
                   legend.position = "none")+
    ggplot2::annotate("text", x = 0.7, y = 31, label = "***")+
    ggplot2::annotate("text", x =1.7 , y = 12, label = "***")+
    ggplot2::annotate("text", x = 2, y = 18, label = "***")+
    ggplot2::annotate("text", x = 2.3, y = 33, label = "***")+
    ggplot2::annotate("text", x = 5.7, y = 10, label = "***")+
    ggplot2::annotate("text", x = 6, y = 20.5, label = "***")+
    ggplot2::annotate("text", x = 6.3, y = 24.5, label = "***")+
    ggplot2::annotate("text", x = 6.7, y = 8.5, label = "***")+
    ggplot2::annotate("text", x = 7, y = 18.5, label = "***")+
    ggplot2::annotate("text", x = 7.3, y = 27.5, label = "***")
  
  
  
  ggplot2::ggsave("outputs/graphique/recouvrement_radiale/recouvrement_organismes_corals_radiale.png",
                  plot = ggplot2::last_plot(), dpi = 500)
  
  return(barplot_organismes_corals_radiale)
  
}

# barplot of mean % cover ± SE of others benthics sessils organisms ------------

graphique_recouvrement_organismes_others_radiale <- function(recouvrement_organismes, overwrite = TRUE){
  
  #targets::tar_load(recouvrement_organismes)
  
  others <- c("COR", "CYA", "GA", "INV", "MAC", "NU" )
  
  organismes_others_radiale <- recouvrement_organismes |>
    dplyr::group_by(radiale, organismes_benthiques) |>
    dplyr::summarise(moyenne_recouvrement = mean(recouvrement),  # mean calcul
                     sd_recouvrement = sd(recouvrement),        # SD calcul
                     erreur_st = plotrix::std.error(recouvrement)) |> # SE calcul
    dplyr::filter(organismes_benthiques %in% others)
  
  organismes_others_radiale$radiale <- forcats::fct_relevel(
    organismes_others_radiale$radiale, c("N", "C", "S"))
  
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
    ggplot2::ylab("mean % cover ± SE")+
    ggplot2::theme(axis.title.x = ggplot2::element_blank())+
    ggplot2::annotate("text", x = 5.7, y = 32, label = "***")+
    ggplot2::annotate("text", x = 6, y = 47, label = "**")+
    ggplot2::annotate("text", x = 6.3, y = 43, label = "**")
  
  ggplot2::ggsave("outputs/graphique/recouvrement_radiale/recouvrement_organismes_others_radiale.png",
                  plot = ggplot2::last_plot(), dpi = 500)
  
  return(barplot_organismes_others_radiale)
  
}

# barplot of mean % cover ± SE of others benthics 
# sessils organisms in each substrat -------------------------------------------

graphique_recouvrement_organismes_others_in_substrat_radiale <- function(recouvrement_organismes_in_substrat, overwrite = TRUE){
  
  #targets::tar_load(recouvrement_organismes_in_substrat)
  
  others <- c("COR", "CYA", "GA", "INV", "MAC", "NU" )
  
  organismes_others_in_substrat_radiale <- recouvrement_organismes_in_substrat |>
    dplyr::group_by(radiale, type_substrat, organismes_benthiques) |>
    dplyr::summarise(moyenne_recouvrement = mean(recouvrement),    # mean calcul
                     sd_recouvrement = sd(recouvrement),         # SD calcul
                     erreur_st = plotrix::std.error(recouvrement)) |>  # SE calcul
    dplyr::filter(type_substrat != "CV") |>
    dplyr::filter(organismes_benthiques %in% others)
  
  barplot_organismes_others_in_substrat_radiale <- ggplot2::ggplot(organismes_others_in_substrat_radiale, 
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
    
  ggplot2::ggsave("outputs/graphique/recouvrement_radiale/recouvrement_organismes_others_in_substrat_radiale.png",
                  plot = ggplot2::last_plot(), dpi = 500)
  
  return(barplot_organismes_others_in_substrat_radiale)
  
}

# merge of substrat, corals and others benthic organisms barplots --------------

assemblage_graph_radiale <- function(barplot_substrat_radiale,
                       barplot_organismes_corals_radiale, 
                       barplot_organismes_others_radiale, overwrite = T){
  
 final_graph_radiale <- cowplot::ggdraw()+
   cowplot::draw_plot(barplot_substrat_radiale, 0, 0.5, 0.5, 0.5)+
   cowplot::draw_plot(barplot_organismes_corals_radiale, 0.5, 0.5, 0.5, 0.5)+
   cowplot::draw_plot(barplot_organismes_others_radiale, 0, 0, 1, 0.5)+
   cowplot::draw_plot_label(c("A", "B", "C"), c(0, 0.5, 0), c(1, 1, 0.5), size = 15)
  
 ggplot2::ggsave("outputs/graphique/recouvrement_radiale/final_graph_radiale.png",
                 plot = ggplot2::last_plot(), dpi = 500, width = 8, height = 5)
 
 return(final_graph_radiale) 
 
}
