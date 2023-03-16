graphique_recouvrement_substrat <- function(recouvrement_substrat){
  
  barplot_substrat_station <- recouvrement_substrat |>
    dplyr::group_by(station, type_substrat) |>
    dplyr::summarise(moyenne_recouvrement = mean(recouvrement),
                     sd_recouvrement = sd(recouvrement))
  
  barplot_substrat_station <-ggplot2::ggplot(barplot_substrat_station, ggplot2::aes(y = moyenne_recouvrement, 
                                                      x = type_substrat,
                                                      fill = station))+
    ggplot2::geom_col(position = "dodge")+
    ggplot2::geom_errorbar(ggplot2::aes(ymin = moyenne_recouvrement,
                                        ymax = moyenne_recouvrement + sd_recouvrement),
                           position = ggplot2::position_dodge(0.9), width = 0.2)
  
  return(barplot_substrat_station)
  
}

graphique_recouvrement_organismes <- function(recouvrement_organismes){
  
  barplot_organismes_station <- recouvrement_organismes |>
    dplyr::group_by(station, organismes_benthiques) |>
    dplyr::summarise(moyenne_recouvrement = mean(recouvrement),
                     sd_recouvrement = sd(recouvrement))
  
  barplot_organismes_station <- ggplot2::ggplot(barplot_organismes_station, ggplot2::aes(y = moyenne_recouvrement, 
                                                        x = organismes_benthiques,
                                                        fill = station ))+
    ggplot2::geom_col(position = "dodge")+
    ggplot2::geom_errorbar(ggplot2::aes(ymin = moyenne_recouvrement,
                                        ymax = moyenne_recouvrement + sd_recouvrement),
                           position = ggplot2::position_dodge(0.9), width = 0.2)
  
  return(barplot_organismes_station)
  
}

graphique_recouvrement_coraux <- function(recouvrement_organismes){
  
  violin_recouvrement_coraux <- recouvrement_organismes |>
    dplyr::filter(type_substrat == "CV") |>
    dplyr::filter(organismes_benthiques %in% c("C_B", "C_D", 
                                           "C_F", "C_M", "C_SM"))
    
  violin_recouvrement_coraux <- ggplot2::ggplot(violin_recouvrement_coraux,
                                                ggplot2::aes(x = organismes_benthiques,
                                                    y = recouvrement,
                                                    fill = station))+
    ggplot2::geom_violin()
  
}
