delta_calculs <- function(substrat){
  
  #targets::tar_load(substrat)
  
  count_substrat <- substrat |>
    dplyr::group_by(groupe, radiale, station, transect, type_substrat) |>
    dplyr::summarise(count = dplyr::n())
  
  test_bias <- list()  
    
  for(i in levels(count$type_substrat)){
    
    filtration <- count |>
    dplyr::filter(type_substrat == i)
  
    fit <- glm(count ~ groupe, data = filtration, family = "poisson")
  
    test_bias[[i]] <- fit

  }
  
  print(lapply(test_bias, car::Anova, test = "F"))
  
  tuk.cld <- list()
  
  for(i in seq_along(test_bias)){
  
    mc_tukey <- (multcomp::glht(test_bias[[i]], 
                                 linfct= multcomp::mcp(groupe="Tukey")))
    
    tuk.cld[[i]] <- multcomp::cld(mc_tukey)  
    
    print(tuk.cld)
    
  }
  
  return(count)
  
}
    

boxplot_bias <- function(count_substrat){
  
  bias_substrat <- ggplot2::ggplot(count_substrat, ggplot2::aes(x = type_substrat, 
                                                      y = count, fill = groupe))+
    ggplot2::geom_boxplot(outlier.alpha = 0.5, outlier.size = 0.5)+
    ggplot2::theme_bw()+
    ggplot2::theme(axis.title.x = ggplot2::element_blank())+
    ggplot2::facet_grid(~ station)
  
  #dir.create("outputs/graphique/analyses_biais")
  ggplot2::ggsave("outputs/graphique/analyses_biais/biais_stations.png", 
                  plot = ggplot2::last_plot(),
                 units = "cm", width = 50, height = 20)
  
  bias_total <- ggplot2::ggplot(count, ggplot2::aes(x = type_substrat, 
                                                    y = count, fill = groupe))+
    ggplot2::geom_boxplot(outlier.alpha = 0.5, outlier.size = 0.5)+
    ggplot2::annotate("text", x = 0.69, y = 45, label = "a")+
    ggplot2::annotate("text", x = 0.81, y = 45, label = "b")+
    ggplot2::annotate("text", x = 0.93 , y = 45, label = "bc")+
    ggplot2::annotate("text", x = 1.06, y = 45, label = "b")+
    ggplot2::annotate("text", x = 1.19 , y = 45, label = "c")+
    ggplot2::annotate("text", x = 1.31 , y = 45, label = "d")+
    ggplot2::annotate("text", x = 1.69, y = 43, label = "ab")+
    ggplot2::annotate("text", x = 1.81, y = 43, label = "b")+
    ggplot2::annotate("text", x = 1.93 , y = 43, label = "b")+
    ggplot2::annotate("text", x = 2.06, y = 43, label = "c")+
    ggplot2::annotate("text", x = 2.19 , y = 43, label = "ad")+
    ggplot2::annotate("text", x = 2.31 , y = 43, label = "cd")+
    ggplot2::annotate("text", x = 2.69, y = 30, label = "a")+
    ggplot2::annotate("text", x = 2.81, y = 30, label = "bc")+
    ggplot2::annotate("text", x = 2.94 , y = 30, label = "b")+
    ggplot2::annotate("text", x = 3.06, y = 30, label = "cd")+
    ggplot2::annotate("text", x = 3.19 , y = 30, label = "cd")+
    ggplot2::annotate("text", x = 3.31 , y = 30, label = "d")+
    ggplot2::annotate("text", x = 3.69, y = 51, label = "a")+
    ggplot2::annotate("text", x = 3.81, y = 51, label = "ab")+
    ggplot2::annotate("text", x = 3.93 , y = 51, label = "bc")+
    ggplot2::annotate("text", x = 4.06, y = 51, label = "ac")+
    ggplot2::annotate("text", x = 4.19 , y = 51, label = "d")+
    ggplot2::annotate("text", x = 4.31 , y = 51, label = "b")+
    ggplot2::theme_bw()+
    ggplot2::theme(axis.title.x = ggplot2::element_blank())
  
  ggplot2::ggsave("outputs/graphique/analyses_biais/biai_total.png", 
                  plot = ggplot2::last_plot(), dpi = 500,
                  units = "cm", width = 40, height = 20)
  
  
  corals <- c("C_B", "C_D", "C_E", "C_F", "C_L", "C_M", "C_SM")
  
  count_orga <- substrat |>
    dplyr::group_by(groupe, radiale, station, transect, organismes_benthiques) |>
    dplyr::summarise(count = dplyr::n()) |>
    dplyr::filter(organismes_benthiques %in% corals)
  
  
  bias_orga <- ggplot2::ggplot(count_orga, ggplot2::aes(x = organismes_benthiques, 
                                                      y = count, fill = groupe))+
    ggplot2::geom_boxplot(outlier.alpha = 0.5, outlier.size = 0.5)+
    ggplot2::theme_bw()+
    ggplot2::theme(axis.title.x = ggplot2::element_blank())+
    ggplot2::facet_grid(~ station)
  
  
  
}
