delta_calculs <- function(substrat){
  
  #targets::tar_load(substrat)
  
  count <- substrat |>
    dplyr::group_by(groupe, radiale, station, transect, type_substrat) |>
    dplyr::summarise(count = dplyr::n())
  
  ggplot2::ggplot(count, ggplot2::aes(x = type_substrat, y = count, fill = groupe))+
    ggplot2::geom_boxplot()+
    ggplot2::facet_grid(~ station)
  
  
  ggplot2::ggplot(count, ggplot2::aes(x = type_substrat, y = count, fill = groupe))+
    ggplot2::geom_boxplot()+
    ggplot2::geom_text(data = myletters_df, ggplot2::aes(label = letters, y = 45))
  
  fit_CM <- count |>
    dplyr::filter(type_substrat == "CM")
  
  fit <- glm(count ~ groupe, data = fit_CM, family = "poisson")
  
  car::Anova(fit, test = "F")
  
  mc_tukey <- multcomp::glht(fit, linfct= multcomp::mcp(groupe="Tukey"))
  
  tuk.cld <- multcomp::cld(mc_tukey)
  letters <- tuk.cld$mcletters$Letters
  myletters_df <- data.frame(groupe=levels(fit_CM$groupe),letters=letters)
  myletters_df
    
  for(i in levels(count$groupe)){
    
    nom_objet <- paste0("count_group_", i)
    assign(nom_objet, count |>
      dplyr::filter(groupe == i)) 
      
  }

  
  groupe <- list(count_group_1 = count_group_1, count_group_2 = count_group_2, 
                 count_group_3 = count_group_3, count_group_4 = count_group_4, 
                 count_group_5 = count_group_5, count_group_6 = count_group_6)
  variable <- c("CM", "CV", "SD", "D")
  
  for(i in names(groupe)){
    
    for(j in variable){
      
      
      name <- paste0(j, "_", i)
      assign(name, groupe[[i]] |>
      dplyr::filter(type_substrat == j) |>
      dplyr::group_by(radiale) |>
      dplyr::summarise(moy = mean(count)))
  
    }
    
  }
  
  diff_SD <- SD_count_group_3$moy - SD_count_group_1$moy
  diff_CM <- CM_count_group_3$moy - CM_count_group_1$moy
  diff_CV <- CV_count_group_3$moy - CV_count_group_1$moy
  diff_D <- D_count_group_3$moy - D_count_group_1$moy
  radiale <- rep(c("C", "N", "S"), 4)
  substrat <- c(rep("CM", 3), rep("CV", 3), rep("D", 3), rep("SD", 3))
  data <- data.frame(cbind(radiale, substrat, rbind(diff_CM, diff_CV, diff_D, diff_SD)))
  
  
  
  
}
