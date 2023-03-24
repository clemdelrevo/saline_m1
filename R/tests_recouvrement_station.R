test_recouvrement_substrat <- function(recouvrement_substrat){
  
  #targets::tar_load(recouvrement_substrat)
  
  fit <- list()
  
  for(i in levels(recouvrement_substrat$type_substrat)) {
    fit_subset <- recouvrement_substrat |>
      dplyr::filter(type_substrat == i)
    
    fit_temp <- glm(recouvrement ~ station, data = fit_subset, family = "poisson")
    
    fit[[i]] <- fit_temp 
    
  }
  
  print(lapply(fit, car::Anova, test = "F"))
  
  for(i in seq_along(fit)) {
    
  print(summary(multcomp::glht(fit[[i]], linfct= multcomp::mcp(station="Tukey"))))

  
  }  
    
}

test_recouvrement_corals <- function(recouvrement_organismes) {
  
  targets::tar_load(recouvrement_organismes)
  fit <- list()
  factor <- c("C_B", "C_D", "C_M", "C_SM")
  
  for(i in factor) {
    
    #i = "C_D"
    recouvrement_corals <- recouvrement_organismes |>
      dplyr::filter(organismes_benthiques == i, type_substrat == "CV")
    
    fit_temp <- glm(recouvrement ~ station, data = recouvrement_corals, family = "poisson")
    
    fit[[i]] <- fit_temp
    
  }  
  
  print(lapply(fit, car::Anova, test = "F"))
  
  for(i in seq_along(fit)) {
    
  print(summary(multcomp::glht(fit[[i]], linfct= multcomp::mcp(station="Tukey"))))
    
  }
  
}
