test_recouvrement_substrat_station <- function(recouvrement_substrat){
  
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

test_recouvrement_corals_station <- function(recouvrement_organismes) {
  
  #targets::tar_load(recouvrement_organismes)
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

test_recouvrement_others_station <- function(recouvrement_organismes) {
  
  #targets::tar_load(recouvrement_organismes)

  substrat <- c("CM", "D", "SD")
  orga <- c("COR", "GA", "INV", "MAC", "NU")
  fit <- list()
  
  for(i in substrat) {
    
    for(j in orga) {
      
      #i = "SD" ; j = "GA"
      test_recouvrement_organismes <- recouvrement_organismes |>
        dplyr::filter(type_substrat == i, organismes_benthiques == j)
      
      fit_temp <- glm(recouvrement ~ station, data = test_recouvrement_organismes, 
                      family = "poisson")
      
      fit[[i]][[j]] <- fit_temp
      
    }
  }
  
  fit$D <- fit$D[-3]
  
  anov <- list()
  
  for(i in names(fit)) {
    
    for(j in names(fit[[i]])) {
    
      anov[[paste(i, j, sep="_")]] <- car::Anova(fit[[i]][[j]], test = "F")
      print(anov)
      
    }
    
  }

  pairwise <- list()
  
  for(i in names(fit)) {
    
    for(j in names(fit[[i]])) {
    
    #i = "D"
    print(pairwise[paste(i, j, sep = "_")] <- summary(multcomp::glht(fit[[i]][[j]], 
                                       linfct= multcomp::mcp(station="Tukey"))))
    
    }
    
  }  
        
}
