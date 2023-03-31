test_recouvrement_substrat_station <- function(recouvrement_substrat){
  
  #targets::tar_load(recouvrement_substrat)
  
  fit <- list()
  
  for(i in levels(recouvrement_substrat$type_substrat)){
    
    #i = "D"
    substrat_filter <- recouvrement_substrat |>
      dplyr::filter(type_substrat == i)
    
    fit_temp <- glm(substrat_filter$recouvrement ~ substrat_filter$station, 
                    family = "poisson")
    
    fit[[i]] <- fit_temp 
    
  }
  
  return(fit)
  
  print(lapply(fit, pgirmess::PermTest, B=9999))
  
  for(i in seq_along(fit)) {
    
  print(summary(multcomp::glht(fit[[i]], linfct= multcomp::mcp(station="Tukey"))))

  }  
    
}

test_recouvrement_corals_station <- function(recouvrement_organismes_in_substrat) {
  
  #targets::tar_load(recouvrement_organismes_in_substrat)
  fit <- list()
  factor <- c("C_B", "C_D", "C_E", "C_F", "C_L", "C_M", "C_SM")
  
  for(i in factor) {
    
    #i = "C_D"
    recouvrement_corals <- recouvrement_organismes_in_substrat |>
      dplyr::filter(organismes_benthiques == i, type_substrat == "CV")
    
    fit_temp <- glm(recouvrement_corals$recouvrement ~ station, data = recouvrement_corals, family = "poisson")
    
    fit[[i]] <- fit_temp
    
  }  
  
  return(fit)
  
  print(lapply(fit, pgirmess::PermTest, B=9999))
 
  
  for(i in seq_along(fit)) {
    
  print(summary(multcomp::glht(fit[[i]], linfct= multcomp::mcp(station="Tukey"))))
    
  }
  
}

test_recouvrement_others_station <- function(recouvrement_organismes_in_substrat) {
  
  #targets::tar_load(recouvrement_organismes_in_substrat)

  substrat <- c("CM", "D", "SD")
  orga <- c("COR", "GA", "INV", "MAC", "NU")
  fit <- list()
  
  for(i in substrat) {
    
    for(j in orga) {
      
      #i = "SD" ; j = "GA"
      recouvrement_others <- recouvrement_organismes_in_substrat |>
        dplyr::filter(type_substrat == i, organismes_benthiques == j)
      
      fit_temp <- glm(recouvrement_others$recouvrement ~ station, data = recouvrement_others, 
                      family = "poisson")
      
      fit[[i]][[j]] <- fit_temp
      
    }
  }
  
  return(fit)
  
  fit$D <- fit$D[-3]
  
  anov <- list()
  glm_perm <- list()
  
  for(i in names(fit)) {
    
    for(j in names(fit[[i]])) {
    
      glm_perm[[paste(i, j, sep="_")]] <- pgirmess::PermTest(fit[[i]][[j]], B=9999)
      print(glm_perm)
      
      anov[[paste(i, j, sep="_")]] <- car::Anova(fit[[i]][[j]], test = "F")
      #print(anov)
      
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
