test_recouvrement_substrat_radiale <- function(recouvrement_substrat){
  
  #targets::tar_load(recouvrement_substrat)
  
  fit_substrat <- list()
  perm_test    <- list()
  tukey_test   <- list()
  
  for(i in levels(recouvrement_substrat$type_substrat)){
    
    #i = "D"
    filter <- recouvrement_substrat |>
      dplyr::filter(type_substrat == i)
    
    fit_temp <- glm(formula = filter$recouvrement ~ radiale, family = "poisson",
                    data = filter)
    fit_substrat[[i]] <- fit_temp
    
    perm <- pgirmess::PermTest(fit_substrat[[i]],  B=9999)
    perm_test[[i]] <- perm
    
    tukey_model <- multcomp::glht(fit_substrat[[i]], linfct= multcomp::mcp(radiale="Tukey"))
    tukey_test[[i]] <- summary(tukey_model)
    
    resultats_substrat <- list(perm_test, tukey_test)
    
  }
  
  return(resultats_substrat)
  
}

test_recouvrement_corals_radiale <- function(recouvrement_organismes_in_substrat) {
  
  #targets::tar_load(recouvrement_organismes_in_substrat)
  
  factor     <- c("C_B", "C_D", "C_E", "C_F", "C_M", "C_SM")
  fit_corals <- list()
  perm_test  <- list()
  tukey_test <- list()
  
  for(i in factor) {
    
    #i = "C_D"
    filter <- recouvrement_organismes_in_substrat |>
      dplyr::filter(organismes_benthiques == i, type_substrat == "CV")
    
    fit_temp <- glm(formula = filter$recouvrement ~ radiale, family = "poisson",
                    data = filter)
    fit_corals[[i]] <- fit_temp
    
    perm <- pgirmess::PermTest(fit_corals[[i]],  B=9999)
    perm_test[[i]] <- perm
    
    tukey_model <- multcomp::glht(fit_corals[[i]], linfct= multcomp::mcp(radiale="Tukey"))
    tukey_test[[i]] <- summary(tukey_model)
    
    resultats_corals <- list(perm_test, tukey_test)
    
  }  
  
  return(resultats_corals)
  
}

test_recouvrement_others_radiale <- function(recouvrement_organismes) {
  
  #targets::tar_load(recouvrement_organismes)
  fit <- list()
  orga <- c("COR", "GA", "INV", "MAC", "NU")
  
  for(i in orga) {
    
    #i = "C_D"
    recouvrement_others <- recouvrement_organismes |>
      dplyr::filter(organismes_benthiques == i)
    
    fit_temp <- glm(recouvrement_others$recouvrement ~ radiale, data = recouvrement_others, family = "poisson")
    
    fit[[i]] <- fit_temp
    
  }  
  
  print(lapply(fit, pgirmess::PermTest, B=9999))
  
  
  for(i in seq_along(fit)) {
    
    print(summary(multcomp::glht(fit[[i]], linfct= multcomp::mcp(radiale="Tukey"))))
    
  }
  
}

test_recouvrement_others_in_substrat_radiale <- function(recouvrement_organismes_in_substrat) {
  
  #targets::tar_load(recouvrement_organismes_in_substrat)
  
  substrat <- c("CM", "D", "SD")
  orga <- c("COR", "GA", "INV", "MAC", "NU")
  fit <- list()
  
  for(i in substrat) {
    
    for(j in orga) {
      
      #i = "SD" ; j = "GA"
      recouvrement_others_in_substrat <- recouvrement_organismes_in_substrat |>
        dplyr::filter(type_substrat == i, organismes_benthiques == j)
      
      fit_temp <- glm(recouvrement_others_in_substrat$recouvrement ~ radiale, data = recouvrement_others_in_substrat, 
                      family = "poisson")
      
      fit[[i]][[j]] <- fit_temp
      
    }
  }
  
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
                                                                       linfct= multcomp::mcp(radiale="Tukey"))))
      
    }
    
  }  
  
}


recouvrement_in_radiale <- function(){
  
  
  recouvrement <- recouvrement_organismes_in_substrat |>
    dplyr::filter(radiale == "DAR", organismes_benthiques %in% c("COR", "GA", "INV", "MAC", "NU"),
                  type_substrat == "CM")
  
  fit_temp <- glm(recouvrement ~ organismes_benthiques, 
                  data = recouvrement, family = "poisson")
  
  pgirmess::PermTest(fit_temp, B = 9999)
  
  print(summary(multcomp::glht(fit_temp, linfct= multcomp::mcp(organismes_benthiques="Tukey"))))
  
  
}
