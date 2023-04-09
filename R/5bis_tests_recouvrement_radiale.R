# --- Tests de recouvrement de chaque type de substrat 
#entre radiales ----------------------------------------------------------------

test_substrat_radiale <- function(recouvrement_substrat){
  
  #targets::tar_load(recouvrement_substrat)
  
  fit_substrat <- list()
  
  for(i in levels(recouvrement_substrat$type_substrat)){
    
    #i = "D"
    filtration <- recouvrement_substrat |>
      dplyr::filter(type_substrat == i)
    
    fit_temp <- glm(formula = recouvrement ~ radiale, 
                    data = filtration, family = "poisson")
    
    fit_substrat[[i]] <- fit_temp
    
  }
  
  print(lapply(fit_substrat, car::Anova, test = "F"))
  
  tukey_test <- list()
  
  for(i in names(fit_substrat)){
    
    tukey_model <- multcomp::glht(fit_substrat[[i]], 
                                  linfct= multcomp::mcp(radiale="Tukey"))
    
    tukey_test[[i]] <- summary(tukey_model)
    
  }
  
  print(tukey_test)
  
}

# --- Tests de recouvrement de chaque formes de coraux 
#entre radiales ----------------------------------------------------------------

test_corals_radiale <- function(recouvrement_organismes_in_substrat){
  
  #targets::tar_load(recouvrement_organismes_in_substrat)
  
  factor     <- c("C_B", "C_D", "C_E", "C_F", "C_M", "C_SM")
  fit_corals <- list()
  tukey_test <- list()
  
  for(i in factor) {
    
    #i = "C_D"
    filtration <- recouvrement_organismes_in_substrat |>
      dplyr::filter(organismes_benthiques == i, type_substrat == "CV")
    
    fit_temp <- glm(formula = recouvrement ~ radiale, 
                    data = filtration, family = "poisson")
    
    fit_corals[[i]] <- fit_temp
    
  }
  
  print(lapply(fit_corals, car::Anova, test = "F"))
  
  tukey_test <- list()
  
  for(i in names(fit_corals)){
    
    tukey_model <- multcomp::glht(fit_corals[[i]], 
                                  linfct= multcomp::mcp(radiale="Tukey"))
    
    tukey_test[[i]] <- summary(tukey_model)
    
  }  
  
  print(tukey_test)
  
}

# --- Tests de recouvrement des autres types d'organismes benthiques 
#entre radiales ----------------------------------------------------------------

test_orga_radiale <- function(recouvrement_organismes){
  
  #targets::tar_load(recouvrement_organismes)
  
  fit_orga <- list()
  
  orga <- c("COR", "GA", "INV", "MAC", "NU")
  
  for(i in orga) {
    
    #i = "C_D"
    filtration <- recouvrement_organismes |>
      dplyr::filter(organismes_benthiques == i)
    
    fit_temp <- glm(formula = recouvrement ~ radiale, 
                    data = filtration, family = "poisson")
    
    fit_orga[[i]] <- fit_temp
    
  }
  
  print(lapply(fit_orga, car::Anova, test = "F"))
  
  tukey_test <- list()
  
  for(i in names(fit_orga)){
    
    tukey_model <- multcomp::glht(fit_orga[[i]], 
                                  linfct= multcomp::mcp(radiale="Tukey"))
    
    tukey_test[[i]] <- summary(tukey_model)
    
  }  
  
  print(tukey_model)
  
}
