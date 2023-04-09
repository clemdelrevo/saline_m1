# --- Tests de recouvrement de chaque type de substrat 
#entre radiale -----------------------------------------------------------------
  
  #targets::tar_load(recouvrement_substrat)
  
  fit_substrat <- list()
  perm_test    <- list()
  tukey_test   <- list()
  
  for(i in levels(recouvrement_substrat$type_substrat)){
    
    #i = "D"
    filter_data <- recouvrement_substrat |>
      dplyr::filter(type_substrat == i)
    
    fit_temp <- glm(formula = filter_data$recouvrement ~ radiale, family = "poisson",
                    data = filter_data)
    fit_substrat[[i]] <- fit_temp
    
    perm <- pgirmess::PermTest(fit_substrat[[i]],  B=9999)
    perm_test[[i]] <- perm
    
    tukey_model <- multcomp::glht(fit_substrat[[i]], linfct= multcomp::mcp(radiale="Tukey"))
    tukey_test[[i]] <- summary(tukey_model)
    
    resultats_substrat <- list(perm_test, tukey_test)
    
  }
  
  print(resultats_substrat)
  
# --- Tests de recouvrement de chaque formes de coraux 
#entre radiale -----------------------------------------------------------------
  
  #targets::tar_load(recouvrement_organismes_in_substrat)
  
  factor     <- c("C_B", "C_D", "C_E", "C_F", "C_M", "C_SM")
  fit_corals <- list()
  perm_test  <- list()
  tukey_test <- list()
  
  for(i in factor) {
    
    #i = "C_D"
    filter_data <- recouvrement_organismes_in_substrat |>
      dplyr::filter(organismes_benthiques == i, type_substrat == "CV")
    
    fit_temp <- glm(formula = filter_data$recouvrement ~ radiale, family = "poisson",
                    data = filter_data)
    fit_corals[[i]] <- fit_temp
    
    perm <- pgirmess::PermTest(fit_corals[[i]],  B=9999)
    perm_test[[i]] <- perm
    
    tukey_model <- multcomp::glht(fit_corals[[i]], linfct= multcomp::mcp(radiale="Tukey"))
    tukey_test[[i]] <- summary(tukey_model)
    
    resultats_corals <- list(perm_test, tukey_test)
    
  }  
  
  print(resultats_corals)
  
# --- Tests de recouvrement des autres types d'organismes benthiques 
#entre radiale --------------------------------------------------

  #targets::tar_load(recouvrement_organismes)
  fit_orga <- list()
  perm_test  <- list()
  tukey_test <- list()
  orga <- c("COR", "GA", "INV", "MAC", "NU")
  
  for(i in orga) {
    
    #i = "C_D"
    filter_data <- recouvrement_organismes |>
      dplyr::filter(organismes_benthiques == i)
    
    fit_temp <- glm(formula = filter_data$recouvrement ~ radiale, family = "poisson",
                    data = filter_data)
    fit_orga[[i]] <- fit_temp
    
    perm <- pgirmess::PermTest(fit_orga[[i]],  B=9999)
    perm_test[[i]] <- perm
    
    tukey_model <- multcomp::glht(fit_orga[[i]], linfct= multcomp::mcp(radiale="Tukey"))
    tukey_test[[i]] <- summary(tukey_model)
    
    resultats_orga <- list(perm_test, tukey_test)
    
  }  
  
  print(resultats_orga)


# --- Tests de recouvrement des autres types d'organismes benthiques 
#au sein de chaque type de substrat entre radiale ------------------------------
  
  #targets::tar_load(recouvrement_organismes_in_substrat)
  
  substrat <- c("CM", "D", "SD")
  orga <- c("COR", "GA", "INV", "MAC", "NU")
  fit <- list()
  
  for(i in substrat) {
    
    for(j in orga) {
      
      #i = "SD" ; j = "GA"
      recouvrement_others_in_substrat <- recouvrement_organismes_in_substrat |>
        dplyr::filter(type_substrat == i, organismes_benthiques == j)
      
      fit_temp <- glm(recouvrement_others_in_substrat$recouvrement ~ radiale, 
                      data = recouvrement_others_in_substrat, 
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
