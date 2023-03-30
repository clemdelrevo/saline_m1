test_recouvrement_substrat_station <- function(recouvrement_substrat){
  
  #targets::tar_load(recouvrement_substrat)
  
  fit <- list()
  
  for(i in levels(recouvrement_substrat$type_substrat)) {
    fit_subset <- recouvrement_substrat |>
      dplyr::filter(type_substrat == i)
    
    fit_temp <- glm(fit_subset$recouvrement ~ station, data = fit_subset, family = "poisson")
    
    fit[[i]] <- fit_temp 
    
  }
  
  print(lapply(fit, pgirmess::PermTest, B=9999))
  print(lapply(fit, car::Anova, test = "F"))
  
  for(i in seq_along(fit)) {
    
  print(summary(multcomp::glht(fit[[i]], linfct= multcomp::mcp(station="Tukey"))))

  
  }  
    
}

test_recouvrement_corals_station <- function(recouvrement_organismes) {
  
  #targets::tar_load(recouvrement_organismes)
  fit <- list()
  factor <- c("C_B", "C_D", "C_E", "C_F", "C_L", "C_M", "C_SM")
  
  for(i in factor) {
    
    #i = "C_D"
    recouvrement_corals <- recouvrement_organismes |>
      dplyr::filter(organismes_benthiques == i, type_substrat == "CV")
    
    fit_temp <- glm(recouvrement_corals$recouvrement ~ station, data = recouvrement_corals, family = "poisson")
    
    fit[[i]] <- fit_temp
    
  }  
  
  print(lapply(fit, pgirmess::PermTest, B=9999))
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
      recouvrement_others <- recouvrement_organismes |>
        dplyr::filter(type_substrat == i, organismes_benthiques == j)
      
      fit_temp <- glm(recouvrement_others$recouvrement ~ station, data = recouvrement_others, 
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
                                       linfct= multcomp::mcp(station="Tukey"))))
    
    }
    
  }  
        
}


glm_function <- function(){
  
  #targets::tar_load(substrat)
  nb_substrat <- substrat |>
    dplyr::group_by(groupe, radiale, station, transect, type_substrat, organismes_benthiques) |>
    dplyr::summarise(count = dplyr::n()) |>
    dplyr::filter(type_substrat == "CV", organismes_benthiques == "C_M")
  
  glm.D93 <- glm(nb_substrat$count ~ nb_substrat$station + nb_substrat$radiale, family=poisson)
  pgirmess::PermTest(glm.D93, B=9999)
  
  car::Anova(glm.D93, test = "F")
  
}
  
  
