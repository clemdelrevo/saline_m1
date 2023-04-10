# --- Tests de recouvrement de chaque type de substrat 
#entre zones géomorphologiques -------------------------------------------------

test_substrat_station <- function(recouvrement_substrat){

#targets::tar_load(recouvrement_substrat)

  message(cli::rule(line_col = "blue", left = "GLM test permutation substrat"))
  
  fit_substrat <- list()

  for(i in levels(recouvrement_substrat$type_substrat)){
  
    #i = "D"
    filtration <- recouvrement_substrat |>
      dplyr::filter(type_substrat == i)
  
    fit_temp <- glm(formula = recouvrement ~ station, 
                    data = filtration, family = "poisson")
  
    fit_substrat[[i]] <- fit_temp
  
  }

  print(lapply(fit_substrat, car::Anova, test = "F"))
  
  tukey_test <- list()
  
  for(i in names(fit_substrat)){
  
  tukey_model <- multcomp::glht(fit_substrat[[i]], 
                                linfct= multcomp::mcp(station="Tukey"))

  tukey_test[[i]] <- summary(tukey_model)
  
  }

  print(tukey_test)
  
  message(cli::rule(line_col = "blue", left = "enf of analyses"))
  
}

# --- Tests de recouvrement de chaque formes de coraux 
#entre zones géomorphologiques -------------------------------------------------

test_corals_station <- function(recouvrement_organismes_in_substrat){

  #targets::tar_load(recouvrement_organismes_in_substrat)

  message(cli::rule(line_col = "coral", left = "GLM test permutation corals forms"))
  
  factor     <- c("C_B", "C_D", "C_E", "C_F", "C_M", "C_SM")
  fit_corals <- list()
  tukey_test <- list()

  for(i in factor) {
  
    #i = "C_D"
    filtration <- recouvrement_organismes_in_substrat |>
     dplyr::filter(organismes_benthiques == i, type_substrat == "CV")
  
    fit_temp <- glm(formula = recouvrement ~ station, 
                    data = filtration, family = "poisson")
    
    fit_corals[[i]] <- fit_temp
  
  }
  
  print(lapply(fit_corals, car::Anova, test = "F"))

  tukey_test <- list()
  
  for(i in names(fit_corals)){
  
    tukey_model <- multcomp::glht(fit_corals[[i]], 
                                  linfct= multcomp::mcp(station="Tukey"))
  
    tukey_test[[i]] <- summary(tukey_model)

  }  

  print(tukey_test)
  
  message(cli::rule(line_col = "coral", left = "enf of analyses"))
  
}

# --- Tests de recouvrement des autres types d'organismes benthiques 
#entre zones géomorphologiques -------------------------------------------------

test_orga_station <- function(recouvrement_organismes){

  #targets::tar_load(recouvrement_organismes)

  message(cli::rule(line_col = "green", left = "GLM test permutation other benthic organisms"))
  
  fit_orga <- list()

  orga <- c("COR", "GA", "INV", "MAC", "NU")

  for(i in orga) {
  
    #i = "C_D"
    filtration <- recouvrement_organismes |>
      dplyr::filter(organismes_benthiques == i)
  
    fit_temp <- glm(formula = recouvrement ~ station, 
                  data = filtration, family = "poisson")
  
    fit_orga[[i]] <- fit_temp
    
  }
  
  print(lapply(fit_orga, car::Anova, test = "F"))
  
  tukey_test <- list()
  
  for(i in names(fit_orga)){
  
    tukey_model <- multcomp::glht(fit_orga[[i]], 
                                  linfct= multcomp::mcp(station="Tukey"))
  
    tukey_test[[i]] <- summary(tukey_model)
  
  }  

  print(tukey_model)

  message(cli::rule(line_col = "green", left = "end of analyses"))
  
}
