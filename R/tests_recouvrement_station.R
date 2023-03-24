test_recouvrement_substrat <- function(recouvrement_substrat){
  
  #targets::tar_load(recouvrement_substrat)
  
  fit <- list()
  
  for(i in levels(recouvrement_substrat$type_substrat)) {
    fit_subset <- recouvrement_substrat %>%
      dplyr::filter(type_substrat == i)
    
    fit_temp <- glm(recouvrement ~ station, data = fit_subset, family = "poisson")
    
    fit[[i]] <- fit_temp 
    
  }
  
  lapply(fit, Anova, test = "F")
  
  for(i in seq_along(fit)) {
    
  print(summary(multcomp::glht(fit[[i]], linfct= multcomp::mcp(station="Tukey"))))

  
  }  
    
}

test_recouvrement_corals <- function(recouvrement_organismes) {
  
  factor_corals <- c("C_D", "C_B", "C_M", "C_SM")
  test_shapiro <- list()
  test_kruskal <- list()
  tra          <- list()
  
  
  for(i in factor_corals) {
    
    #i = "C_L"
    recouvrement_corals <- recouvrement_organismes |>
      dplyr::filter(organismes_benthiques == i, type_substrat == "CV")
    
    num_groups <- length(unique(recouvrement_corals$station))
    
    if(num_groups > 1) {
    
    test_shapiro[[i]] <- tapply(recouvrement_corals$recouvrement, 
                                recouvrement_corals$station, shapiro.test)
    
    test_kruskal[[i]] <- kruskal.test(recouvrement_corals$recouvrement ~ 
                                        recouvrement_corals$station)
    
    tra[[i]]<-pairwise.wilcox.test(recouvrement_corals$recouvrement, 
                                   recouvrement_corals$station, 
                                   p.adjust.method = "BH") 
    } else {
      
      print(paste("Cannot perform Kruskal-Wallis test for combination of factors", 
                  i,"because there is only 1 group."))
      
    }
    
  }
  
  result <- list(test_shapiro = test_shapiro, test_kruskal = test_kruskal, tra = tra)
  return(result)
  
}
    

