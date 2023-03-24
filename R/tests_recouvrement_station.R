test_recouvrement_substrat <- function(recouvrement_substrat){
  
  #targets::tar_load(recouvrement_substrat)
  factor <- levels(recouvrement_substrat$type_substrat)
  test_shapiro <- list()
  test_kruskal <- list()
  tra <- list()
  for(i in factor) {
    
    recouvrement_substrat_factor <- recouvrement_substrat |>
    dplyr::select(station, type_substrat, recouvrement) |>
    dplyr::filter(type_substrat == i)
  
    test_shapiro[[i]] <- tapply(recouvrement_substrat_factor$recouvrement, 
                         recouvrement_substrat_factor$station, shapiro.test)
    
    test_kruskal[[i]] <- kruskal.test(recouvrement_substrat_factor$recouvrement ~ 
                                    recouvrement_substrat_factor$station)
  
    tra[[i]]<-pairwise.wilcox.test(recouvrement_substrat_factor$recouvrement, 
                            recouvrement_substrat_factor$station, 
                            p.adjust.method = "BH") 
  }
  
  print(test_shapiro)
  print(test_kruskal)
  print(tra)
}
