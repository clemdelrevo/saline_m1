test_recouvrement_substrat <- function(recouvrement_substrat){
  
  recouvrement_substrat_long <- recouvrement_substrat |>
    dplyr::select(station, type_substrat, recouvrement) |>
    tidyr::spread(key = type_substrat, value = recouvrement, -station)
  
  test_shapiro <- tapply(recouvrement_substrat$recouvrement, 
                         recouvrement_substrat$type_substrat, shapiro.test)
  print(test_shapiro)
  
  test_kruskal <- kruskal.test(recouvrement_substrat$recouvrement ~ 
                                 interaction(recouvrement_substrat$type_substrat,
                                             recouvrement_substrat$station))
  
  tra<-pairwise.wilcox.test(recouvrement_substrat$recouvrement, 
                            interaction(recouvrement_substrat$type_substrat,
                                        recouvrement_substrat$station), 
                            p.adjust.method = "BH") 
}
