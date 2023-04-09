PERMANOVA <- function(substrat){
  
  #targets::tar_load(substrat)
  
  recouvrement_organismes <- substrat |>
    dplyr::group_by(groupe, radiale, station, transect, organismes_benthiques) |>
    dplyr::summarise(recouvrement = (dplyr::n()/50)*100) 
  
  
   recouvrement_organismes_wild <- recouvrement_organismes |>
    tidyr::spread(organismes_benthiques, recouvrement)
  
  recouvrement_organismes_wild[is.na(recouvrement_organismes_wild)] <- 0
  
  dist_matrix <- dist(recouvrement_organismes_wild[ , 5:17], method = "euclidean")
  
  permanova <- vegan::adonis(formula = dist_matrix ~ station * radiale, 
                             data = recouvrement_organismes_wild, permutations = 9999)
  
  summary(permanova)
  
  permanova$aov.tab
  
  table_station <- table(substrat$station, substrat$organismes_benthiques)
  
  chisq.test(table_station)
  
  table_radiale <- table(substrat$radiale, substrat$organismes_benthiques)
  
  chisq.test(table_radiale)

}
  
