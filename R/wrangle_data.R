wrangle_substrat <- function(substrat_csv){
  
  # selection de colonnes et retrait des valeurs NA
  substrat <- substrat_csv |>
    dplyr::select(groupe, radiale, station, 
                  transect, type_substrat, organismes_benthiques) |>
    na.omit()
  
  # transformation des variables en facteur
  substrat$groupe                <- as.factor(substrat$groupe)
  substrat$radiale               <- as.factor(substrat$radiale)
  substrat$station               <- as.factor(substrat$station)
  substrat$transect              <- as.factor(substrat$transect)
  substrat$type_substrat         <- as.factor(substrat$type_substrat)
  substrat$organismes_benthiques <- as.factor(substrat$organismes_benthiques)
  
  return(substrat)
}
