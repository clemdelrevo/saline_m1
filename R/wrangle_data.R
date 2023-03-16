wrangle_substrat <- function(substrat_csv){
  
  # transformation des variables en facteur
  substrat$groupe                <- as.factor(substrat$groupe)
  substrat$radiale               <- as.factor(substrat$radiale)
  substrat$station               <- as.factor(substrat$station)
  substrat$transect              <- as.factor(substrat$transect)
  substrat$type_substrat         <- as.factor(substrat$type_substrat)
  substrat$organismes_benthiques <- as.factor(substrat$organismes_benthiques)
  
  # selection de colonnes et retrait des valeurs NA
  substrat <- substrat_csv |>
    dplyr::select(groupe, radiale, station, 
                  transect, type_substrat, organismes_benthiques) |>
    na.omit()
  
  return(substrat)
}

recouvrement_substrat <- function(substrat){
  
  recouvrement_substrat <- substrat |>
    dplyr::group_by(groupe, radiale, station, transect, type_substrat) |>
    dplyr::summarise(recouvrement = (dplyr::n()/50)*100)
  
  
  recouvrement_substrat <- recouvrement_substrat |>
    dplyr::group_by(station, type_substrat) |>
    dplyr::summarise(moyenne_recouvrement = mean(recouvrement),
                     sd_recouvrement = sd(recouvrement))
  
  return(recouvrement_substrat)
  
}

recouvrement_organismes <- function(substrat){
  
  recouvrement_organismes <- substrat |>
    dplyr::group_by(groupe, radiale, station, transect, type_substrat,
                    organismes_benthiques) |>
    dplyr::summarise(recouvrement = (dplyr::n()/50)*100) 
  
}
