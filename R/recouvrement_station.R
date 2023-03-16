calculs_recouvrement_substrat <- function(substrat){
  
  recouvrement_substrat <- substrat |>
    dplyr::group_by(groupe, radiale, station, transect, type_substrat) |>
    dplyr::summarise(recouvrement = (dplyr::n()/50)*100)
  
  
  recouvrement_substrat <- recouvrement_substrat |>
    dplyr::group_by(station, type_substrat) |>
    dplyr::summarise(moyenne_recouvrement = mean(recouvrement),
                     sd_recouvrement = sd(recouvrement))
  
  return(recouvrement_substrat)
  
}

calculs_recouvrement_organismes <- function(substrat){
  
  recouvrement_organismes <- substrat |>
    dplyr::group_by(groupe, radiale, station, transect, type_substrat,
                    organismes_benthiques) |>
    dplyr::summarise(recouvrement = (dplyr::n()/50)*100) 
  
  recouvrement_organismes <- recouvrement_organismes |>
    dplyr::group_by(station, type_substrat, organismes_benthiques) |>
    dplyr::summarise(moyenne_recouvrement = mean(recouvrement),
                     sd_recouvrement = sd(recouvrement))
  
  return(recouvrement_organismes)
}
