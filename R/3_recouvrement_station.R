# calculs of substrat cover ----------------------------------------------------

calculs_recouvrement_substrat <- function(substrat){
  
  #targets::tar_load(substrat)
  recouvrement_substrat <- substrat |>
    dplyr::group_by(groupe, radiale, station, transect, type_substrat) |>
    dplyr::summarise(recouvrement= (dplyr::n()/50)*100)

  return(recouvrement_substrat)
  
}

# calculs of benthic organisms cover -------------------------------------------

calculs_recouvrement_organismes <- function(substrat){
  
  #targets::tar_load(substrat)
  recouvrement_organismes <- substrat |>
    dplyr::group_by(groupe, radiale, station, transect, organismes_benthiques) |>
    dplyr::summarise(recouvrement = (dplyr::n()/50)*100)
  
  return(recouvrement_organismes)
  
}

# calculs of benthic organisms cover in each substrat --------------------------

calculs_recouvrement_organismes_in_substrat <- function(substrat){
  
  #targets::tar_load(substrat)
  recouvrement_organismes_in_substrat <- substrat |>
    dplyr::group_by(groupe, radiale, station, transect, type_substrat,
                    organismes_benthiques) |>
    dplyr::summarise(recouvrement = (dplyr::n()/50)*100) 
  
  return(recouvrement_organismes_in_substrat)
  
}


  
