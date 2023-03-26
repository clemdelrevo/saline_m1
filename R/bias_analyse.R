delta_calculs <- function(substrat){
  
  #targets::tar_load(substrat)
  
  count <- substrat |>
    dplyr::group_by(groupe, radiale, station, transect, type_substrat) |>
    dplyr::summarise(count = dplyr::n())
    
  for(i in levels(count$groupe)){
    
    nom_objet <- paste0("count_group_", i)
    assign(nom_objet, count |>
      dplyr::group_by(groupe, radiale, station, type_substrat) |>
      dplyr::summarise(moy = mean(count)) |>
      dplyr::filter(groupe == i)) 
      
  }
  
  groupe <- list(count_group_1 = count_group_1, count_group_2 = count_group_2, 
                 count_group_3 = count_group_3, count_group_4 = count_group_4, 
                 count_group_5 = count_group_5, count_group_6 = count_group_6)
  variable <- c("CM", "CV", "SD", "D")
  
  for(i in names(groupe)){
    
    for(j in variable){
      
      
      name <- paste0(j, "_", i)
      assign(name, groupe[[i]] |>
      dplyr::group_by(radiale, station) |>
      dplyr::filter(type_substrat == j) |>
      dplyr::select(groupe, station, moy))
  
    }
    
  }
  
  diff <- SD_count_group_3$moy - SD_count_group_1$moy
  
  
}
