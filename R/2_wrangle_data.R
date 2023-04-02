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

wrangle_reunion_map <- function(reunion_map_shp, crs){
  
  reunion_map <- sf::st_transform(reunion_map_shp, crs = crs)
  reunion_map <- sf::st_make_valid(reunion_map)
  
  return(reunion_map)
}

wrangle_points_transect <- function(points_transect_csv, crs){
  
  points_transect <- sf::st_as_sf(points_transect_csv, coords = c("Est", "Sud"))
  sf::st_crs(points_transect) <- crs
  points_transect <- sf::st_make_valid(points_transect)
  
  return(points_transect)
  
}
