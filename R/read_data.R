# --- SUBSTRAT -----------------------------------------------------------------

# Read the data

read_substrat <- function(){
  
  #dir.create("data/substrat_saline")
  substrat_csv <- read.csv2("data/substrat_saline/substrat_saline_m1.csv", dec = ",")
  
  return(substrat_csv)
  
}

read_map <- function(){
  
  #dir.create("data/reunion_map")
  reunion_map_path <- "data/reunion_map/la_reunion.shp"
  reunion_map_shp <- sf::st_read(reunion_map_path)
  
  return(reunion_map_shp)
  
}

read_points_transect <- function(){
  
  #dir.create("data/points_transect")
  points_transect_csv <- read.csv2("data/points_transect/TPM1_mars2023.csv")
  
  return(points_transect_csv)
  
}
