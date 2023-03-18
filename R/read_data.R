# --- SUBSTRAT -----------------------------------------------------------------

# Read the data

read_data <- function(){
  
  #dir.create("data/substrat_saline")
  substrat_csv <- read.csv2("data/substrat_saline/substrat_saline_m1.csv", dec = ",")
  
}


read_map <- function(){
  
  #dir.create("data/reunion_map")
  reunion_map_path <- "data/reunion_map/la_reunion.shp"
  reunion <- sf::st_read(reunion_map_path)
  
}
