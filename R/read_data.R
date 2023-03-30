# --- SUBSTRAT -----------------------------------------------------------------

# Read the data

read_substrat <- function(){
  
  #dir.create("data/substrat_saline")
  substrat_csv <- read.csv2("data/substrat_saline/substrat_saline_m1.csv", 
                            dec = "," , sep = ";", header = T)
  
  return(substrat_csv)
  
}

# --- REUNION MAP --------------------------------------------------------------

# read the Reunion shapefile

read_map <- function(){
  
  #dir.create("data/reunion_map")
  reunion_map_path <- "data/reunion_map/la_reunion.shp"
  
  shapefile <- sf::st_read(reunion_map_path)
  shapefile_lisse <- sf::st_simplify(shapefile, dTolerance = 0.4)
  sf::st_write(shapefile_lisse, "data/reunion_map/shapefile.shp")
  reunion_map_shp <- sf::st_read(reunion_map_path)
  
  
  return(reunion_map_shp)
  
}

# --- TRANSECT POINTS ----------------------------------------------------------

# read georeferencement transect point file

read_points_transect <- function(){
  
  #dir.create("data/points_transect")
  points_transect_csv <- read.csv2("data/points_transect/TPM1_mars2023.csv")
  
  return(points_transect_csv)
  
}
