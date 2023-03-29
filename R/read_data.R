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
  reunion_map_shp <- sf::st_read(reunion_map_path)
  
  reunion_sat_path <- "data/reunion_map/saline.jp2"
  reunion_sat_gdal <- rgdal::readGDAL(reunion_sat_path)
  reunion_sat <- stars::st_as_stars(reunion_sat_gdal)
  reunion_sat2 <- sf::st_as_sf(reunion_sat_gdal)
  dev.new()
  
  ggplot2::ggplot()+
    ggplot2::geom_sf(reunion_sat)
  plot(reunion_sat)
  
  return(reunion_map_shp)
  
}

# --- TRANSECT POINTS ----------------------------------------------------------

# read georeferencement transect point file

read_points_transect <- function(){
  
  #dir.create("data/points_transect")
  points_transect_csv <- read.csv2("data/points_transect/TPM1_mars2023.csv")
  
  return(points_transect_csv)
  
}
