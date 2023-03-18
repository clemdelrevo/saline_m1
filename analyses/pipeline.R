library(targets)

tar_option_set(format = "qs")

tar_source()

# pipeline

list(
  
  # crs
  tar_target(crs, "EPSG:4326")
  # read data
  ,tar_target(substrat_csv, read_substrat())
  ,tar_target(reunion_map_shp, read_map())
  ,tar_target(points_transect_csv, read_points_transect())
  # wrangle data
 ,tar_target(substrat, wrangle_substrat(substrat_csv))
 ,tar_target(reunion_map, wrangle_reunion_map(reunion_map_shp, crs))
 ,tar_target(points_transect, wrangle_points_transect(points_transect_csv, crs))
  # recouvrement par station
 ,tar_target(recouvrement_substrat, calculs_recouvrement_substrat(substrat))
 ,tar_target(recouvrement_organismes, calculs_recouvrement_organismes(substrat))
 # graphiques de recouvrement moyen par station
 ,tar_target(barplot_substrat_station, graphique_recouvrement_substrat(recouvrement_substrat))
 ,tar_target(barplot_organismes_station, graphique_recouvrement_organismes(recouvrement_organismes))
 # map echantillonnage
 ,tar_target(map_saline, map_echantillonnage(points_transect, reunion_map))
  
)
