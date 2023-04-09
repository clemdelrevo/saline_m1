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
 ,tar_target(recouvrement_organismes_in_substrat, calculs_recouvrement_organismes_in_substrat(substrat))

 # graphiques de recouvrement moyen par station
 ,tar_target(barplot_substrat_station, graphique_recouvrement_substrat_station(recouvrement_substrat))
 ,tar_target(barplot_organismes_corals_station, 
             graphique_recouvrement_organismes_corals_station(recouvrement_organismes_in_substrat))
 ,tar_target(barplot_organismes_others_station,
             graphique_recouvrement_organismes_others_station(recouvrement_organismes))
 ,tar_target(final_graph_station, 
             assemblage_graph_station(barplot_substrat_station,
                                           barplot_organismes_corals_station, 
                                           barplot_organismes_others_station))
 
 # graphiques de recouvrement moyen par radiale
 ,tar_target(barplot_substrat_radiale, graphique_recouvrement_substrat_radiale(recouvrement_substrat))
 ,tar_target(barplot_organismes_corals_radiale, 
             graphique_recouvrement_organismes_corals_radiale(recouvrement_organismes_in_substrat))
 ,tar_target(barplot_organismes_others_radiale,
             graphique_recouvrement_organismes_others_radiale(recouvrement_organismes))
 ,tar_target(final_graph_radiale, 
             assemblage_graph_radiale(barplot_substrat_radiale,
                                           barplot_organismes_corals_radiale, 
                                           barplot_organismes_others_radiale))
 
 # analyse du biais
 ,tar_target(calculs_bias, delta_calculs(substrat))
 ,tar_target(graphiques_bias, boxplot_bias(calculs_bias))
 # map echantillonnage
 ,tar_target(map, map_echantillonnage(points_transect, reunion_map))
 
)
