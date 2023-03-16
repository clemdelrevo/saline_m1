library(targets)

tar_option_set(format = "qs")

tar_source()

# pipeline

list(
  
  # read data
  tar_target(substrat_csv, read_data())
  # wrangle data
 ,tar_target(substrat, wrangle_substrat(substrat_csv))
  # recouvrement par station
 ,tar_target(recouvrement_substrat, calculs_recouvrement_substrat(substrat))
 ,tar_target(recouvrement_organismes, calculs_recouvrement_organismes(substrat))
  
)
