# Create R project structure

#renv::init()
#renv::install("usethis") ; renv::snapshot() ; renv::status()
#usethis::use_description(check_name = FALSE)

#dir.create("data")
#dir.create("outputs")
#dir.create("analyses")
#dir.create("R")

#dir.create("data/points_transect")
#dir.create("data/reunion_map")
#dir.create("data/substrat_saline")
#dir.create("outputs/graphique")
#dir.create("outputs/graphique/cartes")
#dir.create("outputs/graphique/recouvrement_station")
#dir.create("outputs/graphique/recouvrement_radiale")

# renv

#renv::install() ; renv::snapshot(prompt = FALSE) ; renv::status()

# devtools

#devtools:: load_all()

# targets

#dir.create("outputs/pipeline/")
targets::tar_config_set(
         store = "outputs/pipeline/",
         script = "analyses/pipeline.R"
)

targets::tar_visnetwork(targets_only = T)
targets::tar_make()
