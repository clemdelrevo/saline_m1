AFC <- function(){
  
  substrat <- substrat_csv |>
    dplyr::group_by(groupe, radiale, station, transect, type_substrat) |>
    dplyr::summarise(count = dplyr::n())
  
  substrat <- substrat |>
    tidyr::spread(type_substrat, count)
  
  CM <- tapply(substrat$CM, substrat$station, sum, na.rm = T)
  CV <- tapply(substrat$CV, substrat$station, sum, na.rm = T)
  SD <- tapply(substrat$SD, substrat$station, sum, na.rm = T)
  D <- tapply(substrat$D, substrat$station, sum, na.rm = T)
  
  df <- cbind(CM, CV, SD, D)

  
afc <- ade4::dudi.coa(df, scannf=F, nf=2)

circle <- ade4::s.corcircle(afc$co)
ade4::scatter(afc)
screeplot <- factoextra::fviz_screeplot (afc, addlabels = TRUE, ylim = c(0, 50))

#Le point auquel le graphique des valeurs propres montre un virage (appelé “coude”) peut être considéré comme indiquant le nombre optimal d’axes principaux à retenir.

factoextra::fviz_ca_biplot (afc, repel = TRUE)

}

