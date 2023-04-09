AFC <- function(substrat_csv){
  
  #targets::tar_load(substrat_csv)
  filter_orga <- substrat_csv |>
    dplyr::group_by(groupe, radiale, station, transect, organismes_benthiques) |>
    dplyr::summarise(count = dplyr::n())
  
  filter_orga <- filter_orga |>
    tidyr::spread(organismes_benthiques, count)
  
  C_B  <- tapply(filter_orga$C_B, filter_orga$station, sum, na.rm = T)
  C_D  <- tapply(filter_orga$C_D, filter_orga$station, sum, na.rm = T)
  C_E  <- tapply(filter_orga$C_E, filter_orga$station, sum, na.rm = T)
  C_F  <- tapply(filter_orga$C_F, filter_orga$station, sum, na.rm = T)
  C_M  <- tapply(filter_orga$C_M, filter_orga$station, sum, na.rm = T)
  C_SM <- tapply(filter_orga$C_SM, filter_orga$station, sum, na.rm = T)
  COR  <- tapply(filter_orga$COR, filter_orga$station, sum, na.rm = T)
  GA   <- tapply(filter_orga$GA, filter_orga$station, sum, na.rm = T)
  INV  <- tapply(filter_orga$INV, filter_orga$station, sum, na.rm = T)
  MAC  <- tapply(filter_orga$MAC, filter_orga$station, sum, na.rm = T)
  NU   <- tapply(filter_orga$NU, filter_orga$station, sum, na.rm = T)
  
  C_B_r  <- tapply(filter_orga$C_B, filter_orga$radiale, sum, na.rm = T)
  C_D_r  <- tapply(filter_orga$C_D, filter_orga$radiale, sum, na.rm = T)
  C_E_r  <- tapply(filter_orga$C_E, filter_orga$radiale, sum, na.rm = T)
  C_F_r  <- tapply(filter_orga$C_F, filter_orga$radiale, sum, na.rm = T)
  C_M_r  <- tapply(filter_orga$C_M, filter_orga$radiale, sum, na.rm = T)
  C_SM_r <- tapply(filter_orga$C_SM, filter_orga$radiale, sum, na.rm = T)
  COR_r  <- tapply(filter_orga$COR, filter_orga$radiale, sum, na.rm = T)
  GA_r   <- tapply(filter_orga$GA, filter_orga$radiale, sum, na.rm = T)
  INV_r  <- tapply(filter_orga$INV, filter_orga$radiale, sum, na.rm = T)
  MAC_r  <- tapply(filter_orga$MAC, filter_orga$radiale, sum, na.rm = T)
  NU_r   <- tapply(filter_orga$NU, filter_orga$radiale, sum, na.rm = T)
  
  df <- rbind(cbind(C_B, C_D, C_E, C_F, C_M, C_SM, COR, GA, INV, MAC, NU), 
        cbind(C_B_r, C_D_r, C_E_r, C_F_r, C_M_r, C_SM_r, COR_r, GA_r, INV_r, MAC_r, NU_r))

  
afc <- ade4::dudi.coa(df, scannf=F, nf=2)

circle <- ade4::s.corcircle(afc$co)
ade4::scatter(afc)
screeplot <- factoextra::fviz_screeplot (afc, addlabels = TRUE, ylim = c(0, 50))

#Le point auquel le graphique des valeurs propres montre un virage (appelé “coude”) peut être considéré comme indiquant le nombre optimal d’axes principaux à retenir.

factoextra::fviz_ca_biplot (afc, repel = TRUE)

#dir.create("outputs/graphique/AFC")
ggplot2::ggsave("outputs/graphique/AFC/afc.png", plot = ggplot2::last_plot(), 
                dpi = 500)

}

