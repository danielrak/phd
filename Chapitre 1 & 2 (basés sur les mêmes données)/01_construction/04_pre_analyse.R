# Post jointures.

library(tidyverse)
library(fastDummies)
library(lubridate)
library(magrittr)
library(stringi)
library(here)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions.rda")
load(here("01_construction", "03_post_jointure.rda"))

# cm2 ---------------------------------------------------------------------

cm2 <- mutate(
  cm2,
  reseau = factor(reseau, levels = c("HEP", "ECLAIR", "RRS")),
  statut = factor(statut, levels = c("PR", "PU")),
  cohorte = factor(cohorte, levels = c("2010", "2011", "2012")),
  sexe = factor(sexe, levels = c("F", "M")),
  position = factor(position, levels = c("Redoublant", "Heure", "Avance")),
  quinze = factor(quinze, levels = as.character(1:24)),
  mois = factor(mois, levels = as.character(1:12)),
  bim = factor(bim, levels = as.character(1:6)),
  trim = factor(trim, levels = as.character(1:4)),
  pcs = factor(
    pcs,
    levels = c(
      "Agriculteurs",
      "ArtComChefEnt",
      "CadrPrIntSup",
      "ProfInt",
      "Employes",
      "Ouvriers",
      "Retraites",
      "Inactifs",
      "Autres"
    )
  ),
  #
  pcsv2 = factor(
    pcsv2,
    levels = c(
      "Agriculteurs",
      "ArtComChefEnt",
      "CadrPrIntSup",
      "ProfInt",
      "Employes",
      "Ouvriers",
      "Retraites",
      "Inactifs",
      "Autres"
    )
  ),
  
  pcs_g = factor(pcs_g, levels = c("unpriv", "priv", "Autres")),
  #
  pcsv2_g = factor(pcsv2_g, levels = c("unpriv", "priv", "Autres")),
  
  reseau_bin = factor(reseau_bin, levels = c("non", "oui")),
  pcs_g2 = factor(pcs_g2, levels = c("Defav", "Moy", "Fav", "Tresfav", "Autres")),
  #
  pcsv2_g2 = factor(pcsv2_g2, levels = c("Defav", "Moy", "Fav", "Tresfav", "Autres")),
  
  arrondissement = fct_collapse(
    factor(commune),
    "saint_benoit" = c(
      "BRAS-PANON",
      "LA PLAINE-DES-PALMISTES",
      "SAINT-ANDRE",
      "SAINT-BENOIT",
      "SAINTE-ROSE",
      "SALAZIE"
    ),
    "saint_denis" = c("SAINT-DENIS", "SAINTE-MARIE", "SAINTE-SUZANNE"),
    "saint_paul" = c(
      "LE PORT",
      "LA POSSESSION",
      "SAINT-LEU",
      "SAINT-PAUL",
      "LES TROIS-BASSINS"
    ),
    "saint_pierre" = c(
      "LES AVIRONS",
      "CILAOS",
      "ENTRE-DEUX",
      "L'ETANG-SALE",
      "PETITE-ILE",
      "SAINT-JOSEPH",
      "SAINT-LOUIS",
      "SAINT-PHILIPPE",
      "SAINT-PIERRE",
      "LE TAMPON"
    )
  )
)

# cm209 -------------------------------------------------------------------

cm209 <- mutate(
  cm209,
  reseau = factor(reseau, levels = c("HEP", "ECLAIR", "RRS")),
  statut = factor(statut, levels = c("PR", "PU")),
  cohorte = factor(cohorte, levels = "2009"),
  sexe = factor(sexe, levels = c("F", "M")),
  position = factor(position, levels = c("Redoublant", "Heure", "Avance")),
  quinze = factor(quinze, levels = as.character(1:24)),
  mois = factor(mois, levels = as.character(1:12)),
  bim = factor(bim, levels = as.character(1:6)),
  trim = factor(trim, levels = as.character(1:4)),
  arrondissement = fct_collapse(
    factor(commune),
    "saint_benoit" = c(
      "BRAS-PANON",
      "LA PLAINE-DES-PALMISTES",
      "SAINT-ANDRE",
      "SAINT-BENOIT",
      "SAINTE-ROSE",
      "SALAZIE"
    ),
    "saint_denis" = c("SAINT-DENIS", "SAINTE-MARIE", "SAINTE-SUZANNE"),
    "saint_paul" = c(
      "LE PORT",
      "LA POSSESSION",
      "SAINT-LEU",
      "SAINT-PAUL",
      "LES TROIS-BASSINS"
    ),
    "saint_pierre" = c(
      "LES AVIRONS",
      "CILAOS",
      "ENTRE-DEUX",
      "L'ETANG-SALE",
      "PETITE-ILE",
      "SAINT-JOSEPH",
      "SAINT-LOUIS",
      "SAINT-PHILIPPE",
      "SAINT-PIERRE",
      "LE TAMPON"
    )
  )
)

# cm2_w09 -----------------------------------------------------------------

cm2_w09 <- rbind(select(cm2, names(cm209)) %>%
                   mutate(cohorte = as.character(cohorte)),
                
                 cm209 %>% 
                   mutate(cohorte = as.character(cohorte)))

# rdcm2 -------------------------------------------------------------------

rdcm2 <- mutate(
  rdcm2,
  reseau = factor(reseau, levels = c("HEP", "ECLAIR", "RRS")),
  statut = factor(statut, levels = c("PR", "PU")),
  cohorte = factor(cohorte, levels = c("2009", "2010", "2011", "2012")),
  sexe = factor(sexe, levels = c("F", "M")),
  position = factor(position, levels = c("Redoublant", "Heure", "Avance")),
  quinze = factor(quinze, levels = as.character(1:24)),
  mois = factor(mois, levels = as.character(1:12)),
  bim = factor(bim, levels = as.character(1:6)),
  trim = factor(trim, levels = as.character(1:4)),
  pcs = factor(
    pcs,
    levels = c(
      "Agriculteurs",
      "ArtComChefEnt",
      "CadrPrIntSup",
      "ProfInt",
      "Employes",
      "Ouvriers",
      "Retraites",
      "Inactifs",
      "Autres"
    )
  ),
  
  pcsv2 = factor(
    pcsv2,
    levels = c(
      "Agriculteurs",
      "ArtComChefEnt",
      "CadrPrIntSup",
      "ProfInt",
      "Employes",
      "Ouvriers",
      "Retraites",
      "Inactifs",
      "Autres"
    )
  ),
  
  pcs_g = factor(pcs_g, levels = c("unpriv", "priv", "Autres")),
  pcsv2_g = factor(pcsv2_g, levels = c("unpriv", "priv", "Autres")),
  reseau_bin = factor(reseau_bin, levels = c("non", "oui")),
  pcsv2_g2 = factor(pcsv2_g2, levels = c("Defav", "Moy", "Fav", "Tresfav", "Autres")),
  
  arrondissement = fct_collapse(
    factor(commune),
    "saint_benoit" = c(
      "BRAS-PANON",
      "LA PLAINE-DES-PALMISTES",
      "SAINT-ANDRE",
      "SAINT-BENOIT",
      "SAINTE-ROSE",
      "SALAZIE"
    ),
    "saint_denis" = c("SAINT-DENIS", "SAINTE-MARIE", "SAINTE-SUZANNE"),
    "saint_paul" = c(
      "LE PORT",
      "LA POSSESSION",
      "SAINT-LEU",
      "SAINT-PAUL",
      "LES TROIS-BASSINS"
    ),
    "saint_pierre" = c(
      "LES AVIRONS",
      "CILAOS",
      "ENTRE-DEUX",
      "L'ETANG-SALE",
      "PETITE-ILE",
      "SAINT-JOSEPH",
      "SAINT-LOUIS",
      "SAINT-PHILIPPE",
      "SAINT-PIERRE",
      "LE TAMPON"
    )
  )
)

# dnb ---------------------------------------------------------------------

dnb <- mutate(
  dnb,
  cod_han_mod = factor(cod_han, levels = c("N", "O")),
  cod_spe_mod = factor(cod_spe, levels = c("P", "G")),
  pcs_mod = factor(
    pcs_mod,
    levels = c(
      "Agriculteurs",
      "ArtComChefEnt",
      "CadrPrIntSup",
      "ProfInt",
      "Employes",
      "Ouvriers",
      "Retraites",
      "Inactifs",
      "Autres"
    )
  ),
  pcs_reg_mod = factor(
    pcs_reg_mod,
    levels = c("Defav", "Moy", "Fav", "Tresfav", "Autres")
  ),
  reseau_mod = factor(reseau, levels = c("HEP", "REP/ex-ECL", "REP+/ex-RRS")),
  sexe_mod = factor(sexe, levels = c("F", "M")),
  session_mod = factor(session, levels = c("2014", "2015", "2016")),
  pcs_gdnb = factor(pcs_gdnb, levels = c("unpriv", "priv", "Autres")),
  statut_constat = factor(statut_constat, levels = c("PR", "PU")),
  lbourses_constat = factor(
    lbourses_constat,
    levels = c("non_boursier", "bourse_des_colleges",
               "bourse_nationale")
  ),
  positiondnb2 = factor(positiondnb2, levels = c("Redoublant", "Heure", "Avance")),
  quinze_dnb = factor(quinze_dnb, levels = as.character(1:24)),
  mois_dnb = factor(mois_dnb, levels = as.character(1:12)),
  bim_dnb = factor(bim_dnb, levels = as.character(1:6)),
  trim_dnb = factor(trim_dnb, levels = as.character(1:4)),
  
  niveauconstat = factor(niveauconstat,
                         levels = c(as.character(6:0),
                                    "autres"))
)

# il y a une observation au dnb14 telle que nchar(bassin) == 0
dnb$bassin_mod[nchar(dnb$bassin_mod) == 0] <- NA

# cm2dnb ------------------------------------------------------------------

cm2dnb <- mutate(
  cm2dnb,
  
  reseau.x = factor(reseau.x, levels = c("HEP", "ECLAIR", "RRS")),
  statut = factor(statut, levels = c("PR", "PU")),
  cohorte = factor(cohorte, levels = c("2010", "2011", "2012")),
  sexe.x = factor(sexe.x, levels = c("F", "M")),
  position = factor(position, levels = c("Redoublant", "Heure", "Avance")),
  quinze = factor(quinze, levels = as.character(1:24)),
  mois = factor(mois, levels = as.character(1:12)),
  bim = factor(bim, levels = as.character(1:6)),
  trim = factor(trim, levels = as.character(1:4)),
  pcs.x = factor(
    pcs.x,
    levels = c(
      "Agriculteurs",
      "ArtComChefEnt",
      "CadrPrIntSup",
      "ProfInt",
      "Employes",
      "Ouvriers",
      "Retraites",
      "Inactifs",
      "Autres"
    )
  ),
  pcs_g = factor(pcs_g, levels = c("unpriv", "priv", "Autres")),
  reseau_bin = factor(reseau_bin, levels = c("non", "oui")),
  
  cod_han_mod = factor(cod_han, levels = c("N", "O")),
  cod_spe_mod = factor(cod_spe, levels = c("P", "G")),
  pcs_mod = factor(
    pcs_mod,
    levels = c(
      "Agriculteurs",
      "ArtComChefEnt",
      "CadrPrIntSup",
      "ProfInt",
      "Employes",
      "Ouvriers",
      "Retraites",
      "Inactifs",
      "Autres"
    )
  ),
  reseau_mod = factor(reseau.y, levels = c("HEP", "REP/ex-ECL", "REP+/ex-RRS")),
  sexe_mod = factor(sexe.y, levels = c("F", "M")),
  session_mod = factor(session, levels = c("2014", "2015", "2016")),
  pcs_gdnb = factor(pcs_gdnb, levels = c("unpriv", "priv", "Autres")),
  pcs_reg_mod = factor(
    pcs_reg_mod,
    levels = c("Defav", "Moy", "Fav", "Tresfav", "Autres")
  ),
  statut_constat = factor(statut_constat, levels = c("PR", "PU")),
  lbourses_constat = factor(
    lbourses_constat,
    levels = c("non_boursier", "bourse_des_colleges",
               "bourse_nationale")
  ),
  positiondnb2 = factor(positiondnb2, levels = c("Redoublant", "Heure", "Avance")),
  quinze_dnb = factor(quinze_dnb, levels = as.character(1:24)),
  mois_dnb = factor(mois_dnb, levels = as.character(1:12)),
  bim_dnb = factor(bim_dnb, levels = as.character(1:6)),
  trim_dnb = factor(trim_dnb, levels = as.character(1:4)),
  
  arrondissement = fct_collapse(
    factor(commune),
    "saint_benoit" = c(
      "BRAS-PANON",
      "LA PLAINE-DES-PALMISTES",
      "SAINT-ANDRE",
      "SAINT-BENOIT",
      "SAINTE-ROSE",
      "SALAZIE"
    ),
    "saint_denis" = c("SAINT-DENIS", "SAINTE-MARIE", "SAINTE-SUZANNE"),
    "saint_paul" = c(
      "LE PORT",
      "LA POSSESSION",
      "SAINT-LEU",
      "SAINT-PAUL",
      "LES TROIS-BASSINS"
    ),
    "saint_pierre" = c(
      "LES AVIRONS",
      "CILAOS",
      "ENTRE-DEUX",
      "L'ETANG-SALE",
      "PETITE-ILE",
      "SAINT-JOSEPH",
      "SAINT-LOUIS",
      "SAINT-PHILIPPE",
      "SAINT-PIERRE",
      "LE TAMPON"
    )
  ),
  niveauconstat = factor(niveauconstat,
                         levels = c(as.character(6:0),
                                    "autres"))
  
)

# dnbcm2 ------------------------------------------------------------------

dnbcm2 <- mutate(
  dnbcm2,
  
  reseau.y = factor(reseau.y, levels = c("HEP", "ECLAIR", "RRS")),
  statut = factor(statut, levels = c("PR", "PU")),
  cohorte = factor(cohorte, levels = c("2010", "2011", "2012")),
  sexe.x = factor(sexe.x, levels = c("F", "M")),
  position = factor(position, levels = c("Redoublant", "Heure", "Avance")),
  quinze = factor(quinze, levels = as.character(1:24)),
  mois = factor(mois, levels = as.character(1:12)),
  bim = factor(bim, levels = as.character(1:6)),
  trim = factor(trim, levels = as.character(1:4)),
  pcs.x = factor(
    pcs.x,
    levels = c(
      "Agriculteurs",
      "ArtComChefEnt",
      "CadrPrIntSup",
      "ProfInt",
      "Employes",
      "Ouvriers",
      "Retraites",
      "Inactifs",
      "Autres"
    )
  ),
  pcs_g = factor(pcs_g, levels = c("unpriv", "priv", "Autres")),
  reseau_bin = factor(reseau_bin, levels = c("non", "oui")),
  
  cod_han_mod = factor(cod_han, levels = c("N", "O")),
  cod_spe_mod = factor(cod_spe, levels = c("P", "G")),
  pcs_mod = factor(
    pcs_mod,
    levels = c(
      "Agriculteurs",
      "ArtComChefEnt",
      "CadrPrIntSup",
      "ProfInt",
      "Employes",
      "Ouvriers",
      "Retraites",
      "Inactifs",
      "Autres"
    )
  ),
  reseau_mod = factor(reseau_mod, levels = c("HEP", "REP/ex-ECL", "REP+/ex-RRS")),
  sexe_mod = factor(sexe.x, levels = c("F", "M")),
  session_mod = factor(session, levels = c("2014", "2015", "2016")),
  pcs_gdnb = factor(pcs_gdnb, levels = c("unpriv", "priv", "Autres")),
  pcs_reg_mod = factor(
    pcs_reg_mod,
    levels = c("Defav", "Moy", "Fav", "Tresfav", "Autres")
  ),
  statut_constat = factor(statut_constat, levels = c("PR", "PU")),
  lbourses_constat = factor(
    lbourses_constat,
    levels = c("non_boursier", "bourse_des_colleges",
               "bourse_nationale")
  ),
  positiondnb2 = factor(positiondnb2, levels = c("Redoublant", "Heure", "Avance")),
  quinze_dnb = factor(quinze_dnb, levels = as.character(1:24)),
  mois_dnb = factor(mois_dnb, levels = as.character(1:12)),
  bim_dnb = factor(bim_dnb, levels = as.character(1:6)),
  trim_dnb = factor(trim_dnb, levels = as.character(1:4)),
  
  arrondissement = fct_collapse(
    factor(commune),
    "saint_benoit" = c(
      "BRAS-PANON",
      "LA PLAINE-DES-PALMISTES",
      "SAINT-ANDRE",
      "SAINT-BENOIT",
      "SAINTE-ROSE",
      "SALAZIE"
    ),
    "saint_denis" = c("SAINT-DENIS", "SAINTE-MARIE", "SAINTE-SUZANNE"),
    "saint_paul" = c(
      "LE PORT",
      "LA POSSESSION",
      "SAINT-LEU",
      "SAINT-PAUL",
      "LES TROIS-BASSINS"
    ),
    "saint_pierre" = c(
      "LES AVIRONS",
      "CILAOS",
      "ENTRE-DEUX",
      "L'ETANG-SALE",
      "PETITE-ILE",
      "SAINT-JOSEPH",
      "SAINT-LOUIS",
      "SAINT-PHILIPPE",
      "SAINT-PIERRE",
      "LE TAMPON"
    )
  ),
  niveauconstat = factor(niveauconstat,
                         levels = c(as.character(6:0),
                                    "autres"))
  
)

# constat1316 -------------------------------------------------------------

constat1316 <- mutate(
  constat1316,
  sexe_mod = factor(sexe_mod, levels = c("F", "M")),
  statut_mod = factor(statut, levels = c("PR", "PU")),
  lbourses_mod = factor(
    lbourses_mod,
    levels = c(
      "non_boursier",
      "bourse_des_colleges",
      "bourse_departementale",
      "bourse_nationale",
      "bourse_ens_sup"
    )
  ),
  session_mod = factor(session, levels = as.character(2014:2017)),
  pcsconstat = factor(
    pcsconstat,
    levels = c(
      "Agriculteurs",
      "ArtComChefEnt",
      "CadrPrIntSup",
      "ProfInt",
      "Employes",
      "Ouvriers",
      "Retraites",
      "Inactifs",
      "Autres"
    )
  ),
  niveauconstat = factor(niveauconstat,
                         levels = c(as.character(6:0), "autres")),
  positionconstat = factor(positionconstat,
                           levels = c("Redoublant", "Heure", "Avance")),
  pcs_gconstat = factor(pcs_gconstat,
                        levels = c("unpriv", "priv", "Autres")),
  pcs_g2constat = factor(
    pcs_g2constat,
    levels = c("Defav", "Moy", "Fav", "Tresfav", "Autres")
  )
)

# Sauvegarde --------------------------------------------------------------

for (i in ls()["function" != ls() %>% 
               sapply(function (x) class(get(x))[1])]) 
  save(list = i, 
       file = here("01_construction", 
                   paste("04_pre_analyse_", i, ".rda", sep = "")), 
       version = 2)
