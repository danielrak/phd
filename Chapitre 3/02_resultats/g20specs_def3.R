# g20specs (def2)

library(here)
library(tidyverse)

# Autres contrôles --------------------------------------------------------

  # Tout ce qu'on a. On met R pour anticiper les version avec datg20. 
    # (pas néo uniquement)
g20_covars <- c("moy_bac", "serie_diplome_psv4", "campus", "filiere",
               "age_26aout", "sexe_ps", "pays_nais_fr",
               "nationalite", "boursier", "statut_etabR",
               "type_etabv2R", "dep_etabv2R")

g20cov0 <- NULL

for (i in 1:length(g20covars)) {
  assign(paste("g20cov", i, sep = ""),
         g20covars[1:i])
}

# covnocor ------------------------------------------------------------------
  # Sans les variables trop correlées. 

g20covnocor <- c("moy_bac", "serie_diplome_psv4", "campus", 
               "filiere", "age_26aout", "sexe_ps", "pays_nais_fr",
               # "nationalite", 
               "boursier", "statut_etabR"
               # , 
               # "type_etabv2", "dep_etabv2"
               )

# Sauvegarde --------------------------------------------------------------

save(list = ls()[str_detect(ls(), "^g20cov")], 
     file = here("g20specs_def3.rda"), 
     version = 2)
