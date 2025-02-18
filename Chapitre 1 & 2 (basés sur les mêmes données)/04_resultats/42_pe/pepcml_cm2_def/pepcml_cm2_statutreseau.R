# pepcml_cm2_statutreseau
  # Séparément par statut, par réseau et par le croisement des deux. 

library(here)
library(tidyverse)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions2.rda")
load("D:/00_phd/02_rectorat/04_resultats/41_age/01_donnees_def.rda")

c101112 <- filter(c, cohorte != "2009")

cov2 <- c("sexe", "pcsR")
cov3 <- c("sexe", "pcsR", "age_abs")
cov4 <- c("sexe", "pcsR", "age_abs", "position")

# Data : statutreseau -----------------------------------------------------

c101112_PR <- filter(c101112, statut == "PR")
c101112_PU <- filter(c101112, statut == "PU")
c101112_PUhep <- filter(c101112, statut == "PU" & 
                          reseau_bin == "non")
c101112_PUep <- filter(c101112, statut == "PU" & 
                          reseau_bin == "oui")

# pepcml_cm2 --------------------------------------------------------------

for (k in c("c101112_PR", 
            "c101112_PU",
            "c101112_PUhep", 
            "c101112_PUep")) {
  for (j in c("cov2", "cov3", "cov4")) {
  for (i in c("score_norm", "score_f_norm", "score_m_norm")) {
  assign("ff", Formula::as.Formula(
    paste(i, " + factor(classecoh) ~ ", 
          get(j) %>% paste(collapse = " + "),
          sep = "")
  ))
  assign(paste("pepcml_", j, "_", i, "_tous_", k, sep = ""),
         pseudo_cml3(ff, get(k) %>% 
                       group_by(classecoh) %>% 
                       filter(n() > 1) %>% 
                       ungroup))
  }
  }
  }

# Sauvegarde --------------------------------------------------------------

save(list = ls()[str_detect(ls(), "pepcml")],
     file = here("pepcml_cm2_def", "pepcml_cm2_statutreseau.rda"),
     version = 2)