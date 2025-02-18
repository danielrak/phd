# agepe_reseau_bin_sep_c101112 (def2).  

library(here)
library(tidyverse)
library(plm)
library(lmtest)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions.rda")
load(here("01_donnees_def.rda"))

# Sur c -------------------------------------------------------------------

agepe_z_reseau_bin_oui_c101112 <- lm(age_abs ~ z 
                         + sexe + pcsR 
                         + cohorte,
                         c %>% filter(cohorte != "2009"), 
                         reseau_bin == "oui")

agepe_z_reseau_bin_non_c101112 <- lm(age_abs ~ z 
                               + sexe + pcsR 
                               + cohorte,
                               c %>% filter(cohorte != "2009"), 
                               reseau_bin == "non")

# Inférence, narsq et resid -----------------------------------------------

for (i in ls()[str_detect(ls(), "^agepe")]) {
  assign(paste("ct.", i, sep = ""),
         rsearellano(get(i)))
  assign(paste("n.", i, sep = ""), nobs(get(i)))
  assign(paste("arsq.", i, sep = ""), ext_adjrsq(get(i)))
  assign(paste("resid.", i, sep = ""), resid(get(i)))
}

# Sauvegarde --------------------------------------------------------------

save(list = ls()[str_detect(ls(), "agepe") & 
                   ! str_detect(ls(), "^agepe")],
     file = here("agepe_def2",
                 "agepe_reseau_bin_sep_c101112.rda"), 
     version = 2)
