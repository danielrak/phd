# agepemt (def2).  

library(here)
library(tidyverse)
library(plm)
library(lmtest)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions.rda")
load(here("01_donnees_def.rda"))

# Sur c -------------------------------------------------------------------

agepemt_z_dnb_tous_d <- lm(age_absdnb ~ z_dnb
                      + sexe_mod + pcs_reg_mod
                      + lregime_constat_g
                      + session_mod,
                      d)

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
     file = here("agepemt_def2", "agepemt.rda"), 
     version = 2)
