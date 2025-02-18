# agepe_a (def2)

library(here)
library(tidyverse)
library(plm)
library(lmtest)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions.rda")
load(here("01_donnees_def.rda"))

# Sur c -------------------------------------------------------------------

agepe_z_tous_c09 <- lm(age_abs ~ z 
                     + sexe,
                     c, cohorte == "2009")

agepe_z_tous_c10 <- lm(age_abs ~ z 
                       + sexe + pcsR,
                       c, cohorte == "2010")

agepe_z_tous_c11 <- lm(age_abs ~ z 
                       + sexe + pcsR,
                       c, cohorte == "2011")

agepe_z_tous_c12 <- lm(age_abs ~ z 
                       + sexe + pcsR,
                       c, cohorte == "2012")

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
     file = here("agepe_def2", "agepe_a.rda"), 
     version = 2)
