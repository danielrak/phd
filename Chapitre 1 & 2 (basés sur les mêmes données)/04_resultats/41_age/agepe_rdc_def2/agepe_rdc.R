# agepe_rdc (def2).  
  # pe normal mais sur rdc.
  # pas de cohorte comme variable explicative car même info que 
  # age_abs. 

library(here)
library(tidyverse)
library(plm)
library(lmtest)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions.rda")
load(here("01_donnees_def.rda"))

# Sur rdc -----------------------------------------------------------------

agepe_z_tous_rdch30 <- lm(age_abs ~ z
                       + sexe + pcsR,
                       hrestr(rdc, 30))

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
     file = here("agepe_rdc_def2", "agepe_rdc.rda"), 
     version = 2)