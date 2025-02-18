# g20rf_robcovs. 
# 2022_06_17 : pour regarder rapidement la robustesse 
# d'un ou plusieurs paramètres. 

library(here)
library(tidyverse)
library(texreg)
library(mfx)
library(needs)
prioritize(dplyr)

load("C:/00_phd/00_fonctions/fonctions2.rda")
load(here("g20rf_def3", "g20rf.rda"))
load(here("01_datg20_neo_def.rda"))
load(here("g20specs_def3.rda"))

# robcovs -----------------------------------------------------------------

l <- ls()[str_detect(ls(), "^ct.") &
             str_detect(ls(), "cov[:digit:]") & 
            str_detect(ls(), "datg20_neo$") & 
            str_detect(ls(), "note_ue") & 
            str_detect(ls(), "normpop")]

covs <- str_extract(l, "cov[:digit:].") %>% 
  str_remove("_") %>% str_remove("cov") %>% as.numeric %>% sort %>% 
  (function (x) c(x, "nocor")) %>% 
  (function (x) paste("cov", x, sep = ""))

lapply(covs, function (x) 
  get(paste("ct.g20rf_z_", x, "_note_ue_normpop_tous_datg20_neo",
            sep = "")) %>% 
    (function (g) g["z1", c(1, 4)] %>% round(2))) %>% do.call(what = rbind) %>% 
  (function (d) cbind(covs, d)) %>% 
  as.data.frame


# rfprobit ----------------------------------------------------------------

screenreg(list(probitmfx(note_td_sup10 ~ 
                           z + moy_bac + serie_diplome_psv4 + campus
                         + filiere + age_26aout + sexe_ps
                         + pays_nais_fr + boursier + statut_etabR,
                         datg20_neo_venus_td),
               probitmfx(note_ctqcm_sup10 ~ 
                           z + moy_bac + serie_diplome_psv4 + campus
                         + filiere + age_26aout + sexe_ps
                         + pays_nais_fr + boursier + statut_etabR,
                         datg20_neo_venus_ctqcm)))
  # Pas terrible. 
          
          