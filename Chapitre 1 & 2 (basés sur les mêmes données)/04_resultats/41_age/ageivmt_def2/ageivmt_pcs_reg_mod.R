# ageivmt_pcs_reg_mod (def2). 

library(here)
library(tidyverse)
library(plm)
library(lmtest)
library(stringi)
library(AER)
library(ivpack)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions.rda")
load(here("01_donnees_def.rda"))
load(here("agepe_def2", "agepe.rda"))
source(here("agedepvars.R"))

# Sur c -------------------------------------------------------------------

for (i in agedepvars_cdi %>% 
     (function (a) a[str_detect(a, "_norm")])) {
  assign(paste("ageivmt_pcs_reg_mod_", 
               i, 
               "_tous_d",
               sep = ""),
         ivreg(as.formula(paste(
           i, " ~ ", 
           c("age_absdnb", "sexe_mod", "pcs_reg_mod", 
             "lregime_constat_g", "session_mod",
             "age_absdnb : pcs_reg_mod") %>% 
             paste(collapse = " + "),
           "|. - age_absdnb + z_dnb", 
           "- age_absdnb : pcs_reg_mod + z_dnb : pcs_reg_mod",
           sep = ""
         )),
         data = d))
}

# Inférence et narsq ------------------------------------------------------

for (i in ls()[str_detect(ls(), "^ageiv")]) {
  assign(paste("ct.", i, sep = ""),
         robust.se(get(i)))
  assign(paste("n.", i, sep = ""), nobs(get(i)))
  assign(paste("arsq.", i, sep = ""), ext_adjrsq(get(i)))
  assign(paste("resid.", i, sep = ""), resid(get(i)))
}

# Sauvegarde --------------------------------------------------------------

save(list = ls()[str_detect(ls(), "ageiv") & 
                   ! str_detect(ls(), "^ageiv")],
     file = here("ageivmt_def2", "ageivmt_pcs_reg_mod.rda"),
     version = 2)
