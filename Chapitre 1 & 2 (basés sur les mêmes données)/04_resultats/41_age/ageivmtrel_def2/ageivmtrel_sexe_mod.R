# ageivmtrel_sexe_mod (def2). 

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
  assign(paste("ageivmtrel_sexe_mod_", 
               i, 
               "_tous_d",
               sep = ""),
         ivreg(as.formula(paste(
           i, " ~ ", 
           c("age_absdnb", "sexe_mod", "pcs_reg_mod", 
             "lregime_constat_g", "session_mod",
             "age_absdnb : sexe_mod",
             
             "I(age_absdnb - p_age_absdnb)",
             "I(age_absdnb - p_age_absdnb) : sexe_mod",
             "p_sexe_mod_M",
             "p_pcs_reg_mod_Moy",
             "p_pcs_reg_mod_Fav",
             "p_pcs_reg_mod_Tresfav",
             "p_pcs_reg_mod_Autres",
             "p_lregime_constat_g_int",
             "p_lregime_constat_g_ext") %>% 
             paste(collapse = " + "),
           "|. - age_absdnb + z_dnb", 
           "- age_absdnb : sexe_mod + z_dnb : sexe_mod",
           "- I(age_absdnb - p_age_absdnb) + I(z_dnb - p_z_dnb)",
           "- I(age_absdnb - p_age_absdnb) : sexe_mod + I(z_dnb - p_z_dnb) : sexe_mod",
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
     file = here("ageivmtrel_def2", "ageivmtrel_sexe_mod.rda"),
     version = 2)
