# ageivmtrel_sexe_mod_a (def2). 
# intérêt : p_z difficilement exogène en 2015. 

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

for (j in c("2014", "2015", "2016")) {
for (i in agedepvars_cdi %>% 
     (function (a) a[str_detect(a, "_norm")])) {
  assign(paste("ageivmtrel0_sexe_mod_", 
               i, 
               "_tous_d_noclniv", substr(j, 3, 4),
               sep = ""),
         ivreg(as.formula(paste(
           i, " ~ ", 
           c("age_absdnb", "sexe_mod", "pcs_reg_mod", 
             "lregime_constat_g",
             "age_absdnb : sexe_mod",
             
             "I(age_absdnb - p_age_absdnb)",
             "I(age_absdnb - p_age_absdnb) : sexe_mod") %>% 
             paste(collapse = " + "),
           "|. - age_absdnb + z_dnb", 
           "- age_absdnb : sexe_mod + z_dnb : sexe_mod",
           "- I(age_absdnb - p_age_absdnb) + I(z_dnb - p_z_dnb)",
           "- I(age_absdnb - p_age_absdnb) : sexe_mod + I(z_dnb - p_z_dnb) : sexe_mod",
           sep = ""
         )),
         data = d_noclniv,
         subset = session_mod == j))
}
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
     file = here("ageivmtrel0_d_noclniv_def2", 
                 "ageivmtrel0_sexe_mod_d_noclniv_a.rda"),
     version = 2)
