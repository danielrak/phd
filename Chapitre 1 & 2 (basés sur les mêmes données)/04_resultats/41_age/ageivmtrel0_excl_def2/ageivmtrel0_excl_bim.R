# ageivmtrel0_excl (def2). 

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
  assign(paste("ageivmtrel0_excl_", 
               i, 
               "_tous_d",
               sep = ""),
         ivreg(as.formula(paste(
           i, " ~ ", 
           c("age_absdnb", "sexe_mod", "pcs_reg_mod", 
             "lregime_constat_g", "session_mod",
             "I(age_absdnb - p_age_absdnb)",
             
             "bim_dnb_2 + bim_dnb_3 + bim_dnb_4 + bim_dnb_5",
             "I(bim_dnb_2 - p_bim_dnb_2)",
             "I(bim_dnb_3 - p_bim_dnb_3)",
             "I(bim_dnb_4 - p_bim_dnb_4)",
             "I(bim_dnb_5 - p_bim_dnb_5)") %>% 
             paste(collapse = " + "),
           "|. - age_absdnb + bim_dnb_1", 
           "- I(age_absdnb - p_age_absdnb) + I(bim_dnb_1 - p_bim_dnb_1)",
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
     file = here("ageivmtrel0_excl_def2", "ageivmtrel0_excl_bim.rda"),
     version = 2)
