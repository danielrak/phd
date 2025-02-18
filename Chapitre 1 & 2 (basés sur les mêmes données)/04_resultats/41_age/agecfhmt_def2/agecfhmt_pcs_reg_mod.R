# agecfhmt_pcs_g2 (def2). 
# Un seul instrument, pour deux termes endogènes, nécessite une 
# hypothèse forte. 
# Source : https://stats.stackexchange.com/questions/207295/control-function-cf-approach-with-nonlinear-functions-of-endogenous-variables
# UPDATE : il faut rajouter resid : pcs_g2 et resid : age_abs : pcs_g2, 
# Voir Woo15, équation 33, le paragraphe juste avant. 
# De plus, un peu en bas, il dit que ces modèles sont les bons si ce sont des tests scores. 
library(here)
library(tidyverse)
library(plm)
library(lmtest)
library(stringi)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions.rda")
load(here("01_donnees_def.rda"))
load(here("agepemt_def2", "agepemt.rda"))
source(here("agedepvars.R"))

# Sur c -------------------------------------------------------------------

for (i in agedepvars_cdi %>% 
     (function (a) a[str_detect(a, "_norm")])) {
  assign(paste("agecfhmt_pcs_reg_mod_", 
               i, 
               "_tous_d",
               sep = ""),
         lm(as.formula(paste(
           i, " ~ ", 
           c("age_absdnb", "age_absdnb : pcs_reg_mod",
             
             "resid", "resid : age_absdnb", 
             "resid : pcs_reg_mod", "resid : age_absdnb : pcs_reg_mod",
             
             "pcs_reg_mod", "sexe_mod", "lregime_constat_g",
             "session_mod") %>% 
             paste(collapse = " + "),
           sep = ""
         )),
         d %>% mutate(resid = resid.agepemt_z_dnb_tous_d)))
}

# Inférence et narsq ------------------------------------------------------

for (i in ls()[str_detect(ls(), "^agecfh")]) {
  assign(paste("ct.", i, sep = ""),
         rsearellano(get(i)))
  assign(paste("n.", i, sep = ""), nobs(get(i)))
  assign(paste("arsq.", i, sep = ""), ext_adjrsq(get(i)))
}

# Sauvegarde --------------------------------------------------------------

save(list = ls()[str_detect(ls(), "agecfh") & 
                   ! str_detect(ls(), "^agecfh")],
     file = here("agecfhmt_def2", "agecfhmt_pcs_reg_mod.rda"),
     version = 2)

