# agecfhabs_pcs_g2_c101112 (def2). 
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
load(here("agepe_def2", "agepe_c101112.rda"))
source(here("agedepvars.R"))

# Sur c -------------------------------------------------------------------

for (i in agedepvars_c %>% 
     (function (a) a[str_detect(a, "_norm")])) {
  assign(paste("agecfhabs_pcs_g2_", 
               i, 
               "_tous_c101112",
               sep = ""),
         lm(as.formula(paste(
           i, " ~ ", 
           c("age_abs", "age_abs : pcsR",
             
             "resid", "resid : age_abs", 
             "resid : pcsR", "resid : age_abs : pcsR",
             
             "pcsR", "sexe", "cohorte",
             "pcsR : cohorte",
             
             "age_abs : cohorte", "age_abs : cohorte : pcsR",
             "resid : cohorte", "resid : age_abs : cohorte",
             "resid : cohorte : pcsR", "resid : age_abs : cohorte : pcsR") %>% 
             paste(collapse = " + "),
           sep = ""
         )),
         c %>% 
           filter(cohorte != "2009") %>% 
           mutate(resid = resid.agepe_z_tous_c101112)))
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
     file = here("agecfhabs_def2", "agecfhabs_pcs_g2_c101112.rda"),
     version = 2)

