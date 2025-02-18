# ageiv_excl (def2). 
  # excl : test de la restriction d'exclusion. 
  # mois de janvier comme instrument et le reste comme variables de contrôle. 

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
source(here("agedepvars.R"))

# Sur c -------------------------------------------------------------------

for (i in agedepvars_c %>% 
     (function (a) a[str_detect(a, "_norm")])) {
  assign(paste("ageiv_excl_", i, "_tous_c",
               sep = ""),
         ivreg(as.formula(paste(i, " ~ ", c("age_abs", "sexe", "pcsR", "cohorte",
                                            "mois_2", "mois_3", "mois_4",
                                            "mois_5", "mois_6", "mois_7",
                                            "mois_8", "mois_9", "mois_10",
                                            "mois_11") %>% 
                                  paste(collapse = " + "),
                                "|. - age_abs + mois_1",
                                sep = "")),
               data = c))
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
     file = here("ageiv_def2", "ageiv_excl.rda"),
     version = 2)
