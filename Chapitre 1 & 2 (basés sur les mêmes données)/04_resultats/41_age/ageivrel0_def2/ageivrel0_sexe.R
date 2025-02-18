# ageivrel0_sexe (def2). 
# pas de p_covars. 

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

for (i in agedepvars_c %>% 
     (function (a) a[str_detect(a, "_norm")])) {
  assign(paste("ageivrel0_sexe_", 
               i, 
               "_tous_c",
               sep = ""),
         ivreg(as.formula(paste(
           i, " ~ ", 
           c("age_abs", 
             "I(age_abs - p_age_abs)",
             "sexe", "pcsR", "cohorte",
             
             "age_abs : sexe",
             "I(age_abs - p_age_abs) : sexe") %>% 
             paste(collapse = " + "),
           "|. - age_abs + z",
           "- I(age_abs - p_age_abs) + I(z - p_z)",
           "- age_abs : sexe + z : sexe",
           "- I(age_abs - p_age_abs) : sexe + I(z - p_z) : sexe",
           sep = ""
         )),
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
     file = here("ageivrel0_def2", "ageivrel0_sexe.rda"),
     version = 2)