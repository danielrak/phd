# ageivabs_c101112 (def2). 
  # 101112 pour mettre en cohérence avec l'idée que 2009 est une année trop unique. 

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
  assign(paste("ageivabs_", 
               i, 
               "_tous_c101112",
               sep = ""),
         ivreg(as.formula(paste(
           i, " ~ ", 
           c("age_abs", "sexe", "pcsR", "cohorte",
             "age_abs : cohorte") %>% 
             paste(collapse = " + "),
           "|. - age_abs + z - age_abs : cohorte + z : cohorte", 
           sep = ""
         )),
         data = c,
         subset = cohorte != "2009"))
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
     file = here("ageivabs_def2", "ageivabs_c101112.rda"),
     version = 2)
