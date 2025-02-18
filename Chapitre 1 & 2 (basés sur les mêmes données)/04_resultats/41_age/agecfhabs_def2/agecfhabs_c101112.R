# agecfhabs_c101112 (def2)

library(here)
library(tidyverse)
library(plm)
library(lmtest)
library(stringi)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions2.rda")
load(here("01_donnees_def.rda"))
load(here("agepe_def2", "agepe_c101112.rda"))
source(here("agedepvars.R"))

# Sur c -------------------------------------------------------------------

for (i in agedepvars_c %>% 
     (function (a) a[str_detect(a, "_norm")])) {
  assign(paste("agecfhabs_", 
               i, 
               "_tous_c101112",
               sep = ""),
         lm(as.formula(paste(
           i, " ~ ", 
           c("age_abs", "resid", "resid : age_abs",
             "sexe", "pcsR", "cohorte",
             
             "age_abs : cohorte",
             "resid : cohorte",
             "resid : age_abs : cohorte") %>% 
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
     file = here("agecfhabs_def2", "agecfhabs_c101112.rda"),
     version = 2)
