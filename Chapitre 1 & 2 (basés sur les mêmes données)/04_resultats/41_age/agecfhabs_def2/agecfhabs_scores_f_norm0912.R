# agecfhabs_scores_f_norm0912 (def2)
  # 2021_11_29 : pour abs => si je ne prends que
  # les vardep french, et que je normalise sur 09 à 12.

library(here)
library(tidyverse)
library(plm)
library(lmtest)
library(stringi)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions2.rda")
load(here("01_donnees_def.rda"))
load(here("agepe_def2", "agepe.rda"))
source(here("agedepvars.R"))

# Sur c -------------------------------------------------------------------

for (i in agedepvars_scores_f_norm0912_c
     # %>% 
     # (function (a) a[str_detect(a, "_norm0912")])
     ) {
  assign(paste("agecfhabs_", 
               i, 
               "_tous_c",
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
         c %>% mutate(resid = resid.agepe_z_tous_c)))
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
     file = here("agecfhabs_def2", 
                 "agecfhabs_scores_f_norm0912.rda"),
     version = 2)
