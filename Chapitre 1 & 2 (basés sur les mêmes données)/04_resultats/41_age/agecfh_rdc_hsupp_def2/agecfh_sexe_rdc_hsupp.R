# agecfh_sexe_rdc_hsupp (def2). 
  # Rappel : ne pas oublier les deux possibilités. 

library(here)
library(tidyverse)
library(plm)
library(lmtest)
library(stringi)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions2.rda")
load(here("01_donnees_def.rda"))
load(here("agepe_rdc_hsupp_def2", "agepe_rdc_hsupp.rda"))
source(here("agedepvars.R"))

# Préparation -------------------------------------------------------------

hsupp <- (5:60) %>% (function (x) x[x != 30])

  # Regarder s'il y a quand même de la variation si l'on bouge la fenêtre. 
cov_hsupp <- lapply(hsupp, function (x) hrestr(rdc, x) %>% 
                      select(sexe, pcsR) %>% summary) %>% setNames(hsupp)
  # ça va même à 5 jours de fenêtre. 

# Sur rdc -----------------------------------------------------------------

for (j in hsupp) {
  for (i in agedepvars_c[
    str_detect(agedepvars_c, "_norm")
  ]) {
    assign(paste("agecfh_sexe_", i, "_tous_rdch", j, sep = ""),
           lm(as.formula(paste(i, " ~ ",
                               c("age_abs",
                                 "age_abs : sexe",
                               "sexe", "pcsR",
                               "resid", "resid : age_abs",
                               "resid : sexe",
                               "resid : age_abs : sexe") %>% 
                                 paste(collapse = " + "),
                               sep = "")),
              data = hrestr(rdc, j) %>% 
                mutate(resid = 
                         eval(parse(text = paste(
                           "resid.agepe_z_tous_rdch", j,
                           sep = ""
                         ))))))
  }
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
     file = here("agecfh_rdc_hsupp_def2", "agecfh_sexe_rdc_hsupp.rda"),
     version = 2)
