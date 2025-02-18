# ageiv_pcs_g2_a (def2). 

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

# Sur c, 2009 -------------------------------------------------------------

  # Néant. 

# Sur c, par année --------------------------------------------------------

for (j in c("2010", "2011", "2012")) {
  for (i in agedepvars_c %>% 
       (function (a) a[str_detect(a, "_norm")])) {
    assign(paste("ageiv_pcs_g2_", 
                 i, 
                 "_tous_c",
                 substr(j, 3, 4),
                 sep = ""),
           ivreg(as.formula(paste(
             i, " ~ ", 
             c("age_abs", "age_abs : pcsR",
               "pcsR", "sexe") %>% 
               paste(collapse = " + "),
             "|. - age_abs + z - age_abs : pcsR + z : pcsR", 
             sep = ""
           )),
           data = c %>% filter(cohorte == j)))
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
     file = here("ageiv_def2", "ageiv_pcs_g2_a.rda"),
     version = 2)
