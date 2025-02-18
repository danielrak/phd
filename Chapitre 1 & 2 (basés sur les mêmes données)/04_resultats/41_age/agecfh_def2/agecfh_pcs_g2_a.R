# agecfh_pcs_g2 (def2). 
# Repère : Woo15, équation 33. 
library(here)
library(tidyverse)
library(plm)
library(lmtest)
library(stringi)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions.rda")
load(here("01_donnees_def.rda"))
load(here("agepe_def2", "agepe_a.rda"))
source(here("agedepvars.R"))

# Sur c, 2009 -------------------------------------------------------------

  # Néant. 

# Sur c, par année --------------------------------------------------------

for (j in c("2010", "2011", "2012")) {
  
  for (i in agedepvars_c %>% 
       (function (a) a[str_detect(a, "_norm")])) {
    assign(paste("agecfh_pcs_g2_", 
                 i, 
                 "_tous_c",
                 substr(j, 3, 4),
                 sep = ""),
           lm(as.formula(paste(
             i, " ~ ", 
             c("age_abs", "age_abs : pcsR",
               
               "resid", "resid : age_abs", 
               "resid : pcsR", "resid : age_abs : pcsR",
               
               "pcsR", "sexe") %>% 
               paste(collapse = " + "),
             sep = ""
           )),
           c %>% filter(cohorte == j) %>% 
             mutate(resid = eval(parse(text = paste(
               "resid.agepe_z_tous_c", substr(j, 3, 4),
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
     file = here("agecfh_def2", "agecfh_pcs_g2_a.rda"),
     version = 2)

