# agecfhabs_c101112 (def2)

library(here)
library(tidyverse)
library(plm)
library(lmtest)
library(stringi)
library(magrittr)

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

# Inférence wbi, B101 -----------------------------------------------------

  # Ne pas oublier detailed = FALSE.
for (i in ls()[str_detect(ls(), "^agecfh")]) {
  assign(paste("ctwbi_B1001.", i, sep = ""),
         wboot_lm(get(i), B = 1001, detailed = FALSE))
}

# Sauvegarde --------------------------------------------------------------

save(list = ls()[str_detect(ls(), "^ctwbi")],
     file = here("agecfhabs_def2_wbi", 
                 "agecfhabs_c101112_wbi_B1001.rda"),
     version = 2)
