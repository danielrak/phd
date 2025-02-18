# agecfh_sexe_rdc (def2). 

library(here)
library(tidyverse)
library(plm)
library(lmtest)
library(stringi)
library(magrittr)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions2.rda")
load(here("01_donnees_def.rda"))
load(here("agepe_rdc_def2", "agepe_rdc.rda"))
source(here("agedepvars.R"))

# Sur c -------------------------------------------------------------------

for (i in agedepvars_c %>% 
     (function (a) a[str_detect(a, "_norm")])) {
  assign(paste("agecfh_sexe_", 
               i, 
               "_tous_rdch30",
               sep = ""),
         lm(as.formula(paste(
           i, " ~ ", 
           c("age_abs", 
             "age_abs : sexe",
             "resid", "resid : age_abs",
             "resid : sexe", "resid : age_abs : sexe",
             "sexe", "pcsR") %>% 
             paste(collapse = " + "),
           sep = ""
         )),
         hrestr(rdc, 30) %>%
           mutate(resid = resid.agepe_z_tous_rdch30)))
}

# Inférence wbi, B101 -----------------------------------------------------

  # Ne pas oublier detailed = FALSE.
for (i in ls()[str_detect(ls(), "^agecfh")]) {
  assign(paste("ctwbi_B1001.", i, sep = ""),
         wboot_lm(get(i), B = 1001, detailed = FALSE))
}

# Sauvegarde --------------------------------------------------------------

save(list = ls()[str_detect(ls(), "^ctwbi")],
     file = here("agecfh_rdc_def2_wbi",  
                 "agecfh_sexe_rdc_wbi_B1001.rda"),
     version = 2)
