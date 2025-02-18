# agecfh_pcs_g2_wbi (def2, B101)

library(here)
library(tidyverse)
library(plm)
library(lmtest)
library(stringi)
library(magrittr)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions2.rda")
load(here("01_donnees_def.rda"))
load(here("agepe_def2", "agepe.rda"))
source(here("agedepvars.R"))

# Sur c -------------------------------------------------------------------

for (i in agedepvars_c %>% 
     (function (a) a[str_detect(a, "_norm")])) {
  assign(paste("agecfh_pcs_g2_", 
               i, 
               "_tous_c",
               sep = ""),
         lm(as.formula(paste(
           i, " ~ ", 
           c("age_abs", "age_abs : pcsR",
             
             "resid", "resid : age_abs", 
             "resid : pcsR", "resid : age_abs : pcsR",
             
             "pcs_g2", "sexe", "cohorte") %>% 
             paste(collapse = " + "),
           sep = ""
         )),
         c %>% mutate(resid = resid.agepe_z_tous_c)))
}

# Inférence wbi, B101 -----------------------------------------------------

  # Ne pas oublier detailed = FALSE.
for (i in ls()[str_detect(ls(), "^agecfh")]) {
  assign(paste("ctwbi_B1001.", i, sep = ""),
         wboot_lm(get(i), B = 1001, detailed = FALSE))
}

# Sauvegarde --------------------------------------------------------------

save(list = ls()[str_detect(ls(), "^ctwbi")],
     file = here("agecfh_def2_wbi", 
                 "agecfh_pcs_g2_wbi_B1001.rda"))
