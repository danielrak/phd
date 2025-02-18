# agecfhmt (def2)

library(here)
library(tidyverse)
library(plm)
library(lmtest)
library(stringi)
library(magrittr)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions2.rda")
load(here("01_donnees_def.rda"))
load(here("agepemt_def2", "agepemt.rda"))
source(here("agedepvars.R"))

# Sur c -------------------------------------------------------------------

for (i in agedepvars_cdi %>% 
     (function (a) a[str_detect(a, "_norm")])) {
  assign(paste("agecfhmt_", 
               i, 
               "_tous_d",
               sep = ""),
         lm(as.formula(paste(
           i, " ~ ", 
           c("age_absdnb", "resid", "resid : age_absdnb",
             "sexe_mod", "pcs_reg_mod", 
             "lregime_constat_g",
             "session_mod") %>% 
             paste(collapse = " + "),
           sep = ""
         )),
         d %>% mutate(resid = resid.agepemt_z_dnb_tous_d)))
}

# Inférence wbi, B101 -----------------------------------------------------

  # Ne pas oublier detailed = FALSE.
for (i in ls()[str_detect(ls(), "^agecfh")]) {
  assign(paste("ctwbi_B1001.", i, sep = ""),
         wboot_lm(get(i), B = 1001, detailed = FALSE))
}

# Sauvegarde --------------------------------------------------------------

save(list = ls()[str_detect(ls(), "^ctwbi")],
     file = here("agecfhmt_def2_wbi", 
                 "agecfhmt_wbi_B1001.rda"),
     version = 2)
