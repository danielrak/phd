# agecfh_pcs_g2_reseau_bin_sep (def2). 
# fee : effets école.
# intérêt : trouver une explication à l'effet 
# plus fort chez les très favorisés. 

library(here)
library(tidyverse)
library(plm)
library(lmtest)
library(stringi)
library(magrittr)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions2.rda")
load(here("01_donnees_def.rda"))
load(here("agepe_def2", "agepe_reseau_bin_sep.rda"))
source(here("agedepvars.R"))

# Sur c -------------------------------------------------------------------

for(j in c("non", "oui")) {
for (i in agedepvars_c %>% 
     (function (a) a[str_detect(a, "_norm")])) {
  assign(paste("agecfh_pcs_g2_", 
               i, 
               "_",
               "reseau_bin_", j, 
               "_c",
               sep = ""),
         lm(as.formula(paste(
           i, " ~ ", 
           c("age_abs", "age_abs : pcsR",
             
             "resid", "resid : age_abs", 
             "resid : pcsR", "resid : age_abs : pcsR",
             
             "pcsR", "sexe", "cohorte") %>% 
             paste(collapse = " + "),
           sep = ""
         )),
         c %>% filter(reseau_bin == j) %>% 
           mutate(resid = eval(parse(text = paste(
                    "resid.agepe_z_reseau_bin_", j, "_c", 
                    sep = ""
                  ))))))
}
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
                 "agecfh_pcs_g2_reseau_bin_sep_wbi_B1001.rda"))
