# ageivrel0_bim_pcs_g2_c101112 (def2). 
# pas de p_covars. 
# c101112 car p_z n'est pas indépendante des classes en 2009. 

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

# Sur c -------------------------------------------------------------------

for (i in agedepvars_c %>% 
     (function (a) a[str_detect(a, "_norm")])) {
  assign(paste("ageivrel0_bim_pcs_g2_", 
               i, 
               "_tous_c101112",
               sep = ""),
         ivreg(as.formula(paste(
           i, " ~ ", 
           c("age_abs", 
             "I(age_abs - p_age_abs)",
             "sexe", "pcsR", "cohorte",
             
             "age_abs : pcsR",
             "I(age_abs - p_age_abs) : pcsR") %>% 
             paste(collapse = " + "),
           "|. - age_abs + bim_1 + bim_2 + bim_3 + bim_4 + bim_5",
           
           "- I(age_abs - p_age_abs)",
           "+ I(bim_1 - p_bim_1)",
           "+ I(bim_2 - p_bim_2)",
           "+ I(bim_3 - p_bim_3)",
           "+ I(bim_4 - p_bim_4)",
           "+ I(bim_5 - p_bim_5)",
           
           "- age_abs : pcsR",
           "+ bim_1 : pcsR",
           "+ bim_2 : pcsR",
           "+ bim_3 : pcsR",
           "+ bim_4 : pcsR",
           "+ bim_5 : pcsR",
           
           "- I(age_abs - p_age_abs) : pcsR",
           "+ I(bim_1 - p_bim_1) : pcsR",
           "+ I(bim_2 - p_bim_2) : pcsR",
           "+ I(bim_3 - p_bim_3) : pcsR",
           "+ I(bim_4 - p_bim_4) : pcsR",
           "+ I(bim_5 - p_bim_5) : pcsR",
           
           sep = ""
         )),
         data = c,
         subset = cohorte != "2009"))
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
     file = here("ageivrel0_def2", "ageivrel0_bim_pcs_g2_c101112.rda"),
     version = 2)
