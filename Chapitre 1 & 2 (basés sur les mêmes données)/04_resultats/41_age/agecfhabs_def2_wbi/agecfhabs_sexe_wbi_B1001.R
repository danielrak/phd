# agecfhabs_sexe (def2). 
  # Un seul instrument, pour deux termes endogènes, nécessite une 
  # hypothèse forte. 
  # Source : https://stats.stackexchange.com/questions/207295/control-function-cf-approach-with-nonlinear-functions-of-endogenous-variables
  # UPDATE : il faut rajouter resid : sexe et resid : age_abs : sexe, 
    # Voir Woo15, équation 33, le paragraphe juste avant. 
    # De plus, un peu en bas, il dit que ces modèles sont les bons si ce sont des tests scores. 
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
  assign(paste("agecfhabs_sexe_", 
               i, 
               "_tous_c",
               sep = ""),
         lm(as.formula(paste(
           i, " ~ ", 
           c("age_abs", "age_abs : sexe",
             
             "resid", "resid : age_abs", 
             "resid : sexe", "resid : age_abs : sexe",
             
             "sexe", "pcsR", "cohorte",
             "sexe : cohorte",
             
             "age_abs : cohorte", "age_abs : cohorte : sexe",
             "resid : cohorte", "resid : age_abs : cohorte",
             "resid : cohorte : sexe", "resid : age_abs : cohorte : sexe") %>% 
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
     file = here("agecfhabs_def2_wbi",  
                 "agecfhabs_sexe_wbi_B1001.rda"),
     version = 2)

