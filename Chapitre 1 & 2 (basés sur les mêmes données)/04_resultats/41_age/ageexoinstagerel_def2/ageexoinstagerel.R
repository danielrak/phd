# ageexoinstagerel (def2). 
  # 2021_12_27. 
  # Sur c101112 uniquement à cause de pcsR_NA complet dans 2009. 
  
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
# load(here("agepe_def2", "agepe.rda"))
# source(here("agedepvars.R"))

# Sur c -------------------------------------------------------------------

ageexoinstagerel_sexe_M_tous_c101112 <- 
  plm(sexe_M ~ z + I(z - p_z) + pcsR, 
      c %>% filter(cohorte != "2009"),
      index = "ecolecoh")

for (i in c("pcsR_Moy", "pcsR_Fav", 
         "pcsR_Tresfav", "pcsR_Autres", 
         "pcsR_NA")) {
  assign(paste("ageexoinstagerel_", 
               i,
               "_tous_c101112",
               sep = ""),
         plm(as.formula(paste(
           i, " ~ ",
           c("z", "I(z - p_z)", "sexe") %>% paste(collapse = " + "),
           sep = ""
         )),
         c %>% filter(cohorte != "2009"),
         index = "ecolecoh"))
}

# Inférence et narsq ------------------------------------------------------

for (i in ls()[str_detect(ls(), "^ageexoinstagerel")]) {
  assign(paste("ct.", i, sep = ""),
         rsearellano(get(i)))
  assign(paste("n.", i, sep = ""), nobs(get(i)))
  assign(paste("arsq.", i, sep = ""), ext_adjrsq(get(i)))
  assign(paste("resid.", i, sep = ""), resid(get(i)))
}

# Sauvegarde --------------------------------------------------------------

save(list = ls()[str_detect(ls(), "ageexoinstagerel") & 
                   ! str_detect(ls(), "^ageexoinstagerel")],
     file = here("ageexoinstagerel_def2", 
                 "ageexoinstagerel.rda"),
     version = 2)