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

ageexoinstagerelmt_sexe_mod_M_tous_d <- 
  plm(sexe_mod_M ~ z_dnb + I(z_dnb - p_z_dnb) + pcs_reg_mod + lregime_constat_g, 
      d,
      index = "rneconstatses")

for (i in c("pcs_reg_mod_Moy", "pcs_reg_mod_Fav", 
         "pcs_reg_mod_Tresfav", "pcs_reg_mod_Autres")) {
  assign(paste("ageexoinstagerelmt_", 
               i,
               "_tous_d",
               sep = ""),
         plm(as.formula(paste(
           i, " ~ ",
           c("z_dnb",
             "I(z_dnb - p_z_dnb)", "sexe_mod",
             "lregime_constat_g") %>% paste(collapse = " + "),
           sep = ""
         )),
         d,
         index = "rneconstatses"))
}

for (i in c("lregime_constat_g_int", "lregime_constat_g_ext")) {
  assign(paste("ageexoinstagerelmt_", 
               i,
               "_tous_d",
               sep = ""),
         plm(as.formula(paste(
           i, " ~ ",
           c("z_dnb",
             "I(z_dnb - p_z_dnb)", "sexe_mod",
             "pcs_reg_mod") %>% paste(collapse = " + "),
           sep = ""
         )),
         d,
         index = "rneconstatses"))
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
     file = here("ageexoinstagerelmt_def2", 
                 "ageexoinstagerelmt.rda"),
     version = 2)