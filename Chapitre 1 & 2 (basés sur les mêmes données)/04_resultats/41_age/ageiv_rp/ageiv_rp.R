# ageiv, tot, french et maths uniquement. 

library(here)
library(tidyverse)
library(plm)
library(lmtest)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions.rda")
load(here("01_donnees.rda"))

# Préparation -------------------------------------------------------------

depvar <- "score_rp"
age <- "age_abs"
z <- "z"
cont <- c("sexe_M", "pcs_g2_Moy", "pcs_g2_Fav", "pcs_g2_Tresfav", "pcs_g2_Autres", 
          "pcs_g2_NA")
coh <- c("cohorte_2010", "cohorte_2011", "cohorte_2012")

# Régressions -------------------------------------------------------------

ageiv_rp <- plm(as.formula(paste(depvar, " ~ ", 
                             age, " + ",
                             cont %>% paste(collapse = " + "), " + ",
                             coh %>% paste(collapse = " + "), 
                             "|. - ", age, " + ", 
                             z)), c, index = "ecolecoh", model = "pooling")

ageiv_rp_french <- update(ageiv_rp, score_f_rp ~ .)

ageiv_rp_maths <- update(ageiv_rp, score_m_rp ~ .)

# Inférence et narsq ------------------------------------------------------

for (i in ls()[str_detect(ls(), "^ageiv")]) {
  assign(paste("ct.", i, sep = ""),
         rsearellano(get(i)))
  assign(paste("n.", i, sep = ""), nobs(get(i)))
  assign(paste("arsq.", i, sep = ""), ext_adjrsq(get(i)))
}

# Sauvegarde --------------------------------------------------------------

save(list = ls()[str_detect(ls(), "^ct\\.|^n\\.|^arsq\\.|^mvc\\.")], 
     file = here("ageiv_rp", "ageiv_rp.rda"), version = 2)