# ageperd (def2). 

library(here)
library(tidyverse)
library(plm)
library(lmtest)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions.rda")
load(here("01_donnees_def.rda"))

# Sur rdc -----------------------------------------------------------------

ageperd_p1_tous_rdch30 <- lm(age_abs ~ old + dist + old : dist
                             + sexe + pcsR,
                             hrestr(rdc, 30))

ageperd_p2_tous_rdch30 <- lm(age_abs ~ old + dist + old : dist
                             + I(dist ^ 2) + old : I(dist ^ 2)
                             + sexe + pcsR,
                             hrestr(rdc, 30))

# Inférence et narsq ------------------------------------------------------

for (i in ls()[str_detect(ls(), "^ageperd")]) {
  assign(paste("ct.", i, sep = ""),
         rsearellano(get(i)))
  assign(paste("n.", i, sep = ""), nobs(get(i)))
  assign(paste("arsq.", i, sep = ""), ext_adjrsq(get(i)))
  assign(paste("resid.", i, sep = ""), resid(get(i)))
}

# Sauvegarde --------------------------------------------------------------

save(list = ls()[str_detect(ls(), "ageperd") & 
                   ! str_detect(ls(), "^ageperd")],
     file = here("ageperd_def2", "ageperd.rda"),
     version = 2)
