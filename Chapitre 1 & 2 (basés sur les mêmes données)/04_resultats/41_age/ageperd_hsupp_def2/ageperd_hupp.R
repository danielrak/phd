# ageperd_hsupp (def2). 
  # Nécessaire pour les tests de robusteese.

library(here)
library(tidyverse)
library(plm)
library(lmtest)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions.rda")
load(here("01_donnees_def.rda"))

# Préparation -------------------------------------------------------------

hsupp <- (5:60) #%>% (function (x) x[x != 30])

# Regarder s'il y a quand même de la variation 
# si l'on bouge la fenêtre. 
cov_hsupp <- lapply(hsupp, function (x) hrestr(rdc, x) %>% 
                      select(sexe, pcsR) %>% summary) %>%
  setNames(hsupp)
# ça va même à 5 jours de fenêtre. 

# Sur rdc -----------------------------------------------------------------

  # inspiré de agefrd_hsupp. 
for (j in hsupp) {
  
  assign(paste("ageperd_p1_tous_rdch", j, sep = ""),
         lm(age_abs ~ old + dist + old : dist
            + sexe + pcsR,
            hrestr(rdc, j)))
  
  assign(paste("ageperd_p2_tous_rdch", j, sep = ""),
         lm(age_abs ~ old + dist + old : dist
            + I(dist ^ 2) + old : I(dist ^ 2)
            + sexe + pcsR,
            hrestr(rdc, j)))
}

# Inférence et narsq ------------------------------------------------------

  # et resid. 
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
     file = here("ageperd_hsupp_def2", "ageperd_hsupp.rda"),
     version = 2)