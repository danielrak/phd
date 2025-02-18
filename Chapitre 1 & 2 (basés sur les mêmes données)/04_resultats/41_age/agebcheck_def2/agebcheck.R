# agebcheck (def). 

library(here)
library(tidyverse)
library(plm)
library(lmtest)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions.rda")
load(here("01_donnees_def.rda"))

# agebcheck_sexe ----------------------------------------------------------

# À faire sur les données restreintes du bandwidth. Voir Kaila (2017). 

# sexe_F. 
agebcheck_p1_sexe_F_tous_rdch30 <- 
  
  lm(sexe_F ~ old + dist + old:dist
     + pcsR,
     hrestr(rdc, 30))

agebcheck_p2_sexe_F_tous_rdch30 <- 
  
  lm(sexe_F ~ old + dist + old:dist
     + I(dist ^ 2) + old:I(dist ^ 2)
     + pcsR,
     hrestr(rdc, 30))

# sexe_M. 
agebcheck_p1_sexe_M_tous_rdch30 <- 
  
  lm(sexe_M ~ old + dist + old:dist + pcsR,
     hrestr(rdc, 30))

agebcheck_p2_sexe_M_tous_rdch30 <- 
  
  lm(sexe_M ~ old + dist + old:dist
     + I(dist ^ 2) + old:I(dist ^ 2)
     + pcsR,
     hrestr(rdc, 30))

# agebcheck ---------------------------------------------------------------

# pcs_g2_Defav. 
agebcheck_p1_pcs_g2_Defav_tous_rdch30 <- 
  
  lm(pcs_g2_Defav ~ old + dist + old:dist + sexe,
     hrestr(rdc, 30))

agebcheck_p2_pcs_g2_Defav_tous_rdch30 <- 
  
  lm(pcs_g2_Defav ~ old + dist + old:dist
     + I(dist ^ 2) + old:I(dist ^ 2)
     + sexe,
     hrestr(rdc, 30))

# pcs_g2_Moy. 
agebcheck_p1_pcs_g2_Moy_tous_rdch30 <- 
  
  lm(pcs_g2_Moy ~ old + dist + old:dist + sexe,
     hrestr(rdc, 30))

agebcheck_p2_pcs_g2_Moy_tous_rdch30 <- 
  
  lm(pcs_g2_Moy ~ old + dist + old:dist
     + I(dist ^ 2) + old:I(dist ^ 2)
     + sexe,
     hrestr(rdc, 30))

# pcs_g2_Fav. 
agebcheck_p1_pcs_g2_Fav_tous_rdch30 <- 
  
  lm(pcs_g2_Fav ~ old + dist + old:dist + sexe,
     hrestr(rdc, 30))

agebcheck_p2_pcs_g2_Fav_tous_rdch30 <- 
  
  lm(pcs_g2_Fav ~ old + dist + old:dist
     + I(dist ^ 2) + old:I(dist ^ 2)
     + sexe,
     hrestr(rdc, 30))

# pcs_g2_Tresfav. 
agebcheck_p1_pcs_g2_Tresfav_tous_rdch30 <- 
  
  lm(pcs_g2_Tresfav ~ old + dist + old:dist + sexe,
     hrestr(rdc, 30))

agebcheck_p2_pcs_g2_Tresfav_tous_rdch30 <- 
  
  lm(pcs_g2_Tresfav ~ old + dist + old:dist
     + I(dist ^ 2) + old:I(dist ^ 2)
     + sexe,
     hrestr(rdc, 30))

# pcs_g2_Autres. 
agebcheck_p1_pcs_g2_Autres_tous_rdch30 <- 
  
  lm(pcs_g2_Autres ~ old + dist + old:dist + sexe,
     hrestr(rdc, 30))

agebcheck_p2_pcs_g2_Autres_tous_rdch30 <- 
  
  lm(pcs_g2_Autres ~ old + dist + old:dist
     + I(dist ^ 2) + old:I(dist ^ 2)
     + sexe,
     hrestr(rdc, 30))

# pcs_g2_NA. 
agebcheck_p1_pcs_g2_NA_tous_rdch30 <- 
  
  lm(pcs_g2_NA ~ old + dist + old:dist + sexe,
     hrestr(rdc, 30))

agebcheck_p2_pcs_g2_NA_tous_rdch30 <- 
  
  lm(pcs_g2_NA ~ old + dist + old:dist
     + I(dist ^ 2) + old:I(dist ^ 2)
     + sexe,
     hrestr(rdc, 30))


# Inférence et narsq ------------------------------------------------------

for (i in ls()[str_detect(ls(), "^agebcheck")]) {
  assign(paste("ct.", i, sep = ""),
         rsearellano(get(i)))
  assign(paste("n.", i, sep = ""), nobs(get(i)))
  assign(paste("arsq.", i, sep = ""), ext_adjrsq(get(i)))
}

# Sauvegarde, light -------------------------------------------------------

save(list = ls()[str_detect(ls(), "agebcheck") & 
                   ! str_detect(ls(), "^agebcheck")],
     file = here("agebcheck_def2", "agebcheck.rda"),
     version = 2)

