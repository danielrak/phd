# agecutcheck. (def). Voir Imbens et Lemieux (2008). 

library(here)
library(tidyverse)
library(plm)
library(lmtest)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions.rda")
load(here("01_donnees_def.rda"))

# Préparation -------------------------------------------------------------

rdcl <- filter(rdc, old == 0)
medl <- rdcl %>% pull(dist) %>% median
rdcl <- mutate(rdcl, oldmedl = as.numeric(dist >= medl))

rdcr <- filter(rdc, old == 1)
medr <- rdcr %>% pull(dist) %>% median
rdcr <- mutate(rdcr, oldmedr = as.numeric(dist >= medr))

# agecutcheckl ------------------------------------------------------------

# Cette fois il faut utiliser tout l'échantillon. 

agecutcheckl_p1_score_norm_tous_rdc <- 
  
  lm(score_norm ~ oldmedl + dist + oldmedl + dist
     + sexe + pcsR, rdcl)

agecutcheckl_p2_score_norm_tous_rdc <- 
  
  lm(
    score_norm ~ oldmedl + dist + oldmedl + dist
    + I(dist ^ 2) + oldmedl:I(dist ^ 2)
    + sexe + pcsR,
    rdcl
  )

# agecutcheckr ------------------------------------------------------------

agecutcheckr_p1_score_norm_tous_rdc <- 
  
  lm(score_norm ~ oldmedr + dist + oldmedr + dist
     + sexe + pcsR, rdcr)

agecutcheckr_p2_score_norm_tous_rdc <- 
  
  lm(
    score_norm ~ oldmedr + dist + oldmedr + dist
    + I(dist ^ 2) + oldmedr:I(dist ^ 2)
    + sexe + pcsR,
    rdcr
  )

# Inférence et narsq ------------------------------------------------------

for (i in ls()[str_detect(ls(), "^agecutcheck")]) {
  assign(paste("ct.", i, sep = ""),
         rsearellano(get(i)))
  assign(paste("n.", i, sep = ""), nobs(get(i)))
  assign(paste("arsq.", i, sep = ""), ext_adjrsq(get(i)))
}

# Sauvegarde, light -------------------------------------------------------

save(list = ls()[str_detect(ls(), "agecutcheck") & 
                   ! str_detect(ls(), "^agecutcheck")],
     file = here("agecutcheck_def2", "agecutcheck.rda"),
     version = 2)
