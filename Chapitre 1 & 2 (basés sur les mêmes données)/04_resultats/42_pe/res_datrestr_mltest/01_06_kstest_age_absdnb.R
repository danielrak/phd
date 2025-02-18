# KS test - AFSE. 
# NB : ne marche pas avec job, raison inconnue. 
# raison : fonction listN imbriquée dans kstester(), ne marche plus
  # Version newcode. 

library(tidyverse)
library(magrittr)
library(here)

load("D:/00_phd/00_fonctions/fonctions2.rda")
load("01_donnees.rda")

# score -----------------------------------------------------------------

R <- 1000

d <- select(dat, rneconstatses, divconstatrneses, age_absdnb) %>% na.omit

kstest_age_absdnb <- split(d, d$rneconstatses) %>% 
  lapply(function (x) {
    m_obs = tapply(x$age_absdnb, x$divconstatrneses, mean)
    m_sim <- lapply(1:R, function (y) {
      set.seed(y)
      tapply(x$age_absdnb, sample(x$divconstatrneses), mean)
    }) %>% do.call(what = rbind)
    rbind(m_obs, m_sim) %>% 
      (function (r) ks.test(r[1, ], as.vector(r[2:(R + 1), ]))$p.value)
  }) %>% unlist
  # Résultat : pas de classes d'âge détecté parmi les données 
  # sans les sections particulières. 

# Sauvegarde --------------------------------------------------------------

save(kstest_age_absdnb, 
     file = here(paste("01_06_kstest_age_absdnb", "_r", R, ".rda", sep = "")), 
     version = 2)