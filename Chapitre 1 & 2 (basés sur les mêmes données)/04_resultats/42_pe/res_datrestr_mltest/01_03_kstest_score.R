# KS test - AFSE. 
# NB : ne marche pas avec job, raison inconnue. 
# raison : fonction listN imbriquée dans kstester(), ne marche plus


library(tidyverse)
library(magrittr)
library(here)

load("D:/00_phd/00_fonctions/fonctions.rda")
load("01_donnees.rda")


# Préparation -------------------------------------------------------------

  # Sans les tailles de classe 2
dat <- group_by(dat, rneconstatses) %>% 
  mutate(ncl0 = length(unique(divconstatrneses))) %>% ungroup %>% 
  filter(ncl0 >= 2)
  
  # Sélection de variables
dat <- select(dat, ine_mod, rneconstatses, divconstatrneses, 
              score_ori, z_dnb, age_absdnb)

  # Moyennes par classe
dat <- group_by(dat, divconstatrneses) %>% 
  mutate(mc_score_ori = mean(score_ori, na.rm = TRUE),
         mc_z_dnb = mean(z_dnb, na.rm = TRUE),
         mc_age_absdnb = mean(age_absdnb, na.rm = TRUE)) %>%
  ungroup

  # r
R <- 1000
  
# score -----------------------------------------------------------------

kstest_score <- kstester(data = dat, 
                         gsup = "rneconstatses", ginf = "divconstatrneses",
                         var = "score_ori", mvar = "mc_score_ori",
                         r = R, detailed = TRUE)

# Sauvegarde --------------------------------------------------------------

save(kstest_score, 
     file = here(paste("01_03_kstest_score", "_r", R, ".rda", sep = "")), 
     version = 2)
