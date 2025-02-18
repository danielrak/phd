# KS test - AFSE. 
# NB : ne marche pas avec job, raison inconnue. 


library(tidyverse)
library(magrittr)

load("D:/00_phd/00_fonctions/fonctions.rda")
load("01_donnees.rda")


# Préparation -------------------------------------------------------------

  # Sans les tailles de classe 2
dat <- group_by(dat, rneconstatses) %>% 
  mutate(ncl0 = length(unique(divconstatrneses))) %>% ungroup %>% 
  filter(ncl0 >= 2)
  
  # Sélection de variables
dat <- select(dat, ine_mod, rneconstatses, divconstatrneses, 
              score, z_dnb, age_absdnb)

  # Moyennes par classe
dat <- group_by(dat, divconstatrneses) %>% 
  mutate(mc_score = mean(score, na.rm = TRUE),
         mc_z_dnb = mean(z_dnb, na.rm = TRUE),
         mc_age_absdnb = mean(age_absdnb, na.rm = TRUE)) %>%
  ungroup
  
# score -----------------------------------------------------------------

kstest_score <- kstester(data = dat, 
                         gsup = "rneconstatses", ginf = "divconstatrneses",
                         var = "score", mvar = "mc_score", r = 1000, detailed = TRUE)

save(kstest_score, file = "01_02_kstest_score.rda", version = 2)


# z_dnb -------------------------------------------------------------------


kstest_z_dnb <- kstester(data = dat, 
                         gsup = "rneconstatses", ginf = "divconstatrneses",
                         var = "z_dnb", mvar = "mc_z_dnb", r = 1000, detailed = TRUE)

save(kstest_z_dnb, file = "01_02_kstest_z_dnb.rda", version = 2)



# age_absdnb -------------------------------------------------------------


kstest_age_absdnb <- kstester(data = dat,
                              gsup = "rneconstatses", ginf = "divconstatrneses",
                              var = "age_absdnb", mvar = "mc_age_absdnb", r = 1000, 
                              detailed = TRUE)

save(kstest_age_absdnb, file = "01_02_kstest_age_absdnb.rda", version = 2)