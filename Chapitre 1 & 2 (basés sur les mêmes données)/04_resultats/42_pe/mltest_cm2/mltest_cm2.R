# mltest_cm2. 

library(here)
library(tidyverse)
library(fastDummies)
library(lmtest)
library(nnet)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions.rda")
load("D:/00_phd/02_rectorat/02_v2_construction_def/01_construction/04_pre_analyse_cm2.rda")

# Préparation -------------------------------------------------------------

cm2 <- dummy_cols(cm2, c("sexe", "pcs_g2", "mois", "position"))

cm2$pcs_g2_Moy[is.na(cm2$pcs)] <- 0
cm2$pcs_g2_Fav[is.na(cm2$pcs)] <- 0
cm2$pcs_g2_Tresfav[is.na(cm2$pcs)] <- 0
cm2$pcs_g2_Autres[is.na(cm2$pcs)] <- 0

c <- select(cm2, ecolecoh, classecoh, sexe_M, pcs_g2_Moy, pcs_g2_Fav, pcs_g2_Tresfav, pcs_g2_Autres, pcs_g2_NA,
            mois_2, mois_3, mois_4, mois_5, mois_6, mois_7, mois_8, mois_9, mois_10, mois_11, mois_12, 
            position_Heure, position_Avance) # 42030. 
c <- group_by(c, ecolecoh) %>% mutate(ncl = length(unique(classecoh))) %>% ungroup %>% 
  filter(ncl >= 2) %>% select(- ncl)

# Tests -------------------------------------------------------------------

mltest_cm2tab <- split(c, c$ecolecoh) %>% 
  sapply(function (x) lrtest(multinom(classecoh ~ pcs_g2_Moy + pcs_g2_Fav + pcs_g2_Tresfav + pcs_g2_Autres + pcs_g2_NA, x),
                             multinom(classecoh ~ 1, x))[["Pr(>Chisq)"]][2]) %>% as.data.frame %>% 
  (function (d) data.frame(e = rownames(d), pval = d[[1]]))

  # résultat : 81 écoles sur 352 rejettent l'hypothèse nulle.

# Sauvegarde --------------------------------------------------------------

save(mltest_cm2tab, file = here("mltest_cm2", "mltest_cm2tab.rda"), version = 2)