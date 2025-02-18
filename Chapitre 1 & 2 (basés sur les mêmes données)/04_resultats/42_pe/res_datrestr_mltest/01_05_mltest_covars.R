# mltest_covars (BM18)

library(tidyverse)
library(magrittr)
library(here)
library(nnet)
library(lmtest)

load("D:/00_phd/00_fonctions/fonctions.rda")
load("01_donnees.rda")

# Préaration --------------------------------------------------------------

  # Sans les tailles de classe 2. 
dat <- group_by(dat, rneconstatses) %>% 
  mutate(ncl0 = length(unique(divconstatrneses))) %>% 
  ungroup %>% filter(ncl0 >= 2)

  # Sélection des variables. 
dat2 <- select(dat, ine_mod, rneconstatses, rneconstat, statut_constat,
               divconstatrneses, score_ori, 
               sexe_mod_M, pcs_reg_mod_Moy, pcs_reg_mod_Fav, pcs_reg_mod_Tresfav, pcs_reg_mod_Autres,
               lregime_constat_g_int, lregime_constat_g_ext, 
               age_absdnb, positiondnb2_Heure, positiondnb2_Avance)

# mltest ------------------------------------------------------------------

datrestr_mltest_covars <- split(dat2, dat2$rneconstatses) %>% 
  sapply(function (x) lrtest(multinom(divconstatrneses ~ score_ori 
                                      + sexe_mod_M + pcs_reg_mod_Moy + pcs_reg_mod_Fav + pcs_reg_mod_Tresfav + pcs_reg_mod_Autres
                                      + lregime_constat_g_int + lregime_constat_g_ext 
                                      + age_absdnb + positiondnb2_Heure + positiondnb2_Avance, na.omit(x)),
                             multinom(divconstatrneses ~ 1 
                                      + sexe_mod_M + pcs_reg_mod_Moy + pcs_reg_mod_Fav + pcs_reg_mod_Tresfav + pcs_reg_mod_Autres
                                      + lregime_constat_g_int + lregime_constat_g_ext
                                      + age_absdnb + positiondnb2_Heure + positiondnb2_Avance,
                                      na.omit(x)))[["Pr(>Chisq)"]][2]) %>% as.data.frame %>% 
  (function (d) data.frame(e = rownames(d), pval = d[[1]])) %>% 
  filter(pval <= .1354) %>% pull(e) %>% 
  substr(1, 8) %>% table %>% (function (t) t[t >= 2]) %>% 
  (function (e) filter(dat, ! rneconstat %in% names(e))) %>% mutate(cg = droplevels(cg))

# Sauvegarde --------------------------------------------------------------

save(datrestr_mltest_covars, file = here("res_datrestr_mltest", "01_datrestr_mltest_covars.rda"))
