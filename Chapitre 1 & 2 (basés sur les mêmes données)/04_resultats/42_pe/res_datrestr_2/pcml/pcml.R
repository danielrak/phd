# Résultats pseudo cml. 

library(maxLik)
library(numDeriv)
library(here)
library(tidyverse)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions.rda")
load(here("res_datrestr_2", "01_datrestr_2.rda"))

# Estimations -------------------------------------------------------------

pcml <- pseudo_cml(
  data = split(datrestr_2, datrestr_2$divconstatrneses) %>% 
    lapply(function (x) 
      data.frame(s = sum(is.na(x$moy_ec1)), n = nrow(x))) %>% 
    do.call(what = rbind) %>% filter(s == 26) %>% rownames %>% 
    (function (r) filter(datrestr_2, divconstatrneses != r)), 
  depvar = "moy_ec1",
  covars = c("sexe_mod_M", "pcs_reg_mod_Moy", 
             "pcs_reg_mod_Fav", "pcs_reg_mod_Tresfav",
             "pcs_reg_mod_Autres",
             "lregime_constat_g_int", "lregime_constat_g_ext",
             "age_absdnb", "positiondnb2_Heure", 
             "positiondnb2_Avance"),
  group = "divconstatrneses"
) %>% summary

pcml_french <- pseudo_cml(
  data = datrestr_2, 
  depvar = "moy_fran_ec",
  covars = c("sexe_mod_M", "pcs_reg_mod_Moy", 
             "pcs_reg_mod_Fav", "pcs_reg_mod_Tresfav",
             "pcs_reg_mod_Autres",
             "lregime_constat_g_int", "lregime_constat_g_ext",
             "age_absdnb", "positiondnb2_Heure", 
             "positiondnb2_Avance"),
  group = "divconstatrneses"
) %>% summary

pcml_red <- pseudo_cml(
  data = datrestr_2, 
  depvar = "moy_red_ec",
  covars = c("sexe_mod_M", "pcs_reg_mod_Moy", 
             "pcs_reg_mod_Fav", "pcs_reg_mod_Tresfav",
             "pcs_reg_mod_Autres",
             "lregime_constat_g_int", "lregime_constat_g_ext",
             "age_absdnb", "positiondnb2_Heure", 
             "positiondnb2_Avance"),
  group = "divconstatrneses"
) %>% summary

pcml_dic <- pseudo_cml(
  data = datrestr_2, 
  depvar = "moy_dic_ec",
  covars = c("sexe_mod_M", "pcs_reg_mod_Moy", 
             "pcs_reg_mod_Fav", "pcs_reg_mod_Tresfav",
             "pcs_reg_mod_Autres",
             "lregime_constat_g_int", "lregime_constat_g_ext",
             "age_absdnb", "positiondnb2_Heure", 
             "positiondnb2_Avance"),
  group = "divconstatrneses"
) %>% summary

pcml_maths <- pseudo_cml(
  data = datrestr_2, 
  depvar = "moy_maths_ec",
  covars = c("sexe_mod_M", "pcs_reg_mod_Moy", 
             "pcs_reg_mod_Fav", "pcs_reg_mod_Tresfav",
             "pcs_reg_mod_Autres",
             "lregime_constat_g_int", "lregime_constat_g_ext",
             "age_absdnb", "positiondnb2_Heure", 
             "positiondnb2_Avance"),
  group = "divconstatrneses"
) %>% summary

pcml_hist <- pseudo_cml(
  data = split(datrestr_2, datrestr_2$divconstatrneses) %>% 
    lapply(function (x) 
      data.frame(s = sum(is.na(x$moy_hist_ec)), n = nrow(x))) %>% 
    do.call(what = rbind) %>% filter(s == 26) %>% rownames %>% 
    (function (r) filter(datrestr_2, divconstatrneses != r)), 
  depvar = "moy_hist_ec",
  covars = c("sexe_mod_M", "pcs_reg_mod_Moy", 
             "pcs_reg_mod_Fav", "pcs_reg_mod_Tresfav",
             "pcs_reg_mod_Autres",
             "lregime_constat_g_int", "lregime_constat_g_ext",
             "age_absdnb", "positiondnb2_Heure", 
             "positiondnb2_Avance"),
  group = "divconstatrneses"
) %>% summary


# Sauvegarde --------------------------------------------------------------

save(list = ls()[str_detect(ls(), "^pcml")], 
     file = here("res_datrestr_2", "pcml.rda"), version = 2)
