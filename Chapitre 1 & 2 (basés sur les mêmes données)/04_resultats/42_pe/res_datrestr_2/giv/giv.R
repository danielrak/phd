# GIV, Boucher et al. 2014. 

library(here)
library(tidyverse)
library(magrittr)
library(AER)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions.rda")
load(here("res_datrestr_2", "01_datrestr_2.rda"))

# pmean depvar ------------------------------------------------------------

datrestr_2 <- group_by(datrestr_2, divconstatrneses) %>% 
  mutate(p_moy_fran_ec = (sum(moy_fran_ec, na.rm = TRUE) - 
                            moy_fran_ec) / (n() - sum(is.na(moy_fran_ec)) - 1),
         p_moy_maths_ec = (sum(moy_maths_ec, na.rm = TRUE) - 
                             moy_maths_ec) / (n() - sum(is.na(moy_maths_ec)) - 1)) %>% 
  ungroup

# Estimations -------------------------------------------------------------

giv <- bealer(
  data = datrestr_2, 
  depvar = "moy_fran_ec", 
  pmeandepvar = "p_moy_fran_ec",
  controls = c("sexe_mod_M", "pcs_reg_mod_Moy",
               "pcs_reg_mod_Fav", "pcs_reg_mod_Tresfav",
               "pcs_reg_mod_Autres",
               "lregime_constat_g_int", "lregime_constat_g_ext",
               "age_absdnb", "positiondnb2_Heure",
               "positiondnb2_Avance"),
  pmeancontrols = paste("p_", c("sexe_mod_M", "pcs_reg_mod_Moy",
                    "pcs_reg_mod_Fav", "pcs_reg_mod_Tresfav",
                    "pcs_reg_mod_Autres",
                    "lregime_constat_g_int", "lregime_constat_g_ext",
                    "age_absdnb", "positiondnb2_Heure",
                    "positiondnb2_Avance"), sep = ""),
  peerlevel = "divconstatrneses", peerlevelsize = "tdivconstatrneses", 
  felevel = "divconstatrneses")

giv_maths <- bealer(
  data = datrestr_2, 
  depvar = "moy_maths_ec", 
  pmeandepvar = "p_moy_maths_ec",
  controls = c("sexe_mod_M", "pcs_reg_mod_Moy",
               "pcs_reg_mod_Fav", "pcs_reg_mod_Tresfav",
               "pcs_reg_mod_Autres",
               "lregime_constat_g_int", "lregime_constat_g_ext",
               "age_absdnb", "positiondnb2_Heure",
               "positiondnb2_Avance"),
  pmeancontrols = paste("p_", c("sexe_mod_M", "pcs_reg_mod_Moy",
                                "pcs_reg_mod_Fav", "pcs_reg_mod_Tresfav",
                                "pcs_reg_mod_Autres",
                                "lregime_constat_g_int", "lregime_constat_g_ext",
                                "age_absdnb", "positiondnb2_Heure",
                                "positiondnb2_Avance"), sep = ""),
  peerlevel = "divconstatrneses", peerlevelsize = "tdivconstatrneses", 
  felevel = "divconstatrneses")
