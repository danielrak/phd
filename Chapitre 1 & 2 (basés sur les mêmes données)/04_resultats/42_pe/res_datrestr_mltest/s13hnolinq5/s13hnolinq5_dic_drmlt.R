# s13hnolinq5, french

library(tidyverse)
library(here)
library(plm)
library(lmtest)
library(magrittr)
library(stringi)

rm(list = ls())

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions.rda")
load(here("res_datrestr_mltest", "01_datrestr_mltest.rda"))

# Préparation -------------------------------------------------------------

depvar <- "moy_dic_ec"
indivs <- paste("q5score_tot_q", (1:5)[- 3], sep = "")
peersindivs <- paste("p_q5score_tot_q", (1:5)[- 3], " : p : ", 
                     sep = "") %>% 
  (function (p) sapply(paste("q5score_tot_q", 1:5, sep = ""), 
                       function (x) paste(p, x, sep = "")) %>% 
     as.vector %>% sort)
sA <- "p"
sB <- c(sA, "sexe_mod_M",
        "pcs_reg_mod_Moy", "pcs_reg_mod_Fav", "pcs_reg_mod_Tresfav",
        "pcs_reg_mod_Autres",
        "lregime_constat_g_int", "lregime_constat_g_ext",
        "age_absdnb", 
        "positiondnb2_Heure", "positiondnb2_Avance")
sC <- c(sB, "tdivconstatrneses")
sD <-c(sC, 
       "p_sexe_mod_M", 
       "p_pcs_reg_mod_Moy", "p_pcs_reg_mod_Fav", 
       "p_pcs_reg_mod_Tresfav", "p_pcs_reg_mod_Autres",
       "p_lregime_constat_g_int", "p_lregime_constat_g_ext",
       "p_age_absdnb", 
       "p_positiondnb2_Heure", "p_positiondnb2_Avance")

# A -----------------------------------------------------------------------

s13hnolinq5_sA_dic_drmlt <- 
  plm(as.formula(paste(depvar, " ~ ", 
                       indivs %>% paste(collapse = " + "), " + ", 
                       peersindivs %>% paste(collapse = " + "), " + ",
                       sA,
                       sep = "")),
      data = datrestr_mltest, index = "rneconstatses")

# B -----------------------------------------------------------------------

s13hnolinq5_sB_dic_drmlt <- 
  plm(as.formula(paste(depvar, " ~ ", 
                       indivs %>% paste(collapse = " + "), " + ", 
                       peersindivs %>% paste(collapse = " + "), " + ",
                       sB %>% paste(collapse = " + "),
                       sep = "")),
      data = datrestr_mltest, index = "rneconstatses")

# C -----------------------------------------------------------------------

s13hnolinq5_sC_dic_drmlt <- 
  plm(as.formula(paste(depvar, " ~ ", 
                       indivs %>% paste(collapse = " + "), " + ", 
                       peersindivs %>% paste(collapse = " + "), " + ",
                       sC %>% paste(collapse = " + "),
                       sep = "")),
      data = datrestr_mltest, index = "rneconstatses")

# D -----------------------------------------------------------------------

s13hnolinq5_sD_dic_drmlt <- 
  plm(as.formula(paste(depvar, " ~ ", 
                       indivs %>% paste(collapse = " + "), " + ", 
                       peersindivs %>% paste(collapse = " + "), " + ",
                       sD %>% paste(collapse = " + "),
                       sep = "")),
      data = datrestr_mltest, index = "rneconstatses")

# Inférence et narsq ------------------------------------------------------

for (i in ls()[str_detect(ls(), "^s13")]) {
  assign(paste("ct.", i, sep = ""),
         rsearellano(get(i)))
  assign(paste("n.", i, sep = ""), nobs(get(i)))
  assign(paste("arsq.", i, sep = ""), summary(get(i))$r.squared["adjrsq"])
  assign(paste("mvc.", i, sep = ""), vcovHC(get(i)))
}

# Sauvegarde --------------------------------------------------------------

save(list = ls()[str_detect(ls(), "^ct\\.|^n\\.|^arsq\\.|^mvc\\.")], 
     file = here("res_datrestr_mltest", "s13hnolinq5", 
                 "s13hnolinq5_dic_drmlt.rda"), version = 2)