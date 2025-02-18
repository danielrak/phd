# s13chge, red, testclniv

library(tidyverse)
library(here)
library(plm)
library(lmtest)
library(magrittr)
library(stringi)

rm(list = ls())

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions.rda")
load(here("res_datrestr_2", "01_datrestr_2.rda"))
load(here("01_datcompl.rda"))

# Repérage des clniv ------------------------------------------------------

clniv_sup <- group_by(datcompl, divconstatrneses) %>%
  summarise(c = paste(sort(unique(niveau_section)), collapse = " ET ")) %>%
  filter(c == "sup") %>% pull(divconstatrneses)
# 61 classes-années : 23 en 2014, 16 en 2015 et 22 en 2016.
# vérifié (avec clefgestion_constat) : ce sont les bons.

clniv_inf <- group_by(datcompl, divconstatrneses) %>%
  summarise(c = paste(sort(unique(niveau_section)), collapse = " ET ")) %>%
  filter(c == "inf") %>% pull(divconstatrneses)
# 248 classes-années : 85 en 2014 82 en 2015 et 81 en 2016.

# Préparation -------------------------------------------------------------

depvar <- "moy_red_ec"
peers <- "p_v_ob_tot : p"
indiv <- "v_ob_tot"
sA <- "p + chge"
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


# testclniv ---------------------------------------------------------------

s13chge_sD_wclnivsup_red_o1_dr2 <- 
  plm(as.formula(paste(depvar, " ~ ",
                       peers, " + ",
                       indiv, " + ",
                       sD %>% paste(collapse = " + "),
                       sep = "")),
      data = rbind(select(datrestr_2,-ncl),
                   filter(datcompl, divconstatrneses %in% clniv_sup)), 
      subset = obs == 1,
      index = "rneconstatses")

s13chge_sD_wclnivinf_red_o1_dr2 <- 
  plm(as.formula(paste(depvar, " ~ ",
                       peers, " + ",
                       indiv, " + ",
                       sD %>% paste(collapse = " + "),
                       sep = "")),
      data = rbind(select(datrestr_2,-ncl),
                   filter(datcompl, divconstatrneses %in% clniv_inf)), 
      subset = obs == 1,
      index = "rneconstatses")

# Inférence et narsq ------------------------------------------------------

for (i in ls()[str_detect(ls(), "^s13chge")]) {
  assign(paste("ct.", i, sep = ""),
         rsearellano(get(i)))
  assign(paste("n.", i, sep = ""), nobs(get(i)))
  assign(paste("arsq.", i, sep = ""), summary(get(i))$r.squared["adjrsq"])
}

# Sauvegarde --------------------------------------------------------------

save(list = ls()[str_detect(ls(), "^ct\\.|^n\\.|^arsq\\.|^mvc\\.")], 
     file = here("res_datrestr_2", "s13chge", 
                 "s13chge_testclniv_red_o1_dr2.rda"), version = 2)