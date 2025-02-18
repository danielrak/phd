# s13npchgesexe, tot, o1, dr2. 

library(here)
source("s13_libraries.R")

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions.rda")
load(here("res_datrestr_2", "01_datrestr_2.rda"))

# Préparation -------------------------------------------------------------

depvar <- "moy_ec1"
peers <- "pnp_score : p + pop_score : p"
indiv <- "v_ob_tot"
source("s13_specs_chge.R")

# Filles - A --------------------------------------------------------------

s13npchge_sA_o1_F_dr2 <- plm(as.formula(paste(depvar, " ~ ", peers, " + ", indiv, " + ", sA_chge, sep = "")), 
                           data = datrestr_2, subset = obs == 1 & sexe_mod == "F", index = "rneconstatses")

# Filles - B --------------------------------------------------------------

s13npchge_sB_o1_F_dr2 <- plm(as.formula(paste(depvar, " ~ ", peers, " + ", indiv, " + ", sB_chge %>% 
                                              (function (x) x[! str_detect(x, "^sexe_mod")]) %>% paste(collapse = " + "), sep = "")), 
                           data = datrestr_2, subset = obs == 1 & sexe_mod == "F", index = "rneconstatses")

# Filles - C --------------------------------------------------------------

s13npchge_sC_o1_F_dr2 <- plm(as.formula(paste(depvar, " ~ ", peers, " + ", indiv, " + ", sC_chge %>% 
                                              (function (x) x[! str_detect(x, "^sexe_mod")]) %>% paste(collapse = " + "), sep = "")), 
                           data = datrestr_2, subset = obs == 1 & sexe_mod == "F", index = "rneconstatses")

# Filles - F --------------------------------------------------------------
  # rappel : F est fait exprès, c'est la spec dans laquelle on contrôle par pop score mais par p covars uniquement. 

s13npchge_sF_o1_F_dr2 <- plm(as.formula(paste(depvar, " ~ ", peers, " + ", indiv, " + ", sD_chge %>% 
                                              (function (x) x[! str_detect(x, "^sexe_mod")]) %>% paste(collapse = " + "), sep = "")), 
                           data = datrestr_2, subset = obs == 1 & sexe_mod == "F", index = "rneconstatses")

# Garçons - A --------------------------------------------------------------

s13npchge_sA_o1_M_dr2 <- plm(as.formula(paste(depvar, " ~ ", peers, " + ", indiv, " + ", sA_chge, sep = "")), 
                           data = datrestr_2, subset = obs == 1 & sexe_mod == "M", index = "rneconstatses")

# Garçons - B --------------------------------------------------------------

s13npchge_sB_o1_M_dr2 <- plm(as.formula(paste(depvar, " ~ ", peers, " + ", indiv, " + ", sB_chge %>% 
                                              (function (x) x[! str_detect(x, "^sexe_mod")]) %>% paste(collapse = " + "), sep = "")), 
                           data = datrestr_2, subset = obs == 1 & sexe_mod == "M", index = "rneconstatses")

# Garçons - C --------------------------------------------------------------

s13npchge_sC_o1_M_dr2 <- plm(as.formula(paste(depvar, " ~ ", peers, " + ", indiv, " + ", sC_chge %>% 
                                              (function (x) x[! str_detect(x, "^sexe_mod")]) %>% paste(collapse = " + "), sep = "")), 
                           data = datrestr_2, subset = obs == 1 & sexe_mod == "M", index = "rneconstatses")

# Garçons - F --------------------------------------------------------------
  # rappel : F est fait exprès, c'est la spec dans laquelle on contrôle par pop score mais par p covars uniquement. 

s13npchge_sF_o1_M_dr2 <- plm(as.formula(paste(depvar, " ~ ", peers, " + ", indiv, " + ", sD_chge %>% 
                                              (function (x) x[! str_detect(x, "^sexe_mod")]) %>% paste(collapse = " + "), sep = "")), 
                           data = datrestr_2, subset = obs == 1 & sexe_mod == "M", index = "rneconstatses")

# Inférence et narsq ------------------------------------------------------

for (i in ls()[str_detect(ls(), "^s13")]) {
  assign(paste("ct.", i, sep = ""),
         rsearellano(get(i)))
  assign(paste("n.", i, sep = ""), nobs(get(i)))
  assign(paste("arsq.", i, sep = ""), summary(get(i))$r.squared["adjrsq"])
}

# Sauvegarde --------------------------------------------------------------

save(list = ls()[str_detect(ls(), "^ct\\.|^n\\.|^arsq\\.")], 
     file = here("res_datrestr_2", "s13npchgesexe", 
                 "s13npchgesexe_o1_dr2.rda"), version = 2)
