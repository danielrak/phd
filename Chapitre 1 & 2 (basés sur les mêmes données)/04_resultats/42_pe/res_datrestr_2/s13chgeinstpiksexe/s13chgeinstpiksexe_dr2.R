# s13chgeinstpiksexe, tot, o1, dr2. 

library(here)
source("s13_libraries.R")

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions.rda")
load(here("res_datrestr_2", "01_datrestr_2.rda"))

# Préparation -------------------------------------------------------------

depvar <- "moy_ec1"
peers <- "p_v_ob_tot : p"
indiv <- "v_ob_tot"
source("s13chgeinstpik_specs.R")

# A -----------------------------------------------------------------------

for (i in c("F", "M")) {
  assign(paste("s13chgeinstpik_sA_o1_", i, "_dr2", sep = ""),
         plm(as.formula(paste(depvar, " ~ ", peers, " + ", indiv, " + ", sA_chgeinstpik, sep = "")),
             data = datrestr_2, subset = obs == 1 & sexe_mod == i, index = "rneconstat"))
}

# B -----------------------------------------------------------------------

for (i in c("F", "M")) {
  assign(paste("s13chgeinstpik_sB_o1_", i, "_dr2", sep = ""),
         plm(as.formula(paste(depvar, " ~ ", peers, " + ", indiv, " + ", sB_chgeinstpik %>% 
                                (function (x) x[! str_detect(x, "^sexe_mod")]) %>% paste(collapse = " + "), sep = "")),
             data = datrestr_2, subset = obs == 1 & sexe_mod == i, index = "rneconstat"))
}

# C -----------------------------------------------------------------------

for (i in c("F", "M")) {
  assign(paste("s13chgeinstpik_sC_o1_", i, "_dr2", sep = ""),
         plm(as.formula(paste(depvar, " ~ ", peers, " + ", indiv, " + ", sC_chgeinstpik %>% 
                                (function (x) x[! str_detect(x, "^sexe_mod")]) %>% paste(collapse = " + "), sep = "")),
             data = datrestr_2, subset = obs == 1 & sexe_mod == i, index = "rneconstat"))
}

# D -----------------------------------------------------------------------

for (i in c("F", "M")) {
  assign(paste("s13chgeinstpik_sD_o1_", i, "_dr2", sep = ""),
         plm(as.formula(paste(depvar, " ~ ", peers, " + ", indiv, " + ", sD_chgeinstpik %>% 
                                (function (x) x[! str_detect(x, "^sexe_mod")]) %>% paste(collapse = " + "), sep = "")),
             data = datrestr_2, subset = obs == 1 & sexe_mod == i, index = "rneconstat"))
}

# Inférence et narsq ------------------------------------------------------

for (i in ls()[str_detect(ls(), "^s13")]) {
  assign(paste("ct.", i, sep = ""),
         rsearellano(get(i)))
  assign(paste("n.", i, sep = ""), nobs(get(i)))
  assign(paste("arsq.", i, sep = ""), summary(get(i))$r.squared["adjrsq"])
}

# Sauvegarde --------------------------------------------------------------

save(list = ls()[str_detect(ls(), "^ct\\.|^n\\.|^arsq\\.")], 
     file = here("res_datrestr_2", "s13chgeinstpiksexe", 
                 "s13chgeinstpiksexe_o1_dr2.rda"), version = 2)
