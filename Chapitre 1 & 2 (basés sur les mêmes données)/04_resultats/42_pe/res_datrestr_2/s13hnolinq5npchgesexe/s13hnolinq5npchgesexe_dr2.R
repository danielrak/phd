# s13hnolinq5npchgesexe, tot, dr2. 

library(here)
source("s13_libraries.R")

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions.rda")
load(here("res_datrestr_2", "01_datrestr_2.rda"))

# Préparation -------------------------------------------------------------

depvar <- "moy_ec1"
peers <- paste("p_q5score_tot_q", (1:5)[- 3], " : p : q5score_tot", sep = "")
indiv <- "q5score_tot"
source("s13_specs_chge.R")

# A -----------------------------------------------------------------------

for (i in c("F", "M")) {
  assign(paste("s13hnolinq5npchge_sA_o1_", i, "_dr2", sep = ""),
         plm(as.formula(paste(depvar, " ~ ", peers, " + ", indiv, " + ", sA_chge, sep = "")), 
             data = datrestr_2, subset = obs == 1 & sexe_mod == i, index = "rneconstatses"))
}

# B -----------------------------------------------------------------------

for (i in c("F", "M")) {
  assign(paste("s13hnolinq5npchge_sB_o1_", i, "_dr2", sep = ""),
         plm(as.formula(paste(depvar, " ~ ", peers, " + ", indiv, " + ", sB_chge %>% 
                                (function (x) x[! str_detect(x, "^sexe_mod_")]) %>% paste(collapse = " + "), sep = "")), 
             data = datrestr_2, subset = obs == 1 & sexe_mod == i, index = "rneconstatses"))
}

# C -----------------------------------------------------------------------

for (i in c("F", "M")) {
  assign(paste("s13hnolinq5npchge_sC_o1_", i, "_dr2", sep = ""),
         plm(as.formula(paste(depvar, " ~ ", peers, " + ", indiv, " + ", sC_chge %>% 
                                (function (x) x[! str_detect(x, "^sexe_mod_")]) %>% paste(collapse = " + "), sep = "")), 
             data = datrestr_2, subset = obs == 1 & sexe_mod == i, index = "rneconstatses"))
}

# F -----------------------------------------------------------------------

for (i in c("F", "M")) {
  assign(paste("s13hnolinq5npchge_sF_o1_", i, "_dr2", sep = ""),
         plm(as.formula(paste(depvar, " ~ ", peers, " + ", indiv, " + ", sD_chge %>% 
                                (function (x) x[! str_detect(x, "^sexe_mod_")]) %>% paste(collapse = " + "), sep = "")), 
             data = datrestr_2, subset = obs == 1 & sexe_mod == i, index = "rneconstatses"))
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
     file = here("res_datrestr_2", "s13hnolinq5npchgesexe", 
                 "s13hnolinq5npchgesexe_o1_dr2.rda"), version = 2)
