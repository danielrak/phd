# s13hnolinq5chgeinstpik, tot, o1, dr2. 

library(here)
source("s13_libraries.R")
rm(list = ls())

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions2.rda")
load(here("res_datrestr_2", "01_datrestr_2.rda"))

# Préparation -------------------------------------------------------------

depvar <- "moy_ec1"
peers <- paste("p_q5score_tot_q", (1:5)[- 3], sep = "") %>% paste(": p : q5score_tot", collapse = " + ")
indiv <- "q5score_tot"

source("s13chgeinstpik_specs.R")

# A -----------------------------------------------------------------------

s13hnolinq5chgeinspik_sA_o1_dr2 <- plm(as.formula(paste(depvar, " ~ ", peers, " + ", indiv, " + ", sA_chgeinstpik, sep  = "")), 
                                 data = datrestr_2, subset = obs == 1, index = "rneconstat")

# B -----------------------------------------------------------------------

s13hnolinq5chgeinspik_sB_o1_dr2 <- plm(as.formula(paste(depvar, " ~ ", peers, " + ", indiv, " + ", sB_chgeinstpik %>% paste(collapse = " + "), sep  = "")), 
                                 data = datrestr_2, subset = obs == 1, index = "rneconstat")

# C -----------------------------------------------------------------------

s13hnolinq5chgeinspik_sC_o1_dr2 <- plm(as.formula(paste(depvar, " ~ ", peers, " + ", indiv, " + ", sC_chgeinstpik %>% paste(collapse = " + "), sep  = "")), 
                                 data = datrestr_2, subset = obs == 1, index = "rneconstat")

# D -----------------------------------------------------------------------

s13hnolinq5chgeinspik_sD_o1_dr2 <- plm(as.formula(paste(depvar, " ~ ", peers, " + ", indiv, " + ", sD_chgeinstpik %>% paste(collapse = " + "), sep  = "")), 
                                 data = datrestr_2, subset = obs == 1, index = "rneconstat")

# Inférence et narsq ------------------------------------------------------

for (i in ls()[str_detect(ls(), "^s13hnolinq5")]) {
  assign(paste("ct.", i, sep = ""),
         rsearellano(get(i)))
  assign(paste("n.", i, sep = ""), nobs(get(i)))
  assign(paste("arsq.", i, sep = ""), summary(get(i))$r.squared["adjrsq"])
}

# Sauvegarde --------------------------------------------------------------

save(list = ls()[str_detect(ls(), "^ct\\.|^n\\.|^arsq\\.")], 
     file = here("res_datrestr_2", "s13hnolinq5chgeinstpik", 
                 "s13hnolinq5chgeinstpik_o1_dr2.rda"), version = 2)
