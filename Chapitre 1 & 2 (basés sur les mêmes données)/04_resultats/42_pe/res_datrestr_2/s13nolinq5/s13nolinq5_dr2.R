# s13nolinq5, tot, o1, dr2, 

library(here)
library(tidyverse)
library(plm)
library(lmtest)

rm(list = ls())

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions.rda")
load(here("res_datrestr_2", "01_datrestr_2.rda"))

# Préparation -------------------------------------------------------------

depvar <- "moy_ec1"
peers <- paste("p_q5score_tot_q", (1:5)[- 3], " : p", sep = "")
indiv <- "v_ob_tot"
source("s13_specs.R")

# A -----------------------------------------------------------------------

s13nolinq5_sA_o1_dr2 <- plm(as.formula(paste(depvar, " ~ ", peers %>% paste(collapse = " + "), " + ", indiv, " + ", sA, sep = "")),
                          data = datrestr_2, subset = obs == 1, index = "rneconstatses")

# B -----------------------------------------------------------------------

s13nolinq5_sB_o1_dr2 <- plm(as.formula(paste(depvar, " ~ ", peers %>% paste(collapse = " + "), " + ", indiv, " + ", sB, sep = "")),
                          data = datrestr_2, subset = obs == 1, index = "rneconstatses")

# C -----------------------------------------------------------------------

s13nolinq5_sC_o1_dr2 <- plm(as.formula(paste(depvar, " ~ ", peers %>% paste(collapse = " + "), " + ", indiv, " + ", sC, sep = "")),
                          data = datrestr_2, subset = obs == 1, index = "rneconstatses")

# D -----------------------------------------------------------------------

s13nolinq5_sD_o1_dr2 <- plm(as.formula(paste(depvar, " ~ ", peers %>% paste(collapse = " + "), " + ", indiv, " + ", sD, sep = "")),
                          data = datrestr_2, subset = obs == 1, index = "rneconstatses")

# Inférence et narsq ------------------------------------------------------

for (i in ls()[str_detect(ls(), "^s13")]) {
  assign(paste("ct.", i, sep = ""),
         rsearellano(get(i)))
  assign(paste("n.", i, sep = ""), nobs(get(i)))
  assign(paste("arsq.", i, sep = ""), summary(get(i))$r.squared["adjrsq"])
}

# Sauvegarde --------------------------------------------------------------

save(list = ls()[str_detect(ls(), "^ct\\.|^n\\.|^arsq\\.")], 
     file = here("res_datrestr_2", "s13nolinq5", 
                 "s13nolinq5_o1_dr2.rda"), version = 2)
