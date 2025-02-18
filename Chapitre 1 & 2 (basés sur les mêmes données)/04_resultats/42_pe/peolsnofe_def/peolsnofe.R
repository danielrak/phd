# peolsnofe (def). 

library(here)
library(tidyverse)
library(plm)
library(lmtest)
library(stringi)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions2.rda")
load(here("01_01_donnees_def.rda"))
load(here("datrestr_2_def", "datrestr_2_def.rda"))
source(here("pedepvars.R"))
source(here("pespecs.R"))
load(here("peclniv.rda"))

# Sur dat et datrestr_2 ---------------------------------------------------

for (k in c("dat", "datrestr_2")) {
for (j in c("sA_ols", "sB_ols", "sC_ols", "sD_ols",
            "sDexo_ols")) {
for (i in pedepvars %>% 
     (function (p) p[str_detect(p, "_norm")])) {
  assign(paste("peolsnofe_", j, "_",
               i,
               "_tous_", k,
               sep = ""),
         lm(as.formula(paste(
           i, " ~ ",
           c("p_score_norm", "score_norm",
             get(j)) %>% 
             paste(collapse = " + ")
         )),
         data = get(k)))
}
}
}

# w_clnivsup --------------------------------------------------------------

for (k in c("dat", "datrestr_2")) {
for (j in c("sA_ols", "sB_ols", "sC_ols", "sD_ols",
            "sDexo_ols")) {
for (i in pedepvars %>% 
     (function (p) p[str_detect(p, "_norm")])) {
  assign(paste("peolsnofe_", j, "_",
               i,
               "_tous_", k, "_wclnivsup",
               sep = ""),
         lm(as.formula(paste(
           i, " ~ ",
           c("p_score_norm", "score_norm",
             get(j)) %>% 
             paste(collapse = " + ")
         )),
         data = get(k) %>% 
           (function (d) rbind(datcompl_clniv_sup, d))))
}
}
}

# w_clnivinf --------------------------------------------------------------

for (k in c("dat", "datrestr_2")) {
for (j in c("sA_ols", "sB_ols", "sC_ols", "sD_ols",
            "sDexo_ols")) {
for (i in pedepvars %>% 
     (function (p) p[str_detect(p, "_norm")])) {
  assign(paste("peolsnofe_", j, "_",
               i,
               "_tous_", k, "_wclnivinf",
               sep = ""),
         lm(as.formula(paste(
           i, " ~ ",
           c("p_score_norm", "score_norm",
             get(j)) %>% 
             paste(collapse = " + ")
         )),
         data = get(k) %>% 
           (function (d) rbind(d, datcompl_clniv_inf))))
}
}
}

# Inférence et narsq ------------------------------------------------------

for (i in ls()[str_detect(ls(), "^peols")]) {
  assign(paste("ct.", i, sep = ""),
         rsearellano(get(i)))
  assign(paste("n.", i, sep = ""), nobs(get(i)))
  assign(paste("arsq.", i, sep = ""), ext_adjrsq(get(i)))
}

# Sauvegarde --------------------------------------------------------------

save(list = ls()[str_detect(ls(), "peols") & 
                  ! str_detect(ls(), "^peols")],
     file = here("peolsnofe_def", "peolsnofe.rda"),
     version = 2)
