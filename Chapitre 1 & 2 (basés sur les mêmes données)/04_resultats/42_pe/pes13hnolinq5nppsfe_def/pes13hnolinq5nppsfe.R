# pes13hnolinq5nppsfe. (def). 

library(here)
library(tidyverse)
library(fixest)
library(lmtest)
library(stringi)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions2.rda")
load(here("01_01_donnees_def.rda"))
load(here("datrestr_2_def", "datrestr_2_def.rda"))
source(here("pedepvars.R"))
source(here("pespecs.R"))
load(here("peclniv.rda"))
source(here("pepeersindivs.R"))
# pedepvars <- pedepvars[str_detect(pedepvars, "_norm")][1] # ok.

# Sur dat et datrestr_2 ---------------------------------------------------

for (k in c("dat", "datrestr_2")) {
for (j in c("sA_s13", "sB_s13", "sC_s13", "sD_s13",
            "sDexo_s13")) {
for (i in pedepvars %>% 
     (function (p) p[str_detect(p, "_norm")])) {
  assign(paste("pes13hnolinq5nppsfe_", j, "_",
               i,
               "_tous_", k,
               sep = ""),
         feols(as.formula(paste(
           i, " ~ ",
           c(peersindivsnp_q5, "q5score",
             get(j)) %>% 
             paste(collapse = " + "),
           " | rneconstatses + ecolecoh"
         )),
         data = get(k)))
}
}
}

# w_clnivsup --------------------------------------------------------------

for (k in c("dat", "datrestr_2")) {
for (j in c("sA_s13", "sB_s13", "sC_s13", "sD_s13",
            "sDexo_s13")) {
for (i in pedepvars %>% 
     (function (p) p[str_detect(p, "_norm")])) {
  assign(paste("pes13hnolinq5nppsfe_", j, "_",
               i,
               "_tous_", k, "_wclnivsup",
               sep = ""),
         feols(as.formula(paste(
           i, " ~ ",
           c(peersindivsnp_q5, "q5score",
             get(j)) %>% 
             paste(collapse = " + "),
           " | rneconstatses + ecolecoh"
         )),
         data = get(k) %>% 
           (function (d) rbind(datcompl_clniv_sup, d))))
}
}
}

# w_clnivinf --------------------------------------------------------------

for (k in c("dat", "datrestr_2")) {
for (j in c("sA_s13", "sB_s13", "sC_s13", "sD_s13",
            "sDexo_s13")) {
for (i in pedepvars %>% 
     (function (p) p[str_detect(p, "_norm")])) {
  assign(paste("pes13hnolinq5nppsfe_", j, "_",
               i,
               "_tous_", k, "_wclnivinf",
               sep = ""),
         feols(as.formula(paste(
           i, " ~ ",
           c(peersindivsnp_q5, "q5score",
             get(j)) %>% 
             paste(collapse = " + "),
           " | rneconstatses + ecolecoh"
         )),
         data = get(k) %>% 
           (function (d) rbind(d, datcompl_clniv_inf))))
}
}
}

# Inférence et narsq ------------------------------------------------------

for (i in ls()[str_detect(ls(), "^pes13")]) {
  assign(paste("ct.", i, sep = ""),
         get(i)$coeftable %>% as.matrix %>% 
           structure(class = "coeftest"))
  assign(paste("n.", i, sep = ""), nobs(get(i)))
  assign(paste("arsq.", i, sep = ""), r2(get(i))["war2"])
}

# Sauvegarde --------------------------------------------------------------

save(list = ls()[str_detect(ls(), "pes13hnolinq5") & 
                  ! str_detect(ls(), "^pes13hnolinq5")],
     file = here("pes13hnolinq5nppsfe_def", 
                 "pes13hnolinq5nppsfe.rda"),
     version = 2)
