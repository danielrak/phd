# agefrd_pcs_g2 (def2). 

library(here)
library(tidyverse)
library(plm)
library(lmtest)
library(AER)
library(ivpack)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions.rda")
load(here("01_donnees_def.rda"))
source(here("agedepvars.R"))

# Sur rdc -----------------------------------------------------------------

for (i in agedepvars_c %>% 
     (function (a) a[str_detect(a, "_norm")])) {
  
  assign(paste("agefrd_pcs_g2_p1_", i, "_tous_rdch30", sep = ""),
         ivreg(as.formula(paste(i, " ~ ", 
                                c("age_abs", 
                                  "dist", "old : dist",
                                  "sexe", "pcsR",
                                  "age_abs : pcsR") %>%
                                  paste(collapse = " + "),
                                " |. - age_abs + old - age_abs : pcsR + old : pcsR",
                                sep = "")),
               data = hrestr(rdc, 30)))
  
  assign(paste("agefrd_pcs_g2_p2_", i, "_tous_rdch30", sep = ""),
         ivreg(as.formula(paste(i, " ~ ", 
                                c("age_abs", 
                                  "dist", "old : dist",
                                  "I(dist ^ 2)", 
                                  "old : I(dist ^ 2)",
                                  "sexe", "pcsR",
                                  "age_abs : pcsR") %>%
                                  paste(collapse = " + "),
                                " |. - age_abs + old - age_abs : pcsR + old : pcsR",
                                sep = "")),
               data = hrestr(rdc, 30)))
}

# Inférence et narsq ------------------------------------------------------

for (i in ls()[str_detect(ls(), "^agefrd_pcs_g2")]) {
  assign(paste("ct.", i, sep = ""),
         robust.se(get(i)))
  assign(paste("n.", i, sep = ""), nobs(get(i)))
  assign(paste("arsq.", i, sep = ""), ext_adjrsq(get(i)))
}

# Sauvegarde --------------------------------------------------------------

save(list = ls()[str_detect(ls(), "agefrd_pcs_g2") & 
                   ! str_detect(ls(), "^agefrd_pcs_g2")],
     file = here("agefrd_def2", "agefrd_pcs_g2.rda"),
     version = 2)