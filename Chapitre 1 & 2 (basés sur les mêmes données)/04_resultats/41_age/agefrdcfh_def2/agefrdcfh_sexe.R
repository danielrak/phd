# agefrdcfh_sexe (def2). 

library(here)
library(tidyverse)
library(plm)
library(lmtest)
library(stringi)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions2.rda")
load(here("01_donnees_def.rda"))
load(here("ageperd_def2", "ageperd.rda"))
source(here("agedepvars.R"))

# Sur rdc -----------------------------------------------------------------

for (i in agedepvars_c %>% 
     (function (a) a[str_detect(a, "_norm")])) {
  
  assign(paste("agefrdcfh_sexe_p1_", i, "_tous_rdch30", sep = ""),
         lm(as.formula(paste(i, " ~ ",
                             c("age_abs",
                               "age_abs : sexe",
                               "dist", "old : dist",
                               "sexe", "pcsR", 
                               "resid", "resid : age_abs",
                               "resid : sexe", 
                               "resid : age_abs : sexe") %>% 
                               paste(collapse = " + "),
                             sep = "")),
            data = hrestr(rdc, 30) %>% 
              mutate(resid = resid.ageperd_p1_tous_rdch30)))
  
  assign(paste("agefrdcfh_sexe_p2_", i, "_tous_rdch30", sep = ""),
         lm(as.formula(paste(i, " ~ ",
                             c("age_abs",
                               "age_abs : sexe",
                               "dist", "old : dist",
                               "I(dist ^ 2)", "old : I(dist ^ 2)",
                               "sexe", "pcsR", 
                               "resid", "resid : age_abs",
                               "resid : sexe",
                               "resid : age_abs : sexe") %>% 
                               paste(collapse = " + "),
                             sep = "")),
            data = hrestr(rdc, 30) %>% 
              mutate(resid = resid.ageperd_p2_tous_rdch30)))
}

# Inférence et narsq ------------------------------------------------------

for (i in ls()[str_detect(ls(), "^agefrd")]) {
  assign(paste("ct.", i, sep = ""),
         rsearellano(get(i)))
  assign(paste("n.", i, sep = ""), nobs(get(i)))
  assign(paste("arsq.", i, sep = ""), ext_adjrsq(get(i)))
}

# Sauvegarde --------------------------------------------------------------

save(list = ls()[str_detect(ls(), "agefrd") & 
                   ! str_detect(ls(), "^agefrd")],
     file = here("agefrdcfh_def2", "agefrdcfh_sexe.rda"),
     version = 2)