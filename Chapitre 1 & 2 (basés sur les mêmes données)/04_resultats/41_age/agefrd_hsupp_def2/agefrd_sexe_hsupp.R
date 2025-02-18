# agefrd_sexe_hsupp (def2). 

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

# Préparation -------------------------------------------------------------

hsupp <- (5:60) #%>% (function (x) x[x != 30])

# Regarder s'il y a quand même de la variation 
# si l'on bouge la fenêtre. 
cov_hsupp <- lapply(hsupp, function (x) hrestr(rdc, x) %>% 
                      select(sexe, pcsR) %>% summary) %>%
  setNames(hsupp)
# ça va même à 5 jours de fenêtre. 

# Sur rdc -----------------------------------------------------------------

for (j in hsupp) {
  
  for (i in agedepvars_c[
    str_detect(agedepvars_c, "_norm")
  ]) {
    
    assign(paste("agefrd_sexe_p1_", i, "_tous_rdch", j, sep = ""),
           ivreg(as.formula(paste(i, " ~ ", 
                                  c("age_abs", 
                                    "dist", "old : dist",
                                    "sexe", "pcsR",
                                    "age_abs : sexe") %>%
                                    paste(collapse = " + "),
                                  " |. - age_abs + old - age_abs : sexe + old : sexe",
                                  sep = "")),
                 data = hrestr(rdc, j)))
    
    assign(paste("agefrd_sexe_p2_", i, "_tous_rdch", j, sep = ""),
           ivreg(as.formula(paste(i, " ~ ", 
                                  c("age_abs", 
                                    "dist", "old : dist",
                                    "I(dist ^ 2)", 
                                    "old : I(dist ^ 2)",
                                    "sexe", "pcsR",
                                    "age_abs : sexe") %>%
                                    paste(collapse = " + "),
                                  " |. - age_abs + old - age_abs : sexe + old : sexe",
                                  sep = "")),
                 data = hrestr(rdc, j)))
  }
}

# Inférence et narsq ------------------------------------------------------

for (i in ls()[str_detect(ls(), "^agefrd")]) {
  assign(paste("ct.", i, sep = ""),
         robust.se(get(i)))
  assign(paste("n.", i, sep = ""), nobs(get(i)))
  assign(paste("arsq.", i, sep = ""), ext_adjrsq(get(i)))
}

# Sauvegarde --------------------------------------------------------------

save(list = ls()[str_detect(ls(), "agefrd") & 
                   ! str_detect(ls(), "^agefrd")],
     file = here("agefrd_hsupp_def2", "agefrd_sexe_hsupp.rda"),
     version = 2)