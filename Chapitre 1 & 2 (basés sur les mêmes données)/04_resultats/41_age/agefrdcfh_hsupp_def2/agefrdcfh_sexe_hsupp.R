# agefrdcfh_sexe_hsupp (def2). 

library(here)
library(tidyverse)
library(plm)
library(lmtest)
library(stringi)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions2.rda")
load(here("01_donnees_def.rda"))
load(here("ageperd_hsupp_def2", "ageperd_hsupp.rda"))
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
for (i in agedepvars_c %>% 
     (function (a) a[str_detect(a, "_norm")])) {
  
  assign(paste("agefrdcfh_sexe_p1_", i, "_tous_rdch", j, sep = ""),
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
            data = hrestr(rdc, j) %>% 
              mutate(resid = 
                       eval(parse(text = paste(
                         "resid.ageperd_p1_tous_rdch", j, 
                         sep = ""
                       ))))))
  
  assign(paste("agefrdcfh_sexe_p2_", i, "_tous_rdch", j, sep = ""),
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
            data = hrestr(rdc, j) %>% 
              mutate(resid = 
                       eval(parse(text = paste(
                         "resid.ageperd_p2_tous_rdch", j, 
                         sep = ""
                       ))))))
  }
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
     file = here("agefrdcfh_hsupp_def2", 
                 "agefrdcfh_sexe_hsupp.rda"),
     version = 2)