# agesrd_hsupp (def). 

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

hsupp <- (5:60) %>% (function (x) x[x != 30])

# Regarder s'il y a quand même de la variation 
# si l'on bouge la fenêtre. 
cov_hsupp <- lapply(hsupp, function (x) hrestr(rdc, x) %>% 
                      select(sexe, pcsR) %>% summary) %>%
  setNames(hsupp)
# ça va même à 5 jours de fenêtre. 

# Sur rdc -----------------------------------------------------------------

for (j in hsupp) {
  
  for (i in agedepvars_c) {
    
    assign(paste("agesrd_p1_", i, "_tous_rdch", j, sep = ""),
           lm(as.formula(paste(i, " ~ ", 
                                  c("old", 
                                    "dist", "old : dist",
                                    "sexe", "pcsR") %>%
                                    paste(collapse = " + "),
                                  sep = "")),
                 data = hrestr(rdc, j)))
    
    assign(paste("agesrd_p2_", i, "_tous_rdch", j, sep = ""),
           lm(as.formula(paste(i, " ~ ", 
                                  c("old", 
                                    "dist", "old : dist",
                                    "I(dist ^ 2)", 
                                    "old : I(dist ^ 2)",
                                    "sexe", "pcsR") %>%
                                    paste(collapse = " + "),
                                  sep = "")),
                 data = hrestr(rdc, j)))
  }
}


# Sur rdcdi ---------------------------------------------------------------

for (j in hsupp) {
  
  for (i in agedepvars_cdi) {
    
    assign(paste("agesrd_p1_", i, "_tous_rdcdih", j, sep = ""),
           lm(as.formula(paste(i, " ~ ", 
                                  c("old", 
                                    "dist", "old : dist",
                                    "sexe", "pcsR") %>%
                                    paste(collapse = " + "),
                                  sep = "")),
                 data = hrestr(rdcdi, j)))
    
    assign(paste("agesrd_p2_", i, "_tous_rdcdih", j, sep = ""),
           lm(as.formula(paste(i, " ~ ", 
                                  c("old", 
                                    "dist", "old : dist",
                                    "I(dist ^ 2)", 
                                    "old : I(dist ^ 2)",
                                    "sexe", "pcsR") %>%
                                    paste(collapse = " + "),
                                  sep = "")),
                 data = hrestr(rdcdi, j)))
  }
}

# Sur cdiscores_cm2 -------------------------------------------------------

for (j in hsupp) {
  
  for (i in agedepvars_cdi_w_scores_cm2) {
    
    assign(paste("agesrd_p1_", i %>% str_extract(".*_AND_") %>% 
                   str_remove("_AND_"), "_w_",
                 i %>% str_extract("_AND_.*") %>% 
                   str_remove("_AND_"), 
                 "_tous_rdcdih", j, sep = ""),
           lm(as.formula(paste(i %>% str_extract(".*_AND_") %>% 
                                    str_remove("_AND_"), 
                                  " ~ ",
                                  i %>% str_extract("_AND_.*") %>% 
                                    str_remove("_AND_"), " + ",
                                  c("old",
                                    "dist", "old : dist",
                                    "sexe", "pcsR") %>% 
                                    paste(collapse = " + "),
                                  sep = "")),
                 data = hrestr(rdcdi, j)))
    
    assign(paste("agesrd_p2_", i %>% str_extract(".*_AND_") %>% 
                   str_remove("_AND_"), "_w_",
                 i %>% str_extract("_AND_.*") %>% 
                   str_remove("_AND_"), 
                 "_tous_rdcdih", j, sep = ""),
           lm(as.formula(paste(i %>% str_extract(".*_AND_") %>% 
                                    str_remove("_AND_"), 
                                  " ~ ",
                                  i %>% str_extract("_AND_.*") %>% 
                                    str_remove("_AND_"), " + ",
                                  c("old",
                                    "dist", "old : dist",
                                    "I(dist ^ 2)", 
                                    "old : I(dist ^ 2)",
                                    "sexe", "pcsR") %>% 
                                    paste(collapse = " + "),
                                  sep = "")),
                 data = hrestr(rdcdi, j)))
  }
}

# Inférence et narsq ------------------------------------------------------

for (i in ls()[str_detect(ls(), "^agesrd")]) {
  assign(paste("ct.", i, sep = ""),
         rsearellano(get(i)))
  assign(paste("n.", i, sep = ""), nobs(get(i)))
  assign(paste("arsq.", i, sep = ""), ext_adjrsq(get(i)))
}

# Dossier de résultats ----------------------------------------------------

dir.create(here("agesrd_hsupp_def", "agesrd_hsupp"))

# Sauvegarde, light -------------------------------------------------------

for (i in ls()[str_detect(ls(), 
                          c("^ct", "^n", "^arsq", "^resid") %>% 
                          paste("\\.", sep = "") %>% 
                          paste(collapse = "|"))]) {
  saveRDS(get(i), 
          file = here("agesrd_hsupp_def", 
                      "agesrd_hsupp",
                      paste(i, ".rds", sep = "")),
          version = 2)
}

