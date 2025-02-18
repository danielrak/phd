# agesrd_pcs_g2reseau_bin (def).

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
source(here("agesubsets.R"))

# Sur rdc -----------------------------------------------------------------

for (j in agepcs_g2reseau_bin[
  ! str_detect(agepcs_g2reseau_bin, "Autres") # pbm nobs faible.
]) {
  for (i in agedepvars_c) {
    
    assign(paste("agesrd_p1_", i, 
                 "_pcs_g2_", j %>% str_extract(".*_AND_") %>% 
                   str_remove("_AND_"),
                 "_reseau_bin_", j %>% 
                   str_extract("_AND_.*") %>% 
                   str_remove("_AND_"),
                 "_rdch30", sep = ""),
           lm(as.formula(paste(i, " ~ ", 
                                  c("old", 
                                    "dist", "old : dist",
                                    "sexe") %>%
                                    paste(collapse = " + "),
                                  sep = "")),
                 data = hrestr(rdc, 30) %>% 
                   filter(pcs_g2 == j %>% 
                            str_extract(".*_AND_") %>% 
                            str_remove("_AND_") & 
                            
                            reseau_bin == j %>% 
                            str_extract("_AND_.*") %>% 
                            str_remove("_AND_"))))
    
    assign(paste("agesrd_p2_", i, 
                 "_pcs_g2_", j %>% str_extract(".*_AND_") %>% 
                   str_remove("_AND_"),
                 "_reseau_bin_", j %>% 
                   str_extract("_AND_.*") %>% 
                   str_remove("_AND_"),
                 "_rdch30", sep = ""),
           lm(as.formula(paste(i, " ~ ", 
                                  c("old", 
                                    "dist", "old : dist",
                                    "I(dist ^ 2)",
                                    "old : I(dist ^ 2)",
                                    "sexe") %>%
                                    paste(collapse = " + "),
                                  sep = "")),
                 data = hrestr(rdc, 30) %>% 
                   filter(pcs_g2 == j %>% 
                            str_extract(".*_AND_") %>% 
                            str_remove("_AND_") & 
                            
                            reseau_bin == j %>% 
                            str_extract("_AND_.*") %>% 
                            str_remove("_AND_"))))
  }
}


# Sur rdcdi ---------------------------------------------------------------

for (j in agepcs_g2reseau_bin[
  ! str_detect(agepcs_g2reseau_bin, "Autres") # pbm nobs faible.
]) {
  for (i in agedepvars_c) {
    
    assign(paste("agesrd_p1_", i, 
                 "_pcs_g2_", j %>% str_extract(".*_AND_") %>% 
                   str_remove("_AND_"),
                 "_reseau_bin_", j %>% 
                   str_extract("_AND_.*") %>% 
                   str_remove("_AND_"),
                 "_rdcdih30", sep = ""),
           lm(as.formula(paste(i, " ~ ", 
                                  c("old", 
                                    "dist", "old : dist",
                                    "sexe") %>%
                                    paste(collapse = " + "),
                                  sep = "")),
                 data = hrestr(rdcdi, 30) %>% 
                   filter(pcs_g2 == j %>% 
                            str_extract(".*_AND_") %>% 
                            str_remove("_AND_") & 
                            
                            reseau_bin == j %>% 
                            str_extract("_AND_.*") %>% 
                            str_remove("_AND_"))))
    
    assign(paste("agesrd_p2_", i, 
                 "_pcs_g2_", j %>% str_extract(".*_AND_") %>% 
                   str_remove("_AND_"),
                 "_reseau_bin_", j %>% 
                   str_extract("_AND_.*") %>% 
                   str_remove("_AND_"),
                 "_rdcdih30", sep = ""),
           lm(as.formula(paste(i, " ~ ", 
                                  c("old", 
                                    "dist", "old : dist",
                                    "I(dist ^ 2)",
                                    "old : I(dist ^ 2)",
                                    "sexe") %>%
                                    paste(collapse = " + "),
                                  sep = "")),
                 data = hrestr(rdcdi, 30) %>% 
                   filter(pcs_g2 == j %>% 
                            str_extract(".*_AND_") %>% 
                            str_remove("_AND_") & 
                            
                            reseau_bin == j %>% 
                            str_extract("_AND_.*") %>% 
                            str_remove("_AND_"))))
  }
}

# Sur rdcdi_scores_cm2 ----------------------------------------------------

for (j in agepcs_g2reseau_bin[
  ! str_detect(agepcs_g2reseau_bin, "Autres") # pbm nobs faible.
]) {
  for (i in agedepvars_cdi_w_scores_cm2) {
    
    assign(paste("agesrd_p1_", i %>% str_extract(".*_AND_") %>% 
                   str_remove("_AND_"), "_w_",
                 i %>% str_extract("_AND_.*") %>% 
                   str_remove("_AND_"), 
                 "_pcs_g2_", j %>% 
                   str_extract(".*_AND_") %>% 
                   str_remove("_AND_"),
                 "_reseau_bin_",
                 j %>% 
                   str_extract("_AND_.*") %>% 
                   str_remove("_AND_"),
                 "_rdcdih30", sep = ""),
           lm(as.formula(paste(i %>% str_extract(".*_AND_") %>% 
                                    str_remove("_AND_"), 
                                  " ~ ",
                                  i %>% str_extract("_AND_.*") %>% 
                                    str_remove("_AND_"), " + ",
                                  c("old",
                                    "dist", "old : dist",
                                    "sexe") %>% 
                                    paste(collapse = " + "),
                                  sep = "")),
                 data = hrestr(rdcdi, 30) %>% 
                   filter(pcs_g2 == j %>% 
                            str_extract(".*_AND_") %>% 
                            str_remove("_AND_") & 
                            reseau_bin == 
                            j %>% 
                            str_extract("_AND_.*") %>% 
                            str_remove("_AND_"))))
    
    assign(paste("agesrd_p2_", i %>% str_extract(".*_AND_") %>% 
                   str_remove("_AND_"), "_w_",
                 i %>% str_extract("_AND_.*") %>% 
                   str_remove("_AND_"), 
                 "_pcs_g2_", j %>% 
                   str_extract(".*_AND_") %>% 
                   str_remove("_AND_"),
                 "_reseau_bin_",
                 j %>% 
                   str_extract("_AND_.*") %>% 
                   str_remove("_AND_"),
                 "_rdcdih30", sep = ""),
           lm(as.formula(paste(i %>% str_extract(".*_AND_") %>% 
                                    str_remove("_AND_"), 
                                  " ~ ",
                                  i %>% str_extract("_AND_.*") %>% 
                                    str_remove("_AND_"), " + ",
                                  c("old",
                                    "dist", "old : dist",
                                    "I(dist ^ 2)", 
                                    "old : I(dist ^ 2)",
                                    "sexe") %>% 
                                    paste(collapse = " + "),
                                  sep = "")),
                 data = hrestr(rdcdi, 30) %>% 
                   filter(pcs_g2 == j %>% 
                            str_extract(".*_AND_") %>% 
                            str_remove("_AND_") & 
                            reseau_bin == 
                            j %>% 
                            str_extract("_AND_.*") %>% 
                            str_remove("_AND_"))))
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

dir.create(here("agesrd_def", "agesrd_pcs_g2reseau_bin"))

# Sauvegarde, light -------------------------------------------------------

for (i in ls()[str_detect(ls(), 
                          c("^ct", "^n", "^arsq", "^resid") %>% 
                          paste("\\.", sep = "") %>% 
                          paste(collapse = "|"))]) {
  saveRDS(get(i), 
          file = here("agesrd_def", 
                      "agesrd_pcs_g2reseau_bin",
                      paste(i, ".rds", sep = "")),
          version = 2)
}
