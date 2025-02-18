# agerf (def). 

library(here)
library(tidyverse)
library(plm)
library(lmtest)
library(stringi)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions.rda")
load(here("01_donnees_def.rda"))
source(here("agedepvars.R"))

# Sur c -------------------------------------------------------------------

for (i in agedepvars_c) {
  assign(paste("agerf_", 
               i, 
               "_tous_c",
               sep = ""),
         plm(as.formula(paste(
           i, " ~ ", 
           c("z",
             "sexe", "pcsR", "cohorte") %>% 
             paste(collapse = " + "),
           sep = ""
         )),
         c,
         index = "ecolecoh", model = "pooling"))
}

# Sur cdi -----------------------------------------------------------------

for (i in agedepvars_cdi) {
  assign(paste("agerf_", 
               i, 
               "_tous_cdi",
               sep = ""),
         plm(as.formula(paste(
           i, " ~ ", 
           c("z",
             "sexe", "pcsR", "cohorte") %>% 
             paste(collapse = " + "),
           sep = ""
         )),
         cdi,
         index = "ecolecoh", model = "pooling"))
}

# Sur cdiscores_cm2 -------------------------------------------------------

for (i in agedepvars_cdi_w_scores_cm2) {
  assign(paste("agerf_",
               i %>% str_extract(".*_AND_") %>% 
                 str_remove("_AND_"),
               "_w_",
               i %>% str_extract("_AND_.*") %>% 
                 str_remove("_AND_"),
               "_tous_cdi",
               sep = ""),
         plm(as.formula(paste(
           i %>% str_extract(".*_AND_") %>% 
             str_remove("_AND_"), " ~ ",
           i %>% str_extract("_AND_.*") %>% 
             str_remove("_AND_"), " + ",
           c("z",
             "sexe", "pcsR", "cohorte") %>% 
             paste(collapse = " + "),
           sep = ""
         )),
         cdi,
         index = "ecolecoh", model = "pooling"))
}

# Inférence et narsq ------------------------------------------------------

for (i in ls()[str_detect(ls(), "^agerf")]) {
  assign(paste("ct.", i, sep = ""),
         rsearellano(get(i)))
  assign(paste("n.", i, sep = ""), nobs(get(i)))
  assign(paste("arsq.", i, sep = ""), ext_adjrsq(get(i)))
}

# Dossier de résultats ----------------------------------------------------

dir.create(here("agerf_def", "agerf"))

# Sauvegarde, light -------------------------------------------------------

for (i in ls()[str_detect(ls(), 
                          c("^ct", "^n", "^arsq", "^resid") %>% 
                          paste("\\.", sep = "") %>% 
                          paste(collapse = "|"))]) {
  saveRDS(get(i), 
          file = here("agerf_def", 
                      "agerf",
                      paste(i, ".rds", sep = "")),
          version = 2)
}

# Sauvegarde, modèles -----------------------------------------------------

# for (i in ls()[str_detect(ls(), "^agerf")]) {
#   saveRDS(get(i),
#           file = here("agerf_def", 
#                       "agerf",
#                       paste("MODELES_", 
#                             i, ".rds",
#                             sep = "")),
#           version = 2)
# }
