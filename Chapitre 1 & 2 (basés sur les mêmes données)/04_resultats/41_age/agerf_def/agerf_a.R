# agerf_a (def). 

library(here)
library(tidyverse)
library(plm)
library(lmtest)
library(stringi)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions.rda")
load(here("01_donnees_def.rda"))
source(here("agedepvars.R"))

# Sur c, 2009 -------------------------------------------------------------

for (i in agedepvars_c) {
  assign(paste("agerf_", 
               i, 
               "_tous_c09",
               sep = ""),
         plm(as.formula(paste(
           i, " ~ ", 
           c("z",
             "sexe") %>% 
             paste(collapse = " + "),
           sep = ""
         )),
         c %>% filter(cohorte == "2009"),
         index = "ecolecoh", model = "pooling"))
}

# Sur c, par année --------------------------------------------------------

for (j in c("2010", "2011", "2012")) {
  for (i in agedepvars_c) {
    assign(paste("agerf_",
                 i,
                 "_tous_c", 
                 substr(j, 3, 4),
                 sep = ""),
           plm(as.formula(paste(
             i, " ~ ",
             c("z",
               "sexe", "pcsR") %>% 
               paste(collapse = " + "),
             sep = ""
           )),
           c %>% filter(cohorte == j),
           index = "ecolecoh", model = "pooling"))
  }
}

# Sur c, 101112 -----------------------------------------------------------

for (i in agedepvars_c) {
  assign(paste("agerf_", 
               i, 
               "_tous_c101112",
               sep = ""),
         plm(as.formula(paste(
           i, " ~ ", 
           c("z",
             "sexe + pcsR + cohorte") %>% 
             paste(collapse = " + "),
           sep = ""
         )),
         c %>% filter(cohorte %in% 
                        c("2010", "2011", "2012")),
         index = "ecolecoh", model = "pooling"))
}

# Sur cdi, 2009 -----------------------------------------------------------

# Néant, pas de 2009 dans cdi. 

# Sur cdi, par année ------------------------------------------------------

for (j in c("2010", "2011", "2012")) {
  for (i in agedepvars_cdi) {
    assign(paste("agerf_",
                 i,
                 "_tous_cdi", 
                 substr(j, 3, 4),
                 sep = ""),
           plm(as.formula(paste(
             i, " ~ ",
             c("z",
               "sexe", "pcsR") %>% 
               paste(collapse = " + "),
             sep = ""
           )),
           cdi %>% filter(cohorte == j),
           index = "ecolecoh", model = "pooling"))
  }
}

# Sur cdi, 101112 ---------------------------------------------------------

# Néant. Même chose que _cdi. 

# Sur cdiscores_cm2, 2009 -------------------------------------------------

# Néant, pas de 2009 dans cdi. 

# Sur cdiscores_cm2, par année --------------------------------------------

for (j in c("2010", "2011", "2012")) {
  for (i in agedepvars_cdi_w_scores_cm2) {
    assign(paste("agerf_",
                 i %>% str_extract(".*_AND_") %>% 
                   str_remove("_AND_"),
                 "_w_",
                 i %>% str_extract("_AND_.*") %>% 
                   str_remove("_AND_"),
                 "_tous_cdi",
                 substr(j, 3, 4),
                 sep = ""),
           plm(as.formula(paste(
             i %>% str_extract(".*_AND_") %>% 
               str_remove("_AND_"), " ~ ",
             i %>% str_extract("_AND_.*") %>% 
               str_remove("_AND_"), " + ",
             c("z",
               "sexe", "pcsR") %>% 
               paste(collapse = " + "),
             sep = ""
           )),
           cdi %>% filter(cohorte == j),
           index = "ecolecoh", model = "pooling"))
  }
}

# Sur cdiscores_cm2, 101112 -----------------------------------------------

# Néant. Même chose que cdiscores_cm2. 

# Inférence et narsq ------------------------------------------------------

for (i in ls()[str_detect(ls(), "^agerf")]) {
  assign(paste("ct.", i, sep = ""),
         rsearellano(get(i)))
  assign(paste("n.", i, sep = ""), nobs(get(i)))
  assign(paste("arsq.", i, sep = ""), ext_adjrsq(get(i)))
}

# Dossier de résultats ----------------------------------------------------

dir.create(here("agerf_def", "agerf_a"))

# Sauvegarde, light -------------------------------------------------------

for (i in ls()[str_detect(ls(), 
                          c("^ct", "^n", "^arsq", "^resid") %>% 
                          paste("\\.", sep = "") %>% 
                          paste(collapse = "|"))]) {
  saveRDS(get(i), 
          file = here("agerf_def", 
                      "agerf_a",
                      paste(i, ".rds", sep = "")),
          version = 2)
}

# Sauvegarde, modèles -----------------------------------------------------

# for (i in ls()[str_detect(ls(), "^agerf")]) {
#   saveRDS(get(i),
#           file = here("agerf_def", 
#                       "agerf_a",
#                       paste("MODELES_", 
#                             i, ".rds",
#                             sep = "")),
#           version = 2)
# }
