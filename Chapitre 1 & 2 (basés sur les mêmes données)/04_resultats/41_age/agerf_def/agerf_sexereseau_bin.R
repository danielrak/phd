# agerf_sexereseau_bin (def). 

library(here)
library(tidyverse)
library(plm)
library(lmtest)
library(stringi)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions.rda")
load(here("01_donnees_def.rda"))
source(here("agedepvars.R"))
source(here("agesubsets.R"))

# Sur c -------------------------------------------------------------------

for(j in agesexereseau_bin) {
  for (i in agedepvars_c) {
    assign(paste("agerf_",
                 i,
                 "_sexe_",
                 j %>% str_extract(".*_AND_") %>% 
                   str_remove("_AND_"),
                 "_reseau_bin_",
                 j %>% str_extract("_AND_.*") %>% 
                   str_remove("_AND_"),
                 "_c",
                 sep = ""),
           plm(as.formula(paste(
             i, " ~ ",
             c("z",
               "pcsR", "cohorte") %>% 
               paste(collapse = " + "),
             sep = ""
           )),
           c %>% filter(sexe == j %>% str_extract(".*_AND_") %>% 
                          str_remove("_AND_") & 
                          reseau_bin == j %>% 
                          str_extract("_AND_.*") %>% 
                          str_remove("_AND_")),
           index = "ecolecoh", model = "pooling"))
  }
  
}

# Sur cdi -----------------------------------------------------------------

for(j in agesexereseau_bin) {
  for (i in agedepvars_cdi) {
    assign(paste("agerf_",
                 i,
                 "_sexe_",
                 j %>% str_extract(".*_AND_") %>% 
                   str_remove("_AND_"),
                 "_reseau_bin_",
                 j %>% str_extract("_AND_.*") %>% 
                   str_remove("_AND_"),
                 "_cdi",
                 sep = ""),
           plm(as.formula(paste(
             i, " ~ ",
             c("z",
               "pcsR", "cohorte") %>% 
               paste(collapse = " + "),
             sep = ""
           )),
           cdi %>% filter(sexe == j %>% str_extract(".*_AND_") %>% 
                            str_remove("_AND_") & 
                            reseau_bin == j %>% 
                            str_extract("_AND_.*") %>% 
                            str_remove("_AND_")),
           index = "ecolecoh", model = "pooling"))
  }
  
}

# Sur cdiscores_cm2 -------------------------------------------------------

for (j in agesexereseau_bin) {
  for (i in agedepvars_cdi_w_scores_cm2) {
    assign(paste("agerf_", 
                 i %>% str_extract(".*_AND_") %>% 
                   str_remove("_AND_"),
                 "_w_",
                 i %>% str_extract("_AND_.*") %>% 
                   str_remove("_AND_"),
                 "_sexe_",
                 j %>% str_extract(".*_AND_") %>% 
                   str_remove("_AND_"),
                 "_reseau_bin_", 
                 j %>% str_extract("_AND_.*") %>% 
                   str_remove("_AND_"),
                 "_cdi", sep = ""
    ),
    plm(as.formula(paste(
      i %>% str_extract(".*_AND_") %>% 
        str_remove("_AND_"), " ~ ",
      i %>% str_extract("_AND_.*") %>% 
        str_remove("_AND_"), " + ",
      c("z",
        "pcsR", "cohorte") %>% 
        paste(collapse = " + "), sep = ""
    )),
    cdi %>% 
      filter(sexe == j %>% str_extract(".*_AND_") %>% 
               str_remove("_AND_") & 
               reseau_bin == 
               j %>% str_extract("_AND_.*") %>% 
               str_remove("_AND_")),
    index = "ecolecoh", model = "pooling"))
  }
}

# Inférence et narsq ------------------------------------------------------

for (i in ls()[str_detect(ls(), "^agerf")]) {
  assign(paste("ct.", i, sep = ""),
         rsearellano(get(i)))
  assign(paste("n.", i, sep = ""), nobs(get(i)))
  assign(paste("arsq.", i, sep = ""), ext_adjrsq(get(i)))
}

# Dossier de résultats ----------------------------------------------------

dir.create(here("agerf_def", "agerf_sexereseau_bin"))

# Sauvegarde, light -------------------------------------------------------

for (i in ls()[str_detect(ls(), 
                          c("^ct", "^n", "^arsq", "^resid") %>% 
                          paste("\\.", sep = "") %>% 
                          paste(collapse = "|"))]) {
  saveRDS(get(i), 
          file = here("agerf_def", 
                      "agerf_sexereseau_bin",
                      paste(i, ".rds", sep = "")),
          version = 2)
}

