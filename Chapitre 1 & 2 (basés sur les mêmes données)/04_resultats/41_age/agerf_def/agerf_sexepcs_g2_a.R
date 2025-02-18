# agerf_sexepcs_g2_a (def). 

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

# Sur c, 2009 -------------------------------------------------------------

# Néant, pas de pcs_g2 dans 2009. 

# Sur c, par année --------------------------------------------------------

for (k in c("2010", "2011", "2012")) {
  for(j in agesexepcs_g2) {
    for (i in agedepvars_c) {
      assign(paste("agerf_",
                   i,
                   "_sexe_",
                   j %>% str_extract(".*_AND_") %>% 
                     str_remove("_AND_"),
                   "_pcs_g2_",
                   j %>% str_extract("_AND_.*") %>% 
                     str_remove("_AND_"),
                   "_c", k %>% substr(3, 4),
                   sep = ""),
             plm(as.formula(paste(
               i, " ~ ",
               c("z") %>% 
                 paste(collapse = " + "),
               sep = ""
             )),
             c %>% filter(cohorte == k & 
                            
                            sexe == j %>% 
                            str_extract(".*_AND_") %>% 
                            str_remove("_AND_") & 
                            
                            pcs_g2 == j %>% 
                            str_extract("_AND_.*") %>% 
                            str_remove("_AND_")),
             index = "ecolecoh", model = "pooling"))
    }
    
  } 
}

# Sur c, 101112 -----------------------------------------------------------

# Néant, équivaut à _c. 

# Sur cdi, 2009 -----------------------------------------------------------

# Néant, pas de 2009 dans cdi.

# Sur cdi, par année ------------------------------------------------------

for (k in c("2010", "2011", "2012")) {
  for(j in agesexepcs_g2) {
    for (i in agedepvars_c) {
      assign(paste("agerf_",
                   i,
                   "_sexe_",
                   j %>% str_extract(".*_AND_") %>% 
                     str_remove("_AND_"),
                   "_pcs_g2_",
                   j %>% str_extract("_AND_.*") %>% 
                     str_remove("_AND_"),
                   "_cdi", k %>% substr(3, 4),
                   sep = ""),
             plm(as.formula(paste(
               i, " ~ ",
               c("z") %>% 
                 paste(collapse = " + "),
               sep = ""
             )),
             cdi %>% filter(cohorte == k & 
                              
                              sexe == j %>% 
                              str_extract(".*_AND_") %>% 
                              str_remove("_AND_") & 
                              
                              pcs_g2 == j %>% 
                              str_extract("_AND_.*") %>% 
                              str_remove("_AND_")),
             index = "ecolecoh", model = "pooling"))
    }
    
  } 
}

# Sur cdi, 101112 ---------------------------------------------------------

# Néant, équivaut à _cdi. 

# Sur cdiscores_cm2, 2009 -------------------------------------------------

# Néant, pas de 2009 dans cdi. 

# Sur cdiscores_cm2, par année --------------------------------------------

for (k in c("2010", "2011", "2012")) {
  for (j in agesexepcs_g2) {
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
                   "_pcs_g2_", 
                   j %>% str_extract("_AND_.*") %>% 
                     str_remove("_AND_"),
                   "_cdi", k %>% substr(3, 4),
                   sep = ""
      ),
      plm(as.formula(paste(
        i %>% str_extract(".*_AND_") %>% 
          str_remove("_AND_"), " ~ ",
        i %>% str_extract("_AND_.*") %>% 
          str_remove("_AND_"), " + ",
        c("z") %>% 
          paste(collapse = " + "), sep = ""
      )),
      cdi %>% 
        filter(cohorte == k &
                 sexe == j %>% str_extract(".*_AND_") %>% 
                 str_remove("_AND_") & 
                 pcs_g2 == 
                 j %>% str_extract("_AND_.*") %>% 
                 str_remove("_AND_")),
      index = "ecolecoh", model = "pooling"))
    }
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

dir.create(here("agerf_def", "agerf_sexepcs_g2_a"))

# Sauvegarde, light -------------------------------------------------------

for (i in ls()[str_detect(ls(), 
                          c("^ct", "^n", "^arsq", "^resid") %>% 
                          paste("\\.", sep = "") %>% 
                          paste(collapse = "|"))]) {
  saveRDS(get(i), 
          file = here("agerf_def", 
                      "agerf_sexepcs_g2_a",
                      paste(i, ".rds", sep = "")),
          version = 2)
}


