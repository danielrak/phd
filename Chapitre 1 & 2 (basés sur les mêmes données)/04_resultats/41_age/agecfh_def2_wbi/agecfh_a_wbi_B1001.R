# agecfh_a (def2). 

library(here)
library(tidyverse)
library(plm)
library(lmtest)
library(stringi)
library(magrittr)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions2.rda")
load(here("01_donnees_def.rda"))
load(here("agepe_def2", "agepe_a.rda"))
source(here("agedepvars.R"))

# Sur c, 2009 -------------------------------------------------------------

for (i in agedepvars_c %>% 
     (function (a) a[str_detect(a, "_norm")])) {
  assign(paste("agecfh_", 
               i, 
               "_tous_c09",
               sep = ""),
         lm(as.formula(paste(
           i, " ~ ", 
           c("age_abs", "resid", "resid : age_abs",
             "sexe") %>% 
             paste(collapse = " + "),
           sep = ""
         )),
         c %>% filter(cohorte == "2009") %>% 
           mutate(resid = resid.agepe_z_tous_c09)))
}

# Sur c, par année --------------------------------------------------------

for (j in c("2010", "2011", "2012")) {
  for (i in agedepvars_c) {
    assign(paste("agecfh_",
                 i,
                 "_tous_c", 
                 substr(j, 3, 4),
                 sep = ""),
           lm(as.formula(paste(
             i, " ~ ",
             c("age_abs", "resid", "resid : age_abs",
               "sexe", "pcsR") %>% 
               paste(collapse = " + "),
             sep = ""
           )),
           c %>% filter(cohorte == j) %>% 
             mutate(resid = 
                      eval(
                        parse(
                          text = 
                            paste("resid.agepe_z_tous_c",
                                  substr(j, 3, 4),
                                  sep = "")
                        )
                      ))))
  }
}

# Inférence wbi, B101 -----------------------------------------------------

  # Ne pas oublier detailed = FALSE.
for (i in ls()[str_detect(ls(), "^agecfh")]) {
  assign(paste("ctwbi_B1001.", i, sep = ""),
         wboot_lm(get(i), B = 1001, detailed = FALSE))
}

# Sauvegarde --------------------------------------------------------------

save(list = ls()[str_detect(ls(), "^ctwbi")],
     file = here("agecfh_def2_wbi", 
                 "agecfh_a_wbi_B1001.rda"),
     version = 2)
