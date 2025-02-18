# agefrdcfh_hsupp (def2). 
  # pour être cohérent avec la méthode cfh. 
  # rappel : deux possibilités. 

library(here)
library(tidyverse)
library(plm)
library(lmtest)
library(stringi)
library(magrittr)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions2.rda")
load(here("01_donnees_def.rda"))
load(here("ageperd_def2", "ageperd.rda"))
source(here("agedepvars.R"))

# Sur rdc -----------------------------------------------------------------

for (i in agedepvars_c %>% 
     (function (a) a[str_detect(a, "_norm")])) {
  
  assign(paste("agefrdcfh_p1_", i, "_tous_rdch30", sep = ""),
         lm(as.formula(paste(i, " ~ ",
                             c("age_abs",
                               "dist", "old : dist",
                               "sexe", "pcsR", 
                               "resid", "resid : age_abs") %>% 
                               paste(collapse = " + "),
                             sep = "")),
            data = hrestr(rdc, 30) %>% 
              mutate(resid = resid.ageperd_p1_tous_rdch30)))
  
  assign(paste("agefrdcfh_p2_", i, "_tous_rdch30", sep = ""),
         lm(as.formula(paste(i, " ~ ",
                             c("age_abs",
                               "dist", "old : dist",
                               "I(dist ^ 2)", "old : I(dist ^ 2)",
                               "sexe", "pcsR", 
                               "resid", "resid : age_abs") %>% 
                               paste(collapse = " + "),
                             sep = "")),
            data = hrestr(rdc, 30) %>% 
              mutate(resid = resid.ageperd_p2_tous_rdch30)))
}

# Inférence wbi, B101 -----------------------------------------------------

  # Ne pas oublier detailed = FALSE.
for (i in ls()[str_detect(ls(), "^agefrdcfh")]) {
  assign(paste("ctwbi_B1001.", i, sep = ""),
         wboot_lm(get(i), B = 1001, detailed = FALSE))
}

# Sauvegarde --------------------------------------------------------------

save(list = ls()[str_detect(ls(), "^ctwbi")],
     file = here("agefrdcfh_def2_wbi", 
                 "agefrdcfh_wbi_B1001.rda"),
     version = 2)