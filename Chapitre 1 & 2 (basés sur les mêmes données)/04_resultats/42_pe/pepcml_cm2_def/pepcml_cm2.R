# pepcml_cm2 (def). 
  # Avoir une idée des effets de pairs au CM2. 
  # N'est pas valable pour les sous-items. 
  # Sans 2009 car pas assez de covars. 
  # sexe et pcs seuls covars. 

library(here)
library(tidyverse)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions2.rda")
load("D:/00_phd/02_rectorat/04_resultats/41_age/01_donnees_def.rda")

c101112 <- filter(c, cohorte != "2009")

cov2 <- c("sexe", "pcsR")
cov3 <- c("sexe", "pcsR", "age_abs")
cov4 <- c("sexe", "pcsR", "age_abs", "position")

# pepcml_cm2 --------------------------------------------------------------

for (j in c("cov2", "cov3", "cov4")) {
  for (i in c("score_norm", "score_f_norm", "score_m_norm")) {
  assign("ff", Formula::as.Formula(
    paste(i, " + factor(classecoh) ~ ", 
          get(j) %>% paste(collapse = " + "),
          sep = "")
  ))
  assign(paste("pepcml_", j, "_", i, "_tous_c101112", sep = ""),
         pseudo_cml3(ff, c101112 %>% 
                       group_by(classecoh) %>% 
                       filter(n() > 1) %>% 
                       ungroup))
  }
}
  # Environ 5 min de calcul. 

# Sauvegarde --------------------------------------------------------------

save(list = ls()[str_detect(ls(), "pepcml")],
     file = here("pepcml_cm2_def", "pepcml_cm2.rda"),
     version = 2)