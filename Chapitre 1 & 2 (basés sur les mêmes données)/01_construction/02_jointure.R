# Jointures. 

library(tidyverse)
library(fastDummies)
library(lubridate)
library(magrittr)
library(stringi)
library(here)

# Chargement --------------------------------------------------------------

# load("D:/00_phd/00_fonctions/fonctions.rda")s
load("C:/00_phd/00_fonctions/fonctions.rda")
load(here("01_construction", "01_pre_jointure.rda"))

# cm2dnb ------------------------------------------------------------------

cm2dnb <- left_join(cm2, dnb, by = c("ine" = "ine_mod"))
  # nb : parmi ceux  is.na(cm2$ine), 247 ont passé deux dnb. 

# dnbcm2 ------------------------------------------------------------------

dnbcm2 <- left_join(dnb, cm2, by = c("ine_mod" = "ine"))
  # nb : parmi ceux dont on retrouve la note au CM2, 5 ont passé deux evals CM2. 

# Sauvegarde --------------------------------------------------------------

save(cm2dnb, dnbcm2,
     file = here("01_construction", "02_jointure.rda"),
     version = 2)
