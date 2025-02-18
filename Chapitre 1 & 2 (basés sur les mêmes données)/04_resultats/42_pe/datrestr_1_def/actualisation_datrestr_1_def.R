# ine, session_mod datrestr_1 
# Pour ne pas avoir à relancer les tests KS à chaque fois. 

library(here)
library(tidyverse)


load(here("01_01_donnees_def.rda"))
load(here("datrestr_1_def", "datrestr_1_def.rda"))
file.copy(here("datrestr_1_def", "datrestr_1_def.rda"),
          here("datrestr_1_def", "datrestr_1_def_avant_actualisation.rda"))
ineses_datrestr_1 <- paste(datrestr_1$ine_mod, 
                           datrestr_1$session_mod)
datrestr_1 <- filter(mutate(dat, id2 = paste(ine_mod, session_mod)),
                     id2 %in% ineses_datrestr_1) %>% select(- id2)
save(datrestr_1, 
     file = here("datrestr_1_def", "datrestr_1_def.rda"),
     version = 2)
  # Vérifié que ça marche. 