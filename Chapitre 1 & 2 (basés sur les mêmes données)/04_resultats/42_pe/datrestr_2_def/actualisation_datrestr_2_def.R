# ine, session_mod datrestr_2 
# Pour ne pas avoir à relancer les tests KS à chaque fois. 

library(here)
library(tidyverse)

load(here("01_01_donnees_def.rda"))
load(here("datrestr_2_def", "datrestr_2_def.rda"))
file.copy(here("datrestr_2_def", "datrestr_2_def.rda"),
          here("datrestr_2_def", "datrestr_2_def_avant_actualisation.rda"))
ineses_datrestr_2 <- paste(datrestr_2$ine_mod, 
                           datrestr_2$session_mod)
datrestr_2 <- filter(mutate(dat, id2 = paste(ine_mod, session_mod)),
                     id2 %in% ineses_datrestr_2) %>% select(- id2)
save(datrestr_2, 
     file = here("datrestr_2_def", "datrestr_2_def.rda"),
     version = 2)
  # Vérifié que ça marche. 