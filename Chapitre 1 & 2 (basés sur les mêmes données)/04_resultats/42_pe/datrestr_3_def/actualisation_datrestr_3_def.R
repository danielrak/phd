# ine, session_mod datrestr_1 
# Pour ne pas avoir à relancer les tests KS à chaque fois. 

library(here)
library(tidyverse)

load(here("01_01_donnees_def.rda"))
load(here("datrestr_3_def", "datrestr_3_def.rda"))
file.copy(here("datrestr_3_def", "datrestr_3_def.rda"),
          here("datrestr_3_def", "datrestr_3_def_avant_actualisation.rda"))
ineses_datrestr_3 <- paste(datrestr_3$ine_mod, 
                           datrestr_3$session_mod)
datrestr_3 <- filter(mutate(dat, id2 = paste(ine_mod, session_mod)),
                     id2 %in% ineses_datrestr_3) %>% select(- id2)
save(datrestr_3, 
     file = here("datrestr_3_def", "datrestr_3_def.rda"),
     version = 2)
  # Vérifié que ça marche. 