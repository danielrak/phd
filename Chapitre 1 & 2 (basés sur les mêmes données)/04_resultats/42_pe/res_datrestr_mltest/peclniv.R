# peclniv. (def). 
# Liste les clniv. Pour ne pas avoir à les trouver à chaque fois. 

# Packages ----------------------------------------------------------------

library(here)
library(tidyverse)

# Chargement --------------------------------------------------------------

load(here("01_01_datcompl_def.rda"))

# clniv -------------------------------------------------------------------

datcompl_clniv_sup <- group_by(datcompl, divconstatrneses) %>%
  mutate(c = paste(sort(unique(niveau_section)), 
                      collapse = " ET ")) %>%
  ungroup %>% 
  filter(c == "sup") %>% select(- c)
# 61 classes-années : 23 en 2014, 16 en 2015 et 22 en 2016.
# vérifié (avec clefgestion_constat) : ce sont les bons.

datcompl_clniv_inf <- group_by(datcompl, divconstatrneses) %>%
  mutate(c = paste(sort(unique(niveau_section)), 
                      collapse = " ET ")) %>%
  ungroup %>% 
  filter(c == "inf") %>% select(- c) 
# 248 classes-années : 85 en 2014 82 en 2015 et 81 en 2016.

# Sauvegarde --------------------------------------------------------------

save(datcompl_clniv_sup, datcompl_clniv_inf,
     file = here("peclniv.rda"),
     version = 2)