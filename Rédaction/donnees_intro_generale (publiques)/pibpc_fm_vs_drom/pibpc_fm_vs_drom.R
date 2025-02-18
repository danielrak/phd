# pibpc_fm_vs_drom. 
  # 2022-08-16.

library(here)
library(tidyverse)
library(readxl)

# Chargement --------------------------------------------------------------

pibpc_fm_vs_drom <- 
  read_xlsx(here("donnees_intro_generale", "pibpc_fm_vs_drom",
          "pibpc_fm_vs_drom.xlsx"))

# Nom des colonnes --------------------------------------------------------

names(pibpc_fm_vs_drom) <- c("annee", "pib_drom", 
                             "pib_france", "pib_fm")

# pivot longer ------------------------------------------------------------

pibpc_fm_vs_drom <- pivot_longer(pibpc_fm_vs_drom, 
                                 c("pib_drom", "pib_france", "pib_fm")) %>% 
  select(name, annee, value) %>% arrange(name, annee) %>% 
  mutate(name = factor(name, levels = c("pib_france", "pib_fm", "pib_drom"),
                       labels = c("France entière", 
                                  "France métropolitaine", "DROM"))) %>% 
  rename(Champ = name)

# graphique ---------------------------------------------------------------

pibpc_fm_drom <- pibpc_fm_vs_drom %>% ggplot(aes(annee, value)) + 
  geom_line(aes(group = Champ, color = Champ, linetype = Champ), size = 1)

# Sauvegarde --------------------------------------------------------------

save(pibpc_fm_drom, 
     file = here("donnees_intro_generale", "pibpc_fm_vs_drom", 
                 "pibpc_fm_vs_drom.rda"), 
     version = 2)

