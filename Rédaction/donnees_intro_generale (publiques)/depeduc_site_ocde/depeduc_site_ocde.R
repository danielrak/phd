# depeduc_site_ocde. 
  # 2022-08-13.

library(here)
library(tidyverse)

# Chargement --------------------------------------------------------------

pibeduc <- read_csv(here("donnees_intro_generale", "depeduc_site_ocde",
                         "pibeduc_site_ocde_v2.csv"))
  
depeleve <- read_csv(here("donnees_intro_generale", "depeduc_site_ocde",
                          "depeleve_site_ocde_primaire_postsecondaire.csv"))

# rename str to lower -----------------------------------------------------

pibeduc <- rename_all(pibeduc, str_to_lower)

depeleve <- rename_all(depeleve, str_to_lower)

# pibeduc - ocde vs france ------------------------------------------------
   # Attention : primaire jusq sup (hors petite enfance).

  # Ne pas oublier de marquer que pas de données entre 2006 et 2007.
  # Et que ça concerne le primaire jusqu'au tertiaire, hors petite enfance.
pibeduc_ocde_fra <- group_by(pibeduc %>% filter(subject %in% c("PRY_NTRY", "TRY")), 
                             location, time) %>%
  summarise(svalue = sum(value)) %>% ungroup %>% na.omit %>% 
  ggplot(aes(time, svalue)) + 
  geom_line(aes(group = location, color = location, linetype = location))
  

# depeleve - ocde vs france

# Ne pas oublier de marquer que pas de données entre 2006 et 2007.
depeleve_ocde_fra <- select(depeleve, location, time, svalue = value) %>% 
  ggplot(aes(time, svalue)) +
  geom_line(aes(group = location, color = location, linetype = location))

# Sauvegarde --------------------------------------------------------------

save(pibeduc_ocde_fra, depeleve_ocde_fra,
     file = here("donnees_intro_generale",
                 "depeduc_site_ocde", "depeduc_site_ocde.rda"),
     version = 2)
