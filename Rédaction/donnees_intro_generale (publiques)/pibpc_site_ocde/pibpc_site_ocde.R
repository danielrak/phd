# depeduc_site_ocde. 
  # 2022-08-14.

library(here)
library(tidyverse)

# Chargement --------------------------------------------------------------

pibpc <- read_csv(here("donnees_intro_generale", "pibpc_site_ocde",
                       "pib_ocde_1960_2021_v2.csv"))

# rename to str to lower --------------------------------------------------

pibpc <- rename_all(pibpc, str_to_lower)

# pibpc - ocde vs fra -----------------------------------------------------

pibpc_ocde_fra <- filter(pibpc, location %in% c("OECD", "FRA")) %>% 
  ggplot(aes(time, value)) +
  geom_line(aes(group = location, color = location, linetype = location))

# Sauvegarde --------------------------------------------------------------

save(pibpc_ocde_fra, 
     file = here("donnees_intro_generale", "pibpc_site_ocde",
                 "pibpc_site_ocde.rda"),
     version = 2)