library(here)
library(tidyverse)

ppc <- read_csv(here("donnees_intro_generale",
                     "cout_education", "pib_site_ocde", 
                     "pibpc_ocde_1960_2021.csv"))

# ppc ---------------------------------------------------------------------

ppc <- rename_all(ppc, str_to_lower) %>% 
  rename_all(function (x) str_replace(x, " ", "_"))

# Moyenne de l'OCDE et France ---------------------------------------------

filter(ppc, location != "OECD") %>% group_by(time) %>% 
  summarise(value = mean(value, na.rm = TRUE)) %>% 
  mutate(location = "oecd") %>% 
  (function (d) rbind(d, filter(ppc, location == "FRA") %>%
                        mutate(location = "fra") %>% 
                        select(time, value, location))) %>% 
  ggplot(aes(time, value)) + 
  geom_line(aes(group = location, 
                color = location, 
                linetype = location)) -> ppc_fra_oecd_graph

# Deuxième version du graphe ci-dessus ------------------------------------
  # Une valeur pour l'OCDE est déjà présente dans la base.

filter(ppc, location %in% c("FRA", "OECD")) %>% 
  ggplot(aes(time, value)) + 
  geom_line(aes(group = location, color = location, 
                linetype = location)) -> ppc_fra_oecd_graph2 

# Sauvegarde --------------------------------------------------------------

save(ppc_fra_oecd_graph, 
     ppc_fra_oecd_graph2,
     file = here("donnees_intro_generale", 
                 "cout_education", "pib_site_ocde", 
                 "code_ppc_fra_oecd_graph.rda"),
     version = 2)
