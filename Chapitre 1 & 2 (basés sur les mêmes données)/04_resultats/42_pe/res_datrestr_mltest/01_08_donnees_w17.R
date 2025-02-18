# Données DNB avec DNB17, pour les effets endo. 
# Pas besoin de données de CM2. 
# Empilement. 
# NB : ne marche pas avece Ctrl + J (job). À cause de ç et é 
  # dans Français et Mathématiques. 

# Packages ----------------------------------------------------------------

library(here)
library(tidyverse)

# Chargement --------------------------------------------------------------

load(here("01_01_datcompl_def.rda"))
load(here("01_01_donnees_def.rda"))

load(here("01_07_datcompl17_def.rda"))
load(here("01_07_donnees17_def.rda"))

# Constructions nécessaires -----------------------------------------------

datcompl17 <- rename(datcompl17, 
                     ine_mod = Ine,
                     moy_ec1_norm = Total_Ecrit_norm,
                     moy_ec1_rp = Total_Ecrit_rp,
                     
                     moy_fran_ec_norm = Français_norm,
                     moy_fran_ec_rp = Français_rp,
                     
                     moy_maths_ec_norm = Mathématiques_norm,
                     moy_maths_ec_rp = Mathématiques_rp) %>% 
  mutate(session_mod == "2017")

dat17 <- rename(dat17, 
                     ine_mod = Ine,
                     moy_ec1_norm = Total_Ecrit_norm,
                     moy_ec1_rp = Total_Ecrit_rp,
                     
                     moy_fran_ec_norm = Français_norm,
                     moy_fran_ec_rp = Français_rp,
                     
                     moy_maths_ec_norm = Mathématiques_norm,
                     moy_maths_ec_rp = Mathématiques_rp) %>% 
  mutate(session_mod == "2017")

# Empilement --------------------------------------------------------------

names <- intersect(names(dat), names(dat17))

datcompl_w17 <- rbind(datcompl %>% select(names),
                      datcompl17 %>% select(names))

dat_w17 <- rbind(dat %>% select(names),
                 dat17 %>% select(names))

  # ok a priori. 

# Sauvegarde --------------------------------------------------------------

save(datcompl_w17, file = here("01_08_datcompl_w17_def.rda"),
     version = 2)

save(dat_w17, file = here("01_08_donnees_w17_def.rda"),
     version = 2)