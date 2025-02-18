# 08_construction_groupes_td. Ne marche pas avec jobs. Bizarre.

library(here)
library(tidyverse)

# Chargement --------------------------------------------------------------

load(here("00_donnees_originales", "groupes_td.rda"))

# Sans date de naissance pour les AES pour uniformiser avec les ECO -------

for (i in ls()[str_detect(ls(), "^aes\\_") & ! str_detect(ls(), "\\_sud")]) {
  assign(i, select(get(i), - `Date de naissance`))
}

aes_sud <- select(aes_sud, - nais)

# Nom de variables --------------------------------------------------------

for (i in ls()[str_detect(ls(), "^aes\\_") & 
               ! str_detect(ls(), "\\_sud")]) {
  assign(i, rename(get(i), 
                   id_univ = `N° Etudiant`,
                   nom = NOM, prenom = PRENOM))
}

for (i in ls()[str_detect(ls(), "^eco\\_") & 
               ! str_detect(ls(), "\\_sud")]) {
  assign(i, rename(get(i),
                   id_univ = `N° ETUDIANT`,
                   nom = NOM, prenom = PRENOM))
}

eco_sud <- mutate(eco_sud, groupe_td_det = groupe_td)

# Empilement --------------------------------------------------------------

groupes_td <- ls()[str_detect(ls(), "^aes\\_|^eco\\_")] %>% lapply(get) %>% 
  do.call(what = rbind)

# Filière -----------------------------------------------------------------

groupes_td <- mutate(groupes_td, filiere = str_extract(groupe_td, "^aes|^eco"))

# Sauvegarde --------------------------------------------------------------

save(groupes_td,
     file = here("01_v2_construction_def", 
                 "02_z01_construction_groupes_td.rda"),
     version = 2)
