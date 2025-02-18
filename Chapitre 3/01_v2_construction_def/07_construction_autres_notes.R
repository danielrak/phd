# 07_construction_autres_notes (def). 
  # Préférer boucle à lapply (job).
  # Ne marche pas avec job (ne sait pas pourquoi)

library(here)
library(tidyverse)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions2.rda")
load(here("00_donnees_originales", "autres_notes.rda"))

# Le code ci-dessous s'assure que toutes les bases 
  # ont le même nom de variable. 
verif <- ls()[str_detect(ls(), "note\\_")] %>% 
  lapply(function (x) get(x) %>% names) %>% (function (l) 
  lapply(l, function (x) identical(l[[1]], x))) %>% unlist %>% table

# Nom des variables et constructions mineures -----------------------------
  # _an : autres notes.
  
for (i in ls()[str_detect(ls(), "note\\_")]) {
  assign(i, 
         get(i) %>% 
           rename_all(function (x) str_to_lower(x) %>% unaccent2) %>% 
           transmute(id_univ = dossier,
                     nom, prenom,
                     nais_an = str_trim(naissance) %>% 
                       as.Date(format = "%d/%m/%Y"),
                     note1, resultat1, note2, resultat2,
                     note_notdbl = if_else(is.na(as.numeric(note2 %>% 
                                                   str_replace(",", "."))), 
                                           note1 %>% str_to_lower %>% 
                                             str_replace(",", "."), 
                                           note2 %>% str_to_lower %>% 
                                             str_replace(",", ".")),
                     note = as.numeric(note_notdbl %>%
                                         str_replace(",", ".")),
                     resultat = if_else(is.na(note2),
                                        resultat1, resultat2),
                     fichier = i,
                     matiere = str_extract(
                       i,
                       paste(c("droit", "economie",
                               "gestion", "mathematiques", "sociologie"),
                             collapse = "|")),
                     filiere = str_extract(i, "aes|eco"),
                     campus_an = str_extract(i, "tampon|moufia")
                     ))
}

  # Pour une filière, campus donnée, on ne détecte pas le même nombre
  # d'individus d'une matière à l'autre à cause des redoublants : 
  # ils ont par exemple déjà passé la matière l'année précédente
  # et ne sont plus inscrits par l'administration dans l'examen du s1. 

# Une ligne par individu --------------------------------------------------

note <- ls()[str_detect(ls(), "note\\_")] %>% 
  lapply(get) %>% do.call(what = rbind)

  # On laisse tomber droit et socio pour avoir les mêmes matières dans
  # les deux filières. 
note <- pivot_wider(note 
                    # %>% filter(! matiere %in% 
                                      # c("droit", "sociologie"))
                    , 
                    id_cols = c(id_univ, nom, prenom, 
                                nais_an, filiere, campus_an),
                    names_from = c(matiere),
                    values_from = c(note_notdbl, note, resultat))
  # des doublons d'id_univ (4) => réorientation.

# Sauvegarde --------------------------------------------------------------

save(note,
     file = here("01_v2_construction_def",
                 "07_construction_autres_notes.rda"),
     version = 2)
