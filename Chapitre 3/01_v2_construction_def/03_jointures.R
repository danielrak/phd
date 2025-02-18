# 03_jointures (def). 
  # lancer en exécution et non avec job.
  # Avec toutes les notes possibles cette fois. 
  # Voir 01_construction/03_jointures_exos_diverses.R (histoire des prenoms).
   

library(here)
library(tidyverse)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions2.rda")
load(here("01_v2_construction_def", "01_construction_pg20.rda"))
load(here("01_v2_construction_def", "02_construction_nm.rda"))
load(here("01_v2_construction_def", "02_z01_construction_groupes_td.rda"))

# Préparation noms et prénoms pg20.  --------------------------------------

pg20 <- mutate(pg20, 
               nom_bdd = unaccent2(nom_bdd) %>% 
                            str_replace(" ", "_") %>% 
                            str_replace("-", "_") %>% str_to_upper,
               prenom_bdd = unaccent2(prenom_bdd) %>% 
                            str_replace(" ", "_") %>% 
                            str_replace("-", "_") %>% str_to_upper,
               prenom2_bdd = unaccent2(prenom2_bdd) %>% 
                            str_replace(" ", "_") %>% 
                            str_replace("-", "_") %>% str_to_upper,
               prenom3_bdd = unaccent2(prenom3_bdd) %>% 
                            str_replace(" ", "_") %>% 
                            str_replace("-", "_") %>% str_to_upper)


# Préparation noms et prénoms notes ---------------------------------------

lapply(list( # ue
  "maths_ue_l1_aes_s1", "maths_ue_l1_eco_s1",
  "maths_ue_l1_aes_sud_s1", "maths_ue_l1_eco_sud_s1",
            
  # autres : 
  "qcm_maths_cc_l1_aes", "maths_td_l1_aes_s1",
  "qcm_maths_cc_l1_eco_s1", "maths_td_l1_eco_s1",
  "maths_ct_l1_aes_sud_s1", "maths_td_l1_aes_sud_s1",
  "maths_ct_l1_eco_sud_s1", "maths_td_l1_eco_sud_s1"),
       function (x) {
         assign(x, 
                mutate(get(x),
                       nom = unaccent2(nom) %>% str_replace(" ", "_") %>% 
               str_replace("-", "_") %>% str_to_upper,
             prenom = unaccent2(prenom) %>% str_replace(" ", "_") %>% 
               str_replace("-", "_") %>% str_to_upper),
                pos = sys.frame())
         invisible()
       })

# rbind : ue uniquement ---------------------------------------------------

maths_ue <- rbind(maths_ue_l1_aes_s1, maths_ue_l1_eco_s1,
                  maths_ue_l1_aes_sud_s1, maths_ue_l1_eco_sud_s1)

# Observations pg20 : 436 étudiants ---------------------------------------

# Observations maths_ue : 1763. -------------------------------------------

# Jointures ue ------------------------------------------------------------

  # left_join. pour comparer retr/non-retr.
g20_tous <- left_join(maths_ue, pg20 %>% 
                           mutate(nais_psverif = nais_ps), 
                         by = c("nom" = "nom_bdd",
                                "nais_nm" = "nais_ps")) # nobs ok. 
  # Il y a des numéros étudiants qui reviennent : normal (?). 
  # exemple : un étudiant qui fait une mineure. 

  # inner_join. 
g20_retr <- inner_join(maths_ue, pg20, 
                         by = c("nom" = "nom_bdd",
                                "nais_nm" = "nais_ps")) # 373. 
  # dans pg20, il n'y a pas de is.na(nais_ps). 
  # je peux donc vérifier : g20_tous_ue %>% filter(! is.na(nais_psverif))
    # même nobs. 
  # rappel : cette jointure contient déjà tout le monde.
    # y compris ce que j'aurais obtenu en utilisant les 3 prénoms comme clefs.
    # donc c'est bon. 

# rbind : td --------------------------------------------------------------


maths_td <- rbind(maths_td_l1_aes_s1, maths_td_l1_eco_s1,
                  maths_td_l1_aes_sud_s1, maths_td_l1_eco_sud_s1)

# Jointures td ------------------------------------------------------------

  # rappel : eco/aes nord/sud : ont des notes de td. 
g20_tous <- left_join(g20_tous, 
                      transmute(maths_td, id_univ, nom, nais_nm,
                             note_td, note_notdbl_td, filiere, site),
                      by = c("id_univ", "nom", "nais_nm", 
                             "filiere", "site")) # nobs ok. 

g20_retr <- inner_join(g20_retr, 
                      transmute(maths_td, id_univ, nom, nais_nm,
                             note_td, note_notdbl_td, filiere, site),
                      by = c("id_univ", "nom", "nais_nm", 
                             "filiere", "site")) # 373. 
  # vérif : filter(g20_tous, ! is.na(nais_psverif))
  # même logique que ci-dessus. 

# rbind : ctqcm -----------------------------------------------------------


maths_ct_l1_aes_sud_s1 <- rename(maths_ct_l1_aes_sud_s1,
                                 note_ctqcm = note_ct, 
                                 bareme_ctqcm = bareme_ct,
                                 note_notdbl_ctqcm = note_notdbl_ct)
maths_ct_l1_eco_sud_s1 <- rename(maths_ct_l1_eco_sud_s1,
                                 note_ctqcm = note_ct, 
                                 bareme_ctqcm = bareme_ct,
                                 note_notdbl_ctqcm = note_notdbl_ct)


qcm_maths_cc_l1_aes <- rename(qcm_maths_cc_l1_aes,
                              note_ctqcm = note_qcm, 
                              bareme_ctqcm = bareme_qcm,
                              note_notdbl_ctqcm = note_notdbl_qcm)
qcm_maths_cc_l1_eco_s1 <- rename(qcm_maths_cc_l1_eco_s1,
                              note_ctqcm = note_qcm, 
                              bareme_ctqcm = bareme_qcm,
                              note_notdbl_ctqcm = note_notdbl_qcm)

maths_ctqcm <- rbind(maths_ct_l1_aes_sud_s1,
                     maths_ct_l1_eco_sud_s1,
                     qcm_maths_cc_l1_aes,
                     qcm_maths_cc_l1_eco_s1)

# Jointures ctqcm ---------------------------------------------------------
  # intérêt d'unifier ctqcm : puisque contrôle terminal et qcm 
  # sont l'examen final, jusqu'au sud ils ont appelé le fichier ct
  # et au nord qcm. 

g20_tous <- left_join(g20_tous, 
                      transmute(maths_ctqcm, id_univ, nom, nais_nm,
                             note_ctqcm, note_notdbl_ctqcm, filiere, site),
                      by = c("id_univ", "nom", "nais_nm", 
                             "filiere", "site")) # nobs ok. 

g20_retr <- inner_join(g20_retr, 
                      transmute(maths_ctqcm, id_univ, nom, nais_nm,
                             note_ctqcm, note_notdbl_ctqcm, filiere, site),
                      by = c("id_univ", "nom", "nais_nm", 
                             "filiere", "site")) # 373. 

# Jointures : groupes_td --------------------------------------------------

groupes_td <- groupes_td %>% mutate(clef = paste(id_univ, filiere)) %>% 
  filter(! duplicated(clef))

g20_tous <- left_join(g20_tous, transmute(groupes_td, id_univ = as.numeric(id_univ), filiere, 
                                          groupe_td, groupe_td_det),
                      by = c("id_univ"
                             , "filiere"
                             )) # nobs ok.
  # Trop de valeurs manquantes.

g20_retr <- left_join(g20_retr, transmute(groupes_td, id_univ = as.numeric(id_univ), filiere,
                                          groupe_td, groupe_td_det),
                      by = c("id_univ", "filiere")) # nobs ok. (373)

# Sauvegarde --------------------------------------------------------------

save(pg20, g20_tous, g20_retr, 
     file = here("01_v2_construction_def", "03_jointures.rda"),
     version = 2)
