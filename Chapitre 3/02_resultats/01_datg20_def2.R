# 01_datg20 (def2). 
  # Fait suite à datg20_neo, 
  # Puisque les résultats semblent tenir sur l'ensemble 
  # et non pas sur les néo uniquement. 


library(here)
library(tidyverse)
library(gtools)
library(fastDummies)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions2.rda")
load("D:/00_phd/03_g20/01_v2_construction_def/04_pre_analyse.rda")
load("D:/00_phd/03_g20/01_v2_construction_def/07_construction_autres_notes.rda")

  # pour récup note_td_normpop (après avoir lancé le script).
load(here("01_donnees_autres_def2.rda"))

# Sélection de variables --------------------------------------------------

  # On prend tout. 
datg20 <- g20_retr

# Filtre : néo-bacheliers -------------------------------------------------

# datg20 <- filter(datg20, annee_bacv2 == "2020" & 
#                    str_sub(id_univ, 1, 2) == "40") %>% 
#   mutate_if(is.factor, droplevels)

# Filtre : sans doublons de personnes -------------------------------------
  # Par rapport à id_univ. 
  # num_ps peut avoir des doublons sans que ça ne soit les mêmes personnes.
datg20 <- filter(datg20, duplicated(id_univ)) %>% pull(id_univ) %>% 
  (function (i) filter(datg20, ! id_univ %in% i)) # 371 individus.

# Récupération note_td_normpop --------------------------------------------

  # voir cahier rouge.
datg20 <- left_join(datg20 %>% mutate(id_univ = as.character(id_univ)), 
                    select(g20_tous, id_univ, filiere, note_td_normpop),
                    by = c("id_univ", "filiere")) # nobs ok.

# Récupération notes normpop ----------------------------------------------

datg20 <- left_join(datg20, 
                    select(g20_tous, id_univ, filiere, 
                           note_ctqcm_normpop, note_ue_normpop,
                           note_gestion_normpop, note_economie_normpop),
                    by = c("id_univ", "filiere")) # nobs ok. 

# Jointure : notes autres matières ----------------------------------------
  
  # Rappel : id_univ suffit comme clef de jointure. 
datg20 <- mutate(datg20, id_univ = as.character(id_univ))
datg20 <- left_join(datg20, 
                    note %>% select(- c(nom, prenom, nais_an, 
                                        campus_an)),
                    by = c("id_univ", "filiere")) # nobs ok. 
  # Cohérence vérifiée : les aes ont socio et droit en plus. 

# Refactorisation filiere -------------------------------------------------

  # Le left_join casse le factor. 
datg20 <- mutate(datg20, 
                 filiere = factor(filiere, levels = c("aes", "eco")))

# Comportement matières majeures ------------------------------------------

datg20 <- mutate(datg20, 
                 note_notdbl_ctqcm_v2 = 
                   if_else(note_notdbl_ctqcm == "abi", "abi", 
                           if_else(note_notdbl_ctqcm == "abj", "abj", 
                                   if_else(as.numeric(note_notdbl_ctqcm) == 0, "zero", "sup0"))), 
                 
                 note_notdbl_droit_v2 = 
                   if_else(is.na(note_notdbl_droit), "NA", # puisque tous ceux qui ont passé les maths n'ont pas forcément
                            # passé les autres matières.
                           if_else(note_notdbl_droit == "abi", "abi", 
                           if_else(note_notdbl_droit == "abj", "abj", 
                                   if_else(as.numeric(note_notdbl_droit) == 0, "zero", "sup0")))),
                 
                  
                 note_notdbl_economie_v2 = 
                   if_else(is.na(note_notdbl_economie), "NA",
                           if_else(note_notdbl_economie == "abi", "abi", 
                           if_else(note_notdbl_economie == "abj", "abj", 
                                   if_else(as.numeric(note_notdbl_economie) == 0, "zero", "sup0")))),
                 
                  
                 note_notdbl_gestion_v2 = 
                   if_else(is.na(note_notdbl_gestion), "NA",
                           if_else(note_notdbl_gestion == "abi", "abi", 
                           if_else(note_notdbl_gestion == "abj", "abj", 
                                   if_else(as.numeric(note_notdbl_gestion) == 0, "zero", "sup0")))),
                 
                  
                 note_notdbl_mathematiques_v2 = 
                   if_else(is.na(note_notdbl_mathematiques), "NA",
                           if_else(note_notdbl_mathematiques == "abi", "abi", 
                           if_else(note_notdbl_mathematiques == "abj", "abj", 
                                   if_else(as.numeric(note_notdbl_mathematiques) == 0, "zero", "sup0")))),
                 
                  
                 note_notdbl_sociologie_v2 = 
                   if_else(is.na(note_notdbl_sociologie), "NA",
                           if_else(note_notdbl_sociologie == "abi", "abi", 
                           if_else(note_notdbl_sociologie == "abj", "abj", 
                                   if_else(as.numeric(note_notdbl_sociologie) == 0, "zero", "sup0")))))
  # warnings mais pas grave. 


  # Rappel : maths, eco, gestion => majeures, en commun ou pas (v2).
# datg20 <- mutate(datg20, 
#                  comp_majeures = paste(note_notdbl_ctqcm,
#                                        note_notdbl_economie,
#                                        note_notdbl_gestion),
#                  comp_majeures_v2 = if_else(filiere == "eco", 
#                                             comp_majeures, paste(note_notdbl_ctqcm, 
#                                                                  note_notdbl_economie,
#                                                                  note_notdbl_gestion, 
#                                                                  note_notdbl_droit,
#                                                                  note_notdbl_sociologie)),
#                  comp_majeures_v2_maxabi = if_else(filiere == "eco", as.numeric(comp_majeures_v2 == "abi abi abi"),
#                                                    as.numeric(comp_majeures_v2 == "abi abi abi abi abi")))

  # comp_autres_majeures. 

datg20 <- mutate(datg20, 
                 comp_autres_majeures = if_else(filiere == "eco",
                                                paste(note_notdbl_economie_v2, 
                                                      note_notdbl_gestion_v2),
                                                paste(note_notdbl_economie_v2,
                                                      note_notdbl_gestion_v2,
                                                      note_notdbl_droit_v2,
                                                      note_notdbl_sociologie_v2)),
                 comp_autres_majeures_maxabi = as.numeric(
                   (filiere == "aes" & 
                      str_count(comp_autres_majeures, "abi") == 4) |
                     (filiere == "eco" & str_count(comp_autres_majeures, "abi") == 2)),
                 
                 comp_autres_majeures_NAabi =
                   if_else(filiere == "aes", 
                           (str_count(comp_autres_majeures, "abi") + 
                             str_count(comp_autres_majeures, "NA")) == 4,
                           (str_count(comp_autres_majeures, "abi") + 
                             str_count(comp_autres_majeures, "NA")) == 2
                           ) %>% as.numeric # ne semble pas avoir de sens particulier. 
                 # Il y a 171 individus qui ont soit NA soit abi partout ailleurs. 
                 )

# Dummy cols --------------------------------------------------------------

  # Certaines n'ont pas spécialement de sens mais pratique pour les stats.
datg20 <- dummy_cols(datg20, 
                     c("serie_diplome_psv3",
                       "serie_diplome_psv4",
                       "mention_diplome_ps",
                       "sexe_ps", "pays_nais_fr", "nationalite",
                       "boursier", "statut_etab", "type_etabv2",
                       "dep_etabv2", "campus", 
                       "filiere_ps",
                       "filiere", "mineure_sante",
                       "niv_classe", "avis_ce")) 

# Traitements : replace_na(0) ---------------------------------------------

  # On garde les variables originales

datg20 <- mutate(datg20, 
                 sessions_b24_ori = sessions_b24,
                 sessions_a23_ori = sessions_a23,
                 videos_views_b24_ori = videos_views_b24,
                 videos_views_a23_ori = videos_views_a23,
                 videos_comp_b24_ori = videos_comp_b24,
                 videos_comp_a23_ori = videos_comp_a23,
                 sheets_views_b24_ori = sheets_views_b24,
                 sheets_views_a23_ori = sheets_views_a23,
                 workpoints_b24_ori = workpoints_b24,
                 workpoints_a23_ori = workpoints_a23)


  # Justifié par les échanges de mail de Moreau, voir Docear. 
datg20 <- mutate(datg20, 
                 connect = as.numeric(! is.na(sessions_b24)),
                 
                 sessions_b24 = replace_na(sessions_b24, 0),
                 sessions_a23 = replace_na(sessions_a23, 0),
                 videos_views_b24 = replace_na(videos_views_b24, 0),
                 videos_views_a23 = replace_na(videos_views_a23, 0),
                 videos_comp_b24 = replace_na(videos_comp_b24, 0),
                 videos_comp_a23 = replace_na(videos_comp_a23, 0),
                 sheets_views_b24 = replace_na(sheets_views_b24, 0),
                 sheets_views_a23 = replace_na(sheets_views_a23, 0),
                 workpoints_b24 = replace_na(workpoints_b24, 0),
                 workpoints_a23 = replace_na(workpoints_a23, 0))

# Traitements : avant et après l'évaluation du 24 août. -------------------

datg20 <- mutate(datg20,
                 sessions_tot = sessions_b24 + sessions_a23,
                 videos_views_tot = videos_views_b24 + videos_views_a23,
                 videos_comp_tot = videos_comp_b24 + videos_comp_a23,
                 sheets_views_tot = sheets_views_b24 + sheets_views_a23,
                 workpoints_tot = workpoints_b24 + workpoints_a23,
                 
                 sessions_tot_ori = sessions_b24_ori + sessions_a23_ori,
                 videos_views_tot_ori = videos_views_b24_ori + videos_views_a23_ori,
                 videos_comp_tot_ori = videos_comp_b24_ori + videos_comp_a23_ori,
                 sheets_views_tot_ori = sheets_views_b24_ori + sheets_views_a23_ori,
                 workpoints_tot_ori = workpoints_b24_ori + workpoints_a23_ori,
                 )

# Traitements : normalisés ------------------------------------------------
  # Pour voir si l'ampleur est grande. 

datg20 <- mutate(datg20, 
                 
                 sessions_tot_norm = 
                   (sessions_tot - mean(sessions_tot, na.rm = TRUE)) / 
                   sd(sessions_tot, na.rm = TRUE),
                 
                 videos_views_tot_norm = 
                   (videos_views_tot - mean(videos_views_tot, 
                                            na.rm = TRUE)) / 
                   sd(videos_views_tot, na.rm = TRUE),
                 
                 videos_comp_tot_norm = 
                   (videos_comp_tot - mean(videos_comp_tot, 
                                            na.rm = TRUE)) / 
                   sd(videos_comp_tot, na.rm = TRUE),
                 
                 sheets_views_tot_norm = 
                   (sheets_views_tot - mean(sheets_views_tot, 
                                            na.rm = TRUE)) / 
                   sd(sheets_views_tot, na.rm = TRUE),
                 
                 workpoints_tot_norm = 
                   (workpoints_tot - mean(workpoints_tot, 
                                          na.rm = TRUE)) / 
                   sd(workpoints_tot, na.rm = TRUE),
                 
                 
                 
                 sessions_tot_ori_norm = 
                   (sessions_tot_ori - mean(sessions_tot_ori,
                                            na.rm = TRUE)) / 
                   sd(sessions_tot_ori, na.rm = TRUE),
                 
                 videos_views_tot_ori_norm = 
                   (videos_views_tot_ori - mean(videos_views_tot_ori, 
                                            na.rm = TRUE)) / 
                   sd(videos_views_tot_ori, na.rm = TRUE),
                 
                 videos_comp_tot_ori_norm = 
                   (videos_comp_tot_ori - mean(videos_comp_tot_ori, 
                                            na.rm = TRUE)) / 
                   sd(videos_comp_tot_ori, na.rm = TRUE),
                 
                 sheets_views_tot_ori_norm = 
                   (sheets_views_tot_ori - mean(sheets_views_tot_ori, 
                                            na.rm = TRUE)) / 
                   sd(sheets_views_tot_ori, na.rm = TRUE),
                 
                 workpoints_tot_ori_norm = 
                   (workpoints_tot_ori - mean(workpoints_tot_ori, 
                                          na.rm = TRUE)) / 
                   sd(workpoints_tot_ori, na.rm = TRUE)
                 )

# Traitement >= 1 (à part workpoints) -------------------------------------

datg20 <- mutate(datg20,
                 
                 sessions_b24_sup1 = as.numeric(sessions_b24 >= 1),
                 sessions_tot_sup1 = as.numeric(sessions_tot >= 1),
                 
                 videos_views_b24_sup1 = as.numeric(videos_views_b24 >= 1),
                 videos_views_tot_sup1 = as.numeric(videos_views_tot >= 1),
                 
                 videos_comp_b24_sup1 = as.numeric(videos_comp_b24 >= 1),
                 videos_comp_tot_sup1 = as.numeric(videos_comp_tot >= 1),
                 
                 sheets_views_b24_sup1 = as.numeric(sheets_views_b24 >= 1),
                 sheets_views_tot_sup1 = as.numeric(sheets_views_tot >= 1),
                 
                 # workpoints : pas pertinent. 
                 )

# Traitement : indicateurs synthétiques -----------------------------------

datg20 <- mutate(datg20, 
                 pratique_tot = videos_comp_tot + sheets_views_tot,
                 pratique_tot_sup1 = as.numeric(videos_comp_tot >= 1 |
                                                  sheets_views_tot >= 1),
                 
                 pratique_tot_norm = 
                   (pratique_tot - mean(pratique_tot,
                                        na.rm = TRUE)) / 
                   sd(pratique_tot, na.rm = TRUE),
                 
                 pratique_tot2 = videos_views_tot + sheets_views_tot,
                 pratique_tot2_sup1 = as.numeric(videos_views_tot >= 1 |
                                                   sheets_views_tot >= 1),
                 pratique_tot2_norm = (pratique_tot2 - 
                                         mean(pratique_tot2,
                                              na.rm = TRUE)) / 
                   sd(pratique_tot2, na.rm = TRUE),
                 
                 
                 
                 pratique_tot_ori = videos_comp_tot_ori 
                 + sheets_views_tot_ori,
                 
                 pratique_tot_ori_norm = (
                   pratique_tot_ori - mean(pratique_tot_ori, na.rm = TRUE)
                   ) / sd(pratique_tot_ori, na.rm = TRUE),
                 
                 pratique_tot2_ori = videos_views_tot_ori 
                 + sheets_views_tot_ori,
                 
                 pratique_tot2_ori_norm = (
                   pratique_tot2_ori - mean(pratique_tot2_ori, na.rm = TRUE)
                   ) / sd(pratique_tot2_ori, na.rm = TRUE),
                 )

# Outcomes : venus --------------------------------------------------------

datg20 <- mutate(datg20, 
                 venu_td = as.numeric(! is.na(note_td)),
                 venu_ctqcm = as.numeric(! is.na(note_ctqcm)),
                 venu = venu_td * venu_ctqcm,
                 
                 venu_gestion = as.numeric(! is.na(note_gestion)),
                 venu_economie = as.numeric(! is.na(note_economie)))

# Note ctqcm replace abi 0 pour ceux qui n'ont pas abi --------

# datg20$note_ctqcm_v2 <- datg20$note_ctqcm
# datg20$note_ctqcm_v2[datg20$note_notdbl_ctqcm == "abi" & 
#                               datg20$comp_autres_majeures_maxabi != 1] <- 0
  # Les seuls qu'on remplace par zéro : non abi partout ailleurs.
  # abi partout ailleurs => pas de vrais étudiants. 
  # abi et NA partout ailleurs : redoublants qui se sont pas pointés à la matière qu'ils sont censés rattraper. 
  # En fait y en a que trois qui sont abi et n'ont pas maxabi ailleurs. 
# select(datg20, note_notdbl_ctqcm, note_ctqcm, note_ctqcm_v2, comp_autres_majeures_maxabi, comp_autres_majeures) %>% filter(note_notdbl_ctqcm == "abi" & ! is.na(note_ctqcm_v2))


  # Équivalent puisque dans les néo, être abi en maths c'est être abi partout aillers dans les autres majeures. 


# Note td replace NA 0 pour ceux qui n'ont pas abi partout ailleur --------

datg20$note_td_v2 <- datg20$note_td
datg20$note_td_v2[
  is.na(datg20$note_td) & 
  datg20$comp_autres_majeures_maxabi == 0] <- 0

# Outcome supérieur à la moitié du total ----------------------------------

datg20 <- mutate(datg20, 
                 maths_30_sup15 = as.numeric(maths_30 >= 15),
                 note_ctqcm_sup10 = as.numeric(note_ctqcm >= 10),
                 note_td_sup10 = as.numeric(note_td >= 10),
                 note_td_v2_sup10 = as.numeric(note_td_v2 >= 10),
                 note_ue_sup10 = as.numeric(note_ue >= 10))

# Note au bac : normalisation ---------------------------------------------

  # Normalisation par rapport à serie_psv2R corrigée. 
  # Trop peu d'observations si on normalise par rapport à l'année.
  # La correction consiste à mettre STI2D dans autres (2obs). 
  # Normalisation sur les NA : questionnable (?). 

datg20 <- mutate(datg20, 
                 serie_psv2Rcorr = fct_collapse(
                   serie_psv2R, "autres" = c("STI2D", "autres")
                 ))

datg20 <- group_by(datg20, serie_psv2Rcorr) %>% 
  mutate(moy_bac_norm = (moy_bac - mean(moy_bac, na.rm = TRUE)) / 
           sd(moy_bac, na.rm = TRUE),
         moy_fran_ec_norm = (moy_fran_ec - mean(moy_fran_ec, 
                                                na.rm = TRUE)) / 
           sd(moy_fran_ec, na.rm = TRUE),
         moy_maths_norm = (moy_maths - mean(moy_maths, na.rm = TRUE)) /
           sd(moy_maths, na.rm = TRUE)) %>% 
  ungroup
  # Il est justifié d'avoir fait cette normalisation : 
    # voir code ci-dessous : 
  # split(datg20, datg20$serie_psv2Rcorr) %>% lapply(function (x) rbind(summary(x$moy_bac), summary(x$moy_bac_norm)) %>% round(2))
  # Moyennes différentes. 
  # La limite de la normalisation c'est que ça élimine juste
  # la différence de moyenne : les différences sur les quartiles restent 
  # les essentiellement les mêmes. 

# Notes évaluation août 2020 : normalisation ------------------------------

  # Si NA : pas venu. 
  # Rappel : on sait uniquement ce qui a été mesuré en maths.

datg20 <- mutate(datg20, 
                 cg_20_norm = (cg_20 - mean(cg_20, na.rm = TRUE)) / 
                   sd(cg_20, na.rm = TRUE),
                 fran1_20_norm = (fran1_20 - mean(fran1_20, 
                                                  na.rm = TRUE)) /
                   sd(fran1_20, na.rm = TRUE),
                 fran2_20_norm = (fran2_20 - mean(fran2_20,
                                                  na.rm = TRUE)) / 
                   sd(fran2_20, na.rm = TRUE),
                 maths_30_norm = (maths_30 - mean(maths_30, 
                                                  na.rm = TRUE)) / 
                   sd(maths_30, na.rm = TRUE))

# Notes maths S1 : normalisation ------------------------------------------

  # On normalise par filière pour l'ue et td car différents td. 
  # On normalise sur tout le monde pour ctqcm. NON : pas les mêmes exams.
datg20 <- group_by(datg20, filiereR, campus) %>% 
  mutate(note_ue_norm = (note_ue - mean(note_ue, na.rm = TRUE)) / 
           sd(note_ue, na.rm = TRUE),
         note_td_norm = (note_td - mean(note_td, na.rm = TRUE)) / 
           sd(note_td, na.rm = TRUE),
         note_ctqcm_norm = (note_ctqcm - mean(note_ctqcm, na.rm = TRUE)) / 
           sd(note_ctqcm, na.rm = TRUE)) %>% 
  ungroup

# Normalisation note_ctqcm_v2 ---------------------------------------------

# datg20 <- group_by(datg20, filiereR, campus) %>% 
#   mutate(note_ctqcm_v2_norm = (note_ctqcm_v2 - 
#                                  mean(note_ctqcm_v2, na.rm = TRUE)) / 
#            sd(note_ctqcm_v2, na.rm = TRUE)) %>% 
#   ungroup 

# Normalisation note_td_v2 ------------------------------------------------

datg20 <- group_by(datg20, filiere, campus) %>% 
  mutate(note_td_v2_norm = (note_td_v2 - mean(note_td_v2, na.rm = TRUE)) / 
           sd(note_td_v2, na.rm = TRUE)) %>% 
  ungroup

# Quintiles  - niveau au bac ----------------------------------------------

  # MAJ2022_03_13 : à y réfléchir encore. 
  # Par année : pas terrible. 
  # => On calcule les quintiles sur tout l'éch. 
  # On le calcule à partir des scores normalisés. Discutable. 
datg20 <- mutate(datg20, 
                 q5moy_bac_norm = 
                   quantcut(moy_bac_norm, 5) %>% 
                   factor(labels = paste("q", 1:5, sep = "")))

datg20 <- dummy_cols(datg20, "q5moy_bac_norm")

# age_26aout : regroupement -----------------------------------------------

datg20 <- mutate(datg20, 
                 q5age_26aout = quantcut(age_26aout, 5) %>% 
                   factor(labels = paste("q", 1:5, sep = "")))

# id_univ : indication si redoublement (univ run ?) -----------------------

datg20 <- mutate(datg20, 
                     id_univ2 = substr(id_univ, 1, 2),
                     id_univ2 = if_else(as.numeric(id_univ2) < 39,
                                        "39m", if_else(id_univ2 == "39",
                                                       "39", "40")) %>% 
                   factor(levels = c("40", "39", "39m"))) %>% 
  mutate_if(is.factor, droplevels)

# Filtre : venus aux td ---------------------------------------------------

datg20_venus_td <- filter(datg20, venu_td == 1)

# Filtre : venus aux examens ----------------------------------------------

datg20_venus_ctqcm <- filter(datg20, venu_ctqcm == 1)

# Données sans les observations questionnables ----------------------------

# datg20_red <- filter(datg20,
#                      ! serie_diplome_psv3 %in% c("bacgp15", "stp15") &
#                       age_26aout <= 25 &
#                        ! (id_univ2 == "39" & boursier == "secondaire") # voir carte mentale
#                       # exploration données (pas cohérent).
#                      ) # 355 observations. 
# (datg20 rend de toute façon tout non significatif maj2022_05_19).

# Données sans NA des variables de contrôle -------------------------------

# datg20 <- filter(datg20,
#                           ! is.na(moy_bac) &
#                             ! is.na(statut_etab) # il n'y a plus
#                           # de datg20 statut etab NA parmi les néo-bacheliers.
#                           ) # 259 obs.

  # Pas besoin puisque de toute façon les estimations se font sans les valeurs manquantes de moy_bac.



# Données sans les observations questionnables et sans NA des vars --------

# datg20_red_noNAcovs <- filter(datg20_red, 
#                           ! is.na(moy_bac) & 
#                             ! is.na(statut_etab)) 
  # équivalent à datg20_noNAcovs. 

# Données (base) sans 0 aux exams ------------------------------------------------
  
# datg20_red_noNAcovs_nozero <- filter(datg20_red_noNAcovs, note_ctqcm > 0)
  # Pas pertinent, et en plus plombe la première étape (voir investigations). 

# Données (base) sans 3 abi aux majeures communes --------------------------------

# datg20_red_noNAcovs_no3abi <- filter(datg20_red_noNAcovs, 
#                                      comp_majeures != "abi abi abi") # intérêt : modifie fait monter note_ue. 
    # Vaux mieux prendre celui d'en dessous. 

# Données (base) sans abi dans les majeures (pas communes) ----------------

datg20_nomaxabi <- filter(datg20,
                          ! (note_notdbl_ctqcm == "abi" & 
                               comp_autres_majeures_maxabi == 1))  
  # 326 observations. 

# Renommage des bases pour éviter confusion avec les anciennes ver --------

# for (i in ls()[str_detect(ls(), "^datg20")]) {
#   assign(paste(str_extract(i, "^datg20"), "_neo", 
#                str_remove(i, "^datg20"), sep = ""),
#          get(i))
# }

# Sauvegarde --------------------------------------------------------------

save(list = ls()[str_detect(ls(), "^datg20")
                 # & 
                   # str_detect(ls(), "neo")
                 ],
     file = here("01_datg20_def2.rda"),
     version = 2)
