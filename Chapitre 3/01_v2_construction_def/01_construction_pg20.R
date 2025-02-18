# 01_construction_pg20. (def)

library(here)
library(tidyverse)
library(lubridate)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions2.rda")
load(here("00_donnees_originales", "pg20.rda"))

# Sélection de variables --------------------------------------------------

pg20_0 <- pg20 # On commence avec 436 étudiants. 

pg20 <- select(pg20_0,
               
               mail = mel, 
               last_session = lastSessionAt,
               sessions_b24 = sessionsCountbefore24,
               sheets_views_b24 = exercisesSheetsViewsbefore24,
               videos_views_b24 = videosPagesViewsbefore24,
               videos_comp_b24 = videosCompleteEventsbefore24,
               workpoints_b24 = workPointsbefore24,
               sessions_a23 = sessionsCountafter23,
               sheets_views_a23 = exercisesSheetsViewsafter23,
               videos_views_a23 = videosPagesViewsafter23,
               videos_comp_a23 = videosCompleteEventsafter23,
               workpoints_a23 = workPointsafter23,
               names(pg20)[str_detect(names(pg20), 
                                      paste("\\_", 1:31, "$",
                                            sep = "") %>% 
                                        paste(collapse = "|"))],
               
               z_msg1_ginit = maths15juillet, 
               z_msg1_gcompl = maths20juillet,
               
               z_rel1_ginit = maths24juillet, 
               z_rel1_gcompl = maths26juillet,
               
               z_rel2 = message14aout,
               
               cg_20 = CG20,
               fran1_20 = FRAN120,
               fran2_20 = FRAN220,
               maths_30 = MATHS30,
               rendu_maths = rendumaths, 
               rendu_cg = renduCG,
               rendu_fran1 = renduFRAN120,
               rendu_fran2 = renduFRAN220,
               num_ps = Numero,
               z = Z,
               connect_maths = connectmaths,
               
               voie, # même info que filière.
               nom_bdd = NomBDD, # peut servir pour les jointures. 
               prenom_bdd = PrenomBDD,
               prenom2_bdd = Prenom2BDD,
               prenom3_bdd = Prenom3BDD,
               sexe_ps = Sexe,
               nais_ps = DateNais,
               ville_nais = VilleNais,
               dep_nais = DepNais,
               pays_nais = PaysNais,
               nationalite = Nationalite,
               mail_ps = email,
               mail_resp1_ps = EmailResp1,
               boursier = Boursier, 
               mail_resp2_ps = EmailResp2,
               type_etab = TypeEtablissement,
               statut_etab = ContratEtablissement,
               dep_etab = depEtablissement,
               pays_etab = PaysEtablissement,
               niv_educ_act = NiveauEtudeActuel,
               ine_ps = INE,
               serie_ps = Serie,
               diplome_ps = Diplome,
               serie_diplome_ps = SerieDiplome,
               mention_diplome_ps = MentionDiplome,
               annee_bac = AnneeBac,
               moy_fran_ec = NoteEcritFrancais,
               moy_maths = NoteEpreuveMaths,
               moy_bac = MoyenneGenBAC,
               niv_classe = NiveauClasse,
               avis_ce = AvisCE)

# SAS vers R : nchar0 en NA -----------------------------------------------

pg20 <- mutate_if(pg20, function (x) ! is.numeric(x),
                  function (x) {
  x[nchar(x) == 0] <- NA
  x
})

# Construction : plateforme -----------------------------------------------

pg20 <- mutate(pg20, 
               
               mail = str_to_lower(mail),
               last_session_day = substr(last_session, 1, 10) %>% 
                 as.Date(format = "%d/%m/%Y"),
               last_session_hour = substr(last_session, 12, 19),
               
               z_msg1 = if_else(z_msg1_ginit == 1 |
                                  z_msg1_gcompl == 1, 1, 0),
               z_rel1 = if_else(z_rel1_ginit == 1 |
                                  z_rel1_gcompl == 1, 1, 0),
               
               rendu_cg = if_else(is.na(rendu_cg), 0, rendu_cg),
               rendu_fran1 = if_else(is.na(rendu_fran1), 0, 
                                     rendu_fran1),
               rendu_fran2 = if_else(is.na(rendu_fran2), 0, 
                                     rendu_fran2),
               rendu_maths = if_else(is.na(rendu_maths), 0,
                                     rendu_maths))

# Constructions : parcoursup ----------------------------------------------

pg20 <- mutate(pg20, 
               
               pays_nais_fr = if_else(pays_nais == "France", 
                                      "oui", "non"), 
               nationalite = str_to_lower(nationalite) %>% 
                 str_replace(" ", "_"),
               
               annee_bac = substr(annee_bac, 1, 4),
               voie = as.character(
                 factor(voie, 
                        labels = c("aes_tampon",
                                   "aes_tampon_sante",
                                   "aes_stdenis",
                                   "aes_stdenis_sante",
                                   "ecoge_tampon",
                                   "ecoge_tampon_sante",
                                   "ecoge_stdenis",
                                   "ecoge_stdenis_sante"))),
              filiere_ps = str_extract(voie, "aes|eco"),
             campus = str_extract(voie, "tampon|stdenis"),
               boursier = as.character(
                 factor(boursier,
                        labels = c("ens_sup", "secondaire", "non"))),
               type_etab = as.character(
                 factor(type_etab, 
                        labels = c("iut",
                                   "institut",
                                   "lycee_postbac",
                                   "lycee_nopostbac",
                                   "universite"))),
               statut_etab = as.character(
                 factor(statut_etab, 
                        labels = c("prive", "public"))),
               dep_etab_run = if_else(dep_etab == "974", 
                                      "oui", "non"),
               pays_etab_fr = if_else(pays_etab == "France",
                                      "oui", "non"),
               niv_educ_act = as.character(
                 factor(niv_educ_act, 
                        labels = c("1asup",
                                   "2asup",
                                   "3asup",
                                   "4asup",
                                   "prepasup",
                                   "premiere",
                                   "terminale"))),
               diplome_ps = as.character(
                 factor(diplome_ps, 
                        labels = c("prep_bac",
                                   "bac",
                                   "bac_equiv"))),
               diplome_ps = if_else(diplome_ps == "bac_equiv", "bac",
                                    diplome_ps),
               
               # Ne doit pas être là mais on laisse une trace d'humanité.
               # Doit être normalement dans pre_analyse.
               serie_diplome_psv2 = str_to_lower(serie_diplome_ps) %>% 
                 str_replace_all(" ", "_") %>% 
                 str_remove_all("\\_de|\\_et|du\\_|la\\_|l'") %>% 
                 str_replace("technologie\\_", "technologies\\_") %>% 
                 str_replace("sciences\\_technologies", "st") %>% 
                 unaccent2 %>% str_replace("st\\_management\\_gestion", 
                                           "st_gestion") %>% 
                 str_replace("serie\\_technologique\\_francaise", "st") %>% 
               str_replace("autre\\_diplôme\\_niveau\\_iv", "autre_niv4"),
               
               mention_diplome_ps = as.character(
                 factor(mention_diplome_ps, 
                        labels = c("ab", "b", "tb", 
                                   "sans", "rattrapage"))),
               
               age_26aout = int_length(
                 interval(nais_ps,
                          as.Date("2020-08-26"))) / 
                 ((3600 * 24) * 365.25),
             
             niv_classe = as.character(
               factor(niv_classe, 
                      labels = c("assez_bon", "bon", "faible",
                                 "moyen", "tres_bon"))),
             avis_ce = as.character(
               factor(avis_ce, 
                      labels = c("assez_satisfaisante",
                                 "peu_demontree", "satisfaisante",
                                 "tres_satisfaisante"))
             )
               )

# str_trim : utile pour les jointures. ------------------------------------

pg20 <- mutate_if(pg20, is.character, str_trim)

# Sauvegarde --------------------------------------------------------------

save(pg20, 
     file = here("01_v2_construction_def", "01_construction_pg20.rda"),
     version = 2)
