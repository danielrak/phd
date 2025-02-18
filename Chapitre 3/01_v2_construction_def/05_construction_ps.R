# 05_construction_ps. (def)
  # Pour comparer l'échantillon "intéressés" avec tout parcoursup.
  # 2022_03_30. 

library(here)
library(tidyverse)
library(lubridate)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions2.rda")
load(here("00_donnees_originales", "ps.rda"))

# Sélection de variables --------------------------------------------------

ps_0 <- ps # 1687 étudiants. 

ps <- select(ps, 
             
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
             
             num_ps = numparcoursup,
             
             niv_classe = NiveauClasse,
             avis_ce = AvisCE)

# SAS vers R : nchar0 en NA -----------------------------------------------

ps <- mutate_if(ps, function (x) ! is.numeric(x), 
                function (x) {
                  x[nchar(x) == 0] <- NA
                  x
                })

# Constructions -----------------------------------------------------------

ps <- mutate(ps, 
               
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
                        labels = c("ecole", # pas la même chose que dans pg20.
                                   "iut",
                                   "institut",
                                   "lycee_postbac",
                                   "lycee_nopostbac",
                                   "universite"))),
               statut_etab = as.character(
                 factor(statut_etab, 
                        labels = c("prive_hors_contrat", 
                                   "prive", "public")) %>% 
                   fct_collapse("prive" = c("prive_hors_contrat",
                                            "prive"),
                                "public" = "public")),
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
                                   "sans", 
                                   "oral_juillet",
                                   # ci-dessus : pas même que pg20.
                                   "rattrapage"))),
               
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

ps <- mutate_if(ps, is.character, str_trim)

# Sauvegarde --------------------------------------------------------------

save(ps, 
     file = here("01_v2_construction_def", "05_construction_ps.rda"),
     version = 2)
