# 06_pre_analyse_ps (def). 
  # Pour comparer échantillon intéressés vs tout parcoursup. 
  # 2022_03_30. 

library(here)
library(tidyverse)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions2.rda")
load(here("01_v2_construction_def", "05_construction_ps.rda"))

# Factors -----------------------------------------------------------------

ps <- mutate(ps, 
             
             voie = factor(voie, 
                            levels = c("aes_stdenis", 
                                       "aes_stdenis_sante",
                                       "aes_tampon", 
                                       "aes_tampon_sante",
                                       "ecoge_stdenis",
                                       "ecoge_stdenis_sante",
                                       "ecoge_tampon",
                                       "ecoge_tampon_sante")),
               
             filiere_ps = factor(filiere_ps, levels = c("aes", "eco")) ,
               campus = factor(campus, levels = c("tampon", "stdenis")),
               
               mineure_sante = if_else(str_detect(voie, "sante"), "oui",
                                       "non") %>% 
                 factor(levels = c("non", "oui")),
             
               sexe_ps = factor(sexe_ps, 
                                levels = c("F", "M")),
               pays_nais_fr = factor(pays_nais_fr, 
                                     levels = c("non", "oui")),
               nationalite = factor(nationalite,
                                    levels = c("hors_ue", "fr")),
               boursier = factor(boursier, 
                                 levels = c("non",
                                            "secondaire",
                                            "ens_sup")),
               niv_educ_act = factor(niv_educ_act, 
                                     levels = c("terminale",
                                                "1asup",
                                                "2asup",
                                                "3asup",
                                                "4asup",
                                                "premiere",
                                                "prepasup")),
               niv_educ_actv2 = fct_collapse(
                 niv_educ_act, 
                 "autres" = c("premiere", "prepasup", "2asup",
                              "3asup", "4asup")
                 ),
               niv_educ_actv2R = if_else(
                 is.na(niv_educ_actv2), "NA",
                 as.character(niv_educ_actv2)
                 ) %>% factor(levels = c(levels(niv_educ_actv2), "NA")),
               
               serie_ps = factor(serie_ps, 
                                 levels = c("L", "ES", "S",
                                            "STMG", "STI2D", "ST2S",
                                            "FC", "P", "PA")),
               serie_psv2 = fct_collapse(serie_ps,
                                         "autres" = c("FC", "P", "PA")),
               serie_psv2R = if_else(is.na(serie_psv2), "NA",
                                     as.character(serie_psv2)) %>% 
                 factor(levels = c(levels(serie_psv2), "NA")),
              
             serie_diplome_psv2 = 
                 factor(serie_diplome_psv2, 
                        levels = c("litteraire", "economique_social",
                                   "scientifique", "st_gestion",
                                   "st_industrie_developpement_durable",
                                   "st_sante_social", "professionnelle",
                                   "professionnelle_agricole",
                                   "st_laboratoire", "daeu",
                                   "bac_general_plus_15_ans", 
                                   "st_plus_15_ans",
                                   "autre_niv4", "brevet_professionnel")),
               serie_diplome_psv2R = if_else(is.na(serie_diplome_psv2), "NA",
                                     as.character(serie_diplome_psv2)) %>% 
                 factor(levels = c(levels(serie_diplome_psv2), "NA")),
             
              serie_diplome_psv3 = 
                 fct_collapse(serie_diplome_psv2, 
                              "l" = "litteraire", 
                              "es" = "economique_social",
                              "s" = "scientifique", 
                              "stmg" = "st_gestion",
                              "st2d" = "st_industrie_developpement_durable",
                              "st2s" = "st_sante_social", 
                              "pro" = "professionnelle",
                              "proagr" = "professionnelle_agricole",
                              "stlabo" = "st_laboratoire", 
                              "daeu" = "daeu",
                              "bacgp15" = "bac_general_plus_15_ans", 
                              "stp15" = "st_plus_15_ans",
                              "autre_niv4" = "autre_niv4",
                              "bp" = "brevet_professionnel"),
               serie_diplome_psv3R = if_else(is.na(serie_diplome_psv3), "NA",
                                     as.character(serie_diplome_psv3)) %>% 
                 factor(levels = c(levels(serie_diplome_psv3), "NA")),
             
             
             serie_diplome_psv4 = 
                 fct_collapse(serie_diplome_psv3, 
                              "pro" = c("pro", "proagr"),
                              "techno" = c("stmg", "st2d", "st2s", 
                                           "stlabo", "stp15")) %>% 
                 as.character,
               serie_diplome_psv4 = 
                 if_else(! serie_diplome_psv4 %in% 
                           c("pro", "techno", "es", "s") & 
                           ! is.na(serie_diplome_psv4), "autre",
                         serie_diplome_psv4) %>% 
                 factor(levels = c("pro", "techno", "es", "s", "autre")),
               serie_diplome_psv4R = if_else(is.na(serie_diplome_psv4), "NA",
                                     as.character(serie_diplome_psv4)) %>% 
                 factor(levels = c(levels(serie_diplome_psv4), "NA")),
             
             
               diplome_ps = factor(diplome_ps, 
                                   levels = c("prep_bac", "bac")),
               diplome_psR = if_else(is.na(diplome_ps), "NA",
                                     as.character(diplome_ps)) %>% 
                 factor(levels = c(levels(diplome_ps), "NA")),
               
               mention_diplome_ps = factor(mention_diplome_ps,
                                           levels = c("sans", 
                                                      "ab", 
                                                      "b", 
                                                      "tb",
                                                      "oral_juillet",
                                                      "rattrapage")),
               mention_diplome_psR = 
                 if_else(is.na(mention_diplome_ps), "NA",
                         as.character(mention_diplome_ps)) %>% 
                 factor(levels = c(levels(mention_diplome_ps),
                                   "NA")),
               
               annee_bac = factor(annee_bac),
              # Hypothèse très crédible : 
              annee_bacv2 = if_else(is.na(annee_bac), "2020", 
                                    as.character(annee_bac)),
              annee_bacv2 = if_else(
                as.numeric(as.character(annee_bacv2)) < 2017,
                "avant_2017", 
                as.character(annee_bacv2)) %>% 
                factor(levels = c("avant_2017", "2017",
                                  "2018", "2019", "2020")),
              # annee_bacv2R = if_else(is.na(annee_bacv2),
              #                        "NA",
              #                        as.character(annee_bacv2)) %>% 
              #   factor(levels = c(levels(annee_bacv2), "NA")),
               
               type_etab = factor(type_etab,
                                  levels = c("lycee_nopostbac",
                                             "lycee_postbac",
                                             "universite",
                                             "institut", 
                                             "iut",
                                             "ecole")),
               
               type_etabv2 = fct_collapse(
                 type_etab, "autres" = c("institut", "iut")),
               type_etabv2R = if_else(is.na(type_etabv2),
                                      "NA", 
                                      as.character(type_etabv2)) %>% 
                 factor(levels = c(levels(type_etabv2), "NA")),
             
             dep_etabv2 = if_else(dep_etab == "974", "run", 
                                  if_else(dep_etab != "99", 
                                          "fr_hors_run", "etranger")) %>% 
               factor(levels = c("run", "fr_hors_run", "etranger")),
             dep_etabv2R = if_else(is.na(dep_etabv2), 
                                    "NA", as.character(dep_etabv2)) %>% 
               factor(levels = c(levels(dep_etabv2), "NA")),
             
               statut_etab = factor(statut_etab, 
                                    levels = c("public", "prive")),
               statut_etabR = if_else(is.na(statut_etab), "NA",
                                      as.character(statut_etab)) %>% 
                 factor(levels = c(levels(statut_etab), "NA")),
               
              pays_etab_fr = factor(pays_etab_fr, 
                                    levels = c("non", "oui")),
              pays_etab_frR = if_else(is.na(pays_etab_fr), "NA",
                                      as.character(pays_etab_fr)) %>% 
                factor(levels = c(levels(pays_etab_fr), "NA")),
             
             
             niv_classe = factor(niv_classe, 
                                 levels = c("faible", "moyen", 
                                            "assez_bon", "bon", 
                                            "tres_bon")),
             niv_classeR = if_else(is.na(niv_classe), "NA",
                                as.character(niv_classe)) %>% 
               factor(levels = c(levels(niv_classe), "NA")),
             
             
             avis_ce = factor(avis_ce, 
                              levels = c("peu_demontree",
                                         "assez_satisfaisante",
                                         "satisfaisante", 
                                         "tres_satisfaisante")),
             avis_ceR = if_else(is.na(avis_ce), "NA",
                                as.character(avis_ce)) %>% 
               factor(levels = c(levels(avis_ce), "NA")))

# Sauvegarde --------------------------------------------------------------

save(ps, 
     file = here("01_v2_construction_def", "06_pre_analyse_ps.rda"),
     version = 2)
