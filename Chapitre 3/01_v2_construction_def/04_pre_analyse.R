# 04_pre_analyse (def). 
  # si on n'assiste pas à l'examen finale ctqcm : ajourné. 
    # mais est-ce qu'on peut remplacer la note des ajournés comme 0 ?. 
  # Il faut lancer sans job sinon on n'a pas serie_psv2R. 

library(here)
library(tidyverse)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions2.rda")
load(here("01_v2_construction_def", "03_jointures.rda"))

# Factors : pg20 ----------------------------------------------------------
  # pas de filiere. pas de site. 

pg20 <- mutate(pg20, 
               z_rel2 = factor(z_rel2),
               rendu_maths = factor(rendu_maths),
               rendu_cg = factor(rendu_cg),
               rendu_fran1 = factor(rendu_fran1),
               rendu_fran2 = factor(rendu_fran2),
               z = factor(z),
               connect_maths = factor(connect_maths),
               z_msg1 = factor(z_msg1),
               z_rel1 = factor(z_rel1),
               
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
                                             "iut")),
               
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

# Factors : g20_tous, g20_retr --------------------------------------

lapply(list("g20_tous", "g20_retr"), function (x) {
  assign(x, 
         mutate(get(x), 
                z_rel2 = factor(z_rel2),
              rendu_maths = factor(rendu_maths),
              rendu_cg = factor(rendu_cg),
              rendu_fran1 = factor(rendu_fran1),
              rendu_fran2 = factor(rendu_fran2),
              z = factor(z),
              connect_maths = factor(connect_maths),
              z_msg1 = factor(z_msg1),
              z_rel1 = factor(z_rel1),
              
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
                                            "iut")),
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
              
              
              filiere = factor(filiere, 
                               levels = c("aes", "eco")),
              filiereR = if_else(is.na(filiere), "NA",
                                as.character(filiere)) %>% 
                factor(levels = c(levels(filiere), "NA")),
              
              site = factor(site, levels = c("sud", "nord")),
              siteR = if_else(is.na(site), "NA",
                              as.character(site)) %>% 
                factor(levels = c(levels(site), "NA")),
             
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
               factor(levels = c(levels(avis_ce), "NA"))),
         
         pos = sys.frame())
  invisible()
})

# connect dans pg20 et g20_tous -------------------------------------------

pg20 <- mutate(pg20, 
               connect = as.numeric(! is.na(sessions_b24)))

g20_tous <- mutate(g20_tous, 
               connect = as.numeric(! is.na(sessions_b24)))
  # pas valide pour g20_tous (ne jamais utiliser). 


# Traitements : replace_na(0) ---------------------------------------------

  # 2022_04_23. 
pg20 <- mutate(pg20, 
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

id <- pg20$num_ps

g20_tous <- mutate(g20_tous, 
                 sessions_b24 = if_else(num_ps %in% id, replace_na(sessions_b24, 0),
                                        sessions_b24),
                 sessions_a23 = if_else(num_ps %in% id, replace_na(sessions_a23, 0),
                                        sessions_a23),
                 videos_views_b24 = if_else(num_ps %in% id, replace_na(videos_views_b24, 0),
                                            videos_views_b24),
                 videos_views_a23 = if_else(num_ps %in% id, replace_na(videos_views_a23, 0),
                                            videos_views_a23),
                 videos_comp_b24 = if_else(num_ps %in% id, replace_na(videos_comp_b24, 0),
                                           videos_comp_b24),
                 videos_comp_a23 = if_else(num_ps %in% id, replace_na(videos_comp_a23, 0),
                                           videos_comp_a23),
                 sheets_views_b24 = if_else(num_ps %in% id, replace_na(sheets_views_b24, 0),
                                            sheets_views_a23),
                 sheets_views_a23 = if_else(num_ps %in% id, replace_na(sheets_views_a23, 0),
                                            sheets_views_a23),
                 workpoints_b24 = if_else(num_ps %in% id, replace_na(workpoints_b24, 0),
                                          workpoints_b24),
                 workpoints_a23 = if_else(num_ps %in% id, replace_na(workpoints_a23, 0),
                                          workpoints_a23))

# Traitements : avant et après l'évaluation du 24 août. -------------------

pg20 <- mutate(pg20,
                 sessions_tot = sessions_b24 + sessions_a23,
                 videos_views_tot = videos_views_b24 + videos_views_a23,
                 videos_comp_tot = videos_comp_b24 + videos_comp_a23,
                 sheets_views_tot = sheets_views_b24 + sheets_views_a23,
                 workpoints_tot = workpoints_b24 + workpoints_a23)

g20_tous <- mutate(g20_tous,
                 sessions_tot = sessions_b24 + sessions_a23,
                 videos_views_tot = videos_views_b24 + videos_views_a23,
                 videos_comp_tot = videos_comp_b24 + videos_comp_a23,
                 sheets_views_tot = sheets_views_b24 + sheets_views_a23,
                 workpoints_tot = workpoints_b24 + workpoints_a23)

# Traitement >= 1 ---------------------------------------------------------

pg20 <- mutate(pg20,
                 
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

g20_tous <- mutate(g20_tous,
                 
                 sessions_b24_sup1 = if_else(num_ps %in% id, as.numeric(sessions_b24 >= 1),
                                             sessions_b24),
                 sessions_tot_sup1 = if_else(num_ps %in% id, as.numeric(sessions_tot >= 1),
                                             sessions_tot),
                 
                 videos_views_b24_sup1 = if_else(num_ps %in% id, as.numeric(videos_views_b24 >= 1),
                                                 videos_views_b24),
                 videos_views_tot_sup1 = if_else(num_ps %in% id, as.numeric(videos_views_tot >= 1),
                                                 videos_views_tot),
                 
                 videos_comp_b24_sup1 = if_else(num_ps %in% id, as.numeric(videos_comp_b24 >= 1),
                                                videos_comp_b24),
                 videos_comp_tot_sup1 = if_else(num_ps %in% id, as.numeric(videos_comp_tot >= 1),
                                                videos_comp_tot),
                 
                 sheets_views_b24_sup1 = if_else(num_ps %in% id, as.numeric(sheets_views_b24 >= 1),
                                                 sheets_views_b24),
                 sheets_views_tot_sup1 = if_else(num_ps %in% id, as.numeric(sheets_views_tot >= 1),
                                                 sheets_views_tot),
                 
                 # workpoints : pas pertinent. 
                 )

# Traitement : indicateurs synthétiques -----------------------------------

pg20 <- mutate(pg20, 
                 pratique_tot = videos_comp_tot + sheets_views_tot,
                 pratique_tot_sup1 = as.numeric(pratique_tot >= 1))

g20_tous <- mutate(g20_tous, 
                 pratique_tot = videos_comp_tot + sheets_views_tot,
                 pratique_tot_sup1 = if_else(num_ps %in% id, as.numeric(pratique_tot >= 1),
                                             pratique_tot))

# Sauvegarde --------------------------------------------------------------

save(pg20, g20_tous, g20_retr,
     file = here("01_v2_construction_def", "04_pre_analyse.rda"),
     version = 2)
