# Construction de données. def.
# 2021_10_26. Construction optimisée. 
# 2022_01_07 : ne marche pas en job à cause du script externe. 
  # Voir section nouveaux pairs - anciens pairs. 

# Packages nécessaires ----------------------------------------------------

library(tidyverse)
library(gtools)
library(fixest)
library(plm)
library(fastDummies)
library(here)

# Chargement --------------------------------------------------------------

load("C:/00_phd/02_rectorat/02_v2_construction_def/01_construction/04_pre_analyse_dnbcm2.rda")
load("C:/00_phd/02_rectorat/02_v2_construction_def/01_construction/04_pre_analyse_constat1316.rda")
# load("D:/00_phd/02_rectorat/02_v2_construction_def/01_construction/04_pre_analyse_cm2.rda")

constat3 <- filter(constat1316, niveauconstat == "3")

# Chargement cm2 def2 (NEW) -----------------------------------------------

  # J'exploite ce que j'ai pu faire dans AGE. 
  
load("C:/00_phd/02_rectorat/04_resultats/41_age/01_donnees_def.rda")
cm2 <- filter(c, cohorte != "2009")
rm(c, cd, cdi, d, d_noclniv, rdc, rdcdi)
  # Exactement les mêmes individus que dans
  # load("D:/00_phd/02_rectorat/02_v2_construction_def/01_construction/04_pre_analyse_cm2.rda")

# Sélection de variables --------------------------------------------------

dnbcm2 <- dummy_cols(dnbcm2, 
                     c("sexe_mod", "pcs_reg_mod", "positiondnb2",
                       "lbourses_constat"))

dat <- select(dnbcm2,
              
              age_abs, 
              ecolecoh, classecoh, 
              
              cohorte, 
              session_mod, 
              rneconstat, rneconstat_debut, rneconstatses_debut,
              cetab_mod, 
              rneconstatses, 
              bassin_mod, rnecirc, statut_constat, sigle_constat, reseauR_mod,
              reseau_mod,
              clefgestion_constat, libmef_constat,
              niveauconstat,
              divconstatrneses, 
              
              ine_mod, lmefstat4_constat,
              sexe_mod, sexe_mod_F, sexe_mod_M, 
              pcs_mod,
              pcs_reg_mod, 
              pcs_reg_mod_Defav, pcs_reg_mod_Moy, 
              pcs_reg_mod_Fav, pcs_reg_mod_Tresfav, pcs_reg_mod_Autres, 
              dnais_constat, age_absdnb, 
              positiondnb2, positiondnb2_Redoublant, positiondnb2_Heure, 
              positiondnb2_Avance, positiondnb2_num,
              
              lbourses_constat, lbourses_constat_non_boursier, 
              lbourses_constat_bourse_des_colleges, 
              lbourses_constat_bourse_nationale, 
              lregime_constat, lregime_constat_dp_dan, 
              lregime_constat_dp_hor, lregime_constat_int_dan, 
              lregime_constat_int_heb, lregime_constat_int_ext, 
              lregime_constat_ext_sur, lregime_constat_ext_lib,
              
              lregime_constat_g, lregime_constat_g_dp, 
              lregime_constat_g_int, lregime_constat_g_ext, 
              
              moyG1,
              moy_fran_cc, 
              moy_fran_ec, 
              moy_red_ec, moy_dic_ec,
              moy_maths_cc, moy_maths_ec,
              moy_hist_cc, moy_hist_ec, 
              moy_cc,
              moy_ec1, moy_ec, 
              score_f, score_m, score, 
              z_dnb, jour_dnb, quinze_dnb, mois_dnb, bim_dnb, trim_dnb, 
              position_num)

# Modalités reseau_mod : simplification -----------------------------------

dat <- mutate(dat, reseau_mod = factor(reseau_mod, labels = c("hep", "rep", "rep+")))

# Indicateur changement d'école -------------------------------------------

dat <- mutate(dat, chge = as.numeric(rneconstat_debut != cetab_mod))

# rnecircses --------------------------------------------------------------

dat <- mutate(dat, rnecircses = paste(rnecirc, session_mod))

# Filtre : sans lycée privé. ----------------------------------------------
  # 2021-07-01, 76 prs. 

dat <- filter(dat, sigle_constat != "LP PR")

# Filtre : sans bourse nationale ------------------------------------------

dat <- filter(dat, lbourses_constat != "bourse_nationale")

# Facilitation labels pcs -------------------------------------------------

dat <- mutate(dat, 
              pcs = factor(pcs_mod, labels = 
                             c("Agr", "Art", "Cadr", "Prof", "Emp", "Ouvr",
                               "Retr", "Inact", "Autres")))

dat <- dummy_cols(dat, "pcs")

# Filtre : duplicatas INE-session à enlever -------------------------------

  # Un même individu de DNB peut avoir deux notes de CM2. 
  # Ils sont au nombre au 5. 
  # On garde la note de CM2 la plus récente. 

remove <- mutate(dat, id = paste(ine_mod, session_mod)) %>% 
  filter(duplicated(id)) %>% pull(ine_mod) %>% 
  (function (i) filter(dat, ine_mod %in% i)) %>% 
  select(ine_mod, cohorte, session_mod, score) %>% 
  mutate(id2 = paste(ine_mod, cohorte)) %>% 
  group_by(ine_mod) %>% 
  filter(cohorte == min(as.numeric(as.character(cohorte)))) %>% pull(id2)

dat <- mutate(dat, id2 = paste(ine_mod, cohorte)) %>% 
  filter(! id2 %in% remove) %>% select(- id2)

# Ind redoublement collège ------------------------------------------------

dat <- mutate(dat, 
              positioncol_num = positiondnb2_num - position_num,
              positioncol = ifelse(positioncol_num > 0, "Saut", 
                                   ifelse(positioncol_num < 0, "Redoublement", 
                                          "Rien")),
              positioncol = ifelse(is.na(score_f), "Inconnue", positioncol) %>% 
                factor(levels = c("Redoublement", "Rien", "Saut", "Inconnue")))

dat <- dummy_cols(dat, "positioncol")

# Rang de percentile : Notes DNB ------------------------------------------

dat <- group_by(dat, session_mod) %>% 
  mutate( 
              
              moyG1_rp = percent_rank(moyG1) * 100,
            
              moy_fran_cc_rp = percent_rank(moy_fran_cc) * 100,
                          
              moy_fran_ec_rp = percent_rank(moy_fran_ec) * 100,
              
              moy_red_ec_rp = percent_rank(moy_red_ec) * 100,
              
              moy_dic_ec_rp = percent_rank(moy_dic_ec) * 100,
              
              moy_maths_cc_rp = percent_rank(moy_maths_cc) * 100,
              
              moy_maths_ec_rp = percent_rank(moy_maths_ec) * 100,
              
              moy_hist_cc_rp = percent_rank(moy_hist_cc) * 100,
              
              moy_hist_ec_rp = percent_rank(moy_hist_ec) * 100,
              
              moy_cc_rp = percent_rank(moy_cc) * 100, 
              
              # écrits uniquement : 
              moy_ec1_rp = percent_rank(moy_ec1) * 100,
              
              moy_ec_rp = percent_rank(moy_ec) * 100
              ) %>% ungroup

# Rangs perc. notes CM2 ---------------------------------------------------

  # Déjà fait grâce à chargement (NEW) ci-dessus. 
  # Insertion dans dat. 
dat <- left_join(dat, 
                 transmute(cm2, ine, cohorte, 
                           score_rp, score_f_rp, score_m_rp),
                 by = c("cohorte", "ine_mod" = "ine"))
  # nobs ok. 

# UET : notes DNB ---------------------------------------------------------

dat <- group_by(dat, session_mod) %>% 
  mutate(
              moyG1_norm = (moyG1 - mean(moyG1, na.rm = TRUE)) / 
                sd(moyG1, na.rm = TRUE),
    
              moy_fran_cc_norm = (moy_fran_cc - mean(moy_fran_cc, na.rm = TRUE)) / 
                sd(moy_fran_cc, na.rm = TRUE),
                          
              moy_fran_ec_norm = (moy_fran_ec - mean(moy_fran_ec, na.rm = TRUE)) / 
                sd(moy_fran_ec, na.rm = TRUE),
              
              moy_red_ec_norm = (moy_red_ec - mean(moy_red_ec, na.rm = TRUE)) / 
                sd(moy_red_ec, na.rm = TRUE),
              
              moy_dic_ec_norm = (moy_dic_ec - mean(moy_dic_ec, na.rm = TRUE)) / 
                sd(moy_dic_ec, na.rm = TRUE),
              
              moy_maths_cc_norm = (moy_maths_cc - mean(moy_maths_cc, na.rm = TRUE)) / 
                sd(moy_maths_cc, na.rm = TRUE),
              
              moy_maths_ec_norm = (moy_maths_ec - mean(moy_maths_ec, na.rm = TRUE)) / 
                sd(moy_maths_ec, na.rm = TRUE),
              
              moy_hist_cc_norm = (moy_hist_cc - mean(moy_hist_cc, na.rm = TRUE)) / 
                sd(moy_hist_cc, na.rm = TRUE),
              
              moy_hist_ec_norm = (moy_hist_ec - mean(moy_hist_ec, na.rm = TRUE)) / 
                sd(moy_hist_ec, na.rm = TRUE),
              
              moy_cc_norm = (moy_cc - mean(moy_cc, na.rm = TRUE)) / 
                sd(moy_cc, na.rm = TRUE), 
              
              # écrits uniquement : 
              moy_ec1_norm = (moy_ec1 - mean(moy_ec1, na.rm = TRUE)) / 
                sd(moy_ec1, na.rm = TRUE),
              
              moy_ec_norm = (moy_ec - mean(moy_ec, na.rm = TRUE)) / 
                sd(moy_ec, na.rm = TRUE)) %>% 
  ungroup

# UET. Notes CM2 ----------------------------------------------------------

  # Déjà fait grâce à chargement (NEW). 
  # Insertion dans dat. 
dat <- left_join(dat, 
                 transmute(cm2, ine, cohorte, 
                           score_norm, score_f_norm, score_m_norm),
                 by = c("cohorte", "ine_mod" = "ine"))
  # nobs ok. 

# Tailles de classe -------------------------------------------------------

dat <- group_by(dat, divconstatrneses) %>% 
  mutate(tdivconstatrneses = n()) %>% ungroup

# Exclusion : classes de taille 1 -----------------------------------------

  # MAJ2022_01_03. C'est ça qui fait la différence avec les chiffres sur les classes 
dat <- filter(dat, tdivconstatrneses != 1)
  # Il y en a 21. C'est des élèves minoritaires 
  # (4G, 4 SPO, DIMA, etc.). 

# Nombre de classes par école(-année) ---------------------------------------------

dat <- group_by(dat, rneconstatses) %>% 
  mutate(ncl0 = length(unique(divconstatrneses))) %>% 
  ungroup

  # Rq : Il n'y a pas d'école identifiée avec une seule classe.

# Caractéristiques totalement observées -----------------------------------

dat <- group_by(dat, divconstatrneses) %>% 
  mutate(p_age_abs = (sum(age_abs, na.rm = TRUE) - age_abs) / (tdivconstatrneses - 1),
         
         p_sexe_mod_F = (sum(sexe_mod_F, na.rm = TRUE) - sexe_mod_F) / 
           (tdivconstatrneses - 1),
         
         p_sexe_mod_M = (sum(sexe_mod_M, na.rm = TRUE) - sexe_mod_M) / 
           (tdivconstatrneses - 1),
         
         p_pcs_reg_mod_Defav = (sum(pcs_reg_mod_Defav, na.rm = TRUE) - 
                                  pcs_reg_mod_Defav) / 
           (tdivconstatrneses - 1),
         
         p_pcs_reg_mod_Moy = (sum(pcs_reg_mod_Moy, na.rm = TRUE) - 
                                pcs_reg_mod_Moy) / 
           (tdivconstatrneses - 1),
         
         p_pcs_reg_mod_Fav = (sum(pcs_reg_mod_Fav, na.rm = TRUE) - 
                                pcs_reg_mod_Fav) / 
           (tdivconstatrneses - 1),
         
         p_pcs_reg_mod_Tresfav = (sum(pcs_reg_mod_Tresfav, na.rm = TRUE) - 
                                    pcs_reg_mod_Tresfav) / 
           (tdivconstatrneses - 1),
         
         p_pcs_reg_mod_Autres = (sum(pcs_reg_mod_Autres, na.rm = TRUE) - 
                                   pcs_reg_mod_Autres) / 
           (tdivconstatrneses - 1),
         
         p_pcs_Agr = (sum(pcs_Agr, na.rm = TRUE) - pcs_Agr) / 
           (tdivconstatrneses - 1),
         
         p_pcs_Art = (sum(pcs_Art, na.rm = TRUE) - pcs_Art) / 
           (tdivconstatrneses - 1),
         
         p_pcs_Cadr = (sum(pcs_Cadr, na.rm = TRUE) - pcs_Cadr) / 
           (tdivconstatrneses - 1),
         
         p_pcs_Prof = (sum(pcs_Prof, na.rm = TRUE) - pcs_Prof) / 
           (tdivconstatrneses - 1),
         
         p_pcs_Emp = (sum(pcs_Emp, na.rm = TRUE) - pcs_Emp) / 
           (tdivconstatrneses - 1),
         
         p_pcs_Ouvr = (sum(pcs_Ouvr, na.rm = TRUE) - pcs_Ouvr) / 
           (tdivconstatrneses - 1),
         
         p_pcs_Retr = (sum(pcs_Retr, na.rm = TRUE) - pcs_Retr) / 
           (tdivconstatrneses - 1),
         
         p_pcs_Inact = (sum(pcs_Inact, na.rm = TRUE) - pcs_Inact) / 
           (tdivconstatrneses - 1),
         
         p_pcs_Autres = (sum(pcs_Autres, na.rm = TRUE) - pcs_Autres) / 
           (tdivconstatrneses - 1),
         
         p_age_absdnb = (sum(age_absdnb, na.rm = TRUE) - age_absdnb) / 
           (tdivconstatrneses - 1),
         
         p_positiondnb2_Redoublant = (sum(positiondnb2_Redoublant, na.rm = TRUE) - 
                                        positiondnb2_Redoublant) / 
           (tdivconstatrneses - 1),
         
         p_positiondnb2_Heure = (sum(positiondnb2_Heure, na.rm = TRUE) - 
                                   positiondnb2_Heure) / (tdivconstatrneses - 1),
         
         p_positiondnb2_Avance = (sum(positiondnb2_Avance, na.rm = TRUE) - 
                                    positiondnb2_Avance) / (tdivconstatrneses - 1),
         
         p_lbourses_constat_non_boursier = 
           (sum(lbourses_constat_non_boursier, na.rm = TRUE) -  
              lbourses_constat_non_boursier) / (tdivconstatrneses - 1),
         
         p_lbourses_constat_bourse_des_colleges = 
           (sum(lbourses_constat_bourse_des_colleges, na.rm = TRUE) -  
              lbourses_constat_bourse_des_colleges) / (tdivconstatrneses - 1),
         
         p_lbourses_constat_bourse_nationale = 
           (sum(lbourses_constat_bourse_nationale, na.rm = TRUE) - 
              lbourses_constat_bourse_nationale) / (tdivconstatrneses - 1),
         
         p_lregime_constat_dp_dan = 
           (sum(lregime_constat_dp_dan, na.rm = TRUE) - 
              lregime_constat_dp_dan) / (tdivconstatrneses - 1),
         
         p_lregime_constat_dp_hor = 
           (sum(lregime_constat_dp_hor, na.rm = TRUE) - 
              lregime_constat_dp_hor) / (tdivconstatrneses - 1),
         
         p_lregime_constat_int_dan = 
           (sum(lregime_constat_int_dan, na.rm = TRUE) - 
              lregime_constat_int_dan) / (tdivconstatrneses - 1),
         
         p_lregime_constat_int_heb = 
           (sum(lregime_constat_int_heb, na.rm = TRUE) -
              lregime_constat_int_dan) / (tdivconstatrneses - 1),
         
         p_lregime_constat_int_ext = (sum(lregime_constat_int_ext, na.rm = TRUE) -
                                        lregime_constat_int_ext) / 
           (tdivconstatrneses - 1),
         
         p_lregime_constat_ext_sur = (sum(lregime_constat_ext_sur, na.rm = TRUE) -
                                        lregime_constat_ext_sur) / 
           (tdivconstatrneses - 1),
         
         p_lregime_constat_ext_lib = (sum(lregime_constat_ext_lib, na.rm = TRUE) -
                                        lregime_constat_ext_lib) / 
           (tdivconstatrneses - 1),
         
         p_lregime_constat_g_dp = (sum(lregime_constat_g_dp, na.rm = TRUE) - 
                                     lregime_constat_g_dp) / 
           (tdivconstatrneses - 1),
         
         p_lregime_constat_g_int = (sum(lregime_constat_g_int, na.rm = TRUE) - 
                                      lregime_constat_g_int) /
           (tdivconstatrneses - 1),
         
         p_lregime_constat_g_ext = (sum(lregime_constat_g_ext, na.rm = TRUE) - 
                                      lregime_constat_g_ext) /
           (tdivconstatrneses - 1),
         
         p_positioncol_Redoublement = (sum(positioncol_Redoublement) - 
                            positioncol_Redoublement) / 
           (tdivconstatrneses - 1),
         
         p_positioncol_Rien = (sum(positioncol_Rien) - positioncol_Rien) / 
           (tdivconstatrneses - 1),
         
         p_positioncol_Saut = (sum(positioncol_Saut) - positioncol_Saut) / 
         (tdivconstatrneses - 1),
         
         p_positioncol_Inconnue = (sum(positioncol_Inconnue) - 
                                     positioncol_Inconnue) / 
           (tdivconstatrneses - 1),
         
         # à utiliser plus tard. 
         p_z_dnb = (sum(z_dnb) - z_dnb) / (tdivconstatrneses - 1)
         
  ) %>% ungroup

# p_notes DNB -------------------------------------------------------------

  # Ajouté le 2021_10_28. 
dat <- group_by(dat, divconstatrneses) %>% 
  mutate(p_moyG1 = (sum(moyG1, na.rm = TRUE) - moyG1) / 
           (tdivconstatrneses - 1),
         
         p_moy_fran_cc = (sum(moy_fran_cc, na.rm = TRUE) - 
                            moy_fran_cc) / 
           (tdivconstatrneses - 1),
         
         p_moy_fran_ec = (sum(moy_fran_ec, na.rm = TRUE) - 
                            moy_fran_ec) / 
           (tdivconstatrneses - 1),
         
         p_moy_red_ec = (sum(moy_red_ec, na.rm = TRUE) - 
                           moy_red_ec) / 
           (tdivconstatrneses - 1),
         
         p_moy_dic_ec = (sum(moy_dic_ec, na.rm = TRUE) - 
                           moy_dic_ec) / 
           (tdivconstatrneses - 1),
         
         p_moy_maths_cc = (sum(moy_maths_cc, na.rm = TRUE) - 
                             moy_maths_cc) / 
           (tdivconstatrneses - 1),
         
         p_moy_maths_ec = (sum(moy_maths_ec, na.rm = TRUE) - 
                            moy_maths_ec) / 
           (tdivconstatrneses - 1),
         
         p_moy_hist_cc = (sum(moy_hist_cc, na.rm = TRUE) - 
                            moy_hist_cc) / 
           (tdivconstatrneses - 1),
         
         p_moy_hist_ec = (sum(moy_hist_ec, na.rm = TRUE) - 
                            moy_hist_ec) / 
           (tdivconstatrneses - 1),
         
         p_moy_cc = (sum(moy_cc, na.rm = TRUE) - moy_cc) / 
           (tdivconstatrneses - 1),
         
         # écrits uniquement. 
         p_moy_ec1 = (sum(moy_ec1, na.rm = TRUE) - 
                             moy_ec1) / 
           (tdivconstatrneses - 1),
         
         p_moy_ec = (sum(moy_ec, na.rm = TRUE) - 
                            moy_ec) / 
           (tdivconstatrneses - 1),
         
         
         
         
         
         p_moyG1_rp = (sum(moyG1_rp, na.rm = TRUE) - moyG1_rp) / 
           (tdivconstatrneses - 1),
         
         p_moy_fran_cc_rp = (sum(moy_fran_cc_rp, na.rm = TRUE) - 
                            moy_fran_cc_rp) / 
           (tdivconstatrneses - 1),
         
         p_moy_fran_ec_rp = (sum(moy_fran_ec_rp, na.rm = TRUE) - 
                            moy_fran_ec_rp) / 
           (tdivconstatrneses - 1),
         
         p_moy_red_ec_rp = (sum(moy_red_ec_rp, na.rm = TRUE) - 
                           moy_red_ec_rp) / 
           (tdivconstatrneses - 1),
         
         p_moy_dic_ec_rp = (sum(moy_dic_ec_rp, na.rm = TRUE) - 
                           moy_dic_ec_rp) / 
           (tdivconstatrneses - 1),
         
         p_moy_maths_cc_rp = (sum(moy_maths_cc_rp, na.rm = TRUE) - 
                             moy_maths_cc_rp) / 
           (tdivconstatrneses - 1),
         
         p_moy_maths_ec_rp = (sum(moy_maths_ec_rp, na.rm = TRUE) - 
                            moy_maths_ec_rp) / 
           (tdivconstatrneses - 1),
         
         p_moy_hist_cc_rp = (sum(moy_hist_cc_rp, na.rm = TRUE) - 
                            moy_hist_cc_rp) / 
           (tdivconstatrneses - 1),
         
         p_moy_hist_ec_rp = (sum(moy_hist_ec_rp, na.rm = TRUE) - 
                            moy_hist_ec_rp) / 
           (tdivconstatrneses - 1),
         
         p_moy_cc_rp = (sum(moy_cc_rp, na.rm = TRUE) - moy_cc_rp) / 
           (tdivconstatrneses - 1),
         
         # écrits uniquement. 
         p_moy_ec1_rp = (sum(moy_ec1_rp, na.rm = TRUE) - 
                             moy_ec1_rp) / 
           (tdivconstatrneses - 1),
         
         p_moy_ec_rp = (sum(moy_ec_rp, na.rm = TRUE) - 
                            moy_ec_rp) / 
           (tdivconstatrneses - 1),
         
         
         
         
         p_moyG1_norm = (sum(moyG1_norm, na.rm = TRUE) - moyG1_norm) / 
           (tdivconstatrneses - 1),
         
         p_moy_fran_cc_norm = (sum(moy_fran_cc_norm, na.rm = TRUE) - 
                            moy_fran_cc_norm) / 
           (tdivconstatrneses - 1),
         
         p_moy_fran_ec_norm = (sum(moy_fran_ec_norm, na.rm = TRUE) - 
                            moy_fran_ec_norm) / 
           (tdivconstatrneses - 1),
         
         p_moy_red_ec_norm = (sum(moy_red_ec_norm, na.rm = TRUE) - 
                           moy_red_ec_norm) / 
           (tdivconstatrneses - 1),
         
         p_moy_dic_ec_norm = (sum(moy_dic_ec_norm, na.rm = TRUE) - 
                           moy_dic_ec_norm) / 
           (tdivconstatrneses - 1),
         
         p_moy_maths_cc_norm = (sum(moy_maths_cc_norm, na.rm = TRUE) - 
                             moy_maths_cc_norm) / 
           (tdivconstatrneses - 1),
         
         p_moy_maths_ec_norm = (sum(moy_maths_ec_norm, na.rm = TRUE) - 
                            moy_maths_ec_norm) / 
           (tdivconstatrneses - 1),
         
         p_moy_hist_cc_norm = (sum(moy_hist_cc_norm, na.rm = TRUE) - 
                            moy_hist_cc_norm) / 
           (tdivconstatrneses - 1),
         
         p_moy_hist_ec_norm = (sum(moy_hist_ec_norm, na.rm = TRUE) - 
                            moy_hist_ec_norm) / 
           (tdivconstatrneses - 1),
         
         p_moy_cc_norm = (sum(moy_cc_norm, na.rm = TRUE) -
                            moy_cc_norm) / 
           (tdivconstatrneses - 1),
         
         # écrits uniquement. 
         p_moy_ec1_norm = (sum(moy_ec1_norm, na.rm = TRUE) - 
                             moy_ec1_norm) / 
           (tdivconstatrneses - 1),
         
         p_moy_ec_norm = (sum(moy_ec_norm, na.rm = TRUE) - 
                            moy_ec_norm) / 
           (tdivconstatrneses - 1)
         
         ) %>% 
  ungroup

# dummies dnais -----------------------------------------------------------

dat <- dat %>% 
  (function (d) cbind(d, 
                      dummy_cols(d, "jour_dnb") %>%
                        select(-names(d)),
                      dummy_cols(d, "quinze_dnb") %>%
                        select(-names(d)),
                      dummy_cols(d, "mois_dnb") %>%
                        select(-names(d)),
                      dummy_cols(d, "bim_dnb") %>%
                        select(-names(d)),
                      dummy_cols(d, "trim_dnb") %>%
                        select(-names(d))))  

# p_dnais -----------------------------------------------------------------

dat <- group_by(dat, divconstatrneses) %>% (function (d) {
  
  p_jour <- lapply(1:365, function (x) transmute(d, p = (sum(eval(parse(text = paste("jour_dnb_", x, sep = "")))) - 
                                                  eval(parse(text = paste("jour_dnb_", x, sep = "")))) / 
                                         (tdivconstatrneses - 1)) %>% ungroup %>% select(- divconstatrneses) %>% 
                     setNames(paste("p_jour_dnb_", x, sep = ""))) %>% do.call(what = cbind)
  
  p_quinze <- lapply(1:24, function (x) transmute(d, p = (sum(eval(parse(text = paste("quinze_dnb_", x, sep = "")))) - 
                                                                 eval(parse(text = paste("quinze_dnb_", x, sep = "")))) / 
                                                     (tdivconstatrneses - 1)) %>% ungroup %>% select(- divconstatrneses) %>%
                       setNames(paste("p_quinze_dnb_", x, sep = ""))) %>% do.call(what = cbind)
  
  p_mois <- lapply(1:12, function (x) transmute(d, p = (sum(eval(parse(text = paste("mois_dnb_", x, sep = "")))) - 
                                                              eval(parse(text = paste("mois_dnb_", x, sep = "")))) / 
                                                  (tdivconstatrneses - 1)) %>% ungroup %>% select(- divconstatrneses) %>%
                     setNames(paste("p_mois_dnb_", x, sep = ""))) %>% do.call(what = cbind)
  
  p_bim <- lapply(1:6, function (x) transmute(d, p = (sum(eval(parse(text = paste("bim_dnb_", x, sep = "")))) - 
                                                             eval(parse(text = paste("bim_dnb_", x, sep = "")))) / 
                                                 (tdivconstatrneses - 1)) %>% ungroup %>% select(- divconstatrneses) %>% 
                    setNames(paste("p_bim_dnb_", x, sep = ""))) %>% do.call(what = cbind)
  
  p_trim <- lapply(1:4, function (x) transmute(d, p = (sum(eval(parse(text = paste("trim_dnb_", x, sep = "")))) - 
                                                            eval(parse(text = paste("trim_dnb_", x, sep = "")))) / 
                                                (tdivconstatrneses - 1)) %>% ungroup %>% select(- divconstatrneses) %>% 
                     setNames(paste("p_trim_dnb_", x, sep = ""))) %>% do.call(what = cbind)
  
  cbind(ungroup(d), p_jour, p_quinze, p_mois, p_bim, p_trim)
  
  })

# Observation de la note au CM2 -------------------------------------------


dat <- mutate(dat, obs = as.numeric(! is.na(score_f)))

  # On appelera "retrouvé" un élève de DNB tel que l'on observe sa note au CM2. 

# Proportion de retrouvés par classe --------------------------------------

dat <- group_by(dat, divconstatrneses) %>% 
  mutate(p = (sum(obs, na.rm = TRUE) - obs) / (tdivconstatrneses - 1)) %>% 
  ungroup


# v_ob _tot, _f et _m : toutes les versions -------------------------------
  # Plus de v_ob. 

dat <- mutate(dat, 
              
              v_ob_tot = replace_na(score, 0),
              v_ob_tot_norm = replace_na(score_norm, 0),
              v_ob_tot_rp = replace_na(score_rp, 0),
              
              v_ob_f = replace_na(score_f, 0),
              v_ob_f_norm = replace_na(score_f_norm, 0),
              v_ob_f_rp = replace_na(score_f_rp, 0),
              
              v_ob_m = replace_na(score_m, 0),
              v_ob_m_norm = replace_na(score_m_norm, 0),
              v_ob_m_rp = replace_na(score_m_rp, 0))

# p_v_ob _tot, _f et _m : toutes les versions -----------------------------

dat <- group_by(dat, divconstatrneses) %>% 
  mutate(
    
         p_v_ob_tot = ifelse(p == 0, 0, (sum(v_ob_tot) - v_ob_tot) / 
                           ((tdivconstatrneses - 1) * p)),
         p_v_ob_tot_norm = ifelse(p == 0, 0, (sum(v_ob_tot_norm) - 
                                                v_ob_tot_norm) / 
                           ((tdivconstatrneses - 1) * p)),
         p_v_ob_tot_rp = ifelse(p == 0, 0, (sum(v_ob_tot_rp) - 
                                              v_ob_tot_rp) / 
                           ((tdivconstatrneses - 1) * p)),
         
         p_v_ob_f = ifelse(p == 0, 0, (sum(v_ob_f) - v_ob_f) / 
                           ((tdivconstatrneses - 1) * p)),
         p_v_ob_f_norm = ifelse(p == 0, 0, (sum(v_ob_f_norm) - 
                                              v_ob_f_norm) / 
                           ((tdivconstatrneses - 1) * p)),
         p_v_ob_f_rp = ifelse(p == 0, 0, (sum(v_ob_f_rp) - 
                                            v_ob_f_rp) / 
                           ((tdivconstatrneses - 1) * p)),
         
         p_v_ob_m = ifelse(p == 0, 0, (sum(v_ob_m) - v_ob_m) / 
                           ((tdivconstatrneses - 1) * p)),
         p_v_ob_m_norm = ifelse(p == 0, 0, (sum(v_ob_m_norm) - 
                                              v_ob_m_norm) / 
                           ((tdivconstatrneses - 1) * p)),
         p_v_ob_m_rp = ifelse(p == 0, 0, (sum(v_ob_m_rp) - 
                                            v_ob_m_rp) / 
                           ((tdivconstatrneses - 1) * p))
         
         ) %>% 
  ungroup

# p_scores ----------------------------------------------------------------

  # Même chose que les p_v_ob 
  # lorsque l'on filtre les données sur obs = 1. 

dat <- group_by(dat, divconstatrneses) %>% 
  mutate(
    
    p_score = (sum(score, na.rm = TRUE) - score) / 
      (sum(! is.na(score)) - 1),
    p_score_rp = (sum(score_rp, na.rm = TRUE) - score_rp) / 
      (sum(! is.na(score_rp)) - 1),
    p_score_norm = (sum(score_norm, na.rm = TRUE) - score_norm) / 
      (sum(! is.na(score_norm)) - 1),
    
    p_score_f = (sum(score_f, na.rm = TRUE) - score_f) / 
      (sum(! is.na(score_f)) - 1),
    p_score_f_rp = (sum(score_f_rp, na.rm = TRUE) - score_f_rp) / 
      (sum(! is.na(score_f_rp)) - 1),
    p_score_f_norm = (sum(score_f_norm, na.rm = TRUE) - score_f_norm) / 
      (sum(! is.na(score_f_norm)) - 1),
    
    p_score_m = (sum(score_m, na.rm = TRUE) - score_m) / 
      (sum(! is.na(score_m)) - 1),
    p_score_m_rp = (sum(score_m_rp, na.rm = TRUE) - score_m_rp) / 
      (sum(! is.na(score_m_rp)) - 1),
    p_score_m_norm = (sum(score_m_norm, na.rm = TRUE) - score_m_norm) / 
      (sum(! is.na(score_m_norm)) - 1)
         ) %>% 
  ungroup

# K-ciles -----------------------------------------------------------------
  # (tot). 
dat <- group_by(dat, rneconstatses) %>% 
  mutate(mescore = mean(score, na.rm = TRUE)) %>% 
  ungroup %>% mutate(kcile3 = quantcut(mescore, 3) %>% 
                       factor(labels = c("q1", "q2", "q3")),
                     kcile15 = quantcut(mescore, 15) %>% 
                       factor(labels = paste("q", 1:15, sep = "")),
                     kcile25 = quantcut(mescore, 25) %>% 
                       factor(labels = paste("q", 1:25, sep = "")))

# K-ciles f ---------------------------------------------------------------
  # (f). 
dat <- group_by(dat, rneconstatses) %>% 
  mutate(mescore_f = mean(score_f, na.rm = TRUE)) %>% 
  ungroup %>% mutate(kcile3_f = quantcut(mescore_f, 3) %>% 
                       factor(labels = c("q1", "q2", "q3")),
                     kcile15_f = quantcut(mescore_f, 15) %>% 
                       factor(labels = paste("q", 1:15, sep = "")),
                     kcile25_f = quantcut(mescore_f, 25) %>% 
                       factor(labels = paste("q", 1:25, sep = "")))

# K-ciles m ---------------------------------------------------------------
  # (m). 
dat <- group_by(dat, rneconstatses) %>% 
  mutate(mescore_m = mean(score_m, na.rm = TRUE)) %>% 
  ungroup %>% mutate(kcile3_m = quantcut(mescore_m, 3) %>% 
                       factor(labels = c("q1", "q2", "q3")),
                     kcile15_m = quantcut(mescore_m, 15) %>% 
                       factor(labels = paste("q", 1:15, sep = "")),
                     kcile25_m = quantcut(mescore_m, 25) %>% 
                       factor(labels = paste("q", 1:25, sep = "")))

# Indicateurs niveau ------------------------------------------------------

  # Terciles, quintiles et catégorisation de BS13. 
  # On calcule les indicateurs par année. Voir BS13, page 72. 
  # Nouveau par rapport à tout ce qui a été fait jusqu'à maintenant. 

dat <- group_by(dat, session_mod) %>% 
  mutate(q3score = quantcut(score, 3) %>% 
                factor(labels = c("q1", "q2", "q3")),
              
              q5score = quantcut(score, 5) %>% 
                factor(labels = paste("q", 1:5, sep = "")),
              
              qbscore = fct_collapse(q5score, 
                                       "low" = "q1", 
                                       "middle" = c("q2", "q3", "q4"),
                                       "high" = "q5"),
         # MAJ2022_01_15. 
         q20score = quantcut(score, 20) %>% 
           factor(labels = paste("q", 1:20, sep = "")),
         qb2score = fct_collapse(q20score, 
                                 "low" = "q1",
                                 "middle" = paste("q", 2:19,
                                                  sep = ""),
                                 "high" = "q20")) %>% 
  ungroup


# Ind. niv f --------------------------------------------------------------

dat <- group_by(dat, session_mod) %>% 
  mutate(q3score_f = quantcut(score_f, 3) %>% 
                factor(labels = c("q1", "q2", "q3")),
              
              q5score_f = quantcut(score_f, 5) %>% 
                factor(labels = paste("q", 1:5, sep = "")),
              
              qbscore_f = fct_collapse(q5score_f, 
                                       "low" = "q1", 
                                       "middle" = c("q2", "q3", "q4"),
                                       "high" = "q5")) %>% 
  ungroup

# Ind. niv m --------------------------------------------------------------

dat <- group_by(dat, session_mod) %>% 
  mutate(q3score_m = quantcut(score_m, 3) %>% 
                factor(labels = c("q1", "q2", "q3")),
              
              q5score_m = quantcut(score_m, 5) %>% 
                factor(labels = paste("q", 1:5, sep = "")),
              
              qbscore_m = fct_collapse(q5score_m, 
                                       "low" = "q1", 
                                       "middle" = c("q2", "q3", "q4"),
                                       "high" = "q5")) %>% 
  ungroup

# Indicateurs de niveau par école : INUTILE.  -----------------------------

# v_ob version indicateurs : INUTILE. -------------------------------------

# Dummies niveau ----------------------------------------------------------

dat <- dummy_cols(dat, c("q3score", "q5score", "qbscore",
                         "q3score_f", "q5score_f", "qbscore_f",
                         "q3score_m", "q5score_m", "qbscore_m",
                         # MAJ2022_01_15. 
                         "q20score", "qb2score"))

# p_ niveau pairs ---------------------------------------------------------

dat <- group_by(dat, divconstatrneses) %>% 
  mutate(p_q3score_q1 = (sum(q3score_q1, na.rm = TRUE) - 
                             q3score_q1) / (sum(! is.na(score)) - 1),
         p_q3score_q2 = (sum(q3score_q2, na.rm = TRUE) - 
                             q3score_q2) / (sum(! is.na(score)) - 1),
         p_q3score_q3 = (sum(q3score_q3, na.rm = TRUE) - 
                             q3score_q3) / (sum(! is.na(score)) - 1),
         
         p_q5score_q1 = (sum(q5score_q1, na.rm = TRUE) - 
                             q5score_q1) / (sum(! is.na(score)) - 1),
         p_q5score_q2 = (sum(q5score_q2, na.rm = TRUE) - 
                             q5score_q2) / (sum(! is.na(score)) - 1),
         p_q5score_q3 = (sum(q5score_q3, na.rm = TRUE) - 
                             q5score_q3) / (sum(! is.na(score)) - 1),
         p_q5score_q4 = (sum(q5score_q4, na.rm = TRUE) - 
                             q5score_q4) / (sum(! is.na(score)) - 1),
         p_q5score_q5 = (sum(q5score_q5, na.rm = TRUE) - 
                             q5score_q5) / (sum(! is.na(score)) - 1),
         
         p_qbscore_low = (sum(qbscore_low, na.rm = TRUE) - 
                              qbscore_low) / (sum(! is.na(score)) - 1),
         p_qbscore_middle = (sum(qbscore_middle, na.rm = TRUE) - 
                              qbscore_middle) / 
           (sum(! is.na(score)) - 1),
         p_qbscore_high = (sum(qbscore_high, na.rm = TRUE) - 
                              qbscore_high) / (sum(! is.na(score)) - 1),
         
         
         # MAJ2022_01_15. 
         p_q20score_q1 = (sum(q20score_q1, na.rm = TRUE) - 
                             q20score_q1) / (sum(! is.na(score)) - 1),
         p_q20score_q2 = (sum(q20score_q2, na.rm = TRUE) - 
                             q20score_q2) / (sum(! is.na(score)) - 1),
         p_q20score_q3 = (sum(q20score_q3, na.rm = TRUE) - 
                             q20score_q3) / (sum(! is.na(score)) - 1),
         p_q20score_q4 = (sum(q20score_q4, na.rm = TRUE) - 
                             q20score_q4) / (sum(! is.na(score)) - 1),
         p_q20score_q5 = (sum(q20score_q5, na.rm = TRUE) - 
                             q20score_q5) / (sum(! is.na(score)) - 1),
         p_q20score_q6 = (sum(q20score_q6, na.rm = TRUE) - 
                             q20score_q6) / (sum(! is.na(score)) - 1),
         p_q20score_q7 = (sum(q20score_q7, na.rm = TRUE) - 
                             q20score_q7) / (sum(! is.na(score)) - 1),
         p_q20score_q8 = (sum(q20score_q8, na.rm = TRUE) - 
                             q20score_q8) / (sum(! is.na(score)) - 1),
         p_q20score_q9 = (sum(q20score_q9, na.rm = TRUE) - 
                             q20score_q9) / (sum(! is.na(score)) - 1),
         p_q20score_q10 = (sum(q20score_q10, na.rm = TRUE) - 
                             q20score_q10) / (sum(! is.na(score)) - 1),
         p_q20score_q11 = (sum(q20score_q11, na.rm = TRUE) - 
                             q20score_q11) / (sum(! is.na(score)) - 1),
         p_q20score_q12 = (sum(q20score_q12, na.rm = TRUE) - 
                             q20score_q12) / (sum(! is.na(score)) - 1),
         p_q20score_q13 = (sum(q20score_q13, na.rm = TRUE) - 
                             q20score_q13) / (sum(! is.na(score)) - 1),
         p_q20score_q14 = (sum(q20score_q14, na.rm = TRUE) - 
                             q20score_q14) / (sum(! is.na(score)) - 1),
         p_q20score_q15 = (sum(q20score_q15, na.rm = TRUE) - 
                             q20score_q15) / (sum(! is.na(score)) - 1),
         p_q20score_q16 = (sum(q20score_q16, na.rm = TRUE) - 
                             q20score_q16) / (sum(! is.na(score)) - 1),
         p_q20score_q17 = (sum(q20score_q17, na.rm = TRUE) - 
                             q20score_q17) / (sum(! is.na(score)) - 1),
         p_q20score_q18 = (sum(q20score_q18, na.rm = TRUE) - 
                             q20score_q18) / (sum(! is.na(score)) - 1),
         p_q20score_q19 = (sum(q20score_q19, na.rm = TRUE) - 
                             q20score_q19) / (sum(! is.na(score)) - 1),
         p_q20score_q20 = (sum(q20score_q20, na.rm = TRUE) - 
                             q20score_q20) / (sum(! is.na(score)) - 1),
         
         #
         p_qb2score_low = (sum(qb2score_low, na.rm = TRUE) - 
                             qb2score_low) / (sum(! is.na(score)) - 1),
         p_qb2score_middle = (sum(qb2score_middle, na.rm = TRUE) - 
                             qb2score_middle) / (sum(! is.na(score)) - 1),
         p_qb2score_high = (sum(qb2score_high, na.rm = TRUE) - 
                             qb2score_high) / (sum(! is.na(score)) - 1)
         
         ) %>% 
  ungroup

# p_niv f pairs -----------------------------------------------------------

dat <- group_by(dat, divconstatrneses) %>% 
  mutate(p_q3score_f_q1 = (sum(q3score_f_q1, na.rm = TRUE) - 
                             q3score_f_q1) / (sum(! is.na(score_f)) - 1),
         p_q3score_f_q2 = (sum(q3score_f_q2, na.rm = TRUE) - 
                             q3score_f_q2) / (sum(! is.na(score_f)) - 1),
         p_q3score_f_q3 = (sum(q3score_f_q3, na.rm = TRUE) - 
                             q3score_f_q3) / (sum(! is.na(score_f)) - 1),
         
         p_q5score_f_q1 = (sum(q5score_f_q1, na.rm = TRUE) - 
                             q5score_f_q1) / (sum(! is.na(score_f)) - 1),
         p_q5score_f_q2 = (sum(q5score_f_q2, na.rm = TRUE) - 
                             q5score_f_q2) / (sum(! is.na(score_f)) - 1),
         p_q5score_f_q3 = (sum(q5score_f_q3, na.rm = TRUE) - 
                             q5score_f_q3) / (sum(! is.na(score_f)) - 1),
         p_q5score_f_q4 = (sum(q5score_f_q4, na.rm = TRUE) - 
                             q5score_f_q4) / (sum(! is.na(score_f)) - 1),
         p_q5score_f_q5 = (sum(q5score_f_q5, na.rm = TRUE) - 
                             q5score_f_q5) / (sum(! is.na(score_f)) - 1),
         
         p_qbscore_f_low = (sum(qbscore_f_low, na.rm = TRUE) - 
                              qbscore_f_low) / (sum(! is.na(score_f)) - 1),
         p_qbscore_f_middle = (sum(qbscore_f_middle, na.rm = TRUE) - 
                              qbscore_f_middle) / 
           (sum(! is.na(score_f)) - 1),
         p_qbscore_f_high = (sum(qbscore_f_high, na.rm = TRUE) - 
                              qbscore_f_high) / (sum(! is.na(score_f)) - 1)
         ) %>% 
  ungroup

# p_niv m pairs -----------------------------------------------------------

dat <- group_by(dat, divconstatrneses) %>% 
  mutate(p_q3score_m_q1 = (sum(q3score_m_q1, na.rm = TRUE) - 
                             q3score_m_q1) / (sum(! is.na(score_m)) - 1),
         p_q3score_m_q2 = (sum(q3score_m_q2, na.rm = TRUE) - 
                             q3score_m_q2) / (sum(! is.na(score_m)) - 1),
         p_q3score_m_q3 = (sum(q3score_m_q3, na.rm = TRUE) - 
                             q3score_m_q3) / (sum(! is.na(score_m)) - 1),
         
         p_q5score_m_q1 = (sum(q5score_m_q1, na.rm = TRUE) - 
                             q5score_m_q1) / (sum(! is.na(score_m)) - 1),
         p_q5score_m_q2 = (sum(q5score_m_q2, na.rm = TRUE) - 
                             q5score_m_q2) / (sum(! is.na(score_m)) - 1),
         p_q5score_m_q3 = (sum(q5score_m_q3, na.rm = TRUE) - 
                             q5score_m_q3) / (sum(! is.na(score_m)) - 1),
         p_q5score_m_q4 = (sum(q5score_m_q4, na.rm = TRUE) - 
                             q5score_m_q4) / (sum(! is.na(score_m)) - 1),
         p_q5score_m_q5 = (sum(q5score_m_q5, na.rm = TRUE) - 
                             q5score_m_q5) / (sum(! is.na(score_m)) - 1),
         
         p_qbscore_m_low = (sum(qbscore_m_low, na.rm = TRUE) - 
                              qbscore_m_low) / (sum(! is.na(score_m)) - 1),
         p_qbscore_m_middle = (sum(qbscore_m_middle, na.rm = TRUE) - 
                                 qbscore_m_middle) / 
           (sum(! is.na(score_m)) - 1),
         p_qbscore_m_high = (sum(qbscore_m_high, na.rm = TRUE) - 
                               qbscore_m_high) / (sum(! is.na(score_m)) - 1)
  ) %>% 
  ungroup

# p_ niveau pairs par école : INUTILE.  -----------------------------------


# p_q4scores_uet : INUTILE. -----------------------------------------------


# Niveau section ----------------------------------------------------------

dat <- mutate(dat, 
              niveau_section = 
                fct_collapse(clefgestion_constat, 
                             "sup" = c("3 INT", "3EUOR", "3BILN", "3 SPO"),
                             "normale" = "3EME", 
                             "inf" = c("3THEA", "3PPRO", "DIMA",
                                       "3ULIS", "3SEGP", 
                                       "3HIAR")) %>% 
                factor(levels = c("inf", "normale", "sup")))

# Indicateurs classes atypiques -------------------------------------------

  # 2022_01_03. 
dat <- group_by(dat, divconstatrneses) %>%
  mutate(classe_atypique = 
           if_else(length(unique(clefgestion_constat)) == 1, 
                   "oui", "non"),
         classe_atypique = if_else(niveau_section == "normale",
         "non", classe_atypique),
         type_classe = if_else(classe_atypique == "oui" & 
                                 niveau_section == "sup",
                               "atypique_sup",
                               if_else(classe_atypique == "oui" & 
                                         niveau_section == "inf",
                                       "atypique_inf", 
                                       "non_atypique"))) %>% 
  ungroup

# Nouveaux pairs - anciens pairs ------------------------------------------

  # Erreurs techniques. À refaire (2021_10_26). 
  # MAJ 2021_12_03 : ASHWGA. Voir à la fin. 
    # Car il faut faire un filtre sur ! is.na (score). 

# cg ----------------------------------------------------------------------

  # cg : clefgestion_constat. 

dat <- mutate(dat, cg = clefgestion_constat) %>% mutate(cg = str_replace(cg, " ", "") %>% str_to_lower) %>% 
  mutate(cg = factor(cg, levels = c("3uppr", "3upe2",
                                    "3segp", "3ulis", "dima", "3ppro", "3thea", "3eme",
                                    "3spo", "3hiar", "3biln", "3euor", "3int")))

# Dummies cg --------------------------------------------------------------

  # cg : clefgestion_constat. 

dat <- cbind(dat, dummy_cols(dat, "cg") %>% (function (d) select(d, names(d)[str_detect(names(d), "cg_")]))) %>% as_tibble

# p_cg --------------------------------------------------------------------

dat <- group_by(dat, divconstatrneses) %>% 
  mutate(p_cg_3segp = (sum(cg_3segp) - cg_3segp) / (tdivconstatrneses - 1),
         p_cg_3ulis = (sum(cg_3ulis) - cg_3ulis) / (tdivconstatrneses - 1),
         p_cg_dima = (sum(cg_dima) - cg_dima) / (tdivconstatrneses - 1),
         p_cg_3ppro = (sum(cg_3ppro) - cg_3ppro) / (tdivconstatrneses - 1),
         p_cg_3thea = (sum(cg_3thea) - cg_3thea) / (tdivconstatrneses - 1),
         p_cg_3eme = (sum(cg_3eme) - cg_3eme) / (tdivconstatrneses - 1),
         p_cg_3spo = (sum(cg_3spo) - cg_3spo) / (tdivconstatrneses - 1),
         p_cg_3hiar = (sum(cg_3hiar) - cg_3hiar) / (tdivconstatrneses - 1),
         p_cg_3biln = (sum(cg_3biln) - cg_3biln) / (tdivconstatrneses - 1),
         p_cg_3euor = (sum(cg_3euor) - cg_3euor) / (tdivconstatrneses - 1),
         p_cg_3int = (sum(cg_3int) - cg_3int) / (tdivconstatrneses - 1))  %>% 
  ungroup

# trneconstatses ----------------------------------------------------------

dat <- group_by(dat, rneconstatses) %>% mutate(trneconstatses = n()) %>% ungroup

# tecole et tclasse (depuis constat) --------------------------------------

c <- group_by(constat3, session_mod, rne_mod) %>% mutate(tecole = n()) %>% ungroup
c <- group_by(c, session_mod, rne_mod, div_mod) %>% mutate(tclasse = n()) %>% ungroup
c <- filter(c, sigle != "LP PR")
dat <- left_join(dat, 
                 select(c, session_mod, ine_mod, tecole, tclasse), 
                 by = c("session_mod", "ine_mod"))
  # nobs ok. 
  
rm(c)  

# Instpiketty -------------------------------------------------------------
  # Piketty et Valdenaire 2006 : tailles de classe théorique : 26 pour les REP, 28 pour les HEP et 30 pour les privés. 
  # en regardant mes moyennes : 23, 25 et 29. 
dat <- mutate(dat, 
              instpik = ifelse(reseauR_mod == 1, tecole / (floor(tecole / 26) + 1),
                               ifelse(reseauR_mod == 0 & statut_constat == "PU", tecole / (floor(tecole / 28) + 1),
                                      tecole / (floor(tecole / 30) + 1)))) %>% 
  (function (d) {d$instpik[is.infinite(d$instpik)] <- NA ; d})

  # Version de F. Payet. 
dat <- mutate(dat, 
              instpik2 = ifelse(reseau_mod == "hep" & statut_constat == "PU", tecole / (floor(tecole / 28) + 1), 
                                ifelse(reseau_mod == "rep", tecole / (floor(tecole / 26) + 1),
                                       ifelse(reseau_mod == "rep+", tecole / (floor(tecole / 24) + 1), 
                                              tecole / (floor(tecole / 30) + 1))))) %>% 
  (function (d) {d$instpik2[is.infinite(d$instpik2)] <- NA ; d})
# pour les 3ème prépa pro, le seuil est de 24, peu importe. Parmi les classes contenant 3ppro, 97% sont des classes avec des prépapro uniquement. 
dat <- group_by(dat, divconstatrneses) %>% mutate(contenu = paste(sort(unique(cg)), collapse = " ET ")) %>% ungroup %>% 
  mutate(instpik2 = ifelse(str_detect(contenu, "3ppro") & ! str_detect(contenu, " ET "), 24, instpik2))


  # Version après analyse empirique : voir explorations_constat3_tecole.R. 
dat <- mutate(dat, 
              instpik3 = ifelse(reseau_mod == "hep" & statut_constat == "PU", tecole / (floor(tecole / 28) + 1), 
                                ifelse(reseau_mod == "rep", tecole / (floor(tecole / 26) + 1),
                                       ifelse(reseau_mod == "rep+", tecole / (floor(tecole / 25) + 1), 
                                              tecole / (floor(tecole / 31) + 1))))) %>% 
  (function (d) {d$instpik3[is.infinite(d$instpik3)] <- NA ; d})
  # la plus mauvaise des trois (lm(tclasse ~ instpik/instpik2/instpik3))

# Nouveaux pairs - anciens pairs ------------------------------------------

dat0 <- dat
dat_nonaine <- filter(dat, ! is.na(score))

  # n() version retrouvés. 
dat_nonaine <- group_by(dat_nonaine, divconstatrneses) %>% 
  mutate(nona = n()) %>% ungroup
  
  # Deux indices de repérages anciens pairs (classe et école). 
dat_nonaine <- mutate(dat_nonaine, 
              clcm2_cldnb = paste(classecoh, divconstatrneses),
              
              ecm2_cldnb = paste(ecolecoh, divconstatrneses))

  # Taille de groupe d'anciens pairs. 
dat_nonaine <- group_by(dat_nonaine, clcm2_cldnb) %>% 
  mutate(tclcm2_cldnb = n()) %>% ungroup

dat_nonaine <- group_by(dat_nonaine, ecm2_cldnb) %>% 
  mutate(tecm2_cldnb = n()) %>% ungroup
  
  # sog : sum old group. 
dat_nonaine <- group_by(dat_nonaine, clcm2_cldnb) %>% 
  mutate(sog_score_norm = sum(score_norm)) %>% 
  ungroup

dat_nonaine <- group_by(dat_nonaine, ecm2_cldnb) %>% 
  mutate(soge_score_norm = sum(score_norm)) %>% 
  ungroup

  # pnp. 
dat_nonaine <- group_by(dat_nonaine, divconstatrneses) %>% 
  mutate(pnp_score_norm = 
           (sum(score_norm) - sog_score_norm) /
      (nona - tclcm2_cldnb),
    
    pnpe_score_norm = 
      (sum(score_norm) - soge_score_norm) /
      (nona - tecm2_cldnb)) %>% 
  ungroup %>% 
  (function (d) {
    d$pnp_score_norm[d$nona <= 1] <- NA
    d$pnpe_score_norm[d$nona <= 1] <- NA
    d$pnp_score_norm[d$nona == d$tclcm2_cldnb & 
                       d$nona > 1] <- 0
    d$pnpe_score_norm[d$nona == d$tclcm2_cldnb & 
                        d$nona > 1] <- 0
    d
  })
  # Pour les n() = 1 ou 2, les pnp sont infinies. 
  # On les remplace par des valeurs manquantes. 
  # 9 valeurs manquantes chez pnp_score_norm. 
  # 9 valeurs manquantes chez les pnpe_score_norm. 

  # pop. 
dat_nonaine <- group_by(dat_nonaine, divconstatrneses) %>% 
  mutate(
    pop_score_norm = 
           (sog_score_norm - score_norm) / 
               (tclcm2_cldnb - 1),
    
    pope_score_norm = 
      (soge_score_norm - score_norm) / 
      (tecm2_cldnb - 1)
    ) %>% 
  ungroup %>% 
  (function (d) {
    d$pop_score_norm[d$tclcm2_cldnb == 1] <- 0
    d$pope_score_norm[d$tecm2_cldnb == 1] <- 0
    d
  })
  # Pour les tclcm2_cldnb ou tecm2_cldnb = 1, 
  # pop = 0 (logique). 

  # Vérifications : 

  # Montre que pop = p si pnp = 0 : 

  # dat_nonaine %>% 
  #   select(pnp_score_norm, nona, pop_score_norm, 
  #          tclcm2_cldnb, p_score_norm) %>% 
  #   filter(pnp_score_norm == 0)

  # Montre que pnp = p si pop = 0, 
  # c'est le cas si tclcm2_cldnb = 1 : 

  # dat_nonaine %>% 
#   select(pnp_score_norm, nona, pop_score_norm, 
#          tclcm2_cldnb, p_score_norm) %>% 
#   filter(tclcm2_cldnb == 1)

  # Les code ci-dessous vérifie bien que la moyenne pondérée
  # des pnp et pop est égale à p_score. 

# dat_nonaine %>% 
#   select(pnp_score_norm, nona,
#          pop_score_norm, tclcm2_cldnb, p_score_norm) %>% 
#   mutate(v = (tclcm2_cldnb - 1) * pop_score_norm / 
#            (nona - 1) + (nona - tclcm2_cldnb) *
#            pnp_score_norm / (nona - 1)) %>% 
#   mutate(v2 = v - p_score_norm) %>% pull(v2) %>% summary()

# dat_nonaine %>% 
#   select(pnpe_score_norm, nona,
#          pope_score_norm, tecm2_cldnb, p_score_norm) %>% 
#   mutate(v = (tecm2_cldnb - 1) * pope_score_norm / 
#            (nona - 1) + (nona - tecm2_cldnb) *
#            pnpe_score_norm / (nona - 1)) %>% 
#   mutate(v2 = v - p_score_norm) %>% pull(v2) %>% summary()

  # sog, score_f_norm : sum old group. 
dat_nonaine <- group_by(dat_nonaine, clcm2_cldnb) %>% 
  mutate(sog_score_f_norm = sum(score_f_norm)) %>% 
  ungroup

dat_nonaine <- group_by(dat_nonaine, ecm2_cldnb) %>% 
  mutate(soge_score_f_norm = sum(score_f_norm)) %>% 
  ungroup

  # pnp, score_f_norm. 
dat_nonaine <- group_by(dat_nonaine, divconstatrneses) %>% 
  mutate(pnp_score_f_norm = 
           (sum(score_f_norm) - sog_score_f_norm) /
      (nona - tclcm2_cldnb),
    
    pnpe_score_f_norm = 
      (sum(score_f_norm) - soge_score_f_norm) /
      (nona - tecm2_cldnb)) %>% 
  ungroup %>% 
  (function (d) {
    d$pnp_score_f_norm[d$nona <= 1] <- NA
    d$pnpe_score_f_norm[d$nona <= 1] <- NA
    d$pnp_score_f_norm[d$nona == d$tclcm2_cldnb & 
                       d$nona > 1] <- 0
    d$pnpe_score_f_norm[d$nona == d$tclcm2_cldnb & 
                        d$nona > 1] <- 0
    d
  })
  # Pour les n() = 1 ou 2, les pnp sont infinies. 
  # On les remplace par des valeurs manquantes. 
  # 9 valeurs manquantes chez pnp_score_f_norm. 
  # 9 valeurs manquantes chez les pnpe_score_f_norm. 

  # pop, score_f_norm. 
dat_nonaine <- group_by(dat_nonaine, divconstatrneses) %>% 
  mutate(
    pop_score_f_norm = 
           (sog_score_f_norm - score_f_norm) / 
               (tclcm2_cldnb - 1),
    
    pope_score_f_norm = 
      (soge_score_f_norm - score_f_norm) / 
      (tecm2_cldnb - 1)
    ) %>% 
  ungroup %>% 
  (function (d) {
    d$pop_score_f_norm[d$tclcm2_cldnb == 1] <- 0
    d$pope_score_f_norm[d$tecm2_cldnb == 1] <- 0
    d
  })

  # Vérification : 

# dat_nonaine %>% 
#   select(pnp_score_f_norm, nona,
#          pop_score_f_norm, tclcm2_cldnb, p_score_f_norm) %>% 
#   mutate(v = (tclcm2_cldnb - 1) * pop_score_f_norm / 
#            (nona - 1) + (nona - tclcm2_cldnb) *
#            pnp_score_f_norm / (nona - 1)) %>% 
#   mutate(v2 = v - p_score_f_norm) %>% pull(v2) %>% summary()

# dat_nonaine %>% 
#   select(pnpe_score_f_norm, nona,
#          pope_score_f_norm, tecm2_cldnb, p_score_f_norm) %>% 
#   mutate(v = (tecm2_cldnb - 1) * pope_score_f_norm / 
#            (nona - 1) + (nona - tecm2_cldnb) *
#            pnpe_score_f_norm / (nona - 1)) %>% 
#   mutate(v2 = v - p_score_f_norm) %>% pull(v2) %>% summary()
  # ok. 

 # sog, score_m_norm : sum old group. 
dat_nonaine <- group_by(dat_nonaine, clcm2_cldnb) %>% 
  mutate(sog_score_m_norm = sum(score_m_norm)) %>% 
  ungroup

dat_nonaine <- group_by(dat_nonaine, ecm2_cldnb) %>% 
  mutate(soge_score_m_norm = sum(score_m_norm)) %>% 
  ungroup

  # pnp, score_m_norm. 
dat_nonaine <- group_by(dat_nonaine, divconstatrneses) %>% 
  mutate(pnp_score_m_norm = 
           (sum(score_m_norm) - sog_score_m_norm) /
      (nona - tclcm2_cldnb),
    
    pnpe_score_m_norm = 
      (sum(score_m_norm) - soge_score_m_norm) /
      (nona - tecm2_cldnb)) %>% 
  ungroup %>% 
  (function (d) {
    d$pnp_score_m_norm[d$nona <= 1] <- NA
    d$pnpe_score_m_norm[d$nona <= 1] <- NA
    d$pnp_score_m_norm[d$nona == d$tclcm2_cldnb & 
                       d$nona > 1] <- 0
    d$pnpe_score_m_norm[d$nona == d$tclcm2_cldnb & 
                        d$nona > 1] <- 0
    d
  })
  # Pour les n() = 1 ou 2, les pnp sont infinies. 
  # On les remplace par des valeurs manquantes. 
  # 9 valeurs manquantes chez pnp_score_m_norm. 
  # 9 valeurs manquantes chez les pnpe_score_m_norm. 

  # pop, score_m_norm. 
dat_nonaine <- group_by(dat_nonaine, divconstatrneses) %>% 
  mutate(
    pop_score_m_norm = 
           (sog_score_m_norm - score_m_norm) / 
               (tclcm2_cldnb - 1),
    
    pope_score_m_norm = 
      (soge_score_m_norm - score_m_norm) / 
      (tecm2_cldnb - 1)
    ) %>% 
  ungroup %>% 
  (function (d) {
    d$pop_score_m_norm[d$tclcm2_cldnb == 1] <- 0
    d$pope_score_m_norm[d$tecm2_cldnb == 1] <- 0
    d
  })
  
  # Vérification : 
  
# dat_nonaine %>% 
#   select(pnp_score_m_norm, nona,
#          pop_score_m_norm, tclcm2_cldnb, p_score_m_norm) %>% 
#   mutate(v = (tclcm2_cldnb - 1) * pop_score_m_norm / 
#            (nona - 1) + (nona - tclcm2_cldnb) *
#            pnp_score_m_norm / (nona - 1)) %>% 
#   mutate(v2 = v - p_score_m_norm) %>% pull(v2) %>% summary()

# dat_nonaine %>% 
#   select(pnpe_score_m_norm, nona,
#          pope_score_m_norm, tecm2_cldnb, p_score_m_norm) %>% 
#   mutate(v = (tecm2_cldnb - 1) * pope_score_m_norm / 
#            (nona - 1) + (nona - tecm2_cldnb) *
#            pnpe_score_m_norm / (nona - 1)) %>% 
#   mutate(v2 = v - p_score_m_norm) %>% pull(v2) %>% summary()
  # ok. 

  # MAJ2022_01_07 : pour les pnp_q5score et pnp_qbscore, 
    # script externe. Contrepartie : ne marche pas en job. 
source(here("01_01_donnees_pnp_supp_def.R"))

  # Jointure : 
dat <- left_join(dat, 
                 select(dat_nonaine, ine_mod, session_mod,
                        names(dat_nonaine)[
                          ! names(dat_nonaine) %in% names(dat)
                        ]),
                 by = c("session_mod", "ine_mod"))
  # nobs ok. 

# Gibbons et Telhaj, équation 5, données ----------------------------------

cm2 <- group_by(cm2, classecoh) %>% 
  mutate(pcm2_score_norm = (sum(score_norm) - score_norm) / 
           (n() - 1),
         pcm2_score_f_norm = (sum(score_f_norm) - score_f_norm) / 
           (n() - 1),
         pcm2_score_m_norm = (sum(score_m_norm) - score_m_norm) / 
           (n() - 1),
         
         pcm2_sexe_M_norm = (sum(sexe_M) - sexe_M) / 
           (n() - 1),
         pcm2_sexe_F_norm = (sum(sexe_F) - sexe_F) / 
           (n() - 1),
         
         pcm2_pcs_g2_Defav = (sum(pcs_g2_Defav) - pcs_g2_Defav) / 
           (n() - 1),
         pcm2_pcs_g2_Moy = (sum(pcs_g2_Moy) - pcs_g2_Moy) / 
           (n() - 1),
         pcm2_pcs_g2_Fav = (sum(pcs_g2_Fav) - pcs_g2_Fav) / 
           (n() - 1),
         pcm2_pcs_g2_Tresfav = (sum(pcs_g2_Tresfav) - pcs_g2_Tresfav) / 
           (n() - 1),
         pcm2_pcs_g2_Autres = (sum(pcs_g2_Autres) - pcs_g2_Autres) / 
           (n() - 1),
         pcm2_pcs_g2_NA = (sum(pcs_g2_NA) - pcs_g2_NA) / 
           (n() - 1),
         
         pcm2_age_abs = (sum(age_abs) - age_abs) / 
           (n() - 1),
         
         pcm2_position_Redoublant = (sum(position_Redoublant) - 
                                       position_Redoublant) / 
           (n() - 1),
         pcm2_position_Heure = (sum(position_Heure) - 
                                  position_Heure) / 
           (n() - 1),
         pcm2_position_Avance = (sum(position_Avance) - 
                                   position_Avance) / 
           (n() - 1)
         ) %>% 
  ungroup

dat <- left_join(dat %>% mutate(id2 = paste(ine_mod, session_mod)), 
                 select(cm2, ine, 
                        ecole, 
                           names(cm2)[str_detect(names(cm2), 
                                                 "^pcm2\\_")]),
                 by = c("ine_mod" =  "ine")) %>% 
  filter(! duplicated(id2)) %>% select(- id2)
  # nobs = ok. 

# Lavy et al. 2012 : within pupil, données --------------------------------

dat_lavy <- transmute(dat, 
                   session_mod,
                   ine_mod,
                   rneconstat,
                   rneconstatses,
                   divconstatrneses,
                   ecole, 
                   ecolecoh, 
                   classecoh, 
                   moy_fran_ec_norm, 
                   moy_maths_ec_norm,
                   score_f_norm,
                   score_m_norm,
                   p_score_f_norm,
                   p_score_m_norm)

dat_lavy <- filter(dat_lavy, ! is.na(p_score_f_norm))
  # ok : même nombre d'observations que dat_nonaine. 
  
# Filtre : sans 3upe2 et 3uppr --------------------------------------------
  # nb : le 2021-06-22-21h55. Modifie très légèrement les nombres d'observation : on enlève 3upe2 et 3uppr qui compte 3 et 2 obs. 
  # et pour lesquels on ne retrouve pas la note au CM2. 

dat <- filter(dat, ! cg %in% c("3upe2", "3uppr")) %>% 
  mutate(cg = droplevels(cg))

# Sauvegarde datcompl ----------------------------------------------------------------

datcompl <- dat
save(datcompl, file = here("01_01_datcompl_def.rda"))

# Filtrage : sans les classes de niveau évidentes -------------------------
  # Est-ce correct de les filtrer d'ici plutôt que depuis constat3 ?

group_by(dat, divconstatrneses) %>% 
  summarise(nclg = length(unique(clefgestion_constat)),
         contenu = paste(sort(unique(clefgestion_constat)),
                         collapse = " ET ")) %>% 
  filter(! str_detect(contenu, "^3EME$") & 
           ! str_detect(contenu, " ET ")) -> 
  dcl_niv
  # MAJ2022_01_21. Le code ci-dessus contenait un sort de trop après 
  # unique. Je l'ai enlevé. Ne change rien. 


  # Les classes de niveaux mélangés entre eux : ex : BILN et EURO. 
group_by(dat, divconstatrneses) %>% 
  summarise(nclg = length(unique(clefgestion_constat)),
            contenu = paste(sort(unique(clefgestion_constat)),
                            collapse = " ET ")) %>% 
  filter(! str_detect(contenu, "3EME") & 
           str_detect(contenu, " ET ") & 
           str_detect(contenu, 
                      "3 SPO|3SEGP|DIMA|3BILN|3BILN|3EUOR|3PPRO")) -> 
  dcl_niv2
  # On remarque que ce sont les 3BILN et 3EUOR qui constituent la majorité
  # des cas. (25 classes-année). 
  # MAJ2022_01_21. Le code ci-dessus contenait un sort de trop après 
  # unique. Je l'ai enlevé. Ne change rien. 

group_by(constat3, divconstatrneses) %>% 
  summarise(nclg = length(unique(clefgestion_mod)),
            contenu = paste(sort(unique(clefgestion_mod)),
                            collapse = " ET ")) %>% 
  filter(! str_detect(contenu, "^3EME$") & 
           ! str_detect(contenu, " ET ")) -> 
  dcl_nivconstat
  # MAJ2022_01_21. Le code ci-dessus contenait un sort de trop après 
  # unique. Je l'ai enlevé. Ne change rien. 

# Les classes de niveaux mélangés entre eux : ex : BILN et EURO. 
group_by(constat3, divconstatrneses) %>% 
  summarise(nclg = length(unique(clefgestion_mod)),
            contenu = paste(sort(unique(clefgestion_mod)),
                            collapse = " ET ")) %>% 
  filter(! str_detect(contenu, "3EME") & 
           str_detect(contenu, " ET ") & 
           str_detect(contenu, 
                      "3 SPO|3SEGP|DIMA|3BILN|3BILN|3EUOR|3PPRO")) -> 
  dcl_nivconstat2
  # MAJ2022_01_21. Le code ci-dessus contenait un sort de trop après 
  # unique. Je l'ai enlevé. Ne change rien. 

dat <- filter(dat, ! divconstatrneses %in% 
                c(dcl_niv$divconstatrneses,
                  dcl_niv2$divconstatrneses))
  # plus que 35979 obs, 84.26% des données. 


dat_noclnivconstat <- filter(dat, ! divconstatrneses %in% 
                               c(dcl_nivconstat$divconstatrneses,
                                 dcl_nivconstat2$divconstatrneses))
  # 36099 obs, soit 84.55% des données. 

# On peut se demander qui choisir. 
  # On choisit le premier car (1) c'est les données utilisées 
  # pour l'estimation. 
  # Ensuite, les résultats en termes de statistiques sont quasiment les 
  # mêmes. 
  # L'argument qui justifie pourquoi on retient le premier est la taille de
  # classe : min 5 pour le premier alors que min 2 pour le second. 

# Sauvegarde --------------------------------------------------------------

save(dat, file = here("01_01_donnees_def.rda"), version = 2)


# NE PAS OUBLIER D ACTUALISER 
# PECLNIV ET DATRESTR 2 ET TOUT AUTRES DONNEES. 
