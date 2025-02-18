# Données (spécifiques au chapitre/papier). 

library(here)
library(tidyverse)
library(magrittr)
library(fastDummies)

# Chargement --------------------------------------------------------------

load("D:/00_phd/02_rectorat/02_v2_construction_def/01_construction/04_pre_analyse_cm2_w09.rda")
load("D:/00_phd/02_rectorat/02_v2_construction_def/01_construction/04_pre_analyse_rdcm2.rda")
load("D:/00_phd/02_rectorat/02_v2_construction_def/01_construction/04_pre_analyse_cm2dnb.rda")

# Sélection de variables --------------------------------------------------

c <- select(cm2_w09, 
            cohorte, clef_cm2, clef_cm2coh, ine, inecoh, 
            commune, reseau, rnecirc, ecole,  ecolecoh,
            arrondissement, reseau_bin,
            statut, classe, classecoh,
            score, score_f, score_m,
            ecrire, lire, grammaire, ortho, voca,
            calcul, geometrie, grand_mes, nombre, org_donnee, 
            score_ori, score_f_ori, score_m_ori, 
            ecrire_ori, lire_ori, grammaire_ori, ortho_ori, voca_ori, 
            calcul_ori, geometrie_ori, grand_mes_ori, nombre_ori, org_donnee_ori,
            nais, age_abs, position_num, position, z, jour, quinze, mois, bim, trim,
            sexe, pcs, pcs_g2, pcsn2)

rdc <- select(rdcm2,
             cohorte, clef_cm2, clef_cm2coh, ine, inecoh, 
             commune, reseau, rnecirc, ecole,  ecolecoh,
             arrondissement, reseau_bin,
             statut, classe, classecoh,
             score, score_f, score_m,
             ecrire, lire, grammaire, ortho, voca,
             calcul, geometrie, grand_mes, nombre, org_donnee, 
             score_ori, score_f_ori, score_m_ori, 
             ecrire_ori, lire_ori, grammaire_ori, ortho_ori, voca_ori, 
             calcul_ori, geometrie_ori, grand_mes_ori, nombre_ori, org_donnee_ori,
             nais, age_abs, position_num, position, z, jour, quinze, mois, bim, trim,
             sexe, pcs, pcs_g2)

cd <- select(cm2dnb, 
             cohorte, clef_cm2, clef_cm2coh, ine, inecoh, 
             commune, reseau = reseau.x, rnecirc, ecole,  ecolecoh,
             arrondissement, reseau_bin,
             statut, classe, classecoh,
             score, score_f, score_m,
             ecrire, lire, grammaire, ortho, voca,
             calcul, geometrie, grand_mes, nombre, org_donnee, 
             score_ori, score_f_ori, score_m_ori, 
             ecrire_ori, lire_ori, grammaire_ori, ortho_ori, voca_ori, 
             calcul_ori, geometrie_ori, grand_mes_ori, nombre_ori, org_donnee_ori,
             nais, age_abs, position_num, position, z, jour, quinze, mois, bim, trim,
             sexe = sexe.x, pcs = pcs.x, pcs_g2,
             clefgestion_constat, 
             libmef_constat,
             session_mod,
             
             moy_ec1, moy_cc,
             moy_fran_ec, moy_fran_cc, 
             moy_maths_ec, moy_maths_cc,
             moy_hist_ec,
             moy_dic_ec, 
             moy_red_ec
             )

# pcsR --------------------------------------------------------------------
# R comme régression. Il s'agit de construire une variable telle que
# NA est une modalité. 

c <- c %>% (function (x) 
  mutate(x, pcsR = if_else(is.na(pcs_g2), "NA", as.character(pcs_g2)) %>% 
           factor(levels = c(levels(x$pcs_g2), "NA"))))

rdc <- left_join(rdc, select(c, clef_cm2coh, pcsR), "clef_cm2coh")
  # nobs ok. 
cd <- left_join(cd, select(c, clef_cm2coh, pcsR), "clef_cm2coh")
  # nobs ok. 

# rdc : dist et old -------------------------------------------------------

rdc <- mutate(rdc, 
              dist = difftime(nais, as.Date("2000-01-01"), 
                              unit = "days") %>% as.numeric,
              old = as.numeric(dist >= 0))

# rdc : renormalisation ---------------------------------------------------

  # choix, probablement non justifié. 
  # Supprimé le 2021_09_27. 

# Notes maths : normalisation ---------------------------------------------

c <- group_by(c, cohorte) %>% 
  mutate(
    
  score_norm = (score - mean(score)) /
    sd(score),
  score_m_norm = (score_m - mean(score_m)) /
    sd(score_m),
  calcul_norm = (calcul - mean(calcul)) /
    sd(calcul),
  geometrie_norm = (geometrie - mean(geometrie)) /
    sd(geometrie),
  grand_mes_norm = (grand_mes - mean(grand_mes)) /
    sd(grand_mes),
  nombre_norm = (nombre - mean(nombre)) /
    sd(nombre),
  org_donnee_norm = (org_donnee - mean(org_donnee)) /
    sd(org_donnee)
) %>% ungroup()

# attention duplication. 
cd <- left_join(select(cd, - score, - score_m,
                       - calcul, - geometrie, - grand_mes,
                       - nombre, - org_donnee), 
                
                select(c, clef_cm2coh,
                       
                           score,
                           score_m,
                           calcul,
                           geometrie,
                           grand_mes,
                           nombre,
                           org_donnee,
                       
                       score_norm,
                       score_m_norm,
                       calcul_norm,
                       geometrie_norm,
                       grand_mes_norm,
                       nombre_norm,
                       org_donnee_norm
                       ), 
                by = "clef_cm2coh")
  # nobs ok. 

# Notes français : normalisation ------------------------------------------

c <- group_by(c, cohorte) %>% 
  mutate(score_f_norm = (score_f - mean(score_f)) / (sd(score_f)),
         ecrire_norm = (ecrire - mean(ecrire)) / (sd(ecrire)),
         lire_norm = (lire - mean(lire)) / (sd(lire)),
         grammaire_norm = (grammaire - mean(grammaire)) / (sd(grammaire)),
         ortho_norm = (ortho - mean(ortho)) / (sd(ortho)),
         voca_norm = (voca - mean(voca)) / (sd(voca))) %>% 
  ungroup

cd <- left_join(select(cd,
                       - score_f, - ecrire, - lire, - grammaire, - ortho, - voca),
                select(c, clef_cm2coh, 
                       score_f, ecrire, lire, grammaire, ortho, voca,
                       score_f_norm, ecrire_norm, lire_norm, grammaire_norm, ortho_norm, voca_norm),
                by = "clef_cm2coh")
  # nobs ok. 


  # Insertions dans rdc. 
rdc <- left_join(rdc, 
                 select(c, clef_cm2coh, 
                        score_norm, score_f_norm, score_m_norm,
                        
                        ecrire_norm, lire_norm, 
                        grammaire_norm, ortho_norm, 
                        voca_norm,
                        
                        calcul_norm, geometrie_norm,
                        grand_mes_norm, nombre_norm,
                        org_donnee_norm),
                 "clef_cm2coh")

# Notes CM2 sur 100 -------------------------------------------------------

  # Néant.   

# Rangs de percentiles  ---------------------------------------------------
  # rp. 

c <- group_by(c, cohorte) %>% 
  mutate(score_rp = percent_rank(score) * 100,
         score_f_rp = percent_rank(score_f) * 100,
         score_m_rp = percent_rank(score_m) * 100,
         
         ecrire_rp = percent_rank(ecrire) * 100,
         lire_rp = percent_rank(lire) * 100,
         grammaire_rp = percent_rank(grammaire) * 100,
         ortho_rp = percent_rank(ortho) * 100,
         voca_rp = percent_rank(voca) * 100,
         
         calcul_rp = percent_rank(calcul) * 100,
         geometrie_rp = percent_rank(geometrie) * 100,
         grand_mes_rp = percent_rank(grand_mes) * 100,
         nombre_rp = percent_rank(nombre) * 100,
         org_donnee_rp = percent_rank(org_donnee) * 100) %>% 
  ungroup

cd <- left_join(cd, select(c, clef_cm2coh, 
                           score_rp, score_f_rp, score_m_rp,
                           
                           ecrire_rp, lire_rp, grammaire_rp, ortho_rp,
                           voca_rp,
                           
                           calcul_rp, geometrie_rp, grand_mes_rp,
                           nombre_rp, org_donnee_rp), 
                by = "clef_cm2coh")
  # nobs ok. 

  # À recalculer sur l'échantillon discontinu. 
  # Non : on ne recalcule pas. (2021_09_27). 
rdc <- left_join(rdc, 
                 select(c, clef_cm2coh, 
                        score_rp,
                        score_f_rp,
                        score_m_rp,
                     
                        ecrire_rp,
                        lire_rp,
                        grammaire_rp,
                        ortho_rp,
                        voca_rp,
                     
                        calcul_rp,
                        geometrie_rp,
                        grand_mes_rp,
                        nombre_rp,
                        org_donnee_rp), "clef_cm2coh")
  # nobs ok. 

# Correction nom pcs ------------------------------------------------------

c <- mutate(c, pcs = factor(pcs, labels = 
                              c("Agr", "Art", "Cadr", "Prof", "Emp", "Ouvr",
                                "Retr", "Inact", "Autres")))

rdc <- mutate(rdc, pcs = factor(pcs, labels = 
                              c("Agr", "Art", "Cadr", "Prof", "Emp", "Ouvr",
                                "Retr", "Inact", "Autres")))

cd <- mutate(cd, pcs = factor(pcs, labels = 
                              c("Agr", "Art", "Cadr", "Prof", "Emp", "Ouvr",
                                "Retr", "Inact", "Autres")))

# Dummy cols c ------------------------------------------------------------

c <- dummy_cols(c, c("cohorte",
                     "reseau", "statut", 
                    "position", "jour", "quinze", "mois", "bim", "trim",
                    "sexe", "pcs", "pcs_g2",
                    "pcsR"))

rdc <- dummy_cols(rdc, c("cohorte",
                       "reseau", "statut", 
                    "position", "jour", "quinze", "mois", "bim", "trim",
                    "sexe", "pcs", "pcs_g2",
                    "pcsR"))

cd <- dummy_cols(cd, c("cohorte",
                       "reseau", "statut", 
                    "position", "jour", "quinze", "mois", "bim", "trim",
                    "sexe", "pcs", "pcs_g2",
                    "pcsR"))

# Dummies NA -> 0 ---------------------------------------------------------
  # pour les pcs. 
  # attention : ne pas faire de stats desc avec ces dummy_cols. 
  
c <- mutate(c, 
            
            pcs_Agr = ifelse(is.na(pcs_Agr), 0, pcs_Agr),
            pcsArt = ifelse(is.na(pcs_Art), 0, pcs_Art),
            pcs_Cadr = ifelse(is.na(pcs_Cadr), 0, pcs_Cadr),
            pcs_Prof = ifelse(is.na(pcs_Prof), 0, pcs_Prof),
            pcs_Emp = ifelse(is.na(pcs_Emp), 0, pcs_Emp),
            pcs_Ouvr = ifelse(is.na(pcs_Ouvr), 0, pcs_Ouvr),
            pcs_Retr = ifelse(is.na(pcs_Retr), 0, pcs_Retr),
            pcs_Inact = ifelse(is.na(pcs_Inact), 0, pcs_Inact),
            pcs_Autres = ifelse(is.na(pcs_Autres), 0, pcs_Autres),
            
            pcs_g2_Defav = ifelse(is.na(pcs_g2_Defav), 0, pcs_g2_Defav),
            pcs_g2_Moy = ifelse(is.na(pcs_g2_Moy), 0, pcs_g2_Moy),
            pcs_g2_Fav = ifelse(is.na(pcs_g2_Fav), 0, pcs_g2_Fav),
            pcs_g2_Tresfav = ifelse(is.na(pcs_g2_Tresfav), 0, 
                                    pcs_g2_Tresfav),
            pcs_g2_Autres = ifelse(is.na(pcs_g2_Autres), 0, 
                                   pcs_g2_Autres))

rdc <- mutate(rdc, 
            
            pcs_Agr = ifelse(is.na(pcs_Agr), 0, pcs_Agr),
            pcsArt = ifelse(is.na(pcs_Art), 0, pcs_Art),
            pcs_Cadr = ifelse(is.na(pcs_Cadr), 0, pcs_Cadr),
            pcs_Prof = ifelse(is.na(pcs_Prof), 0, pcs_Prof),
            pcs_Emp = ifelse(is.na(pcs_Emp), 0, pcs_Emp),
            pcs_Ouvr = ifelse(is.na(pcs_Ouvr), 0, pcs_Ouvr),
            pcs_Retr = ifelse(is.na(pcs_Retr), 0, pcs_Retr),
            pcs_Inact = ifelse(is.na(pcs_Inact), 0, pcs_Inact),
            pcs_Autres = ifelse(is.na(pcs_Autres), 0, pcs_Autres),
            
            pcs_g2_Defav = ifelse(is.na(pcs_g2_Defav), 0, pcs_g2_Defav),
            pcs_g2_Moy = ifelse(is.na(pcs_g2_Moy), 0, pcs_g2_Moy),
            pcs_g2_Fav = ifelse(is.na(pcs_g2_Fav), 0, pcs_g2_Fav),
            pcs_g2_Tresfav = ifelse(is.na(pcs_g2_Tresfav), 0, 
                                    pcs_g2_Tresfav),
            pcs_g2_Autres = ifelse(is.na(pcs_g2_Autres), 0, 
                                   pcs_g2_Autres))

cd <- mutate(cd, 
            
            pcs_Agr = ifelse(is.na(pcs_Agr), 0, pcs_Agr),
            pcsArt = ifelse(is.na(pcs_Art), 0, pcs_Art),
            pcs_Cadr = ifelse(is.na(pcs_Cadr), 0, pcs_Cadr),
            pcs_Prof = ifelse(is.na(pcs_Prof), 0, pcs_Prof),
            pcs_Emp = ifelse(is.na(pcs_Emp), 0, pcs_Emp),
            pcs_Ouvr = ifelse(is.na(pcs_Ouvr), 0, pcs_Ouvr),
            pcs_Retr = ifelse(is.na(pcs_Retr), 0, pcs_Retr),
            pcs_Inact = ifelse(is.na(pcs_Inact), 0, pcs_Inact),
            pcs_Autres = ifelse(is.na(pcs_Autres), 0, pcs_Autres),
            
            pcs_g2_Defav = ifelse(is.na(pcs_g2_Defav), 0, pcs_g2_Defav),
            pcs_g2_Moy = ifelse(is.na(pcs_g2_Moy), 0, pcs_g2_Moy),
            pcs_g2_Fav = ifelse(is.na(pcs_g2_Fav), 0, pcs_g2_Fav),
            pcs_g2_Tresfav = ifelse(is.na(pcs_g2_Tresfav), 0, 
                                    pcs_g2_Tresfav),
            pcs_g2_Autres = ifelse(is.na(pcs_g2_Autres), 0, 
                                   pcs_g2_Autres))

# tclassecoh --------------------------------------------------------------
  # c uniquement. pour cd : il faut joindre depuis c.

c <- group_by(c, classecoh) %>% 
  mutate(tclassecoh = n()) %>% 
  ungroup

cd <- left_join(cd, group_by(c, classecoh, tclassecoh) %>% summarise,
                by = "classecoh")
  # nobs ok. 

# p_age_abs et p_z ----------------------------------------------------------------

c <- group_by(c, classecoh) %>% 
  mutate(p_age_abs = (sum(age_abs) - age_abs) / (tclassecoh - 1),
         p_z = (sum(z) - z) / (tclassecoh - 1)) %>% 
  ungroup

  # prudence par rapport aux duplicatas. 
cd <- left_join(cd, select(c, clef_cm2coh, p_age_abs, p_z),
                by = "clef_cm2coh")
  # nobs ok. 

# p_jour, p_quinze, p_mois, p_bim et p_trim -------------------------------

  # 2022_04_20. 
c <- group_by(c, classecoh) %>% (function (d) {
  
  p_jour <- lapply(1:365, function (x) transmute(d, p = (sum(eval(parse(text = paste("jour_", x, sep = "")))) - 
                                                  eval(parse(text = paste("jour_", x, sep = "")))) / 
                                         (tclassecoh - 1)) %>% ungroup %>% select(- classecoh) %>% 
                     setNames(paste("p_jour_", x, sep = ""))) %>% do.call(what = cbind)
  
  p_quinze <- lapply(1:24, function (x) transmute(d, p = (sum(eval(parse(text = paste("quinze_", x, sep = "")))) - 
                                                                 eval(parse(text = paste("quinze_", x, sep = "")))) / 
                                                     (tclassecoh - 1)) %>% ungroup %>% select(- classecoh) %>%
                       setNames(paste("p_quinze_", x, sep = ""))) %>% do.call(what = cbind)
  
  p_mois <- lapply(1:12, function (x) transmute(d, p = (sum(eval(parse(text = paste("mois_", x, sep = "")))) - 
                                                              eval(parse(text = paste("mois_", x, sep = "")))) / 
                                                  (tclassecoh - 1)) %>% ungroup %>% select(- classecoh) %>%
                     setNames(paste("p_mois_", x, sep = ""))) %>% do.call(what = cbind)
  
  p_bim <- lapply(1:6, function (x) transmute(d, p = (sum(eval(parse(text = paste("bim_", x, sep = "")))) - 
                                                             eval(parse(text = paste("bim_", x, sep = "")))) / 
                                                 (tclassecoh - 1)) %>% ungroup %>% select(- classecoh) %>% 
                    setNames(paste("p_bim_", x, sep = ""))) %>% do.call(what = cbind)
  
  p_trim <- lapply(1:4, function (x) transmute(d, p = (sum(eval(parse(text = paste("trim_", x, sep = "")))) - 
                                                            eval(parse(text = paste("trim_", x, sep = "")))) / 
                                                (tclassecoh - 1)) %>% ungroup %>% select(- classecoh) %>% 
                     setNames(paste("p_trim_", x, sep = ""))) %>% do.call(what = cbind)
  
  cbind(ungroup(d), p_jour, p_quinze, p_mois, p_bim, p_trim)
  
  })

cd <- left_join(cd, select(c, clef_cm2coh, names(c)[
  str_detect(names(c), 
             paste("p_", c("jour", "mois", "bim", "trim"),
                   sep = "") %>% paste(collapse = "|"))]), 
  by = "clef_cm2coh")
  # nobs ok. 

# Filtre : cd alline et sans dup notes dnb --------------------------------

  # en filtrant sur ceux dont on retrouve l'ine uniquement : 36141
  # puis en ne prenant que la première note au DNB : 35894

  # premier dnb : first_dnb
first_dnb <- filter(cd, inecoh %in% 
                      unique(filter(cd, ! is.na(inecoh) & 
                                      duplicated(inecoh))$inecoh)) %>% 
  group_by(inecoh) %>% 
  summarise(first = min(as.numeric(as.character(session_mod))))
  # vérifié : ok. 

cdi <- left_join(cd, first_dnb, by = "inecoh") %>% 
  filter(! is.na(inecoh)) %>% 
  mutate(first = if_else(is.na(first), 
                         as.numeric(as.character(session_mod)),
                         first)) %>% 
  group_by(inecoh) %>% arrange(desc(session_mod)) %>% 
  filter(! duplicated(inecoh)) %>% ungroup
  # jusqu'ici : 36141 = 35894 + 247. ok.
  # nobs ok : 35894. 
  # vérifié que cela garde bien la première note au DNB. 

# Normalisation notes dnb -------------------------------------------------

cdi <- cdi %>% 
  
  group_by(cohorte) %>% 
  
  mutate(moy_ec1_norm = (moy_ec1 - mean(moy_ec1, na.rm = TRUE)) / 
           sd(moy_ec1, na.rm = TRUE),
         
         moy_cc_norm = (moy_cc - mean(moy_cc, na.rm = TRUE)) / 
           sd(moy_cc, na.rm = TRUE),
         
         moy_fran_ec_norm = (moy_fran_ec - mean(moy_fran_ec, na.rm = TRUE)) / 
           sd(moy_fran_ec, na.rm = TRUE),
         
         moy_fran_cc_norm = (moy_fran_cc - mean(moy_fran_cc, na.rm = TRUE)) / 
           sd(moy_fran_cc, na.rm = TRUE),
         
         moy_maths_ec_norm = (moy_maths_ec - mean(moy_maths_ec, na.rm = TRUE)) / 
           sd(moy_maths_ec, na.rm = TRUE),
         
         moy_maths_cc_norm = (moy_maths_cc - mean(moy_maths_cc, na.rm = TRUE)) / 
           sd(moy_maths_cc, na.rm = TRUE),
         
         moy_hist_ec_norm = (moy_hist_ec - mean(moy_hist_ec, na.rm = TRUE)) / 
           sd(moy_hist_ec, na.rm = TRUE),
         
         moy_dic_ec_norm = (moy_dic_ec - mean(moy_dic_ec, na.rm = TRUE)) / 
           sd(moy_dic_ec, na.rm = TRUE),
         
         moy_red_ec_norm = (moy_red_ec - mean(moy_red_ec, na.rm = TRUE)) / 
           sd(moy_red_ec, na.rm = TRUE)
         
         ) %>% 
  
  ungroup

# Rang perc  : notes dnb --------------------------------------------------

cdi <- group_by(cdi, cohorte) %>% 
  mutate(moy_ec1_rp = percent_rank(moy_ec1) * 100,
         
         moy_cc_rp = percent_rank(moy_cc) * 100,
         
         moy_fran_ec_rp = percent_rank(moy_fran_ec) * 100,
         
         moy_fran_cc_rp = percent_rank(moy_fran_cc) * 100,
         
         moy_maths_ec_rp = percent_rank(moy_maths_ec) * 100,
         
         moy_maths_cc_rp = percent_rank(moy_maths_cc) * 100,
         
         moy_hist_ec_rp = percent_rank(moy_hist_ec) * 100,
         
         moy_dic_ec_rp = percent_rank(moy_dic_ec) * 100,
         
         moy_red_ec_rp = percent_rank(moy_red_ec) * 100) %>% 
  ungroup

# rdcdi -------------------------------------------------------------------
  # di : notes aux dnb, ine retrouvés. 

rdcdi <- left_join(rdc, 
                   select(cdi, inecoh, 
                          moy_ec1, moy_cc, 
                          moy_fran_ec, moy_fran_cc,
                          moy_maths_ec, moy_maths_cc,
                          moy_hist_ec, 
                          moy_dic_ec,
                          moy_red_ec,
                          
                          moy_ec1_norm, moy_cc_norm, 
                          moy_fran_ec_norm, moy_fran_cc_norm,
                          moy_maths_ec_norm, moy_maths_cc_norm,
                          moy_hist_ec_norm, 
                          moy_dic_ec_norm,
                          moy_red_ec_norm,
                          
                          moy_ec1_rp, moy_cc_rp, 
                          moy_fran_ec_rp, moy_fran_cc_rp,
                          moy_maths_ec_rp, moy_maths_cc_rp,
                          moy_hist_ec_rp, 
                          moy_dic_ec_rp,
                          moy_red_ec_rp),
                   
                   by = "inecoh")
  # nobs ok. 

# Normalisation notes dnb rdcdi -------------------------------------------

  # Plus besoin. (2021_09_27). 

# Rang perc rdcdi ---------------------------------------------------------

  # Plus besoin. (2021_09_27)

# p_covars CM2 ------------------------------------------------------------

c <- group_by(c, cohorte) %>% 
  mutate(p_sexe_M = (sum(sexe_M) - sexe_M) / 
           (tclassecoh - 1),
         
         p_pcsR_Defav = (sum(pcsR_Defav) - pcsR_Defav) / 
           (tclassecoh - 1),
         
         p_pcsR_Moy = (sum(pcsR_Moy) - pcsR_Moy) / 
           (tclassecoh - 1),
         
         p_pcsR_Fav = (sum(pcsR_Fav) - pcsR_Fav) / 
           (tclassecoh - 1),
         
         p_pcsR_Tresfav = (sum(pcsR_Tresfav) - pcsR_Tresfav) / 
           (tclassecoh - 1),
         
         p_pcsR_Autres = (sum(pcsR_Autres) - pcsR_Autres) / 
           (tclassecoh - 1),
         
         p_pcsR_NA = (sum(pcsR_NA) - pcsR_NA) / 
           (tclassecoh - 1)) %>% 
  ungroup

# p_covars Inf en NA.  ----------------------------------------------------

c <- mutate_at(c, names(c)[str_detect(names(c), "^p_")],
               function (x) {
                 x[is.infinite(x)] <- NA
                 x
               })

# Données isolées DNB -----------------------------------------------------

load("D:/00_phd/02_rectorat/04_resultats/42_pe/01_01_datcompl_def.rda")

  # On garde tout (2021_11_22). 
d <- datcompl
# d <- select(datcompl,
#             names(datcompl)[! names(datcompl) %in%
#                               names(c)],
#             score_norm, score_f_norm, score_m_norm)

# session_mod : droplevels ------------------------------------------------

d <- mutate(d, session_mod = droplevels(session_mod))
  # Rappel : si j'ai le temps : prendre DNB17. 

# Correction des noms -----------------------------------------------------

  # 2021_11_22 : plus besoin car change en 01_01_datcompl_def.

# d <- mutate(d, 
#             moy_ec1 = moy_ec1_ori, 
#             moy_cc = moy_cc_ori,
#             moy_fran_ec = moy_fran_ec_ori,
#             moy_fran_cc = moy_fran_cc_ori,
#             moy_maths_ec = moy_maths_ec_ori,
#             moy_maths_cc = moy_maths_cc_ori,
#             moy_hist_ec = moy_hist_ec_ori,
#             moy_dic_ec = moy_dic_ec_ori,
#             moy_red_ec = moy_red_ec_ori) %>% 
#   select(- moy_ec1_ori, 
#          - moy_cc_ori,
#          - moy_fran_ec_ori,
#          - moy_fran_cc_ori,
#          - moy_maths_ec_ori,
#          - moy_maths_cc_ori,
#          - moy_hist_ec_ori,
#          - moy_dic_ec_ori,
#          - moy_red_ec_ori)

# mt : normalisation ------------------------------------------------------

  # 2021_11_22 : plus besoin car chargement de 01_01_datcompl_def. 
# d <- d %>% 
#   
#   group_by(session_mod) %>% 
#   
#   mutate(moy_ec1_norm = (moy_ec1 - mean(moy_ec1, na.rm = TRUE)) / 
#            sd(moy_ec1, na.rm = TRUE),
#          
#          moy_cc_norm = (moy_cc - mean(moy_cc, na.rm = TRUE)) / 
#            sd(moy_cc, na.rm = TRUE),
#          
#          moy_fran_ec_norm = (moy_fran_ec - mean(moy_fran_ec, na.rm = TRUE)) / 
#            sd(moy_fran_ec, na.rm = TRUE),
#          
#          moy_fran_cc_norm = (moy_fran_cc - mean(moy_fran_cc, na.rm = TRUE)) / 
#            sd(moy_fran_cc, na.rm = TRUE),
#          
#          moy_maths_ec_norm = (moy_maths_ec - mean(moy_maths_ec, na.rm = TRUE)) / 
#            sd(moy_maths_ec, na.rm = TRUE),
#          
#          moy_maths_cc_norm = (moy_maths_cc - mean(moy_maths_cc, na.rm = TRUE)) / 
#            sd(moy_maths_cc, na.rm = TRUE),
#          
#          moy_hist_ec_norm = (moy_hist_ec - mean(moy_hist_ec, na.rm = TRUE)) / 
#            sd(moy_hist_ec, na.rm = TRUE),
#          
#          moy_dic_ec_norm = (moy_dic_ec - mean(moy_dic_ec, na.rm = TRUE)) / 
#            sd(moy_dic_ec, na.rm = TRUE),
#          
#          moy_red_ec_norm = (moy_red_ec - mean(moy_red_ec, na.rm = TRUE)) / 
#            sd(moy_red_ec, na.rm = TRUE)
#          
#   ) %>% 
#   
#   ungroup


# mt : rp -----------------------------------------------------------------

  # 2021_11_22 : plus besoin car chargement de 01_01_donnees_def.rda. 

# d <- group_by(d, session_mod) %>% 
#   mutate(moy_ec1_rp = percent_rank(moy_ec1) * 100,
#          
#          moy_cc_rp = percent_rank(moy_cc) * 100,
#          
#          moy_fran_ec_rp = percent_rank(moy_fran_ec) * 100,
#          
#          moy_fran_cc_rp = percent_rank(moy_fran_cc) * 100,
#          
#          moy_maths_ec_rp = percent_rank(moy_maths_ec) * 100,
#          
#          moy_maths_cc_rp = percent_rank(moy_maths_cc) * 100,
#          
#          moy_hist_ec_rp = percent_rank(moy_hist_ec) * 100,
#          
#          moy_dic_ec_rp = percent_rank(moy_dic_ec) * 100,
#          
#          moy_red_ec_rp = percent_rank(moy_red_ec) * 100) %>% 
#   ungroup

# Données isolées : sans sections spécifiques -----------------------------

  # "dat" dans le projet pe. 
load("D:/00_phd/02_rectorat/04_resultats/42_pe/01_01_donnees_def.rda")

  # noclniv : pas de sections spécifiques. 
d_noclniv <- filter(mutate(d, id3 = paste(ine_mod, session_mod)),
                    id3 %in% paste(dat$ine_mod, dat$session_mod))
  # nobs ok. 


# Rajout scores_f_norm0912, scores_f_norm101112 ---------------------------
  # Pour modèle ABS.

c <- mutate(c, 
            score_f_norm0912 = (score_f - mean(score_f)) / 
              sd(score_f),
            
            ecrire_norm0912 = (ecrire - mean(ecrire)) / 
              sd(ecrire),
            
            grammaire_norm0912 = (grammaire - mean(grammaire)) / 
              sd(grammaire),
            
            lire_norm0912 = (lire - mean(lire)) / 
              sd(lire),
            
            ortho_norm0912 = (ortho - mean(ortho)) / 
              sd(ortho),
            
            voca_norm0912 = (voca - mean(voca)) / 
              sd(voca))

c101112 <- filter(c, cohorte != "2009")
c101112 <- mutate(c101112, 
                  score_f_norm101112 = (score_f - mean(score_f)) / 
                    sd(score_f),
                  
                  ecrire_norm101112 = (ecrire - mean(ecrire)) / 
                    sd(ecrire),
                  
                  grammaire_norm101112 = (grammaire - mean(grammaire)) / 
                    sd(grammaire),
                  
                  lire_norm101112 = (lire - mean(lire)) / 
                    sd(lire),
            
                  ortho_norm101112 = (ortho - mean(ortho)) / 
                    sd(ortho),
            
                  voca_norm101112 = (voca - mean(voca)) / 
                    sd(voca))

# Récupération option_2nde ------------------------------------------------

load("D:/00_phd/02_rectorat/02_v2_construction_def/01_construction/04_pre_analyse_constat1316.rda")
  # NB : on se limite aux filières générales et pro. 
d <- left_join(d, 
                select(constat1316, cohorte_mod, ine_mod,
                       libmef, 
                       option_annee_suivante = lmefstat4, # non modifié. 
                       ),
                by = c("session_mod" = "cohorte_mod",
                       "ine_mod")) %>% 
  mutate(option_annee_suivante = str_replace(option_annee_suivante,
                                             "3EME GENE", "3EME"))
  # nobs ok. 
  # 1CAP2 : 1ère année d'une CAP de 2 ans : 4224 obs. 
  # 1ERE G-T : 1 obs.
  # 3EME et 3EME GENE (redoublants) : 521. 
  # MGI : 6 personnes. 
  # NA : 2385 (vraisemblablement alétoire, à vérifier).

# Sauvegarde --------------------------------------------------------------

save(c, 
     c101112,
     rdc, rdcdi,
     cd, cdi,
     d, d_noclniv,
     file = here("01_donnees_def.rda"), version = 2)
