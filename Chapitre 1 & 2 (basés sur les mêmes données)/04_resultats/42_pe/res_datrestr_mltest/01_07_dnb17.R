# Construction dnb17. def. 
# 2021_10_26. Construction optimisée. 
# But : empiler avec une partie des variables de dnb pour les effets endogènes. 
# Pas besoin des variables de CM2 pour cela. 
# Attention à la comparabilité des notes. Utiliser uniquement norm ou rp. 

# Packages ----------------------------------------------------------------

library(tidyverse)
library(fastDummies)
library(here)
library(lubridate)

# Chargement --------------------------------------------------------------

load("D:/00_phd/02_rectorat/02_v2_construction_def/00_donnees_originales/00_donnees_originales.rda")
rm(list = ls()[! str_detect(ls(), "dnb17")])

load("D:/00_phd/02_rectorat/02_v2_construction_def/01_construction/04_pre_analyse_constat1316.rda")

  # Obs de départ : 14595. 

# constat16 ---------------------------------------------------------------
  # Rappel : cohorte ==  2016 mais session_mod == 2017. 
constat16 <- filter(constat1316, cohorte_mod == "2016")

# Filtre : sans l'Ine dupliqué mais le second a des notes de 0 ------------
  # Les observations diffèrent grâce à IDCandidat. 
  
dnb17 <- filter(dnb17, ! is.na(Ine)) %>% 
  filter(duplicated(Ine)) %>% pull(Ine) %>% 
  (function (i) filter(dnb17, Ine == i & Total_Ecrit == 0)) %>%
  pull(IDCandidat) %>% 
  (function (i2) filter(dnb17, IDCandidat != i2))
  # nobs : 14594. 
  # Il n'y a pas d'Ine dupliqué à part cela. 

# Récupération variables via constat --------------------------------------

  # Par constat16 uniquement d'abord. 
  # age_absdnb de 2017 ?
    # Date : 29 et 30 juin uniquement : je prends le 30 juin. 
    # Source : https://www.ac-reunion.fr/fileadmin/ANNEXES-ACADEMIQUES/01-SERVICES-ACADEMIQUES/service-communication/01-actualites-academiques/2016-2017-actus/2017-06-actus/DNB/DP-DNB-2017.pdf
dnb17 <- left_join(dnb17, 
                   transmute(constat16, ine_mod, 
                             rneconstatses,
                             statut_constat = statut_mod, 
                             sigle_constat = sigle_mod, 
                             clefgestion_constat = clefgestion_mod,
                             libmef_constat = libmef_mod,
                             niveauconstat,
                             divconstatrneses, 
                             lmefstat4_constat = lmefstat4_mod,
                             sexe_mod, 
                             # pour pcs : j'essaie de faire comme dans dat
                             # voir 01_01_donnees_def, Facilitaion labels pcs. 
                             pcs = substr(pcsconstat_corr, 1, 1), 
                             pcs_reg_mod = pcs_g2constat, 
                             dnais_constat = dnais_mod,
                             age_absdnb = int_length(
                               interval(dnais_constat, 
                                        as.Date("2017-06-30"))
                             ) / (365.25 * 3600 * 24),
                             positiondnb2 = positionconstat,
                             positiondnb2_num = positionconstat_num,
                             lbourses_constat = lbourses_mod,
                             lregime_constat = lregime_mod,
                             lregime_constat_g = lregime_mod_g
                             ),
                   by = c("Ine" = "ine_mod"))
  # nobs ok : 14594. 
  
  # Par les années précédentes : pour les variables individuelles uniquement. 
  # Ces variables sont : sexe, pcs, dnais et ses conséquences. 

dnb17$Ine[! dnb17$Ine %in% constat16$ine_mod] %>% na.omit %>% 
  (function (i)
  filter(constat1316, cohorte_mod != "2016" & 
           ine_mod %in% i) %>% 
  transmute(cohorte_mod, ine_mod,
            sexe_mod, 
            pcs = substr(pcsconstat_corr, 1, 1),
            pcs_reg_mod = pcs_g2constat,
            dnais_constat = dnais_mod,
            age_absdnb = int_length(
                               interval(dnais_constat, 
                                        as.Date("2017-06-30"))
                             ) / (365.25 * 3600 * 24),
            positiondnb2 = positionconstat,
            positiondnb2_num = positionconstat_num) %>% 
  # 145 valeurs uniques de ine_mod jusque-là. 
  # Puis je prends la dernière cohorte uniquement : 
  group_by(ine_mod) %>% 
  filter(cohorte_mod == max(as.numeric(cohorte_mod))) %>% 
  ungroup %>% select(- cohorte_mod) %>% 
    (function (d) dnb17 %>% (function (d2)
      # Je divise dnb17 en deux : une partie avec les infos récupérées
        # via constat16 et une autre sans. 
      rbind(filter(d2, ! Ine %in% i | is.na(Ine)),
            filter(d2, Ine %in% i) %>% select(- sexe_mod, 
                                              - pcs, 
                                              - pcs_reg_mod,
                                              - dnais_constat,
                                              - age_absdnb,
                                              - positiondnb2,
                                              - positiondnb2_num) %>% 
              left_join(d, by = c("Ine" = "ine_mod")) %>% 
              select(names(d2)))))) -> dnb17
  # nobs ok : 14594. 
  # Et on vérifie bien que les NA des variables individuelles
  # ci-dessus sont de 125 seulement au lieu de 270. 

# z_dnb -------------------------------------------------------------------

dnb17 <- mutate(dnb17, 
                z_dnb = (int_length(interval(
                  dnais_constat, 
                  as.Date(paste(year(dnais_constat), 
                                "-12-31", sep = ""))) / 365.25)) / 
                  (3600 * 24))

# Facilitation labels pcs -------------------------------------------------

dnb17 <- mutate(dnb17,
                pcs = factor(pcs) %>% 
                  factor(labels = c("Agr", "Art",
                                    "Cadr", "Prof",
                                    "Emp", "Ouvr",
                                    "Retr", "Inact",
                                    "Autres")))
  # ok : proportions vérifiées. 

# ATTENTION :  ------------------------------------------------------------

  # Il reste des valeurs manquantes dans toutes les variables récupérées.
  # Et filter de dplyr avec "!" enlève les NA (pas idéal). 
  # Décision du 2021_10_27 : en fait on peut les enlever car
  # ils ne serviront à rien dans les effets de pairs puisqu'
  # on n'a pas la taille de classe ni d'autres covars, on a juste les notes. 
  # Ceux is.na(Ine) sont légèrement plus faibles. 

dnb17 <- filter(dnb17, ! is.na(Ine))
  #ok. nobs = 14531. 

# droplevels --------------------------------------------------------------

dnb17 <- mutate_if(dnb17, is.factor, droplevels)

# Filtre : sans lycée privé.  ---------------------------------------------
  # 16 personnes. 
  
dnb17 <- filter(dnb17, 
                sigle_constat != "LP PR" | is.na(sigle_constat))
  # ok. nobs = 14515. 

# Filtre : sans bourse nationale : INUTILE. -------------------------------

  # Il n'y a plus de bénéficiaire de bourse nationale. 

# Dummy pcs ---------------------------------------------------------------

dnb17 <- dummy_cols(dnb17, "pcs")  

# Plus de duplicatas INE --------------------------------------------------


# Sélection de variables : pour empilement --------------------------------


# Les matières au DNB17 ---------------------------------------------------

mat_dnb17 <- c("MathsSciences", "Mathématiques",
               "Sciences", "Sciences_Partie1",
               "Sciences_Partie2", "Francais_HG_EMC", "HG_EMC",
               "Fran_analyse",
               "Dictee",
               "Réécriture",
               "Travail_Ecriture", "Français", "Total_Ecrit",
               "LeSocle",
               "Projet",
               "Socle1", "Socle2", "Socle3", "Socle4",
               "Socle5", "Socle6", "Socle7", "Socle8"
               )

# On fait quand même 
# mais on s'assurera bien de ce que les notes veulent dire. 
# Les notes ci-dessous n'ont pas de distribution normale. 
mat_dnb17_distnon_norm <- c("Dictee",
                            "Réécriture",
                            "LeSocle",
                            "Projet",
                            "Socle1",
                            "Socle2",
                            "Socle3",
                            "Socle4",
                            "Socle5",
                            "Socle6",
                            "Socle7",
                            "Socle8")

# Rang de percentile : Notes DNB ------------------------------------------

dnb17 <- cbind(dnb17, 
               transmute_at(dnb17, mat_dnb17, 
                            function (x) percent_rank(x) * 100) %>% 
                 rename_all(function (x) paste(x, "_rp", sep = "")))

# UET : notes DNB ---------------------------------------------------------

dnb17 <- cbind(dnb17, 
               transmute_at(dnb17, mat_dnb17, 
                            function (x) 
                              (x - mean(x, na.rm = TRUE)) / 
                              sd(x, na.rm = TRUE)) %>% 
                 rename_all(function (x) paste(x, "_norm", sep = "")))

# Tailles de classe -------------------------------------------------------
  # Attention aux valeurs manquantes. 
  # Il en reste parmi les ! is.na(Ine).

dnb17 <- group_by(dnb17, divconstatrneses) %>% 
  mutate(tdivconstatrneses = sum(! is.na(divconstatrneses))) %>% 
  ungroup %>% 
  (function (d) {
    d$tdivconstatrneses[is.na(d$divconstatrneses)] <- NA
    d
  })
  # ok. 

# Exclusion : classes de taille 1 -----------------------------------------

dnb17 <- filter(dnb17, tdivconstatrneses != 1)
  # Il y en a 21. C'est des élèves minoritaires 
  # (4G, 4 SPO, DIMA, etc.). 

# Nombre de classes par école(-année) ---------------------------------------------

dnb17 <- group_by(dnb17, rneconstatses) %>% 
  mutate(ncl0 = length(unique(divconstatrneses))) %>% 
  ungroup %>% 
  (function (d) {
    d$ncl0[is.na(d$divconstatrneses)] <- NA
    d
  })

# Dummies pour p_covars ---------------------------------------------------

dnb17 <- dummy_cols(dnb17, c("sexe_mod", "pcs_reg_mod",
                             "positiondnb2", "lbourses_constat",
                             "lregime_constat", "lregime_constat_g"))

# Caractéristiques totalement observées -----------------------------------
  # Rq : le nombre de Autres est de 384 ici :
  # c'est élevé par rapport aux autres années du DNB. Revoir cela. 

dnb17 <- group_by(dnb17, divconstatrneses) %>% 
  mutate(
    # p_age_abs = (sum(age_abs, na.rm = TRUE) - age_abs) / (tdivconstatrneses - 1),
         
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
         
         # p_lbourses_constat_bourse_nationale = 
         #   (sum(lbourses_constat_bourse_nationale, na.rm = TRUE) - 
         #      lbourses_constat_bourse_nationale) / (tdivconstatrneses - 1),
         
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
         
         # p_positioncol_Redoublement = (sum(positioncol_Redoublement) - 
         #                    positioncol_Redoublement) / 
         #   (tdivconstatrneses - 1),
         # 
         # p_positioncol_Rien = (sum(positioncol_Rien) - positioncol_Rien) / 
         #   (tdivconstatrneses - 1),
         # 
         # p_positioncol_Saut = (sum(positioncol_Saut) - positioncol_Saut) / 
         # (tdivconstatrneses - 1),
         # 
         # p_positioncol_Inconnue = (sum(positioncol_Inconnue) - 
         #                             positioncol_Inconnue) / 
         #   (tdivconstatrneses - 1),
         
         # à utiliser plus tard. 
         p_z_dnb = (sum(z_dnb) - z_dnb) / (tdivconstatrneses - 1)
         
  ) %>% ungroup


# jour, quinze, mois, bim, trim -------------------------------------------

dnb17 <- mutate(dnb17, 
                
                jour_dnb = round((1 - z_dnb) * 365.25),
              
              # quinze
              quinze_dnb = relevel(factor(if_else(as.numeric(substr(dnais_constat, 9, 10)) <= 15, 1, 2) + 
                                            (month(dnais_constat)-1)*2), "24"),
              
              # mois
              mois_dnb = month(dnais_constat) %>% factor %>% relevel("12"), 
              
              # bim
              bim_dnb = if_else(month(dnais_constat) %in% c(1, 2), 1,
                                if_else(month(dnais_constat) %in% c(3, 4), 2, 
                                        if_else(month(dnais_constat) %in% c(5, 6), 3,
                                                if_else(month(dnais_constat) %in% c(7, 8), 4, 
                                                        if_else(month(dnais_constat) %in% c(9, 10), 5, 6))))) %>% 
                factor %>% relevel("6"),
              
              # trim
              trim_dnb = if_else(month(dnais_constat) %in% c(1:3), 1,
                                 if_else(month(dnais_constat) %in% c(4:6), 2,
                                         if_else(month(dnais_constat) %in% c(7:9), 3, 4))) %>% factor %>% relevel("4"))

# dummies dnais -----------------------------------------------------------

dnb17 <- dnb17 %>% 
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
                        select(-names(d)))) %>% as_tibble()

# p_dnais -----------------------------------------------------------------

dnb17 <- group_by(dnb17, divconstatrneses) %>% (function (d) {
  p_jour <-
    lapply(1:365, function (x)
      transmute(d, p = (sum(
        eval(parse(text = paste(
          "jour_dnb_", x, sep = ""
        ))) -
          eval(parse(text = paste(
            "jour_dnb_", x, sep = ""
          )))
      )) /
        tdivconstatrneses) %>% ungroup %>% select(-divconstatrneses) %>%
        setNames(paste("p_jour_dnb_", x, sep = ""))) %>% do.call(what = cbind)
  
  p_quinze <-
    lapply(1:24, function (x)
      transmute(d, p = (sum(
        eval(parse(text = paste(
          "quinze_dnb_", x, sep = ""
        ))) -
          eval(parse(text = paste(
            "quinze_dnb_", x, sep = ""
          )))
      )) /
        tdivconstatrneses) %>% ungroup %>% select(-divconstatrneses) %>%
        setNames(paste("p_quinze_dnb_", x, sep = ""))) %>% do.call(what = cbind)
  
  p_mois <-
    lapply(1:12, function (x)
      transmute(d, p = (sum(
        eval(parse(text = paste(
          "mois_dnb_", x, sep = ""
        ))) -
          eval(parse(text = paste(
            "mois_dnb_", x, sep = ""
          )))
      )) /
        tdivconstatrneses) %>% ungroup %>% select(-divconstatrneses) %>%
        setNames(paste("p_mois_dnb_", x, sep = ""))) %>% do.call(what = cbind)
  
  p_bim <-
    lapply(1:6, function (x)
      transmute(d, p = (sum(
        eval(parse(text = paste(
          "bim_dnb_", x, sep = ""
        ))) -
          eval(parse(text = paste(
            "bim_dnb_", x, sep = ""
          )))
      )) /
        tdivconstatrneses) %>% ungroup %>% select(-divconstatrneses) %>%
        setNames(paste("p_bim_dnb_", x, sep = ""))) %>% do.call(what = cbind)
  
  p_trim <-
    lapply(1:4, function (x)
      transmute(d, p = (sum(
        eval(parse(text = paste(
          "trim_dnb_", x, sep = ""
        ))) -
          eval(parse(text = paste(
            "trim_dnb_", x, sep = ""
          )))
      )) /
        tdivconstatrneses) %>% ungroup %>% select(-divconstatrneses) %>%
        setNames(paste("p_trim_dnb_", x, sep = ""))) %>% do.call(what = cbind)
  
  cbind(ungroup(d), p_jour, p_quinze, p_mois, p_bim, p_trim)
  
}) %>% as_tibble

# Niveau section ----------------------------------------------------------

dnb17 <- mutate(dnb17, 
              niveau_section = 
                fct_collapse(clefgestion_constat, 
                             "sup" = c("3 INT", "3EUOR", 
                                       # "3BILN", plus de bilingue
                                       # Présentation d'un travail tepp2021, 
                                       # session éducation
                                       # Yassif (nom de l'auteur de mémoire). 
                                       "3 SPO"),
                             "normale" = "3EME", 
                             "inf" = c("3THEA", "3PPRO",
                                       # "DIMA", plus de DIMA. 
                                       # Présentation d'un travail tepp2021, 
                                       # session éducation
                                       # Yassif (nom de l'auteur de mémoire). 
                                       "3ULIS", "3SEGP"
                                       #, 
                                       # "3HIAR" plus de bilingue
                                       # Présentation d'un travail tepp2021, 
                                       # session éducation
                                       # Yassif (nom de l'auteur de mémoire). 
                                       )) %>% 
                factor(levels = c("inf", "normale", "sup")))

# cg ----------------------------------------------------------------------

  # cg : clefgestion_constat. 

dnb17 <- mutate(dnb17, cg = clefgestion_constat) %>%
  mutate(cg = str_replace(cg, " ", "") %>% str_to_lower) %>% 
  mutate(cg = factor(cg, 
                     levels = c("3uppr", "3upe2",
                                "3segp", "3ulis", "dima",
                                "3ppro", "3thea", "3eme",
                                "3spo", "3hiar", "3biln", 
                                "3euor", "3int")))

# Dummies cg --------------------------------------------------------------

  # cg : clefgestion_constat. 

dnb17 <- cbind(dnb17, dummy_cols(dnb17, "cg") %>% 
                 (function (d) 
                   select(d, names(d)[
                     str_detect(names(d), "cg_")]))) %>% as_tibble

# p_cg --------------------------------------------------------------------

dnb17 <- group_by(dnb17, divconstatrneses) %>%
  mutate(
    p_cg_3segp = (sum(cg_3segp) - cg_3segp) / (tdivconstatrneses - 1),
    p_cg_3ulis = (sum(cg_3ulis) - cg_3ulis) / (tdivconstatrneses - 1),
    p_cg_dima = (sum(cg_dima) - cg_dima) / (tdivconstatrneses - 1),
    p_cg_3ppro = (sum(cg_3ppro) - cg_3ppro) / (tdivconstatrneses - 1),
    p_cg_3thea = (sum(cg_3thea) - cg_3thea) / (tdivconstatrneses - 1),
    p_cg_3eme = (sum(cg_3eme) - cg_3eme) / (tdivconstatrneses - 1),
    p_cg_3spo = (sum(cg_3spo) - cg_3spo) / (tdivconstatrneses - 1),
    p_cg_3hiar = (sum(cg_3hiar) - cg_3hiar) / (tdivconstatrneses - 1),
    p_cg_3biln = (sum(cg_3biln) - cg_3biln) / (tdivconstatrneses - 1),
    p_cg_3euor = (sum(cg_3euor) - cg_3euor) / (tdivconstatrneses - 1),
    p_cg_3int = (sum(cg_3int) - cg_3int) / (tdivconstatrneses - 1)
  )  %>%
  ungroup

# trneconstatses ----------------------------------------------------------

dnb17 <- group_by(dnb17, rneconstatses) %>% 
  mutate(trneconstatses = n()) %>% ungroup

# tecole et tclasse (depuis constat) --------------------------------------

constat163 <- filter(constat16, niveauconstat == "3")

c <- group_by(constat163, session_mod, rne_mod) %>% 
  mutate(tecole = n()) %>% ungroup
c <- group_by(c, session_mod, rne_mod, div_mod) %>% 
  mutate(tclasse = n()) %>% ungroup
c <- filter(c, sigle != "LP PR")
dnb17 <- left_join(dnb17, 
                   select(c, session_mod, 
                          ine_mod, tecole,
                          tclasse), by = c("Ine" = "ine_mod"))
  # nobs ok. 
  
rm(c)  

# Instpiketty -------------------------------------------------------------

  # 2021_10_27 : je n'ai pas le réseau pour DNB17. 

  # Piketty et Valdenaire 2006 : tailles de classe théorique : 26 pour les REP, 28 pour les HEP et 30 pour les privés. 
  # en regardant mes moyennes : 23, 25 et 29. 
# dnb17 <- mutate(dnb17, 
#               instpik = ifelse(reseauR_mod == 1, tecole / (floor(tecole / 26) + 1),
#                                ifelse(reseauR_mod == 0 & statut_constat == "PU", tecole / (floor(tecole / 28) + 1),
#                                       tecole / (floor(tecole / 30) + 1)))) %>% 
#   (function (d) {d$instpik[is.infinite(d$instpik)] <- NA ; d})
# 
#   # Version de F. Payet. 
# dnb17 <- mutate(dnb17, 
#               instpik2 = ifelse(reseau_mod == "hep" & statut_constat == "PU", tecole / (floor(tecole / 28) + 1), 
#                                 ifelse(reseau_mod == "rep", tecole / (floor(tecole / 26) + 1),
#                                        ifelse(reseau_mod == "rep+", tecole / (floor(tecole / 24) + 1), 
#                                               tecole / (floor(tecole / 30) + 1))))) %>% 
#   (function (d) {d$instpik2[is.infinite(d$instpik2)] <- NA ; d})
# # pour les 3ème prépa pro, le seuil est de 24, peu importe. Parmi les classes contenant 3ppro, 97% sont des classes avec des prépapro uniquement. 
# dnb17 <- group_by(dnb17, divconstatrneses) %>% mutate(contenu = paste(sort(unique(cg)), collapse = " ET ")) %>% ungroup %>% 
#   mutate(instpik2 = ifelse(str_detect(contenu, "3ppro") & ! str_detect(contenu, " ET "), 24, instpik2))
# 
# 
#   # Version après analyse empirique : voir explorations_constat3_tecole.R. 
# dnb17 <- mutate(dnb17, 
#               instpik3 = ifelse(reseau_mod == "hep" & statut_constat == "PU", tecole / (floor(tecole / 28) + 1), 
#                                 ifelse(reseau_mod == "rep", tecole / (floor(tecole / 26) + 1),
#                                        ifelse(reseau_mod == "rep+", tecole / (floor(tecole / 25) + 1), 
#                                               tecole / (floor(tecole / 31) + 1))))) %>% 
#   (function (d) {d$instpik3[is.infinite(d$instpik3)] <- NA ; d})
#   # la plus mauvaise des trois (lm(tclasse ~ instpik/instpik2/instpik3))

# Filtre : sans 3upe2 et 3uppr --------------------------------------------
  # nb : le 2021-06-22-21h55. Modifie très légèrement les nombres d'observation : on enlève 3upe2 et 3uppr qui compte 3 et 2 obs. 
  # et pour lesquels on ne retrouve pas la note au CM2. 

dnb17 <- filter(dnb17, ! cg %in% c("3upe2", "3uppr")) %>% mutate(cg = droplevels(cg))

# datcompl17 --------------------------------------------------------------

datcompl17 <- dnb17
save(datcompl17, file = here("01_07_datcompl17_def.rda"))

# Filtrage : sans les classes de niveau évidentes -------------------------
  # Est-ce correct de les filtrer d'ici plutôt que depuis constat3 ?

group_by(dnb17, divconstatrneses) %>% 
  summarise(nclg = length(unique(clefgestion_constat)),
         contenu = paste(sort(unique(sort(clefgestion_constat))),
                         collapse = " ET ")) %>% 
  filter(! str_detect(contenu, "^3EME$") & 
           ! str_detect(contenu, " ET ")) -> 
  dcl_niv

  # Les classes de niveaux mélangés entre eux : ex : BILN et EURO. 
group_by(dnb17, divconstatrneses) %>% 
  summarise(nclg = length(unique(clefgestion_constat)),
            contenu = paste(sort(unique(sort(clefgestion_constat))),
                            collapse = " ET ")) %>% 
  filter(! str_detect(contenu, "3EME") & 
           str_detect(contenu, " ET ") & 
           str_detect(contenu, 
                      "3 SPO|3SEGP|DIMA|3BILN|3BILN|3EUOR|3PPRO")) -> 
  dcl_niv2
  # On remarque que ce sont les 3BILN et 3EUOR qui constituent la majorité
  # des cas. (25 classes-année). 

group_by(constat163, divconstatrneses) %>% 
  summarise(nclg = length(unique(clefgestion_mod)),
            contenu = paste(sort(unique(sort(clefgestion_mod))),
                            collapse = " ET ")) %>% 
  filter(! str_detect(contenu, "^3EME$") & 
           ! str_detect(contenu, " ET ")) -> 
  dcl_nivconstat

# Les classes de niveaux mélangés entre eux : ex : BILN et EURO. 
group_by(constat163, divconstatrneses) %>% 
  summarise(nclg = length(unique(clefgestion_mod)),
            contenu = paste(sort(unique(sort(clefgestion_mod))),
                            collapse = " ET ")) %>% 
  filter(! str_detect(contenu, "3EME") & 
           str_detect(contenu, " ET ") & 
           str_detect(contenu, 
                      "3 SPO|3SEGP|DIMA|3BILN|3BILN|3EUOR|3PPRO")) -> 
  dcl_nivconstat2 

dnb17 <- filter(dnb17, ! divconstatrneses %in% 
                c(dcl_niv$divconstatrneses,
                  dcl_niv2$divconstatrneses))
  # plus que 35979 obs, 84.26% des données. 


dnb17_noclnivconstat <- filter(dnb17, ! divconstatrneses %in% 
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

dat17 <- dnb17
save(dat17, file = here("01_07_donnees17_def.rda"), version = 2)
