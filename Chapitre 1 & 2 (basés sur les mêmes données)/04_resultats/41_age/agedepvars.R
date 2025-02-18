# agedepvars. 
  # Synthétise les variables dépendantes. 
  # À sourcer. 
  
# Sur c -------------------------------------------------------------------

agedepvars_c <- c("score", "score_f", "score_m",
                "ecrire", "lire", "grammaire", 
                
                "ortho", "voca",
                "calcul", "geometrie", "grand_mes", 
                "nombre", "org_donnee") %>% 
  (function (x) c(x,
                  paste(x, "_norm", sep = ""),
                  paste(x, "_rp", sep = "")))
  # 2021_11_29. 
agedepvars_scores_f_norm0912_c <- c("score_f_norm0912",
                    "ecrire_norm0912",
                    "grammaire_norm0912",
                    "lire_norm0912",
                    "ortho_norm0912",
                    "voca_norm0912")

agedepvars_scores_f_norm101112_c <- c("score_f_norm101112",
                    "ecrire_norm101112",
                    "grammaire_norm101112",
                    "lire_norm101112",
                    "ortho_norm101112",
                    "voca_norm101112")

# Sur cdi -----------------------------------------------------------------

agedepvars_cdi <- c("moy_ec1", "moy_cc",
                    "moy_fran_ec", "moy_fran_cc",
                    "moy_maths_ec", "moy_maths_cc",
                    "moy_hist_ec", 
                    "moy_dic_ec",
                    "moy_red_ec") %>% 
  (function (x) c(x,
                  paste(x, "_norm", sep = ""),
                  paste(x, "_rp", sep = "")))

# Sur cdi_w_scores_cm2 ----------------------------------------------------

agedepvars_c_bases <- c("score", "score_f", "score_m",
                        
                        "score_norm", "score_f_norm", 
                        "score_m_norm",
                        
                        "score_rp", "score_f_rp", 
                        "score_m_rp")

agedepvars_cdi_w_scores_cm2_1 <- 
  
  agedepvars_cdi %>% (function (a) 
    a[! str_detect(a, "_norm") & 
        ! str_detect(a, "_rp")] %>% 
  sapply(function (x) 
    paste(x, agedepvars_c_bases %>% 
            (function (a2) a2[! str_detect(a2, "_norm") & 
                                ! str_detect(a2, "_rp")]),
          sep = "_AND_")) %>% 
  as.vector)

agedepvars_cdi_w_scores_cm2_2 <- 
  
  agedepvars_cdi %>% (function (a) 
    a[str_detect(a, "_norm")] %>% 
      sapply(function (x) 
        paste(x, agedepvars_c_bases %>% 
                (function (a2) a2[str_detect(a2, "_norm")]),
              sep = "_AND_")) %>% 
      as.vector)

agedepvars_cdi_w_scores_cm2_3 <- 
  
  agedepvars_cdi %>% (function (a) 
    a[str_detect(a, "_rp")] %>% 
      sapply(function (x) 
        paste(x, agedepvars_c_bases %>% 
                (function (a2) a2[str_detect(a2, "_rp")]),
              sep = "_AND_")) %>% 
      as.vector)

agedepvars_cdi_w_scores_cm2 <- 
  
  c(agedepvars_cdi_w_scores_cm2_1,
    agedepvars_cdi_w_scores_cm2_2,
    agedepvars_cdi_w_scores_cm2_3)

rm(agedepvars_cdi_w_scores_cm2_1,
   agedepvars_cdi_w_scores_cm2_2,
   agedepvars_cdi_w_scores_cm2_3)
