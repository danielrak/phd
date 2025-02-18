# pespecs. 
# Essentiellement inspiré de s13_specs. 
# Il faut juste différencier par modèle. 

# ols ---------------------------------------------------------------------

sA_ols <- NULL

sB_ols <- c("sexe_mod", "pcs_reg_mod",
            "lregime_constat_g", "age_absdnb", 
            "positiondnb2") %>% 
  paste(collapse = " + ")

sC_ols <- c(sB_ols, "tdivconstatrneses") %>% 
  paste(collapse = " + ")

sD_ols <- c(sC_ols, 
            "p_sexe_mod_M", 
            "p_pcs_reg_mod_Moy", "p_pcs_reg_mod_Fav", 
            "p_pcs_reg_mod_Tresfav", "p_pcs_reg_mod_Autres",
            "p_lregime_constat_g_int", "p_lregime_constat_g_ext",
            "p_age_absdnb", 
            "p_positiondnb2_Heure", "p_positiondnb2_Avance") %>% 
  paste(collapse = " + ")

  # sans variables potentiellement endogènes. 
  # âge, position, taille de classe. 
sDexo_ols <- c(sB_ols %>% str_remove("\\+ age_absdnb \\+ positiondnb2"),
               "p_sexe_mod_M",
               "p_pcs_reg_mod_Moy", "p_pcs_reg_mod_Fav",
               "p_pcs_reg_mod_Tresfav", "p_pcs_reg_mod_Autres",
               "p_lregime_constat_g_int", 
               "p_lregime_constat_g_ext") %>% 
  paste(collapse = " + ")

# s13 ---------------------------------------------------------------------

sA_s13 <- "p"

sB_s13 <- c(sA_s13,
            "sexe_mod", "pcs_reg_mod",
            "lregime_constat_g", "age_absdnb", 
            "positiondnb2") %>% 
  paste(collapse = " + ")

sC_s13 <- c(sB_s13, "tdivconstatrneses") %>% 
  paste(collapse = " + ")

sD_s13 <- c(sC_s13, 
            "p_sexe_mod_M", 
            "p_pcs_reg_mod_Moy", "p_pcs_reg_mod_Fav", 
            "p_pcs_reg_mod_Tresfav", "p_pcs_reg_mod_Autres",
            "p_lregime_constat_g_int", "p_lregime_constat_g_ext",
            "p_age_absdnb", 
            "p_positiondnb2_Heure", "p_positiondnb2_Avance") %>% 
  paste(collapse = " + ")

  # exo : sans variables potentiellement endogènes. 
sDexo_s13 <- c(sB_s13 %>% str_remove("\\+ age_absdnb \\+ positiondnb2"),
               "p_sexe_mod_M",
               "p_pcs_reg_mod_Moy", "p_pcs_reg_mod_Fav",
               "p_pcs_reg_mod_Tresfav", "p_pcs_reg_mod_Autres",
               "p_lregime_constat_g_int", 
               "p_lregime_constat_g_ext") %>% 
  paste(collapse = " + ")


