# s13chgeinst_specs. 

sA_chgeinstpik <- "p + chge + session_mod"

sB_chgeinstpik <- c(sA_chgeinstpik, "sexe_mod_M",
                    "pcs_reg_mod_Moy", "pcs_reg_mod_Fav", "pcs_reg_mod_Tresfav",
                    "pcs_reg_mod_Autres",
                    "lregime_constat_g_int", "lregime_constat_g_ext",
                    "age_absdnb", 
                    "positiondnb2_Heure", "positiondnb2_Avance")

sC_chgeinstpik <- c(sB_chgeinstpik, "tdivconstatrneses | . - tdivconstatrneses + instpik")

sD_chgeinstpik <- c("p_sexe_mod_M", 
                    "p_pcs_reg_mod_Moy", "p_pcs_reg_mod_Fav", 
                    "p_pcs_reg_mod_Tresfav", "p_pcs_reg_mod_Autres",
                    "p_lregime_constat_g_int", "p_lregime_constat_g_ext",
                    "p_age_absdnb", 
                    "p_positiondnb2_Heure", "p_positiondnb2_Avance", sC_chgeinstpik)