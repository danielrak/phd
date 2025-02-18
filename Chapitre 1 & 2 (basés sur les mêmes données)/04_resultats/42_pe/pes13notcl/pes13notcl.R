# pes13notcl. 
  # 2022_01_11. 

library(here)
library(tidyverse)
library(plm)
library(lmtest)

load(here("datrestr_2_def", "datrestr_2_def.rda"))

  # Concession nom. 

pes13 <- plm(moy_ec1_norm ~ 
               p_score_norm : p + p
             + score_norm 
             + sexe_mod + pcs_reg_mod
             + lregime_constat_g
             + p_sexe_mod_M + p_pcs_reg_mod_Moy
             + p_pcs_reg_mod_Fav + p_pcs_reg_mod_Tresfav
             + p_pcs_reg_mod_Autres + p_lregime_constat_g_int
            + p_lregime_constat_g_ext, 
            datrestr_2, index = "rneconstatses")
  
pes13notcl <- plm(moy_ec1_norm ~ 
                    p_score_norm : p + p 
                  + score_norm
                  + sexe_mod + pcs_reg_mod
                  + age_absdnb + positiondnb2
                  + lregime_constat_g
                  + p_sexe_mod_M + p_pcs_reg_mod_Moy
                  + p_pcs_reg_mod_Fav + p_pcs_reg_mod_Tresfav
                  + p_pcs_reg_mod_Autres + p_age_absdnb 
                  + p_positiondnb2_Heure + p_positiondnb2_Avance
                  + p_lregime_constat_g_int
                  + p_lregime_constat_g_ext,
                  datrestr_2, index = "rneconstatses")

pes13noagepos <- plm(moy_ec1_norm ~ 
                    p_score_norm : p + p 
                  + score_norm
                  + sexe_mod + pcs_reg_mod
                  + lregime_constat_g
                  + p_sexe_mod_M + p_pcs_reg_mod_Moy
                  + p_pcs_reg_mod_Fav + p_pcs_reg_mod_Tresfav
                  + p_pcs_reg_mod_Autres
                  + p_lregime_constat_g_int
                  + p_lregime_constat_g_ext
                  + tdivconstatrneses,
                  datrestr_2, index = "rneconstatses")

pes13sD <- plm(moy_ec1_norm ~ 
                    p_score_norm : p + p 
                  + score_norm
                  + sexe_mod + pcs_reg_mod
                  + age_absdnb + positiondnb2
                  + lregime_constat_g
                  + p_sexe_mod_M + p_pcs_reg_mod_Moy
                  + p_pcs_reg_mod_Fav + p_pcs_reg_mod_Tresfav
                  + p_pcs_reg_mod_Autres + p_age_absdnb 
                  + p_positiondnb2_Heure + p_positiondnb2_Avance
                  + p_lregime_constat_g_int
                  + p_lregime_constat_g_ext
               + tdivconstatrneses,
                  datrestr_2, index = "rneconstatses")

save(pes13, pes13sD, pes13notcl, pes13noagepos,
     file = here("pes13notcl", "pes13notcl.rda"),
     version = 2)
