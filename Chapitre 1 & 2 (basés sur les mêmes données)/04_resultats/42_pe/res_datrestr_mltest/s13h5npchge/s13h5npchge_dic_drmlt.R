# s13h5npchge_o1_dic_drmlt, moy_dic_ec. 

library(here)
library(tidyverse)
library(plm)
library(lmtest)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions.rda")
load(here("res_datrestr_mltest", "01_datrestr_mltest.rda"))

# sA ----------------------------------------------------------------------

s13h5npchge_sA_dic_drmlt <- plm(moy_dic_ec ~ 
                               pnp_score : p : q5score_tot
                             + pop_score : p : q5score_tot
                             + v_ob_tot + p, 
                             datrestr_mltest, subset = obs == 1, index = "rneconstatses")

# sB ----------------------------------------------------------------------

s13h5npchge_sB_dic_drmlt <- update(s13h5npchge_sA_dic_drmlt, . ~ . 
                                + sexe_mod_M + pcs_reg_mod_Moy + pcs_reg_mod_Fav + pcs_reg_mod_Tresfav + pcs_reg_mod_Autres
                                + lregime_constat_g_int + lregime_constat_g_ext + age_absdnb + positiondnb2_Heure + positiondnb2_Avance + chge)

# sC ----------------------------------------------------------------------

s13h5npchge_sC_dic_drmlt <- update(s13h5npchge_sB_dic_drmlt, . ~ . + tdivconstatrneses)

# sD ----------------------------------------------------------------------

s13h5npchge_sD_dic_drmlt <- update(s13h5npchge_sC_dic_drmlt, . ~ . 
                                + pnp_sexe_mod_M + pnp_pcs_reg_mod_Moy + pnp_pcs_reg_mod_Fav + pnp_pcs_reg_mod_Tresfav + pnp_pcs_reg_mod_Autres
                                + pnp_lregime_constat_g_int + pnp_lregime_constat_g_ext + pnp_age_absdnb + pnp_positiondnb2_Heure + pnp_positiondnb2_Avance)

# sE ----------------------------------------------------------------------

s13h5npchge_sE_dic_drmlt <- update(s13h5npchge_sD_dic_drmlt, . ~ . 
                                + pop_sexe_mod_M + pop_pcs_reg_mod_Moy + pop_pcs_reg_mod_Fav + pop_pcs_reg_mod_Tresfav + pop_pcs_reg_mod_Autres
                                + pop_lregime_constat_g_int + pop_lregime_constat_g_ext + pop_age_absdnb + pop_positiondnb2_Heure + pop_positiondnb2_Avance)

# sF ----------------------------------------------------------------------

s13h5npchge_sF_dic_drmlt <- update(s13h5npchge_sC_dic_drmlt, . ~ . 
                                   + p_sexe_mod_M + p_pcs_reg_mod_Moy + p_pcs_reg_mod_Fav + p_pcs_reg_mod_Tresfav + p_pcs_reg_mod_Autres
                                   + p_lregime_constat_g_int + p_lregime_constat_g_ext + p_age_absdnb + p_positiondnb2_Heure + p_positiondnb2_Avance)

# Inférence et narsq ------------------------------------------------------

for (i in ls()[str_detect(ls(), "^s13")]) {
  assign(paste("ct.", i, sep = ""),
         rsearellano(get(i)))
  assign(paste("n.", i, sep = ""), nobs(get(i)))
  assign(paste("arsq.", i, sep = ""), summary(get(i))$r.squared["adjrsq"])
}

# Sauvegarde --------------------------------------------------------------

save(list = ls()[str_detect(ls(), "^ct\\.|^n\\.|^arsq\\.|^mvc\\.")], 
     file = here("res_datrestr_mltest", "s13h5npchge", 
                 "s13h5npchge_dic_drmlt.rda"), version = 2)