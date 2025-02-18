# agefrd_sexe, exceptionnellement sans préparation. 

library(here)
library(tidyverse)
library(plm)
library(lmtest)
library(AER)
library(ivpack)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions.rda")
load(here("01_donnees.rda"))

# Régressions -------------------------------------------------------------

agefrd_rp_sexe_h30 <- ivreg(score_rp ~ 
                           sexe_F : (age_abs + dist + old : dist 
                         + pcs_g2_Moy + pcs_g2_Fav + pcs_g2_Tresfav
                         + pcs_g2_Autres + pcs_g2_NA)
                         + sexe_M : (age_abs + dist + old : dist 
                                     + pcs_g2_Moy + pcs_g2_Fav + pcs_g2_Tresfav
                                     + pcs_g2_Autres + pcs_g2_NA)
                         + sexe_M
                         |
                           sexe_F : (old + dist + old : dist 
                                     + pcs_g2_Moy + pcs_g2_Fav + pcs_g2_Tresfav
                                     + pcs_g2_Autres + pcs_g2_NA)
                         + sexe_M : (old + dist + old : dist 
                                     + pcs_g2_Moy + pcs_g2_Fav + pcs_g2_Tresfav
                                     + pcs_g2_Autres + pcs_g2_NA)
                         + sexe_M, 
                         data = hrestr(rdc, 30))

agefrd_rp_p2_sexe_h30 <- ivreg(score_rp ~ 
                           sexe_F : (age_abs + dist + old : dist 
                                     + I(dist ^ 2) + old : I(dist ^ 2) 
                                     + pcs_g2_Moy + pcs_g2_Fav + pcs_g2_Tresfav
                                     + pcs_g2_Autres + pcs_g2_NA)
                         + sexe_M : (age_abs + dist + old : dist  
                                     + I(dist ^ 2) + old : I(dist ^ 2) 
                                     + pcs_g2_Moy + pcs_g2_Fav + pcs_g2_Tresfav
                                     + pcs_g2_Autres + pcs_g2_NA)
                         + sexe_M
                         |
                           sexe_F : (old + dist + old : dist  
                                     + I(dist ^ 2) + old : I(dist ^ 2) 
                                     + pcs_g2_Moy + pcs_g2_Fav + pcs_g2_Tresfav
                                     + pcs_g2_Autres + pcs_g2_NA)
                         + sexe_M : (old + dist + old : dist  
                                     + I(dist ^ 2) + old : I(dist ^ 2) 
                                     + pcs_g2_Moy + pcs_g2_Fav + pcs_g2_Tresfav
                                     + pcs_g2_Autres + pcs_g2_NA)
                         + sexe_M, 
                         data = hrestr(rdc, 30))

for (i in c("score_f_rp", "ecrire_rp", "grammaire_rp", "lire_rp", "ortho_rp", "voca_rp",
            "score_m_rp", "calcul_rp", "geometrie_rp", "grand_mes_rp", "nombre_rp", 
            "org_donnee_rp")) {
  assign(paste("agefrd_rp_sexe_", 
               i %>%   str_replace("score_f", "french") %>% 
                 str_replace("score_m", "maths"), "_h30", sep = ""),
         update(agefrd_rp_sexe_h30, as.formula(paste(i, " ~ .", sep = ""))))
  assign(paste("agefrd_rp_p2_sexe_", 
               i %>%   str_replace("score_f", "french") %>% 
                 str_replace("score_m", "maths"), "_h30", sep = ""),
         update(agefrd_rp_p2_sexe_h30, as.formula(paste(i, " ~ .", sep = ""))))
}

# Sur rdcdi ---------------------------------------------------------------

agefrd_rp_sexe_h30_rdcdi <- update(agefrd_rp_sexe_h30, moy_ec1_rp ~ .,
                              data = hrestr(rdcdi, 30))

agefrd_rp_p2_sexe_h30_rdcdi <- update(agefrd_rp_p2_sexe_h30, moy_ec1_rp ~ .,
                              data = hrestr(rdcdi, 30))

for (i in c("moy_fran_ec_rp", "moy_maths_ec_rp")) {
  assign(paste("agefrd_rp_sexe_", 
               i, "_h30_rdcdi", sep = ""),
         update(agefrd_rp_sexe_h30_rdcdi,
                as.formula(paste(i, " ~ .", sep = ""))))
  assign(paste("agefrd_rp_p2_sexe_", 
               i, "_h30_rdcdi", sep = ""),
         update(agefrd_rp_p2_sexe_h30_rdcdi,
                as.formula(paste(i, " ~ .", sep = ""))))
}

# Inférence et narsq ------------------------------------------------------

for (i in ls()[str_detect(ls(), "^agefrd")]) {
  assign(paste("ct.", i, sep = ""),
         robust.se(get(i)))
  assign(paste("n.", i, sep = ""), nobs(get(i)))
  assign(paste("arsq.", i, sep = ""), ext_adjrsq(get(i)))
}

# Sauvegarde --------------------------------------------------------------

save(list = ls()[str_detect(ls(), "^ct\\.|^n\\.|^arsq\\.|^mvc\\.")], 
     file = here("agefrd_rp", "agefrd_rp_sexe.rda"), version = 2)
