# agesrd_pcs, h30 par défaut, exceptionnellement sans préparation. 

library(here)
library(tidyverse)
library(plm)
library(lmtest)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions.rda")
load(here("01_donnees.rda"))

# Régressions -------------------------------------------------------------

# attention : baisé à cause des notes non comparables et des avances
# à droite. 
agesrd_rp_pcs_h30 <- lm(score_rp ~ 
                       pcs_g2_Defav : (old + dist + old : dist + sexe_M)
                     + pcs_g2_Moy : (old + dist + old : dist + sexe_M)
                     + pcs_g2_Fav : (old + dist + old : dist + sexe_M)
                     + pcs_g2_Tresfav : (old + dist + old : dist + sexe_M)
                     + pcs_g2_Autres : (old + dist + old : dist + sexe_M)
                     + pcs_g2_NA : (old + dist + old : dist + sexe_M)
                     + pcs_g2_Moy + pcs_g2_Fav
                     + pcs_g2_Tresfav + pcs_g2_Autres 
                     + pcs_g2_NA,
                     hrestr(rdc, 30))
  # vérifié : correspond estimations sample split. 

agesrd_rp_p2_pcs_h30 <- lm(score_rp ~ 
                          pcs_g2_Defav : (old + dist + I(dist ^ 2)
                                          + old : dist + old : I(dist ^ 2) + sexe_M)
                        + pcs_g2_Moy : (old + dist + I(dist ^ 2)
                                        + old : dist + old : I(dist ^ 2) + sexe_M)
                        + pcs_g2_Fav : (old + dist + I(dist ^ 2)
                                        + old : dist + old : I(dist ^ 2) + sexe_M)
                        + pcs_g2_Tresfav : (old + dist + I(dist ^ 2)
                                            + old : dist + old : I(dist ^ 2) + sexe_M)
                        + pcs_g2_Autres : (old + dist + I(dist ^ 2)
                                           + old : dist + old : I(dist ^ 2) + sexe_M)
                        + pcs_g2_NA : (old + dist + I(dist ^ 2)
                                       + old : dist + old : I(dist ^ 2) + sexe_M)
                        + pcs_g2_Moy + pcs_g2_Fav
                        + pcs_g2_Tresfav + pcs_g2_Autres 
                        + pcs_g2_NA,
                        hrestr(rdc, 30))
  # vérifié : correspond estimations sample split. 

for (i in c("score_f_rp", "ecrire_rp", "grammaire_rp", "lire_rp", "ortho_rp",
            "voca_rp",
            "score_m_rp", "calcul_rp", "geometrie_rp", "grand_mes_rp", 
            "nombre_rp", 
            "org_donnee_rp")) {
  assign(paste("agesrd_rp_pcs_", 
               i %>%   str_replace("score_f", "french") %>% 
                 str_replace("score_m", "maths"), "_h30", sep = ""),
         update(agesrd_rp_pcs_h30, as.formula(paste(i, " ~ .", sep = ""))))
  assign(paste("agesrd_rp_p2_pcs_", 
               i %>%   str_replace("score_f", "french") %>% 
                 str_replace("score_m", "maths"), "_h30", sep = ""),
         update(agesrd_rp_p2_pcs_h30, as.formula(paste(i, " ~ .", sep = ""))))
}

# Sur rdcdi ---------------------------------------------------------------

agesrd_rp_pcs_h30_rdcdi <- update(agesrd_rp_pcs_h30, moy_ec1_rp ~ .,
                             data = hrestr(rdcdi, 30))

agesrd_rp_p2_pcs_h30_rdcdi <- update(agesrd_rp_p2_pcs_h30, moy_ec1_rp ~ .,
                                data = hrestr(rdcdi, 30))

for (i in c("moy_fran_ec_rp", "moy_maths_ec_rp")) {
  assign(paste("agesrd_rp_pcs_", 
               i, "_h30_rdcdi", sep = ""),
         update(agesrd_rp_pcs_h30_rdcdi,
                as.formula(paste(i, " ~ .", sep = ""))))
  assign(paste("agesrd_rp_p2_pcs_", 
               i, "_h30_rdcdi", sep = ""),
         update(agesrd_rp_p2_pcs_h30_rdcdi,
                as.formula(paste(i, " ~ .", sep = ""))))
}

# Inférence et narsq ------------------------------------------------------

for (i in ls()[str_detect(ls(), "^agesrd")]) {
  assign(paste("ct.", i, sep = ""),
         rsearellano(get(i)))
  assign(paste("n.", i, sep = ""), nobs(get(i)))
  assign(paste("arsq.", i, sep = ""), ext_adjrsq(get(i)))
}

# Sauvegarde --------------------------------------------------------------

save(list = ls()[str_detect(ls(), "^ct\\.|^n\\.|^arsq\\.|^mvc\\.")], 
     file = here("agesrd_rp", "agesrd_rp_pcs.rda"), version = 2)