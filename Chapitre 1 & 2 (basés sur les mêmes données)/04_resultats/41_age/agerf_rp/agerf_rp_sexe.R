# agerf_sexe

library(here)
library(tidyverse)
library(plm)
library(lmtest)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions.rda")
load(here("01_donnees.rda"))

# Préparation -------------------------------------------------------------

depvar <- "score_rp"
depvar_cdi <- "moy_ec1_rp"

z_sexe <- paste("z", c("sexe_F", "sexe_M",
                       
                       "pcs_g2_Defav", "pcs_g2_Moy", "pcs_g2_Fav",
                       "pcs_g2_Tresfav", "pcs_g2_Autres", "pcs_g2_NA",
                       "cohorte_2009", "cohorte_2010", "cohorte_2011",
                       "cohorte_2012"),
                sep = " : ")

z_sexe_cdi <- paste("z", c("sexe_F", "sexe_M",
                           
                           "pcs_g2_Defav", "pcs_g2_Moy", "pcs_g2_Fav",
                           "pcs_g2_Tresfav", "pcs_g2_Autres", "pcs_g2_NA",
                           "cohorte_2010", "cohorte_2011",
                           "cohorte_2012"),
                    sep = " : ")


cont <- c("sexe_M", "pcs_g2_Moy", "pcs_g2_Fav", 
          "pcs_g2_Tresfav", "pcs_g2_Autres", 
          "pcs_g2_NA")

coh <- c("cohorte_2010", "cohorte_2011", "cohorte_2012")
coh_cdi <- c("cohorte_2011", "cohorte_2012")

# Régressions -------------------------------------------------------------

agerf_rp_sexe <- plm(as.formula(paste(depvar, " ~ ",
                                   z_sexe %>% 
                                     paste(collapse = " + "), " + ",
                                   
                                   cont %>% 
                                     paste(collapse = " + "), " + ",
                                   coh %>% 
                                     paste(collapse = " + "))),
                  c, index = "ecolecoh", model = "pooling")

for (i in c("score_f_rp", "ecrire_rp", "grammaire_rp", "lire_rp", "ortho_rp", 
            "voca_rp",
            "score_m_rp", "calcul_rp", "geometrie_rp", "grand_mes_rp", 
            "nombre_rp", 
            "org_donnee_rp")) assign(
              paste("agerf_rp_sexe_",
                    i %>% str_replace("score_f", "french") %>% 
                      str_replace("score_m", "maths"),
                    sep = ""),
              update(agerf_rp_sexe, as.formula(paste(i, " ~ .", sep = "")))
            )

# Sur cdi -----------------------------------------------------------------

agerf_rp_sexe_cdi <- plm(as.formula(paste(depvar_cdi, " ~ ",
                                   z_sexe_cdi %>% 
                                     paste(collapse = " + "), " + ",
                                   
                                   cont %>% 
                                     paste(collapse = " + "), " + ",
                                   coh_cdi %>% 
                                     paste(collapse = " + "))),
                  cdi, index = "ecolecoh", model = "pooling")

for (i in c("moy_fran_ec_rp", "moy_maths_ec_rp")) assign(
              paste("agerf_rp_sexe_", i, "_cdi",
                    sep = ""),
              update(agerf_rp_sexe_cdi,
                     as.formula(paste(i, " ~ .", sep = "")))
            )

# Inférence et narsq ------------------------------------------------------

for (i in ls()[str_detect(ls(), "^agerf")]) {
  assign(paste("ct.", i, sep = ""),
         rsearellano(get(i)))
  assign(paste("n.", i, sep = ""), nobs(get(i)))
  assign(paste("arsq.", i, sep = ""), ext_adjrsq(get(i)))
}

# Sauvegarde --------------------------------------------------------------

save(list = ls()[str_detect(ls(), "^ct\\.|^n\\.|^arsq\\.|^mvc\\.")], 
     file = here("agerf_rp", "agerf_rp_sexe.rda"), version = 2)
