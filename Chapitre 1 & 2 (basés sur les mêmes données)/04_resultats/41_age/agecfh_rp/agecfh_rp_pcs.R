# agecfh_pcs

library(here)
library(tidyverse)
library(plm)
library(lmtest)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions.rda")
load(here("01_donnees.rda"))
load(here("agepe", "agepe.rda"))

# Préparation -------------------------------------------------------------

depvar <- "score_rp"
depvar_cdi <- "moy_ec1_rp"

age_pcs <- paste("age_abs", c("pcs_g2_Defav", "pcs_g2_Moy",
                              "pcs_g2_Fav", "pcs_g2_Tresfav",
                              "pcs_g2_Autres", "pcs_g2_NA",
                              
                              "sexe_F", "sexe_M",
                              "cohorte_2009", "cohorte_2010", 
                              "cohorte_2011", "cohorte_2012"),
                 
                 sep = " : ")

age_pcs_cdi <- paste("age_abs", c("pcs_g2_Defav", "pcs_g2_Moy",
                              "pcs_g2_Fav", "pcs_g2_Tresfav",
                              "pcs_g2_Autres", "pcs_g2_NA",
                              
                              "sexe_F", "sexe_M",
                              "cohorte_2010", 
                              "cohorte_2011", "cohorte_2012"),
                 
                 sep = " : ")

resid_pcs <- paste("resid.agepe_", c("pcs_g2_Defav", "pcs_g2_Moy",
                                     "pcs_g2_Fav", "pcs_g2_Tresfav",
                                     "pcs_g2_Autres", "pcs_g2_NA"), sep = "")
resid_pcs_cdi <- paste("resid.agepe_", c("pcs_g2_Defav", "pcs_g2_Moy",
                                     "pcs_g2_Fav", "pcs_g2_Tresfav",
                                     "pcs_g2_Autres", "pcs_g2_NA"), "_cdi",
                   sep = "")

resid_pcs_age <- paste(resid_pcs, " : age_abs", sep = "")
resid_pcs_age_cdi <- paste(resid_pcs_cdi, " : age_abs", sep = "")

cont <- c("sexe_M", "pcs_g2_Moy", "pcs_g2_Fav", 
          "pcs_g2_Tresfav", "pcs_g2_Autres", 
          "pcs_g2_NA")

coh <- c("cohorte_2010", "cohorte_2011", "cohorte_2012")
coh_cdi <- c("cohorte_2011", "cohorte_2012")

# Régressions -------------------------------------------------------------

agecfh_rp_pcs <- plm(as.formula(paste(depvar, " ~ ",
                                   age_pcs %>% paste(collapse = " + "),
                                   " + ",
                                   
                                   resid_pcs %>% paste(collapse = " + "),
                                   " + ",
                                   
                                   resid_pcs_age %>% 
                                     paste(collapse = " + "), " + ",
                                   
                                   cont %>% paste(collapse = " + "), " + ",
                                   coh %>% paste(collapse = " + "))),
                  c %>% arrange(ecolecoh) %>% 
                    mutate(resid.agepe_pcs_g2_Defav = resid.agepe_pcs_g2_Defav,
                           resid.agepe_pcs_g2_Moy = resid.agepe_pcs_g2_Moy,
                           resid.agepe_pcs_g2_Fav = resid.agepe_pcs_g2_Fav,
                           
                           resid.agepe_pcs_g2_Tresfav = 
                             resid.agepe_pcs_g2_Tresfav,
                           
                           resid.agepe_pcs_g2_Autres = 
                             resid.agepe_pcs_g2_Autres,
                           
                           resid.agepe_pcs_g2_NA = 
                             resid.agepe_pcs_g2_NA,
                           ), index = "ecolecoh", model = "pooling")

for (i in c("score_f_rp", "ecrire_rp", "grammaire_rp", "lire_rp", "ortho_rp", "voca_rp",
            "score_m_rp", "calcul_rp", "geometrie_rp", "grand_mes_rp", "nombre_rp", 
            "org_donnee_rp")) assign(
              paste("agecfh_rp_pcs_", 
                    i %>% str_replace("score_f", "french") %>% 
                      str_replace("score_m", "maths"), 
                    sep = ""),
              update(agecfh_rp_pcs, as.formula(paste(i, " ~ .", sep = "")))
            )

# Sur cdi -----------------------------------------------------------------

agecfh_rp_pcs_cdi <- plm(as.formula(paste(depvar_cdi, " ~ ",
                                   age_pcs_cdi %>% paste(collapse = " + "),
                                   " + ",
                                   
                                   resid_pcs_cdi %>%
                                     paste(collapse = " + "),
                                   " + ",
                                   
                                   resid_pcs_age_cdi %>% 
                                     paste(collapse = " + "), " + ",
                                   
                                   cont %>% paste(collapse = " + "), " + ",
                                   coh_cdi %>% paste(collapse = " + "))),
                  cdi %>% arrange(ecolecoh) %>% 
                    mutate(resid.agepe_pcs_g2_Defav_cdi =
                             resid.agepe_pcs_g2_Defav_cdi,
                           resid.agepe_pcs_g2_Moy_cdi = 
                             resid.agepe_pcs_g2_Moy_cdi,
                           resid.agepe_pcs_g2_Fav_cdi = 
                             resid.agepe_pcs_g2_Fav_cdi,
                           
                           resid.agepe_pcs_g2_Tresfav_cdi = 
                             resid.agepe_pcs_g2_Tresfav_cdi,
                           
                           resid.agepe_pcs_g2_Autres_cdi = 
                             resid.agepe_pcs_g2_Autres_cdi,
                           
                           resid.agepe_pcs_g2_NA_cdi = 
                             resid.agepe_pcs_g2_NA_cdi,
                    ), index = "ecolecoh", model = "pooling")

for (i in c("moy_fran_ec_rp", "moy_maths_ec_rp")) assign(
              paste("agecfh_rp_pcs_", 
                    i, "_cdi", 
                    sep = ""),
              update(agecfh_rp_pcs_cdi, 
                     as.formula(paste(i, " ~ .", sep = "")))
            )

# Inférence et narsq ------------------------------------------------------

for (i in ls()[str_detect(ls(), "^agecfh")]) {
  assign(paste("ct.", i, sep = ""),
         rsearellano(get(i)))
  assign(paste("n.", i, sep = ""), nobs(get(i)))
  assign(paste("arsq.", i, sep = ""), ext_adjrsq(get(i)))
  assign(paste("resid.", i, sep = ""), resid(get(i)))
}

# Sauvegarde --------------------------------------------------------------

save(list = ls()[str_detect(ls(), "^ct\\.|^n\\.|^arsq\\.|^mvc\\.")], 
     file = here("agecfh_rp", "agecfh_rp_pcs.rda"), version = 2)