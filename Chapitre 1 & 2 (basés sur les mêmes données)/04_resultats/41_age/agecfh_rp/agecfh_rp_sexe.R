# agecfh_sexe

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

age_sexe <- paste("age_abs", c("sexe_F", "sexe_M"), sep = " : ")

resid_sexe <- paste("resid.agepe_", c("sexe_F", "sexe_M"), sep = "")
resid_sexe_cdi <- paste("resid.agepe_", c("sexe_F", "sexe_M"), "_cdi",
                        sep = "")
resid_sexe_age <- paste(resid_sexe, " : age_abs", sep = "") 
resid_sexe_age_cdi <- paste(resid_sexe_cdi, " : age_abs", sep = "") 

cont <- c("sexe_M", "pcs_g2_Moy", "pcs_g2_Fav", 
          "pcs_g2_Tresfav", "pcs_g2_Autres", 
          "pcs_g2_NA")

coh <- c("cohorte_2010", "cohorte_2011", "cohorte_2012")
coh_cdi <- c("cohorte_2011", "cohorte_2012")

# Régressions -------------------------------------------------------------

agecfh_rp_sexe <- plm(as.formula(paste(depvar, " ~ ", 
                                    age_sexe %>% 
                                      paste(collapse = " + "), " + ",
                                    
                                    resid_sexe %>% paste(collapse = " + "),
                                    " + ",
                                    resid_sexe_age %>% 
                                      paste(collapse = " + "),
                                    " + ",
                                    
                                    cont %>% paste(collapse = " + "), " + ",
                                    coh %>% paste(collapse = " + "))), 
                   c %>% arrange(ecolecoh) %>% 
                     mutate(resid.agepe_sexe_F = resid.agepe_sexe_F,
                            resid.agepe_sexe_M = resid.agepe_sexe_M),
                   index = "ecolecoh", model = "pooling")
  # vérifié : mêmes coef que iv avec interaction avec sexe. 
  # délicat. 

for (i in c("score_f_rp", "ecrire_rp", "grammaire_rp", "lire_rp", "ortho_rp", 
            "voca_rp",
            "score_m_rp", "calcul_rp", "geometrie_rp", "grand_mes_rp",
            "nombre_rp", 
            "org_donnee_rp")) assign(
              paste("agecfh_rp_sexe_",
                    i %>% str_replace("score_f", "french") %>% 
                      str_replace("score_m", "maths"),
                    sep = ""),
              update(agecfh_rp_sexe, as.formula(paste(i, " ~ .", sep = "")))
            )

# sur cdi -----------------------------------------------------------------

agecfh_rp_sexe_cdi <- plm(as.formula(paste(depvar_cdi, " ~ ", 
                                    age_sexe %>% 
                                      paste(collapse = " + "), " + ",
                                    
                                    resid_sexe_cdi %>% paste(collapse = " + "),
                                    " + ",
                                    resid_sexe_age_cdi %>% 
                                      paste(collapse = " + "),
                                    " + ",
                                    
                                    cont %>% paste(collapse = " + "), " + ",
                                    coh_cdi %>% paste(collapse = " + "))), 
                   cdi %>% arrange(ecolecoh) %>% 
                     mutate(resid.agepe_sexe_F_cdi = 
                              resid.agepe_sexe_F_cdi,
                            
                            resid.agepe_sexe_M_cdi = 
                              resid.agepe_sexe_M_cdi),
                   index = "ecolecoh", model = "pooling")

for (i in c("moy_fran_ec_rp", "moy_maths_ec_rp")) assign(
              paste("agecfh_rp_sexe_",
                    i, "_cdi",
                    sep = ""),
              update(agecfh_rp_sexe_cdi, 
                     as.formula(paste(i, " ~ .", sep = "")))
            )

# Inférence et narsq ------------------------------------------------------

for (i in ls()[str_detect(ls(), "^agecfh")]) {
  assign(paste("ct.", i, sep = ""),
         rsearellano(get(i)))
  assign(paste("n.", i, sep = ""), nobs(get(i)))
  assign(paste("arsq.", i, sep = ""), ext_adjrsq(get(i)))
}

# Sauvegarde --------------------------------------------------------------

save(list = ls()[str_detect(ls(), "^ct\\.|^n\\.|^arsq\\.|^mvc\\.")], 
     file = here("agecfh_rp", "agecfh_rp_sexe.rda"), version = 2)