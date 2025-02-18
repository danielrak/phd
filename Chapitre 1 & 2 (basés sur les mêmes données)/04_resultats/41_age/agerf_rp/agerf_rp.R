# agerf, tot, french et maths uniquement. 

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
z <- "z"
cont <- c("sexe_M", "pcs_g2_Moy", "pcs_g2_Fav", "pcs_g2_Tresfav", "pcs_g2_Autres", 
          "pcs_g2_NA")
coh <- c("cohorte_2010", "cohorte_2011", "cohorte_2012")
coh_cdi <- c("cohorte_2011", "cohorte_2012")

# Régressions -------------------------------------------------------------

agerf_rp <- lm(as.formula(paste(depvar, " ~ ", 
                              z, " + ",
                              cont %>% paste(collapse = " + "), " + ",
                              coh %>% paste(collapse = " + "))), c)


for (i in c("score_f_rp", "ecrire_rp", "grammaire_rp", 
            "lire_rp", "ortho_rp", "voca_rp",
            "score_m_rp", "calcul_rp", "geometrie_rp", "grand_mes_rp", 
            "nombre_rp", 
            "org_donnee_rp")) assign(
              paste("agerf_rp_", 
                    i %>% str_replace("score_f", "french") %>% 
                      str_replace("score_m", "maths"), sep = ""),
              update(agerf_rp, as.formula(paste(i, " ~ .", sep = "")))
            )

# sur cdi -----------------------------------------------------------------

agerf_rp_cdi <- lm(as.formula(paste(depvar, " ~ ", 
                             z, " + ",
                             cont %>% paste(collapse = " + "), " + ",
                             coh_cdi %>% paste(collapse = " + "))), cdi)

for (i in c("moy_fran_ec_rp", "moy_maths_ec_rp")) assign(
              paste("agerf_rp_", 
                    i, "_cdi", sep = ""),
              update(agerf_rp_cdi, as.formula(paste(i, " ~ .", sep = "")))
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
     file = here("agerf_rp", "agerf_rp.rda"), version = 2)
