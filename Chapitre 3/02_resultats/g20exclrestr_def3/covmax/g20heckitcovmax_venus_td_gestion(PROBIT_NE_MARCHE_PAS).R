# heckitivcovmax_venus_td_gestion
  # Suite à l'observation de l'influence de l'instrument sur la note de gestion, 
  # Je fais du heckit venus_gestion sur les venus_td pour vérifier que
  # l'effet direct de l'instrument est robuste. 
  # NB : c'est une reduced form. 

library(here)
library(tidyverse)
library(lmtest)
library(plm)
library(Formula)
library(texreg)
library(sampleSelection)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions2.rda")
load(here("01_datg20_neo_def.rda"))
load(here("g20specs_def3.rda"))
load(here("g20depvars_def3.rda"))
load(here("g20treats_def3.rda"))

g20specs <- ls()[str_detect(ls(), "g20cov")]
g20dats <- ls()[str_detect(ls(), "datg20")]

# covmax ------------------------------------------------------------------

g20specs <- 
  g20specs %>% (function (g) {
    g0 <- g
  g <- str_remove(g, "g20")
  g <- g[str_detect(g, "cov[:digit:]|cov[:digit:][:digit:]")]
  max <- str_remove(g, "cov") %>% as.numeric() %>% 
    max
  g <- g0[str_detect(g0, paste("g20cov", max, sep = ""))]
  c(g, "g20covnocor")
})

# Repérage de variables d'exclusion ---------------------------------------

selection0 <- lm(venu_gestion ~ z + moy_bac + serie_diplome_psv4
                 + campus + filiere + age_26aout + sexe_ps
                 + pays_nais_fr + boursier + statut_etabR, datg20_neo_venus_td) %>% 
  rsewhite

selection0_probit <- glm(venu_gestion ~ z + moy_bac + serie_diplome_psv4
                         + campus + filiere + age_26aout + sexe_ps
                         + pays_nais_fr
                         + boursier + statut_etabR, 
                         binomial("probit"), 
                         datg20_neo_venus_td)
  # Rien à signaler ici. ça ne marche pas bien. 

rf0 <- lm(note_gestion ~ z + moy_bac + serie_diplome_psv4
          + campus + filiere + age_26aout + sexe_ps
          + pays_nais_fr + boursier + statut_etabR, datg20_neo_venus_td)


screenreg(list(selection0, selection0_probit, rf0))
  # Au vu de ces résultats, le pays de naissance semble être déterminant dans la
  # présence aux examens de gestion. 
  # Mais le probit ne donne rien donc je ne peux rien faire. 

