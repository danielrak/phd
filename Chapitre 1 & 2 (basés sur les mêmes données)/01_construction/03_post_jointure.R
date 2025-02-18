# Post jointures.

library(tidyverse)
library(fastDummies)
library(lubridate)
library(magrittr)
library(stringi)
library(here)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions.rda")
load(here("01_construction", "01_pre_jointure.rda"))
load(here("01_construction", "02_jointure.rda"))

# cm2dnb ------------------------------------------------------------------

# dummy_cols

cm2dnb <- dummy_cols(
  cm2dnb,
  select_columns = c(
    "lbourses_constat",
    "sexe_mod",
    "positiondnb2",
    "pcs_reg_mod",
    "reseau_mod",
    "reseauR_mod",
    "session_mod",
    "quinze_dnb",
    "mois_dnb",
    "bim_dnb",
    "trim_dnb"
  )
)

# dnbcm2 ------------------------------------------------------------------

# rneconstatses et divconstatrneses
dnbcm2 <- left_join(
  dnbcm2,
  transmute(
    constat1316,
    session_mod,
    ine,
    rneconstatses = paste(rne_mod, session_mod),
    divconstatrneses
  ),
  by = c("session_mod", "ine")
)
# nobs ok

# Il y a des élèves qui changent d'établissement entre le début et la fin
dnbcm2 <- mutate(
  dnbcm2,
  rneconstatses_debut = rneconstatses,
  rneconstatses = ifelse(rneses != rneconstatses,
                         rneconstatses, rneses)
)
# rneconstat_debut
dnbcm2 <- mutate(dnbcm2,
                 rneconstat_debut =
                   str_remove(rneconstatses_debut, " 2.*"))

# rneconstat
dnbcm2 <-
  mutate(dnbcm2, rneconstat = str_remove(rneconstatses, " 2.*"))
# ok

# Le 06 Avril 2021.
# Rappel : cetab_mod est le nom de la variable d'établissement
# original dans dnb.

dnbcm2 <-
  filter(dnbcm2, session_mod == "2014") %>%
  group_by(cetab_mod, bassin_mod) %>%
  summarise %>% (function (d)
    left_join(select(dnbcm2,-bassin_mod), d,
              by = c("rneconstat" = "cetab_mod")))

# Le 07 Avril 2021.
# But : pour que rneconstat et statut_constat soient one-one match

dnbcm2 <-
  group_by(dnbcm2, cetab_mod, statut_constat) %>%
  # ci-dessus : vérifié one-one
  summarise %>% (function (d)
    left_join(
      select(dnbcm2,-statut_constat),
      d,
      by = c("rneconstat" = "cetab_mod")
    ))

# Le 07 avril 2021.
# But : pour que rneconstatses et reseau_mod soient one-one match.

dnbcm2 <-
  group_by(dnbcm2, rneses, reseau_mod) %>%
  # ci-dessus : vérifié one-one
  summarise %>% (function (d)
    left_join(select(dnbcm2,-reseau_mod), d,
              by = c("rneconstatses" = "rneses")))

# Le 07 avril 2021.
# But : pour que rneconstatses et reseauR_mod soient one-one match.

dnbcm2 <-
  group_by(dnbcm2, rneses, reseauR_mod) %>%
  # ci-dessus : vérifié one-one.
  summarise %>% (function (d)
    left_join(
      select(dnbcm2,-reseauR_mod),
      d,
      by = c("rneconstatses" = "rneses")
    ))

# Sauvegarde --------------------------------------------------------------

save(list = ls()["function" != ls() %>% 
                   sapply(function (x) class(get(x))[1])],
     file = here("01_construction", "03_post_jointure.rda"),
     version = 2)
