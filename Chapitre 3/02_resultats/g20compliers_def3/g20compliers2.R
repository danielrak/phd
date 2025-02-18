# g20compliers2 (def3). 
  # 2022_08_19 : 2 dans le sens où je prend en compte
  # les corrections de N. Moreau sur la note au bac et l'âge.

library(here)
library(tidyverse)
library(lubridate)

# Chargement --------------------------------------------------------------

load(here("01_datg20_neo_def.rda"))

# Création variable position ----------------------------------------------

for (i in ls()[str_detect(ls(), "datg20")]) {
  assign(i, 
         mutate(get(i), 
                position = if_else(year(nais_nm) == 2003,
                                   "Avance", 
                                   if_else(year(nais_nm) == 2002,
                                           "Heure", "Redoublant")))
  )
}

# Création de variable mention sans/ab/btb --------------------------------
  # btb : bien ou très bien.

for (i in ls()[str_detect(ls(), "datg20")]) {
  assign(i, 
         mutate(get(i), 
                mention_diplome_ps_v2 = 
                  fct_collapse(mention_diplome_ps,
                               "sans" = "sans",
                               "ab" = "ab", 
                               "btb" = c("b", "tb"))))
}

# Création de variables serie_diplome_ps_v5 ----------------------------------

for (i in ls()[str_detect(ls(), "datg20")]) {
  assign(i, 
         mutate(get(i), 
                serie_diplome_psv5 = 
                  fct_collapse(serie_diplome_psv4,
                               "protechnoautre" = c("pro", "techno", "autre"),
                               "ess" = c("es", "s"))))
}


# Compliers, videos_comp_tot ----------------------------------------------

g20compliers_videos_comp_tot <-
  lapply(list(datg20_neo_venus_td, datg20_neo_venus_ctqcm), 
         function (y) {
    lapply(c(
      "serie_diplome_psv5",
      "filiere",
      "mention_diplome_ps_v2",
      "position",
      "sexe_ps",
      "campus",
      "boursier",
      "pays_nais_fr",
      "statut_etab"
    ),
    function (x) {
      var <- x
      levels <- levels(y[[x]])
      table <- as.data.frame(table(y[[x]]))
      data.frame(var = var, table)
    }) %>% do.call(what = rbind) %>%
      (function (d)
        cbind(d,
              "compliers" = apply(d, 1, function (x)
                lm(videos_comp_tot_sup1 ~ z, y) %>%
                  (function (l)
                    coef(update(
                      l, data = y %>%
                        filter(eval(parse(
                          text = paste(x[1], " == '", x[2], "'", sep = "")
                        )))
                    ))[2] /
                     coef(l)[2])))) # Vérifié (investigation 26).
  }) %>% do.call(what = cbind) %>%
  (function (d)
    d[,-c(5, 6)]) %>%
  setNames(c(
    "var",
    "mod",
    "freq_td",
    "compl_td",
    "freq_ctqcm",
    "compl_ctqcm"
  )) %>%
  mutate_if(is.numeric, function (x)
    round(x, 2))
  # 2022_08_19 : on aboutit à la même conclusion => 
  # Les compliers ne sont pas des individus spécialement forts.


# Compliers, videos_views_tot ---------------------------------------------

g20compliers_videos_views_tot <- 
  lapply(list(datg20_neo_venus_td, datg20_neo_venus_ctqcm), 
         function (y) {
    lapply(c(
      "serie_diplome_psv5",
      "filiere",
      "mention_diplome_ps_v2",
      "position",
      "sexe_ps",
      "campus",
      "boursier",
      "pays_nais_fr",
      "statut_etab"
    ),
    function (x) {
      var <- x
      levels <- levels(y[[x]])
      table <- as.data.frame(table(y[[x]]))
      data.frame(var = var, table)
    }) %>% do.call(what = rbind) %>%
      (function (d)
        cbind(d,
              "compliers" = apply(d, 1, function (x)
                lm(videos_views_tot_sup1 ~ z, y) %>%
                  (function (l)
                    coef(update(
                      l, data = y %>%
                        filter(eval(parse(
                          text = paste(x[1], " == '",
                                       x[2], "'", sep = "")
                        )))
                    ))[2] /
                     coef(l)[2])))) # Vérifié (investigation 26).
  }) %>% do.call(what = cbind) %>%
  (function (d)
    d[,-c(5, 6)]) %>%
  setNames(c(
    "var",
    "mod",
    "freq_td",
    "compl_td",
    "freq_ctqcm",
    "compl_ctqcm"
  )) %>%
  mutate_if(is.numeric, function (x)
    round(x, 2))

# Compliers, sheets_views_tot ---------------------------------------------

g20compliers_sheets_views_tot <- 
  lapply(list(datg20_neo_venus_td, datg20_neo_venus_ctqcm), 
         function (y) {
    lapply(c(
      "serie_diplome_psv5",
      "filiere",
      "mention_diplome_ps_v2",
      "position",
      "sexe_ps",
      "campus",
      "boursier",
      "pays_nais_fr",
      "statut_etab"
    ),
    function (x) {
      var <- x
      levels <- levels(y[[x]])
      table <- as.data.frame(table(y[[x]]))
      data.frame(var = var, table)
    }) %>% do.call(what = rbind) %>%
      (function (d)
        cbind(d,
              "compliers" = apply(d, 1,
                                  function (x)
                                    lm(sheets_views_tot_sup1 ~ z, y) %>%
                                    (function (l)
                                      coef(update(
                                        l, data = y %>%
                                          filter(eval(parse(
                                            text = paste(x[1],
                                                         " == '", x[2], "'", sep = "")
                                          )))
                                      ))[2] /
                                       coef(l)[2])))) # Vérifié (investigation 26).
  }) %>% do.call(what = cbind) %>%
  (function (d)
    d[,-c(5, 6)]) %>%
  setNames(c(
    "var",
    "mod",
    "freq_td",
    "compl_td",
    "freq_ctqcm",
    "compl_ctqcm"
  )) %>%
  mutate_if(is.numeric, function (x)
    round(x, 2))

# Sauvegarde --------------------------------------------------------------

save(list = ls()[str_detect(ls(), "g20compliers")],
     file = here("g20compliers_def3", "g20compliers2.rda"),
     version = 2)
