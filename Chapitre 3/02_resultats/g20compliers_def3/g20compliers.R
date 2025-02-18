# g20compliers (def3). 

library(here)
library(tidyverse)

# Chargement --------------------------------------------------------------

load(here("01_datg20_neo_def.rda"))

# Compliers, videos_comp_tot ----------------------------------------------

g20compliers_videos_comp_tot <-
  lapply(list(datg20_neo_venus_td, datg20_neo_venus_ctqcm), 
         function (y) {
    lapply(c(
      "serie_diplome_psv4",
      "filiere",
      "q5moy_bac_norm",
      "q5age_26aout",
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


# Compliers, videos_views_tot ---------------------------------------------

g20compliers_videos_views_tot <- 
  lapply(list(datg20_neo_venus_td, datg20_neo_venus_ctqcm), 
         function (y) {
    lapply(c(
      "serie_diplome_psv4",
      "filiere",
      "q5moy_bac_norm",
      "q5age_26aout",
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
      "serie_diplome_psv4",
      "filiere",
      "q5moy_bac_norm",
      "q5age_26aout",
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
     file = here("g20compliers_def3", "g20compliers.rda"),
     version = 2)
