# g20heckitiv_venus_ctqcm (def3) covmax

library(here)
library(tidyverse)
library(lmtest)
library(plm)
library(Formula)
library(texreg)
library(sampleSelection)
library(AER)
library(ivpack)

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


# Repérage de variables d'exclusion ----------------------------------------
  # Forcément à la main.

selection0 <- lm(venu_ctqcm ~ z + moy_bac + serie_diplome_psv4
          + campus + filiere + age_26aout + sexe_ps
          + pays_nais_fr + boursier + statut_etabR, datg20_neo) %>% 
  rsewhite

selection0_probit <- glm(venu_ctqcm ~ z + moy_bac + serie_diplome_psv4
          + campus + filiere + age_26aout + sexe_ps
          + pays_nais_fr + boursier + statut_etabR, 
          binomial("probit"),
          datg20_neo)

rf0 <- lm(note_ctqcm ~ z + moy_bac + serie_diplome_psv4
          + campus + filiere + age_26aout + sexe_ps
          + pays_nais_fr + boursier + statut_etabR, datg20_neo_venus_ctqcm)

screenreg(list(selection0, selection0_probit, rf0)) 
  # On vérifie bien que le statut de boursier est une variable d'exclusion.

# IMR ---------------------------------------------------------------------


imr <- invMillsRatio(selection0_probit)$IMR1

datg20_neo_venus_ctqcm <- 
  filter(datg20_neo, ! is.na(moy_bac)) %>% 
  mutate(imr = imr) %>% filter(venu_ctqcm == 1)

for (i in g20specs) {
  assign(i, c(get(i) %>% (function (g) g[! str_detect(g, "boursier")]), "imr"))
}

# Heckitiv -------------------------------------------------------------

for (m in c("z")) {
for (i in g20treats) {
  for (j in g20specs) {
    for (k in g20depvars) {
      for (l in g20dats[g20dats == "datg20_neo_venus_ctqcm"]) {
        assign(paste("g20heckitiv_", i, "_", j %>% str_remove("g20"), 
                     "_", k, "_tous_", l, sep = ""),
               ivreg(as.formula(
                 paste(
                   k, " ~ ", 
                   paste(c(i, 
                         get(j) %>% paste(collapse = " + ")),
                         collapse = " + ") %>% 
                     (function (x) ifelse(str_detect(str_trim(x), "\\+$"),
                                          str_remove(x, "\\+ $"), x)),
                   "|. - ", i, " + ", m,
                   sep = ""
                   )
               ), 
               data = get(l)))
      }
    }
  }
}
}

# Inférence simple et autres éléments --------------------------------------------

for (i in ls()[str_detect(ls(), "^g20heckitiv")]) {
  
  assign(paste("ct.", i, sep = ""),
         robust.se(get(i)))
  
  assign(paste("n.", i, sep = ""), nobs(get(i)))
  
  assign(paste("arsq.", i, sep = ""),
         get(i) %>% ext_adjrsq)
}

# Sauvegarde --------------------------------------------------------------

save(list = ls()[str_detect(ls(), "g20heckitiv") & 
                   ! str_detect(ls(), "^g20heckitiv")],
     file = here("g20heckitiv_def3", "covmax",
                 "g20heckitivcovmax_venus_ctqcm.rda"),
     version = 2)
