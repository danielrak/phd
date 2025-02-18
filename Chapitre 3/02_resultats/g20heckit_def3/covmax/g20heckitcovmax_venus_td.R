# g20heckit_venus_td (def3) covmax

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

selection0 <- lm(venu_td ~ z + moy_bac + serie_diplome_psv4
                 + campus + filiere + age_26aout + sexe_ps
                 + pays_nais_fr + boursier + statut_etabR, datg20_neo) %>% 
  rsewhite
  # Le statut de boursier est à peine déterminant. 
  # Le campus est intéressant (lié aux transports par exemple, mais aucune preuve).

selection0_probit <- glm(venu_td ~ z + moy_bac + serie_diplome_psv4
                 + campus + filiere + age_26aout + sexe_ps
                 + pays_nais_fr + boursier + statut_etabR, 
                 binomial("probit"),
                 datg20_neo)

rf0 <- lm(note_td ~ z + moy_bac + serie_diplome_psv4
          + campus + filiere + age_26aout + sexe_ps
          + pays_nais_fr + boursier + statut_etabR, datg20_neo_venus_td)

screenreg(list(selection0, selection0_probit, rf0))
  # Au vu de ces résultats, on prend le statut de boursier et le campus
  # Comme variables d'exclusion. 

# IMR ---------------------------------------------------------------------

imr <- invMillsRatio(selection0_probit)$IMR1

datg20_neo_venus_td <- 
  filter(datg20_neo, ! is.na(moy_bac)) %>% 
  mutate(imr = imr) %>% filter(venu_td == 1)

for (i in g20specs) {
  assign(i, c(get(i) %>% 
                (function (g) g[! str_detect(g, "boursier|campus")]),
              "imr"))
}

# Heckit ------------------------------------------------------------------

for (m in c("z")) {
for (i in g20treats) {
  for (j in g20specs) {
    for (k in g20depvars) {
      for (l in g20dats[g20dats == "datg20_neo_venus_td"]) {
        assign(paste("g20heckit_", i, "_", j %>% str_remove("g20"), 
                     "_", k, "_tous_", l, sep = ""),
               lm(as.formula(
                 paste(
                   k, " ~ ", 
                   paste(c(i, 
                         get(j) %>% paste(collapse = " + ")),
                         collapse = " + ") %>% 
                     (function (x) ifelse(str_detect(str_trim(x), "\\+$"),
                                          str_remove(x, "\\+ $"), x)),
                   sep = ""
                   )
               ), 
               data = get(l)))
      }
    }
  }
}
}

# Inférence et autres éléments --------------------------------------------

for (i in ls()[str_detect(ls(), "^g20heckit")]) {
  
  assign(paste("ct.", i, sep = ""),
         rsewhite(get(i))) 
  
  assign(paste("n.", i, sep = ""), nobs(get(i)))
  
  assign(paste("arsq.", i, sep = ""),
         get(i) %>% ext_adjrsq)
}

# Sauvegarde --------------------------------------------------------------

save(list = ls()[str_detect(ls(), "g20heckit") & 
                   ! str_detect(ls(), "^g20heckit")],
     file = here("g20heckit_def3", "covmax",
                 "g20heckitcovmax_venus_td.rda"),
     version = 2)
