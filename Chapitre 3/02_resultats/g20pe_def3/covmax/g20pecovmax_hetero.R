# g20pecovmax_hetero (def3). 

library(here)
library(tidyverse)
library(lmtest)
library(plm)
library(Formula)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions2.rda")
load(here("01_datg20_neo_def.rda"))
load(here("g20specs_def3.rda"))
# source(here("g20depvars_def.R"))
load(here("g20treats_def3.rda"))

g20specs <- ls()[str_detect(ls(), "g20cov")]
g20dats <- ls()[str_detect(ls(), "datg20")]

# # Relevel sur les plus grands effectifs -----------------------------------
# 
# for (i in g20dats) 
#   assign(i, mutate(get(i), 
#                    pays_nais_fr = relevel(pays_nais_fr, "oui"),
#                    serie_diplome_psv4R = relevel(serie_diplome_psv4R, 
#                                                  "es")))

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

# treats utiles uniquement ------------------------------------------------

# g20treats <- g20treats[! str_detect(g20treats, "\\_sup")]

# hetero pertinents -------------------------------------------------------

g20heterocovars <- c("mention_diplome_ps", "serie_diplome_psv4", 
                         "filiere", "q5age_26aout", "sexe_ps",
                         "pays_nais_fr", # problématique : effectifs. (rappel)
                     "boursier")
for (i in g20specs) assign(i, get(i) %>% str_replace("moy_bac", "mention_diplome_ps"))

# Régressions -------------------------------------------------------------

for (i in c("z") %>% sapply(function (x) 
  paste(x, g20heterocovars, sep = "_")) %>% 
  as.vector) {
  for (j in g20specs) {
    for (k in g20treats) {
      for (l in g20dats) {
        assign(paste("g20pe_", i, "_", j %>% str_remove("g20"), 
                     "_", k, "_tous_", l, sep = ""),
               lm(as.Formula(
                 paste(
                   k, " ~ ", 
                   paste(c(i %>% str_replace("^z_", "z * "), 
                         get(j) %>% paste(collapse = " + ")),
                         collapse = " + ") %>% 
                     (function (x) ifelse(str_detect(str_trim(x), "\\+$"),
                                          str_remove(x, "\\+ $"), x)),
                   sep = ""
                   )
               ), 
               get(l)))
      }
    }
  }
}

# Inférence et autres éléments --------------------------------------------

for (i in ls()[str_detect(ls(), "^g20pe")]) {
  
  assign(paste("ct.", i, sep = ""),
         rsewhite(get(i))) # 0 warnings. 

    # Pas correct, à corriger plus tard si possible. Si non, à préciser.   
  assign(paste("f.", i, sep = ""),
         get(i) %>% summary %>% (function (s) s$fstatistic) %>%
           (function (f) c(f[1],
                           pf(f[1], f[2], f[3], lower.tail = FALSE)) %>%
              setNames(c("fstat", "pval"))))
  
  
  
  assign(paste("n.", i, sep = ""), nobs(get(i)))
  
  assign(paste("arsq.", i, sep = ""),
         get(i) %>% ext_adjrsq)
}

# Sauvegarde --------------------------------------------------------------

save(list = ls()[str_detect(ls(), "g20pe") & 
                   ! str_detect(ls(), "^g20pe")],
     file = here("g20pe_def3", "covmax",
                 "g20pecovmax_hetero.rda"),
     version = 2)
