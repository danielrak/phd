# g20rfcovmax (def3). 

library(here)
library(tidyverse)
library(lmtest)
library(plm)
library(Formula)

# Chargement --------------------------------------------------------------

load("C:/00_phd/00_fonctions/fonctions2.rda")
load(here("01_datg20_neo_def.rda"))
load(here("g20specs_def3.rda"))
load(here("g20depvars_def3.rda"))
# load(here("g20treats_def.rda"))

g20specs <- ls()[str_detect(ls(), "g20cov")]
g20dats <- ls()[str_detect(ls(), "datg20")]

# Relevel sur les plus grands effectifs -----------------------------------

# for (i in g20dats) 
#   assign(i, mutate(get(i), 
#                    pays_nais_fr = relevel(pays_nais_fr, "oui"),
#                    serie_diplome_psv4R = relevel(serie_diplome_psv4R, 
#                                                  "es")))


# covmax ------------------------------------------------------------------

# g20specs <- 
#   g20specs %>% (function (g) {
#     g0 <- g
#   g <- str_remove(g, "g20")
#   g <- g[str_detect(g, "cov[:digit:]|cov[:digit:][:digit:]")]
#   max <- str_remove(g, "cov") %>% as.numeric() %>% 
#     max
#   g <- g0[str_detect(g0, paste("g20cov", max, sep = ""))]
#   c(g, "g20covnocor")
# })

# depvars utiles uniquement -----------------------------------------------

# g20depvars <- g20depvars[! str_detect(g20depvars, "\\_sup")]

# Régressions -------------------------------------------------------------

for (i in c("z")) {
  for (j in g20specs) {
    for (k in g20depvars) {
      for (l in g20dats) {
        assign(paste("g20rf_", i, "_", j %>% str_remove("g20"), 
                     "_", k, "_tous_", l, sep = ""),
               lm(as.Formula(
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
               get(l)))
      }
    }
  }
  }

# Inférence simple et autres éléments -------------------------------------

for (i in ls()[str_detect(ls(), "^g20rf")]) {
  
  assign(paste("ct.", i, sep = ""),
         rsewhite(get(i))) # 0 warnings. 
  
  assign(paste("n.", i, sep = ""), nobs(get(i)))
  
  assign(paste("arsq.", i, sep = ""),
         get(i) %>% ext_adjrsq)
}

# Sauvegarde --------------------------------------------------------------

save(list = ls()[str_detect(ls(), "g20rf") & 
                   ! str_detect(ls(), "^g20rf")],
     file = here("g20rf_def3",
                 "g20rf.rda"),
     version = 2)
