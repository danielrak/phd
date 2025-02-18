# g20peprob (def2). 

library(here)
library(tidyverse)
library(lmtest)
library(plm)
library(Formula)
library(ivprobit)
library(mfx)
library(needs)
prioritize(dplyr)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions2.rda")
load(here("01_datg20_def.rda"))
load(here("g20specs_def2.rda"))
# source(here("g20depvars_def.R"))
load(here("g20treats_def2.rda"))

g20specs <- ls()[str_detect(ls(), "g20cov")]
g20dats <- ls()[str_detect(ls(), "datg20")]

# Filtres nécessaires -----------------------------------------------------

g20treats <- g20treats[str_detect(g20treats, "sup1")]

# Régressions -------------------------------------------------------------

for (i in c("z")) {
  for (j in g20specs) {
    for (k in g20treats) {
      for (l in g20dats) {
        assign(paste("g20peprob_", i, "_", j %>% str_remove("g20"), 
                     "_", k, "_tous_", l, sep = ""),
               probitmfx(as.formula(
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

# Inférence et autres éléments --------------------------------------------

for (i in ls()[str_detect(ls(), "^g20pe")]) {
  
  assign(paste("ct.", i, sep = ""),
         get(i)$mfxest %>% structure(class = "coeftest"))
  
  assign(paste("aic.", i, sep = ""),
         get(i)$fit %>% summary %>% (function (s) s$aic))
  
  assign(paste("n.", i, sep = ""), nobs(get(i)$fit))
}

# Sauvegarde --------------------------------------------------------------

save(list = ls()[str_detect(ls(), "g20pe") & 
                   ! str_detect(ls(), "^g20pe")],
     file = here("g20peprob_def2", "g20peprob.rda"),
     version = 2)
