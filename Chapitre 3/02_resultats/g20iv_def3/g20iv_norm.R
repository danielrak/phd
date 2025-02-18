# g20if_norm (def3). 
  # Je sépare pour facilier la création des tableaux.

library(here)
library(tidyverse)
library(lmtest)
library(plm)
# library(Formula) # as.Formula pose problème avec ivreg. 
library(AER)
library(ivpack)

# Chargement --------------------------------------------------------------

load("C:/00_phd/00_fonctions/fonctions2.rda")
load(here("01_datg20_neo_def.rda"))
load(here("g20specs_def3.rda"))
load(here("g20depvars_def3.rda"))
load(here("g20treats_def3.rda"))
# load(here("g20z_hetero_def.rda"))

g20specs <- ls()[str_detect(ls(), "g20cov")]
# g20heterocovars <- sapply(g20specs, get) %>% unlist %>% unname %>% unique
g20dats <- ls()[str_detect(ls(), "datg20")]

# Limitation treats -------------------------------------------------------

g20treats <- g20treats[str_detect(g20treats, "norm$")]

# Régressions -------------------------------------------------------------

for (m in c("z")) {
for (i in g20treats) {
  for (j in g20specs) {
    for (k in g20depvars) {
      for (l in g20dats) {
        assign(paste("g20iv_", i, "_", j %>% str_remove("g20"), 
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

for (i in ls()[str_detect(ls(), "^g20iv")]) {
  
  assign(paste("ct.", i, sep = ""),
         robust.se(get(i)))
  
  assign(paste("n.", i, sep = ""), nobs(get(i)))
  
  assign(paste("arsq.", i, sep = ""),
         get(i) %>% ext_adjrsq)
}

# Sauvegarde --------------------------------------------------------------

save(list = ls()[str_detect(ls(), "g20iv") & 
                   ! str_detect(ls(), "^g20iv")],
     file = here("g20iv_def3", "g20iv_norm.rda"),
     version = 2)
