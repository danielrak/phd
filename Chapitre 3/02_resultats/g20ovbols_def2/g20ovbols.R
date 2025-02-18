# g20ovbols (def2). 

library(here)
library(tidyverse)
library(lmtest)
library(plm)
library(Formula)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions2.rda")
load(here("01_datg20_def.rda"))
load(here("g20specs_def2.rda"))
# source(here("g20depvars_def.R"))
load(here("g20treats_def2.rda"))

g20specs <- ls()[str_detect(ls(), "g20cov") & 
  ls() != "g20cov0"]
g20dats <- ls()[str_detect(ls(), "datg20")]

# Régressions -------------------------------------------------------------

for (i in c("z")) {
  for (j in g20specs) {
    for (k in g20treats) {
      for (l in g20dats) {
        assign(paste("g20ovbols_", j %>% str_remove("g20"), 
                     "_", k, "_tous_", l, sep = ""),
               lm(as.Formula(
                 paste(
                   k, " ~ ", 
                   paste(c(
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

for (i in ls()[str_detect(ls(), "^g20ovbols")]) {
  
  assign(paste("ct.", i, sep = ""),
         coef(summary(get(i))) %>% structure(class = "coeftest"))
  
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

save(list = ls()[str_detect(ls(), "g20ovbols") & 
                   ! str_detect(ls(), "^g20ovbols")],
     file = here("g20ovbols_def2", "g20ovbols.rda"),
     version = 2)
