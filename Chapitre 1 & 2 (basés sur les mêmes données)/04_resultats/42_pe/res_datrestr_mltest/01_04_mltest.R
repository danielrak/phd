# tests mlogit (BM18) (mltest)

library(tidyverse)
library(magrittr)
library(here)
library(nnet) #
library(lmtest)

load("D:/00_phd/00_fonctions/fonctions.rda")
load("01_donnees.rda")

# Préparation -------------------------------------------------------------

  # Sans les tailles de classe 2. 
dat <- group_by(dat, rneconstatses) %>% 
  mutate(ncl0 = length(unique(divconstatrneses))) %>% ungroup %>% 
  filter(ncl0 >= 2)

  # Sélection des variables. 
dat2 <- select(dat, ine_mod, rneconstatses, rneconstat,
              divconstatrneses, score_ori)

# mltest ------------------------------------------------------------------

datrestr_mltest <- split(dat2, dat2$rneconstatses) %>% 
  sapply(function (x) lrtest(multinom(divconstatrneses ~ score_ori, na.omit(x)),
                             multinom(divconstatrneses ~ 1, na.omit(x)))[[
                               "Pr(>Chisq)"]][2]) %>% as.data.frame %>% 
  (function (d) data.frame(e = rownames(d), pval = d[[1]])) %>% 
  filter(pval <= .1354) %>% pull(e) %>% 
  substr(1, 8) %>% table %>% (function (t) t[t >= 2]) %>% 
  (function (e) filter(dat, ! rneconstat %in% names(e))) %>% mutate(cg = droplevels(cg))

# Sauvegarde --------------------------------------------------------------

save(datrestr_mltest, file = here("res_datrestr_mltest", "01_datrestr_mltest.rda"))