# hnolintab_q5


library(here)
library(tidyverse)
library(plm)
library(magrittr)
library(gtools)
library(fastDummies)
library(lmtest)

rm(list = ls())

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions.rda")

load(here("res_datrestr_mltest", "s13hnolinq5", 
          "s13hnolinq5_testclniv_drmlt.rda"))

load(here("res_datrestr_mltest", "s13hnolinq5", 
          "s13hnolinq5_french_testclniv_drmlt.rda"))

load(here("res_datrestr_mltest", "s13hnolinq5", 
          "s13hnolinq5_maths_testclniv_drmlt.rda"))

# hnolintab_q5_french, tot ----------------------------------------------------------

  # wclnivsup
hnolintab_q5_wclnivsup_drmlt <- data.frame(
  "covar" = rownames(ct.s13hnolinq5_sD_wclnivsup_drmlt) %>%
    (function (x)
      x[str_detect(x, "p\\_q5")]) %>%
    str_remove("p:") %>% str_remove_all("q5score\\_tot\\_") %>% 
    str_remove("_") %>% str_remove(":p$")
)

hnolintab_q5_wclnivsup_drmlt <- mutate(
  hnolintab_q5_wclnivsup_drmlt,
  indiv = str_remove(covar, "pq[:digit:]") %>% str_remove(":"),
  peer = str_extract(covar, "pq[:digit:]"),
  coef = ct.s13hnolinq5_sD_wclnivsup_drmlt %>%
    (function (x)
      x[str_detect(rownames(x), "p\\_q5"), "Estimate"]),
  sd = ct.s13hnolinq5_sD_wclnivsup_drmlt %>%
    (function (x)
      x[str_detect(rownames(x), "p\\_q5"), "Std. Error"])
)

hnolintab_q5_wclnivsup_drmlt <- mutate(hnolintab_q5_wclnivsup_drmlt,
                           up = coef + 1.96 * sd,
                           low = coef - 1.96 * sd)

hnolintab_q5_wclnivsup_drmlt <- mutate(hnolintab_q5_wclnivsup_drmlt, 
                                     depvar = "tot")

# wclnivinf
hnolintab_q5_wclnivinf_drmlt <- data.frame(
  "covar" = rownames(ct.s13hnolinq5_sD_wclnivinf_drmlt) %>%
    (function (x)
      x[str_detect(x, "p\\_q5")]) %>%
    str_remove("p:") %>% str_remove_all("q5score\\_tot\\_") %>% 
    str_remove("_") %>% str_remove(":p$")
)

hnolintab_q5_wclnivinf_drmlt <- mutate(
  hnolintab_q5_wclnivinf_drmlt,
  indiv = str_remove(covar, "pq[:digit:]") %>% str_remove(":"),
  peer = str_extract(covar, "pq[:digit:]"),
  coef = ct.s13hnolinq5_sD_wclnivinf_drmlt %>%
    (function (x)
      x[str_detect(rownames(x), "p\\_q5"), "Estimate"]),
  sd = ct.s13hnolinq5_sD_wclnivinf_drmlt %>%
    (function (x)
      x[str_detect(rownames(x), "p\\_q5"), "Std. Error"])
)

hnolintab_q5_wclnivinf_drmlt <- mutate(hnolintab_q5_wclnivinf_drmlt,
                                     up = coef + 1.96 * sd,
                                     low = coef - 1.96 * sd)

hnolintab_q5_wclnivinf_drmlt <- mutate(hnolintab_q5_wclnivinf_drmlt, 
                                     depvar = "tot")

# hnolintab_q5, french -------------------------------------------------------

  #wclnivsup
hnolintab_q5_french_wclnivsup_drmlt <- data.frame(
  "covar" = rownames(ct.s13hnolinq5_sD_french_wclnivsup_drmlt) %>%
    (function (x)
      x[str_detect(x, "p\\_q5")]) %>%
    str_remove("p:") %>% str_remove_all("q5score\\_tot\\_") %>% 
    str_remove("_") %>% str_remove(":p$")
)

hnolintab_q5_french_wclnivsup_drmlt <- mutate(
  hnolintab_q5_french_wclnivsup_drmlt,
  indiv = str_remove(covar, "pq[:digit:]") %>% str_remove(":"),
  peer = str_extract(covar, "pq[:digit:]"),
  coef = ct.s13hnolinq5_sD_french_wclnivsup_drmlt %>%
    (function (x)
      x[str_detect(rownames(x), "p\\_q5"), "Estimate"]),
  sd = ct.s13hnolinq5_sD_french_wclnivsup_drmlt %>%
    (function (x)
      x[str_detect(rownames(x), "p\\_q5"), "Std. Error"])
)

hnolintab_q5_french_wclnivsup_drmlt <- 
  mutate(hnolintab_q5_french_wclnivsup_drmlt,
         up = coef + 1.96 * sd,
         low = coef - 1.96 * sd)

hnolintab_q5_french_wclnivsup_drmlt <- 
  mutate(hnolintab_q5_french_wclnivsup_drmlt, depvar = "french")

#wclnivinf
hnolintab_q5_french_wclnivinf_drmlt <- data.frame(
  "covar" = rownames(ct.s13hnolinq5_sD_french_wclnivinf_drmlt) %>%
    (function (x)
      x[str_detect(x, "p\\_q5")]) %>%
    str_remove("p:") %>% str_remove_all("q5score\\_tot\\_") %>% 
    str_remove("_") %>% str_remove(":p$")
)

hnolintab_q5_french_wclnivinf_drmlt <- mutate(
  hnolintab_q5_french_wclnivinf_drmlt,
  indiv = str_remove(covar, "pq[:digit:]") %>% str_remove(":"),
  peer = str_extract(covar, "pq[:digit:]"),
  coef = ct.s13hnolinq5_sD_french_wclnivinf_drmlt %>%
    (function (x)
      x[str_detect(rownames(x), "p\\_q5"), "Estimate"]),
  sd = ct.s13hnolinq5_sD_french_wclnivinf_drmlt %>%
    (function (x)
      x[str_detect(rownames(x), "p\\_q5"), "Std. Error"])
)

hnolintab_q5_french_wclnivinf_drmlt <- 
  mutate(hnolintab_q5_french_wclnivinf_drmlt,
         up = coef + 1.96 * sd,
         low = coef - 1.96 * sd)

hnolintab_q5_french_wclnivinf_drmlt <- 
  mutate(hnolintab_q5_french_wclnivinf_drmlt, depvar = "french")

# hnolintab_q5, maths -------------------------------------------------------

  # wclnivsup
hnolintab_q5_maths_wclnivsup_drmlt <- data.frame(
  "covar" = rownames(ct.s13hnolinq5_sD_maths_wclnivsup_drmlt) %>%
    (function (x)
      x[str_detect(x, "p\\_q5")]) %>%
    str_remove("p:") %>% str_remove_all("q5score\\_tot\\_") %>% 
    str_remove("_") %>% str_remove(":p$")
)

hnolintab_q5_maths_wclnivsup_drmlt <- mutate(
  hnolintab_q5_maths_wclnivsup_drmlt,
  indiv = str_remove(covar, "pq[:digit:]") %>% str_remove(":"),
  peer = str_extract(covar, "pq[:digit:]"),
  coef = ct.s13hnolinq5_sD_maths_wclnivsup_drmlt %>%
    (function (x)
      x[str_detect(rownames(x), "p\\_q5"), "Estimate"]),
  sd = ct.s13hnolinq5_sD_maths_wclnivsup_drmlt %>%
    (function (x)
      x[str_detect(rownames(x), "p\\_q5"), "Std. Error"])
)

hnolintab_q5_maths_wclnivsup_drmlt <- 
  mutate(hnolintab_q5_maths_wclnivsup_drmlt,
         up = coef + 1.96 * sd,
         low = coef - 1.96 * sd)

hnolintab_q5_maths_wclnivsup_drmlt <- 
  mutate(hnolintab_q5_maths_wclnivsup_drmlt, depvar = "maths")


# wclnivinf
hnolintab_q5_maths_wclnivinf_drmlt <- data.frame(
  "covar" = rownames(ct.s13hnolinq5_sD_maths_wclnivinf_drmlt) %>%
    (function (x)
      x[str_detect(x, "p\\_q5")]) %>%
    str_remove("p:") %>% str_remove_all("q5score\\_tot\\_") %>% 
    str_remove("_") %>% str_remove(":p$")
)

hnolintab_q5_maths_wclnivinf_drmlt <- mutate(
  hnolintab_q5_maths_wclnivinf_drmlt,
  indiv = str_remove(covar, "pq[:digit:]") %>% str_remove(":"),
  peer = str_extract(covar, "pq[:digit:]"),
  coef = ct.s13hnolinq5_sD_maths_wclnivinf_drmlt %>%
    (function (x)
      x[str_detect(rownames(x), "p\\_q5"), "Estimate"]),
  sd = ct.s13hnolinq5_sD_maths_wclnivinf_drmlt %>%
    (function (x)
      x[str_detect(rownames(x), "p\\_q5"), "Std. Error"])
)

hnolintab_q5_maths_wclnivinf_drmlt <- 
  mutate(hnolintab_q5_maths_wclnivinf_drmlt,
         up = coef + 1.96 * sd,
         low = coef - 1.96 * sd)

hnolintab_q5_maths_wclnivinf_drmlt <- 
  mutate(hnolintab_q5_maths_wclnivinf_drmlt, depvar = "maths")


# Sauvegarde --------------------------------------------------------------

save(hnolintab_q5_wclnivsup_drmlt,
     hnolintab_q5_wclnivinf_drmlt,
     
     hnolintab_q5_french_wclnivsup_drmlt,
     hnolintab_q5_french_wclnivinf_drmlt,
     
     hnolintab_q5_maths_wclnivsup_drmlt,
     hnolintab_q5_maths_wclnivinf_drmlt,
     
     file = here("res_datrestr_mltest", "s13hnolinq5",
                 "hnolintab", "hnolintab_q5_testclniv_drmlt.rda"), 
     version = 2)
