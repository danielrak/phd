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
rda.files <- list.files(here("res_datrestr_mltest", "s13hnolinq5")) %>% 
  (function (l) l[str_detect(l, "\\.rda$")])
for (i in rda.files) load(here("res_datrestr_mltest", "s13hnolinq5", i))

# tot ---------------------------------------------------------------------

hnolintab_q5_drmlt <- data.frame(
  "covar" = rownames(ct.s13hnolinq5_sD_drmlt) %>%
    (function (x)
      x[str_detect(x, "p\\_q5")]) %>%
    str_remove("p:") %>% str_remove_all("q5score\\_tot\\_") %>% 
    str_remove("_") %>% str_remove(":p$")
)

hnolintab_q5_drmlt <- mutate(
  hnolintab_q5_drmlt,
  indiv = str_remove(covar, "pq[:digit:]") %>% str_remove(":"),
  peer = str_extract(covar, "pq[:digit:]"),
  coef = ct.s13hnolinq5_sD_drmlt %>%
    (function (x)
      x[str_detect(rownames(x), "p\\_q5"), "Estimate"]),
  sd = ct.s13hnolinq5_sD_drmlt %>%
    (function (x)
      x[str_detect(rownames(x), "p\\_q5"), "Std. Error"])
)

hnolintab_q5_drmlt <- mutate(hnolintab_q5_drmlt,
                       up = coef + 1.96 * sd,
                       low = coef - 1.96 * sd)

hnolintab_q5_drmlt <- mutate(hnolintab_q5_drmlt, depvar = "tot")

# hnolintab_q5, french -------------------------------------------------------

hnolintab_q5_french_drmlt <- data.frame(
  "covar" = rownames(ct.s13hnolinq5_sD_french_drmlt) %>%
    (function (x)
      x[str_detect(x, "p\\_q5")]) %>%
    str_remove("p:") %>% str_remove_all("q5score\\_tot\\_") %>% 
    str_remove("_") %>% str_remove(":p$")
)

hnolintab_q5_french_drmlt <- mutate(
  hnolintab_q5_french_drmlt,
  indiv = str_remove(covar, "pq[:digit:]") %>% str_remove(":"),
  peer = str_extract(covar, "pq[:digit:]"),
  coef = ct.s13hnolinq5_sD_french_drmlt %>%
    (function (x)
      x[str_detect(rownames(x), "p\\_q5"), "Estimate"]),
  sd = ct.s13hnolinq5_sD_french_drmlt %>%
    (function (x)
      x[str_detect(rownames(x), "p\\_q5"), "Std. Error"])
)

hnolintab_q5_french_drmlt <- mutate(hnolintab_q5_french_drmlt,
                              up = coef + 1.96 * sd,
                              low = coef - 1.96 * sd)

hnolintab_q5_french_drmlt <- mutate(hnolintab_q5_french_drmlt, depvar = "french")

# hnolintab_q5, maths -------------------------------------------------------

hnolintab_q5_maths_drmlt <- data.frame(
  "covar" = rownames(ct.s13hnolinq5_sD_maths_drmlt) %>%
    (function (x)
      x[str_detect(x, "p\\_q5")]) %>%
    str_remove("p:") %>% str_remove_all("q5score\\_tot\\_") %>% 
    str_remove("_") %>% str_remove(":p$")
)

hnolintab_q5_maths_drmlt <- mutate(
  hnolintab_q5_maths_drmlt,
  indiv = str_remove(covar, "pq[:digit:]") %>% str_remove(":"),
  peer = str_extract(covar, "pq[:digit:]"),
  coef = ct.s13hnolinq5_sD_maths_drmlt %>%
    (function (x)
      x[str_detect(rownames(x), "p\\_q5"), "Estimate"]),
  sd = ct.s13hnolinq5_sD_maths_drmlt %>%
    (function (x)
      x[str_detect(rownames(x), "p\\_q5"), "Std. Error"])
)

hnolintab_q5_maths_drmlt <- mutate(hnolintab_q5_maths_drmlt,
                             up = coef + 1.96 * sd,
                             low = coef - 1.96 * sd)

hnolintab_q5_maths_drmlt <- mutate(hnolintab_q5_maths_drmlt, depvar = "maths")

# hnolintab_q5, dic -------------------------------------------------------

hnolintab_q5_dic_drmlt <- data.frame(
  "covar" = rownames(ct.s13hnolinq5_sD_dic_drmlt) %>%
    (function (x)
      x[str_detect(x, "p\\_q5")]) %>%
    str_remove("p:") %>% str_remove_all("q5score\\_tot\\_") %>% 
    str_remove("_") %>% str_remove(":p$")
)

hnolintab_q5_dic_drmlt <- mutate(
  hnolintab_q5_dic_drmlt,
  indiv = str_remove(covar, "pq[:digit:]") %>% str_remove(":"),
  peer = str_extract(covar, "pq[:digit:]"),
  coef = ct.s13hnolinq5_sD_dic_drmlt %>%
    (function (x)
      x[str_detect(rownames(x), "p\\_q5"), "Estimate"]),
  sd = ct.s13hnolinq5_sD_dic_drmlt %>%
    (function (x)
      x[str_detect(rownames(x), "p\\_q5"), "Std. Error"])
)

hnolintab_q5_dic_drmlt <- mutate(hnolintab_q5_dic_drmlt,
                                 up = coef + 1.96 * sd,
                                 low = coef - 1.96 * sd)

hnolintab_q5_dic_drmlt <- mutate(hnolintab_q5_dic_drmlt, depvar = "dic")

# hnolintab_q5, red -------------------------------------------------------

hnolintab_q5_red_drmlt <- data.frame(
  "covar" = rownames(ct.s13hnolinq5_sD_red_drmlt) %>%
    (function (x)
      x[str_detect(x, "p\\_q5")]) %>%
    str_remove("p:") %>% str_remove_all("q5score\\_tot\\_") %>% 
    str_remove("_") %>% str_remove(":p$")
)

hnolintab_q5_red_drmlt <- mutate(
  hnolintab_q5_red_drmlt,
  indiv = str_remove(covar, "pq[:digit:]") %>% str_remove(":"),
  peer = str_extract(covar, "pq[:digit:]"),
  coef = ct.s13hnolinq5_sD_red_drmlt %>%
    (function (x)
      x[str_detect(rownames(x), "p\\_q5"), "Estimate"]),
  sd = ct.s13hnolinq5_sD_red_drmlt %>%
    (function (x)
      x[str_detect(rownames(x), "p\\_q5"), "Std. Error"])
)

hnolintab_q5_red_drmlt <- mutate(hnolintab_q5_red_drmlt,
                                 up = coef + 1.96 * sd,
                                 low = coef - 1.96 * sd)

hnolintab_q5_red_drmlt <- mutate(hnolintab_q5_red_drmlt, depvar = "red")

# hnolintab_q5, hist -------------------------------------------------------

hnolintab_q5_hist_drmlt <- data.frame(
  "covar" = rownames(ct.s13hnolinq5_sD_hist_drmlt) %>%
    (function (x)
      x[str_detect(x, "p\\_q5")]) %>%
    str_remove("p:") %>% str_remove_all("q5score\\_tot\\_") %>% 
    str_remove("_") %>% str_remove(":p$")
)

hnolintab_q5_hist_drmlt <- mutate(
  hnolintab_q5_hist_drmlt,
  indiv = str_remove(covar, "pq[:digit:]") %>% str_remove(":"),
  peer = str_extract(covar, "pq[:digit:]"),
  coef = ct.s13hnolinq5_sD_hist_drmlt %>%
    (function (x)
      x[str_detect(rownames(x), "p\\_q5"), "Estimate"]),
  sd = ct.s13hnolinq5_sD_hist_drmlt %>%
    (function (x)
      x[str_detect(rownames(x), "p\\_q5"), "Std. Error"])
)

hnolintab_q5_hist_drmlt <- mutate(hnolintab_q5_hist_drmlt,
                                 up = coef + 1.96 * sd,
                                 low = coef - 1.96 * sd)

hnolintab_q5_hist_drmlt <- mutate(hnolintab_q5_hist_drmlt, depvar = "hist")

# Sauvegarde --------------------------------------------------------------

save(hnolintab_q5_drmlt, hnolintab_q5_french_drmlt, hnolintab_q5_maths_drmlt,
     file = here("res_datrestr_mltest", "s13hnolinq5",
                 "hnolintab", "hnolintab_q5_drmlt.rda"), version = 2)
