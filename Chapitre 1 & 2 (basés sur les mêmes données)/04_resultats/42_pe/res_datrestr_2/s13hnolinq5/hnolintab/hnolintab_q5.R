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
rda.files <- list.files(here("res_datrestr_2", "s13hnolinq5")) %>% 
  (function (l) l[str_detect(l, "\\.rda$")])
for (i in rda.files) load(here("res_datrestr_2", "s13hnolinq5", i))

# tot ---------------------------------------------------------------------

hnolintab_q5_dr2 <- data.frame(
  "covar" = rownames(ct.s13hnolinq5_sD_dr2) %>%
    (function (x)
      x[str_detect(x, "p\\_q5")]) %>%
    str_remove("p:") %>% str_remove_all("q5score\\_tot\\_") %>% 
    str_remove("_") %>% str_remove(":p$")
)

hnolintab_q5_dr2 <- mutate(
  hnolintab_q5_dr2,
  indiv = str_remove(covar, "pq[:digit:]") %>% str_remove(":"),
  peer = str_extract(covar, "pq[:digit:]"),
  coef = ct.s13hnolinq5_sD_dr2 %>%
    (function (x)
      x[str_detect(rownames(x), "p\\_q5"), "Estimate"]),
  sd = ct.s13hnolinq5_sD_dr2 %>%
    (function (x)
      x[str_detect(rownames(x), "p\\_q5"), "Std. Error"])
)

hnolintab_q5_dr2 <- mutate(hnolintab_q5_dr2,
                       up = coef + 1.96 * sd,
                       low = coef - 1.96 * sd)

hnolintab_q5_dr2 <- mutate(hnolintab_q5_dr2, depvar = "tot")

# hnolintab_q5, french -------------------------------------------------------

hnolintab_q5_french_dr2 <- data.frame(
  "covar" = rownames(ct.s13hnolinq5_sD_french_dr2) %>%
    (function (x)
      x[str_detect(x, "p\\_q5")]) %>%
    str_remove("p:") %>% str_remove_all("q5score\\_tot\\_") %>% 
    str_remove("_") %>% str_remove(":p$")
)

hnolintab_q5_french_dr2 <- mutate(
  hnolintab_q5_french_dr2,
  indiv = str_remove(covar, "pq[:digit:]") %>% str_remove(":"),
  peer = str_extract(covar, "pq[:digit:]"),
  coef = ct.s13hnolinq5_sD_french_dr2 %>%
    (function (x)
      x[str_detect(rownames(x), "p\\_q5"), "Estimate"]),
  sd = ct.s13hnolinq5_sD_french_dr2 %>%
    (function (x)
      x[str_detect(rownames(x), "p\\_q5"), "Std. Error"])
)

hnolintab_q5_french_dr2 <- mutate(hnolintab_q5_french_dr2,
                              up = coef + 1.96 * sd,
                              low = coef - 1.96 * sd)

hnolintab_q5_french_dr2 <- mutate(hnolintab_q5_french_dr2, depvar = "french")

# hnolintab_q5, maths -------------------------------------------------------

hnolintab_q5_maths_dr2 <- data.frame(
  "covar" = rownames(ct.s13hnolinq5_sD_maths_dr2) %>%
    (function (x)
      x[str_detect(x, "p\\_q5")]) %>%
    str_remove("p:") %>% str_remove_all("q5score\\_tot\\_") %>% 
    str_remove("_") %>% str_remove(":p$")
)

hnolintab_q5_maths_dr2 <- mutate(
  hnolintab_q5_maths_dr2,
  indiv = str_remove(covar, "pq[:digit:]") %>% str_remove(":"),
  peer = str_extract(covar, "pq[:digit:]"),
  coef = ct.s13hnolinq5_sD_maths_dr2 %>%
    (function (x)
      x[str_detect(rownames(x), "p\\_q5"), "Estimate"]),
  sd = ct.s13hnolinq5_sD_maths_dr2 %>%
    (function (x)
      x[str_detect(rownames(x), "p\\_q5"), "Std. Error"])
)

hnolintab_q5_maths_dr2 <- mutate(hnolintab_q5_maths_dr2,
                             up = coef + 1.96 * sd,
                             low = coef - 1.96 * sd)

hnolintab_q5_maths_dr2 <- mutate(hnolintab_q5_maths_dr2, depvar = "maths")

# hnolintab_q5, dic -------------------------------------------------------

hnolintab_q5_dic_dr2 <- data.frame(
  "covar" = rownames(ct.s13hnolinq5_sD_dic_dr2) %>%
    (function (x)
      x[str_detect(x, "p\\_q5")]) %>%
    str_remove("p:") %>% str_remove_all("q5score\\_tot\\_") %>% 
    str_remove("_") %>% str_remove(":p$")
)

hnolintab_q5_dic_dr2 <- mutate(
  hnolintab_q5_dic_dr2,
  indiv = str_remove(covar, "pq[:digit:]") %>% str_remove(":"),
  peer = str_extract(covar, "pq[:digit:]"),
  coef = ct.s13hnolinq5_sD_dic_dr2 %>%
    (function (x)
      x[str_detect(rownames(x), "p\\_q5"), "Estimate"]),
  sd = ct.s13hnolinq5_sD_dic_dr2 %>%
    (function (x)
      x[str_detect(rownames(x), "p\\_q5"), "Std. Error"])
)

hnolintab_q5_dic_dr2 <- mutate(hnolintab_q5_dic_dr2,
                                 up = coef + 1.96 * sd,
                                 low = coef - 1.96 * sd)

hnolintab_q5_dic_dr2 <- mutate(hnolintab_q5_dic_dr2, depvar = "dic")

# hnolintab_q5, red -------------------------------------------------------

hnolintab_q5_red_dr2 <- data.frame(
  "covar" = rownames(ct.s13hnolinq5_sD_red_dr2) %>%
    (function (x)
      x[str_detect(x, "p\\_q5")]) %>%
    str_remove("p:") %>% str_remove_all("q5score\\_tot\\_") %>% 
    str_remove("_") %>% str_remove(":p$")
)

hnolintab_q5_red_dr2 <- mutate(
  hnolintab_q5_red_dr2,
  indiv = str_remove(covar, "pq[:digit:]") %>% str_remove(":"),
  peer = str_extract(covar, "pq[:digit:]"),
  coef = ct.s13hnolinq5_sD_red_dr2 %>%
    (function (x)
      x[str_detect(rownames(x), "p\\_q5"), "Estimate"]),
  sd = ct.s13hnolinq5_sD_red_dr2 %>%
    (function (x)
      x[str_detect(rownames(x), "p\\_q5"), "Std. Error"])
)

hnolintab_q5_red_dr2 <- mutate(hnolintab_q5_red_dr2,
                                 up = coef + 1.96 * sd,
                                 low = coef - 1.96 * sd)

hnolintab_q5_red_dr2 <- mutate(hnolintab_q5_red_dr2, depvar = "red")

# hnolintab_q5, hist -------------------------------------------------------

hnolintab_q5_hist_dr2 <- data.frame(
  "covar" = rownames(ct.s13hnolinq5_sD_hist_dr2) %>%
    (function (x)
      x[str_detect(x, "p\\_q5")]) %>%
    str_remove("p:") %>% str_remove_all("q5score\\_tot\\_") %>% 
    str_remove("_") %>% str_remove(":p$")
)

hnolintab_q5_hist_dr2 <- mutate(
  hnolintab_q5_hist_dr2,
  indiv = str_remove(covar, "pq[:digit:]") %>% str_remove(":"),
  peer = str_extract(covar, "pq[:digit:]"),
  coef = ct.s13hnolinq5_sD_hist_dr2 %>%
    (function (x)
      x[str_detect(rownames(x), "p\\_q5"), "Estimate"]),
  sd = ct.s13hnolinq5_sD_hist_dr2 %>%
    (function (x)
      x[str_detect(rownames(x), "p\\_q5"), "Std. Error"])
)

hnolintab_q5_hist_dr2 <- mutate(hnolintab_q5_hist_dr2,
                                 up = coef + 1.96 * sd,
                                 low = coef - 1.96 * sd)

hnolintab_q5_hist_dr2 <- mutate(hnolintab_q5_hist_dr2, depvar = "hist")

# Sauvegarde --------------------------------------------------------------

save(hnolintab_q5_dr2, hnolintab_q5_french_dr2, hnolintab_q5_maths_dr2,
     file = here("res_datrestr_2", "s13hnolinq5",
                 "hnolintab", "hnolintab_q5_dr2.rda"), version = 2)
