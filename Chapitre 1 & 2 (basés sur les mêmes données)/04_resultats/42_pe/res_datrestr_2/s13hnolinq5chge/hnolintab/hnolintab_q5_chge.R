# hnolintab_q5_chge_dr2. 

library(here)
library(tidyverse)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions2.rda") # rappel. 
load(here("res_datrestr_2", "s13hnolinq5chge", "s13hnolinq5chge_dr2.rda"))
load(here("res_datrestr_2", "s13hnolinq5chge", "s13hnolinq5chge_french_dr2.rda"))
load(here("res_datrestr_2", "s13hnolinq5chge", "s13hnolinq5chge_maths_dr2.rda"))

# hnolintab ---------------------------------------------------------------

ct.s13hnolinq5chge_sD_dr2 %>% (function (x) x[str_detect(rownames(x), "p_q5score_tot"), ]) %>% 
  (function (x) {
    rownames(x) <- str_remove(rownames(x), "p:") %>% str_remove(":p$")
    rownames(x) <- ifelse(! str_detect(rownames(x), "^q5"), invcov(rownames(x)), rownames(x))
    x
  }) %>% 
  (function (x) {
    data.frame(covar = str_remove_all(rownames(x), "q5score_tot_") %>% 
                 str_remove("_")) %>% 
      mutate(indiv = str_extract(covar, ".*:") %>% str_remove(":"),
             peer = str_extract(covar, ":.*") %>% str_remove(":"),
             coef = x[, 1],
             sd = x[, 2],
             up = coef + 1.96 * sd,
             low = coef - 1.96 * sd,
             depvar = "tot")
  }) -> hnolintab_q5_chge_dr2

# hnolintab, french -------------------------------------------------------

ct.s13hnolinq5chge_sD_french_dr2 %>% (function (x) x[str_detect(rownames(x), "p_q5score_tot"), ]) %>% 
  (function (x) {
    rownames(x) <- str_remove(rownames(x), "p:") %>% str_remove(":p$")
    rownames(x) <- ifelse(! str_detect(rownames(x), "^q5"), invcov(rownames(x)), rownames(x))
    x
  }) %>% 
  (function (x) {
    data.frame(covar = str_remove_all(rownames(x), "q5score_tot_") %>% 
                 str_remove("_")) %>% 
      mutate(indiv = str_extract(covar, ".*:") %>% str_remove(":"),
             peer = str_extract(covar, ":.*") %>% str_remove(":"),
             coef = x[, 1],
             sd = x[, 2],
             up = coef + 1.96 * sd,
             low = coef - 1.96 * sd,
             depvar = "french")
  }) -> hnolintab_q5_chge_french_dr2

# hnolintab, maths -------------------------------------------------------

ct.s13hnolinq5chge_sD_maths_dr2 %>% (function (x) x[str_detect(rownames(x), "p_q5score_tot"), ]) %>% 
  (function (x) {
    rownames(x) <- str_remove(rownames(x), "p:") %>% str_remove(":p$")
    rownames(x) <- ifelse(! str_detect(rownames(x), "^q5"), invcov(rownames(x)), rownames(x))
    x
  }) %>% 
  (function (x) {
    data.frame(covar = str_remove_all(rownames(x), "q5score_tot_") %>% 
                 str_remove("_")) %>% 
      mutate(indiv = str_extract(covar, ".*:") %>% str_remove(":"),
             peer = str_extract(covar, ":.*") %>% str_remove(":"),
             coef = x[, 1],
             sd = x[, 2],
             up = coef + 1.96 * sd,
             low = coef - 1.96 * sd,
             depvar = "maths")
  }) -> hnolintab_q5_chge_maths_dr2


# Sauvegarde --------------------------------------------------------------

save(list = ls()[str_detect(ls(), "^hnolintab")], file = here("res_datrestr_2", "s13hnolinq5chge", "hnolintab", "hnolintab_q5_chge_dr2.rda"),
     version = 2)