# hnolintab_q5_npchge_drmlt. 

library(here)
library(tidyverse)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions2.rda") # rappel. 
load(here("res_datrestr_mltest", "s13hnolinq5npchge", "s13hnolinq5npchge_drmlt.rda"))
load(here("res_datrestr_mltest", "s13hnolinq5npchge", "s13hnolinq5npchge_french_drmlt.rda"))
load(here("res_datrestr_mltest", "s13hnolinq5npchge", "s13hnolinq5npchge_maths_drmlt.rda"))

# hnolintab ---------------------------------------------------------------

ct.s13hnolinq5npchge_sF_drmlt %>% (function (x) x[str_detect(rownames(x), "pnp_q5score_tot"), ]) %>% 
  (function (x) {
    rownames(x) <- str_remove(rownames(x), "p:") %>% str_remove(":p$")
    rownames(x) <- ifelse(! str_detect(rownames(x), "^q5"), invcov(rownames(x)), rownames(x)) %>% 
      str_replace("totq", "tot_q")
    x
  }) %>% 
  (function (x) {
    data.frame(covar = str_remove_all(rownames(x), "q5score_tot_") %>% str_remove("pnp_")) %>% 
      mutate(indiv = str_extract(covar, ".*:") %>% str_remove(":"),
             peer = str_extract(covar, ":.*") %>% str_remove(":"),
             coef = x[, 1],
             sd = x[, 2],
             up = coef + 1.96 * sd,
             low = coef - 1.96 * sd,
             depvar = "tot")
  }) -> hnolintab_q5_npchge_drmlt

# hnolintab, french -------------------------------------------------------

ct.s13hnolinq5npchge_sF_french_drmlt %>% (function (x) x[str_detect(rownames(x), "pnp_q5score_tot"), ]) %>% 
  (function (x) {
    rownames(x) <- str_remove(rownames(x), "p:") %>% str_remove(":p$")
    rownames(x) <- ifelse(! str_detect(rownames(x), "^q5"), invcov(rownames(x)), rownames(x)) %>% 
      str_replace("totq", "tot_q")
    x
  }) %>% 
  (function (x) {
    data.frame(covar = str_remove_all(rownames(x), "q5score_tot_") %>% str_remove("pnp_")) %>% 
      mutate(indiv = str_extract(covar, ".*:") %>% str_remove(":"),
             peer = str_extract(covar, ":.*") %>% str_remove(":"),
             coef = x[, 1],
             sd = x[, 2],
             up = coef + 1.96 * sd,
             low = coef - 1.96 * sd,
             depvar = "french")
  }) -> hnolintab_q5_npchge_french_drmlt

# hnolintab, maths -------------------------------------------------------

ct.s13hnolinq5npchge_sF_maths_drmlt %>% (function (x) x[str_detect(rownames(x), "pnp_q5score_tot"), ]) %>% 
  (function (x) {
    rownames(x) <- str_remove(rownames(x), "p:") %>% str_remove(":p$")
    rownames(x) <- ifelse(! str_detect(rownames(x), "^q5"), invcov(rownames(x)), rownames(x)) %>% 
      str_replace("totq", "tot_q")
    x
  }) %>% 
  (function (x) {
    data.frame(covar = str_remove_all(rownames(x), "q5score_tot_") %>% str_remove("pnp_")) %>% 
      mutate(indiv = str_extract(covar, ".*:") %>% str_remove(":"),
             peer = str_extract(covar, ":.*") %>% str_remove(":"),
             coef = x[, 1],
             sd = x[, 2],
             up = coef + 1.96 * sd,
             low = coef - 1.96 * sd,
             depvar = "maths")
  }) -> hnolintab_q5_npchge_maths_drmlt

# Sauvegarde --------------------------------------------------------------

save(list = ls()[str_detect(ls(), "^hnolintab")], file = here("res_datrestr_mltest", "s13hnolinq5npchge", "hnolintab", "hnolintab_q5_npchge_drmlt.rda"),
     version = 2)
