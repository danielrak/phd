# pes13hnolinq20graph. 
  # Peut-être pas besoin de mettre dans tabfig. 

library(here)
library(tidyverse)
library(plm)
library(lmtest)
library(stringi)

# Chargement --------------------------------------------------------------

load(here("pes13hnolinq20_def", "pes13hnolinq20.rda"))

# tab ---------------------------------------------------------------------

pemodelshnolinq20_tab <- 
ls()[str_detect(ls(), "^ct\\.pes13hnolinq20") & 
       ! str_detect(ls(), "sexe\\_mod|pcs\\_reg\\_mod") & 
       ! str_detect(ls(), "np")] %>% 
  (function (l) {
    data.frame(obj = l,
               spec = 
                 str_extract(l, 
                             paste(c("sA_", "sB_", "sC_",
                                     "sD_", "sDexo_") %>% 
                                     sapply(
                                       function (x) 
                                         paste(x, c("ols", "s13"),
                                               sep = "")
                                     ), collapse = "|")),
               depvar = str_extract(l, "moy_.*_norm"),
               data = 
                 str_extract(l, 
                             paste(c("dat$", 
                                     "dat_wclnivsup$",
                                     "dat_wclnivinf$",
                                     "datrestr_2$",
                                     "datrestr_2_wclnivsup$",
                                     "datrestr_2_wclnivinf$"),
                                   collapse = "|")))
  }) %>% as_tibble %>% 
  (function (d) split(d, d$obj)) %>% 
  lapply(function (d2) {
    data.frame(obj = d2$obj, 
               spec = d2$spec,
               depvar = d2$depvar,
               data = d2$data,
               covar = rownames(get(d2$obj)) %>% 
                 (function (r) r[str_detect(r, "p\\_") & 
                                   str_detect(r, "q20score\\_")])) %>% 
      mutate(indiv = str_extract(covar, 
                                 "p:q20score\\_q[:digit:]{1,2}") %>% 
               str_remove("p:q20score_"),
             peer = str_extract(covar, 
                                "p\\_q20score\\_q[:digit:]{1,2}") %>% 
               str_remove("\\_q20score\\_")) %>% 
      (function (d3) 
        mutate(d3, 
               coef = apply(d3, 1, function (x) 
                 get(x["obj"])[x["covar"], 1]),
               coef = coef / 10,
               sd = apply(d3, 1, function (x) 
                 get(x["obj"])[x["covar"], 2]),
               sd = sd / 10)) %>% 
      mutate(up = coef + 1.96 * sd,
             low = coef - 1.96 * sd)
  }) %>% do.call(what = rbind) %>% as_tibble %>% 
  mutate(indiv = factor(indiv, 
                        levels = paste("q", 1:20, sep = "")),
         peer = factor(peer, 
                        levels = paste("pq", (1:20)[-10],
                                       sep = "")))

# graph -------------------------------------------------------------------

pemodelshnolinq20_sDexo_graph <- 
  filter(pemodelshnolinq20_tab, 
         data == "datrestr_2" & 
           spec %in% c("sDexo_s13") & 
           depvar %in% c("moy_ec1_norm",
                         "moy_fran_ec_norm",
                         "moy_maths_ec_norm")) %>% 
  ggplot(aes(peer, coef)) + 
  geom_point() + 
  geom_line(aes(group = indiv)) + 
  geom_errorbar(aes(ymin = low, ymax = up), width = .1) + 
  geom_hline(aes(yintercept = 0), 
             color = "red", linetype = "dashed") + 
  facet_grid(depvar ~ indiv)
  # On n'y voit rien. 
