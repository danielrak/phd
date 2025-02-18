# tests near. 

library(tidyverse)
library(here)
library(plm)
library(lmtest)

load(here("res_datrestr_2", "01_datrestr_2.rda"))

split(datrestr_2, datrestr_2$rneconstatses) %>% 
  sapply(function (x) filter(x, q5score_tot == "q1")[["p_q5score_tot_q5"]] %>% 
           mean) %>% (function (x) x[near(x, .2, .01)]) %>% 
  names %>% (function (x) filter(datrestr_2, rneconstatses %in% x)) -> datrestr_near
  # 1800 obs. 

# Estimation

depvar <- "moy_ec1"
indivs <- paste("q5score_tot_q", (1:5)[- 3], sep = "")
peersindivs <- paste("p_q5score_tot_q", (1:5)[- 3], " : p : ", 
                     sep = "") %>% 
  (function (p) sapply(paste("q5score_tot_q", 1:5, sep = ""), 
                       function (x) paste(p, x, sep = "")) %>% 
     as.vector %>% sort)
sA <- "p"
sB <- c(sA, "sexe_mod_M",
        "pcs_reg_mod_Moy", "pcs_reg_mod_Fav", "pcs_reg_mod_Tresfav",
        "pcs_reg_mod_Autres",
        "lregime_constat_g_int", "lregime_constat_g_ext",
        "age_absdnb", 
        "positiondnb2_Heure", "positiondnb2_Avance")
sC <- c(sB, "tdivconstatrneses")
sD <-c(sC, 
       "p_sexe_mod_M", 
       "p_pcs_reg_mod_Moy", "p_pcs_reg_mod_Fav", 
       "p_pcs_reg_mod_Tresfav", "p_pcs_reg_mod_Autres",
       "p_lregime_constat_g_int", "p_lregime_constat_g_ext",
       "p_age_absdnb", 
       "p_positiondnb2_Heure", "p_positiondnb2_Avance")

s13hnolinq5_sD_drnear <- 
  plm(as.formula(paste(depvar, " ~ ", 
                       indivs %>% paste(collapse = " + "), " + ", 
                       peersindivs %>% paste(collapse = " + "), " + ",
                       sD %>% paste(collapse = " + "),
                       sep = "")),
      data = datrestr_near, index = "rneconstatses")

ct.s13hnolinq5_sD_drnear <- coeftest(s13hnolinq5_sD_drnear,
                                   vcov = vcovHC(s13hnolinq5_sD_drnear))

hnolintab_q5_drnear <- data.frame(
  "covar" = rownames(ct.s13hnolinq5_sD_drnear) %>%
    (function (x)
      x[str_detect(x, "p\\_q5")]) %>%
    str_remove("p:") %>% str_remove_all("q5score\\_tot\\_") %>% 
    str_remove("_") %>% str_remove(":p$")
)

hnolintab_q5_drnear <- mutate(
  hnolintab_q5_drnear,
  indiv = str_remove(covar, "pq[:digit:]") %>% str_remove(":"),
  peer = str_extract(covar, "pq[:digit:]"),
  coef = ct.s13hnolinq5_sD_drnear %>%
    (function (x)
      x[str_detect(rownames(x), "p\\_q5"), "Estimate"]),
  sd = ct.s13hnolinq5_sD_drnear %>%
    (function (x)
      x[str_detect(rownames(x), "p\\_q5"), "Std. Error"])
)

hnolintab_q5_drnear <- mutate(hnolintab_q5_drnear,
                            up = coef + 1.96 * sd,
                            low = coef - 1.96 * sd)

hnolintab_q5_drnear <- mutate(hnolintab_q5_drnear, depvar = "tot")
# on retrouve tjr les mêmes résultats. 

# french. 
s13hnolinq5_sD_french_drnear <- update(s13hnolinq5_sD_drnear, 
                                     moy_fran_ec ~ .)
ct.s13hnolinq5_sD_french_drnear <- coeftest(s13hnolinq5_sD_french_drnear,
                                          vcov = vcovHC(s13hnolinq5_sD_french_drnear))

hnolintab_q5_french_drnear <- data.frame(
  covar = rownames(ct.s13hnolinq5_sD_french_drnear) %>%
    (function (x)
      x[str_detect(x, "p\\_q5")]) %>%
    str_remove("p:") %>% str_remove_all("q5score\\_tot\\_") %>% 
    str_remove("_") %>% str_remove(":p$")) %>% 
  mutate(
    indiv = str_remove(covar, "pq[:digit:]") %>% str_remove(":"),
    peer = str_extract(covar, "pq[:digit:]"),
    coef = ct.s13hnolinq5_sD_french_drnear %>%
      (function (x)
        x[str_detect(rownames(x), "p\\_q5"), "Estimate"]),
    sd = ct.s13hnolinq5_sD_french_drnear %>%
      (function (x)
        x[str_detect(rownames(x), "p\\_q5"), "Std. Error"]),
    up = coef + 1.96 * sd,
    low = coef - 1.96 * sd,
    depvar = "french"
  )


# maths.
s13hnolinq5_sD_maths_drnear <- update(s13hnolinq5_sD_drnear, 
                                    moy_maths_ec ~ .)
ct.s13hnolinq5_sD_maths_drnear <- coeftest(s13hnolinq5_sD_maths_drnear,
                                         vcov = vcovHC(s13hnolinq5_sD_maths_drnear))

hnolintab_q5_maths_drnear <- data.frame(
  "covar" = rownames(ct.s13hnolinq5_sD_maths_drnear) %>%
    (function (x)
      x[str_detect(x, "p\\_q5")]) %>%
    str_remove("p:") %>% str_remove_all("q5score\\_tot\\_") %>% 
    str_remove("_") %>% str_remove(":p$")) %>% 
  mutate(
    indiv = str_remove(covar, "pq[:digit:]") %>% str_remove(":"),
    peer = str_extract(covar, "pq[:digit:]"),
    coef = ct.s13hnolinq5_sD_maths_drnear %>%
      (function (x)
        x[str_detect(rownames(x), "p\\_q5"), "Estimate"]),
    sd = ct.s13hnolinq5_sD_maths_drnear %>%
      (function (x)
        x[str_detect(rownames(x), "p\\_q5"), "Std. Error"]),
    up = coef + 1.96 * sd,
    low = coef - 1.96 * sd,
    depvar = "maths"
  )


rbind(hnolintab_q5_drnear, 
      hnolintab_q5_french_drnear, 
      hnolintab_q5_maths_drnear) %>% 
  ggplot(aes(peer, coef)) + geom_point() + geom_line(aes(group = indiv)) + 
  facet_grid(depvar ~ indiv) + geom_errorbar(aes(ymin = low, ymax = up),
                                             width = .1) + 
  geom_hline(aes(yintercept = 0), color = "red", linetype = "dashed") + 
  theme_bw()
