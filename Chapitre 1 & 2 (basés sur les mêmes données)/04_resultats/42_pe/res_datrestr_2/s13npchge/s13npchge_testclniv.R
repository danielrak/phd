# s13npchge_testclniv, tot, dr2. 

library(here)
source("s13_libraries.R")

rm(list = ls())

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions.rda")
load(here("res_datrestr_2", "01_datrestr_2.rda"))
load(here("01_datcompl.rda"))

# Repérage clniv ----------------------------------------------------------

clniv_sup <- group_by(datcompl, divconstatrneses) %>%
  summarise(c = paste(sort(unique(niveau_section)), collapse = " ET ")) %>%
  filter(c == "sup") %>% pull(divconstatrneses)
# 61 classes-années : 23 en 2014, 16 en 2015 et 22 en 2016.
# vérifié (avec clefgestion_constat) : ce sont les bons.

clniv_inf <- group_by(datcompl, divconstatrneses) %>%
  summarise(c = paste(sort(unique(niveau_section)), collapse = " ET ")) %>%
  filter(c == "inf") %>% pull(divconstatrneses)
# 248 classes-années : 85 en 2014 82 en 2015 et 81 en 2016.

# Préparation -------------------------------------------------------------

depvar <- "moy_ec1"
peers <- "pnp_score : p + pop_score : p"
indiv <- "v_ob_tot"
source("s13_specs_chge.R")

# testclniv ---------------------------------------------------------------

s13npchge_sF_wclnivsup_o1_dr2 <- plm(as.formula(paste(depvar, " ~ ", peers, " + ", indiv, " + ", sD_chge %>% paste(collapse = " + "), sep = "")),
                                     data = rbind(select(datrestr_2, - ncl), filter(datcompl, divconstatrneses %in% clniv_sup)), subset = obs == 1, 
                                     index = "rneconstatses")

s13npchge_sF_wclnivinf_o1_dr2 <- plm(as.formula(paste(depvar, " ~ ", peers, " + ", indiv, " + ", sD_chge %>% paste(collapse = " + "), sep = "")),
                                     data = rbind(select(datrestr_2, - ncl), filter(datcompl, divconstatrneses %in% clniv_inf)), subset = obs == 1, 
                                     index = "rneconstatses")

# Inférence et narsq ------------------------------------------------------

for (i in ls()[str_detect(ls(), "^s13")]) {
  assign(paste("ct.", i, sep = ""),
         rsearellano(get(i)))
  assign(paste("n.", i, sep = ""), nobs(get(i)))
  assign(paste("arsq.", i, sep = ""), summary(get(i))$r.squared["adjrsq"])
}

# Sauvegarde --------------------------------------------------------------

save(list = ls()[str_detect(ls(), "^ct\\.|^n\\.|^arsq\\.")], 
     file = here("res_datrestr_2", "s13npchge", 
                 "s13npchge_testclniv_o1_dr2.rda"), version = 2)
