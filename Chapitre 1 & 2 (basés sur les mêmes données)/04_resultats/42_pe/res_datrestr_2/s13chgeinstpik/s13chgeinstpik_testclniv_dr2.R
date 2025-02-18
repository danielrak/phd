# s13chgeinstpik_testclniv, tot, o1, dr2. 

library(here)
source("s13_libraries.R")
rm(list = ls())

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions2.rda")
load("01_datcompl.rda")
load(here("res_datrestr_2", "01_datrestr_2.rda"))

# Repérage des clniv ------------------------------------------------------

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
peers <- "p_v_ob_tot : p"
indiv <- "v_ob_tot"

source("s13chgeinstpik_specs.R")

# testclniv ---------------------------------------------------------------

s13chgeinspik_sD_wclnivsup_o1_dr2 <- plm(as.formula(paste(depvar, " ~ ", peers, " + ", indiv, " + ", sD_chgeinstpik %>% paste(collapse = " + "), sep  = "")), 
                               data = rbind(select(datrestr_2, - ncl), filter(datcompl, divconstatrneses %in% clniv_sup)), 
                               subset = obs == 1, index = "rneconstat")

s13chgeinspik_sD_wclnivinf_o1_dr2 <- plm(as.formula(paste(depvar, " ~ ", peers, " + ", indiv, " + ", sD_chgeinstpik %>% paste(collapse = " + "), sep  = "")), 
                                         data = rbind(select(datrestr_2, - ncl), filter(datcompl, divconstatrneses %in% clniv_inf)), 
                                         subset = obs == 1, index = "rneconstat")

# Inférence et narsq ------------------------------------------------------

for (i in ls()[str_detect(ls(), "^s13chgeinstpik")]) {
  assign(paste("ct.", i, sep = ""),
         rsearellano(get(i)))
  assign(paste("n.", i, sep = ""), nobs(get(i)))
  assign(paste("arsq.", i, sep = ""), summary(get(i))$r.squared["adjrsq"])
}

# Sauvegarde --------------------------------------------------------------

save(list = ls()[str_detect(ls(), "^ct\\.|^n\\.|^arsq\\.")], 
     file = here("res_datrestr_2", "s13chgeinstpik", 
                 "s13chgeinstpik_testclniv_o1_dr2.rda"), version = 2)
