# pes13hnolinqbcorr. (def). 
  # corr : score_norm en var expl. MAJ2022_02_09.

library(here)
library(tidyverse)
library(plm)
library(lmtest)
library(stringi)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions2.rda")
load(here("01_01_donnees_def.rda"))
load(here("datrestr_2_def", "datrestr_2_def.rda"))
source(here("pedepvars.R"))
source(here("pespecs.R"))
load(here("peclniv.rda"))
source(here("pepeersindivs.R"))
# pedepvars <- pedepvars[str_detect(pedepvars, "_norm")][1] # ok.

# Sur dat et datrestr_2 ---------------------------------------------------

for (k in c("dat", "datrestr_2")) {
for (j in c("sA_s13", "sB_s13", "sC_s13", "sD_s13",
            "sDexo_s13")) {
for (i in pedepvars %>% 
     (function (p) p[str_detect(p, "_norm")])) {
  assign(paste("pes13hnolinqbcorr_", j, "_",
               i,
               "_tous_", k,
               sep = ""),
         plm(as.formula(paste(
           i, " ~ ",
           c(peersindivs_qb, "score_norm",
             get(j)) %>% 
             paste(collapse = " + ")
         )),
         data = get(k), 
         index = "rneconstatses"))
}
}
}

# w_clnivsup --------------------------------------------------------------

  # MAJ2022_02_09 : plus besoin à ce point.

# for (k in c("dat", "datrestr_2")) {
# for (j in c("sA_s13", "sB_s13", "sC_s13", "sD_s13",
#             "sDexo_s13")) {
# for (i in pedepvars %>% 
#      (function (p) p[str_detect(p, "_norm")])) {
#   assign(paste("pes13hnolinqbcorr_", j, "_",
#                i,
#                "_tous_", k, "_wclnivsup",
#                sep = ""),
#          plm(as.formula(paste(
#            i, " ~ ",
#            c(peersindivs_qb, "score_norm",
#              get(j)) %>% 
#              paste(collapse = " + ")
#          )),
#          data = get(k) %>% 
#            (function (d) rbind(datcompl_clniv_sup, d)),
#          index = "rneconstatses"))
# }
# }
# }

# w_clnivinf --------------------------------------------------------------

  # MAJ2022_02_09 : plus besoin à ce point.

# for (k in c("dat", "datrestr_2")) {
# for (j in c("sA_s13", "sB_s13", "sC_s13", "sD_s13",
#             "sDexo_s13")) {
# for (i in pedepvars %>% 
#      (function (p) p[str_detect(p, "_norm")])) {
#   assign(paste("pes13hnolinqbcorr_", j, "_",
#                i,
#                "_tous_", k, "_wclnivinf",
#                sep = ""),
#          plm(as.formula(paste(
#            i, " ~ ",
#            c(peersindivs_qb, "score_norm",
#              get(j)) %>% 
#              paste(collapse = " + ")
#          )),
#          data = get(k) %>% 
#            (function (d) rbind(d, datcompl_clniv_inf)),
#          index = "rneconstatses"))
# }
# }
# }

# Inférence et narsq ------------------------------------------------------

for (i in ls()[str_detect(ls(), "^pes13hnolinqbcorr")]) {
  assign(paste("ct.", i, sep = ""),
         rsearellano(get(i)))
  assign(paste("n.", i, sep = ""), nobs(get(i)))
  assign(paste("arsq.", i, sep = ""), ext_adjrsq(get(i)))
}

# Sauvegarde --------------------------------------------------------------

save(list = ls()[str_detect(ls(), "pes13hnolinqbcorr") & 
                  ! str_detect(ls(), "^pes13hnolinqbcorr")],
     file = here("pes13hnolinqbcorr_def",
                 "pes13hnolinqbcorr.rda"),
     version = 2)
