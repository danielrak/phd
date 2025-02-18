# pegiv (def). 
  # Version G2SLS de pepcml_cm2.R. 
  # On ne fait pas les régressions pour les sous-items. 
  # Sans 2009 car trop peu de covars. 
  # sexe et pcs seuls covars exo. 

library(here)
library(tidyverse)
library(ivpack)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions2.rda")
load(here("01_01_datcompl_def.rda"))
source(here("pedepvars.R"))

# Filtre : sans cls_prob --------------------------------------------------

  # cls_prob : une classe avec quasiment que des NA de y. 

cls_prob <- group_by(datcompl, divconstatrneses) %>% 
  filter(sum(is.na(moy_ec1)) > 20) %>% pull(divconstatrneses) %>% 
  unique

datcompl <- filter(datcompl, divconstatrneses != cls_prob)

# pepcmlcovars ------------------------------------------------------------

cov2 <- c("sexe_mod_M", 
          "pcs_reg_mod_Moy", "pcs_reg_mod_Fav", 
          "pcs_reg_mod_Tresfav", "pcs_reg_mod_Autres")
cov3 <- c(cov2, "lregime_constat_g_int", "lregime_constat_g_ext")
cov4 <- c(cov3, "age_absdnb")
cov5 <- c(cov4, "positiondnb2_Heure", "positiondnb2_Avance")

cov5_noregime <- cov5[! str_detect(cov5, "lregime")]

datcompl_PR <- filter(datcompl, statut_constat == "PR")
datcompl_PU <- filter(datcompl, statut_constat == "PU")
datcompl_PUhep <- filter(datcompl, statut_constat == "PU" & 
                           reseauR_mod == 0)
datcompl_PUep <- filter(datcompl, statut_constat == "PU" & 
                          reseauR_mod == 1)

# pegiv pour datcompl_PR --------------------------------------------------

  # cas exceptionnel : pas de lregime int. 
for (k in c("datcompl_PR")) {
for (j in c("cov2", "cov3", "cov4", "cov5", 
            "cov5_noregime")[2]) {
  for (i in pedepvars[1]) {
    assign(paste("pegiv_", j, "_", i, "_tous_", k, sep = ""),
           bealer6(data = get(k) %>% 
                     filter(! is.na(eval(
                       parse(text = i)
                     ))) %>% 
                     group_by(divconstatrneses) %>% 
                     filter(n() > 1) %>% ungroup, 
                   depvar = i, covars = get(j) %>% 
                     (function (g) g[g != "lregime_constat_g_int"]),
                   group = "divconstatrneses"))
  }
}
}

# pegiv -------------------------------------------------------------------

for (k in c("datcompl_PU",
            "datcompl_PUhep",
            "datcompl_PUep"
            )) {
for (j in c("cov2", "cov3", "cov4", "cov5",
            "cov5_noregime")) {
  for (i in pedepvars) {
    assign(paste("pegiv_", j, "_", i, "_tous_", k, sep = ""),
           bealer6(data = get(k) %>% 
                     filter(! is.na(eval(
                       parse(text = i)
                     ))) %>% 
                     group_by(divconstatrneses) %>% 
                     filter(n() > 1) %>% ungroup, 
                   depvar = i, covars = get(j),
                   group = "divconstatrneses"))
  }
}
}

# Inférence et autres éléments utiles -------------------------------------

for (i in ls()[str_detect(ls(), "^pegiv")]) {
  
  # pe. 
  assign(paste("ct.pe.", i, sep = ""),
         rsearellano(get(i)$pe))
  assign(paste("f.pe.", i, sep = ""),
         get(i)$pe %>% summary %>% (function (s) s$fstatistic) %>% 
           (function (f) c(f[1], # valeur
                          pf(f[1], f[2], f[3], lower.tail = FALSE)) %>% 
              setNames(c("fstat", "pval"))))
  assign(paste("n.pe.", i, sep = ""), nobs(get(i)$pe))
  assign(paste("arsq.pe.", i, sep = ""), 
         get(i)$pe %>% ext_adjrsq)
  
  # e1. 
  assign(paste("ct.e1.", i, sep = ""),
         robust.se(get(i)$e1))
  assign(paste("fweak.e1.", i, sep = ""),
         get(i)$e1 %>% summary(diagnostics = TRUE) %>% 
           (function (s) s$diagnostics) %>% 
           (function (d) d["Weak instruments", "statistic"]))
  assign(paste("pvalweak.e1.", i, sep = ""),
         get(i)$e1 %>% summary(diagnostics = TRUE) %>% 
           (function (s) s$diagnostics) %>% 
           (function (d) d["Weak instruments", "p-value"]))
  assign(paste("fendo.e1.", i, sep = ""), # test endo Hausman. 
         get(i)$e1 %>% summary(diagnostics = TRUE) %>% 
           (function (s) s$diagnostics) %>% 
           (function (d) d["Wu-Hausman", "statistic"]))
  assign(paste("pvalendo.e1.", i, sep = ""),
         get(i)$e1 %>% summary(diagnostics = TRUE) %>% 
           (function (s) s$diagnostics) %>% 
           (function (d) d["Wu-Hausman", "p-value"]))
  assign(paste("foverident.e1.", i, sep = ""), # test endo Hausman. 
         get(i)$e1 %>% summary(diagnostics = TRUE) %>% 
           (function (s) s$diagnostics) %>% 
           (function (d) d["Sargan", "statistic"]))
  assign(paste("pvaloverident.e1.", i, sep = ""),
         get(i)$e1 %>% summary(diagnostics = TRUE) %>% 
           (function (s) s$diagnostics) %>% 
           (function (d) d["Sargan", "p-value"]))
  assign(paste("n.e1.", i, sep = ""),
         nobs(get(i)$e1))
  # pas de arsq puisque c'est un iv. 
  
  # pe2. 
  assign(paste("ct.pe2.", i, sep = ""),
         rsearellano(get(i)$pe2))
  assign(paste("f.pe2.", i, sep = ""),
         get(i)$pe2 %>% summary %>% (function (s) s$fstatistic) %>% 
           (function (f) c(f[1], # valeur
                          pf(f[1], f[2], f[3], lower.tail = FALSE)) %>% 
              setNames(c("fstat", "pval"))))
  assign(paste("n.pe2.", i, sep = ""), nobs(get(i)$pe2))
  assign(paste("arsq.pe2.", i, sep = ""), 
         get(i)$pe2 %>% ext_adjrsq)
  
  # e2. 
  assign(paste("ct.e2.", i, sep = ""),
         robust.se(get(i)$e2))
  assign(paste("fweak.e2.", i, sep = ""),
         get(i)$e2 %>% summary(diagnostics = TRUE) %>% 
           (function (s) s$diagnostics) %>% 
           (function (d) d["Weak instruments", "statistic"]))
  assign(paste("pvalweak.e2.", i, sep = ""),
         get(i)$e2 %>% summary(diagnostics = TRUE) %>% 
           (function (s) s$diagnostics) %>% 
           (function (d) d["Weak instruments", "p-value"]))
  assign(paste("fendo.e2.", i, sep = ""), # test endo Hausman. 
         get(i)$e2 %>% summary(diagnostics = TRUE) %>% 
           (function (s) s$diagnostics) %>% 
           (function (d) d["Wu-Hausman", "statistic"]))
  assign(paste("pvalendo.e2.", i, sep = ""),
         get(i)$e2 %>% summary(diagnostics = TRUE) %>% 
           (function (s) s$diagnostics) %>% 
           (function (d) d["Wu-Hausman", "p-value"]))
  assign(paste("foverident.e2.", i, sep = ""), # test endo Hausman. 
         get(i)$e2 %>% summary(diagnostics = TRUE) %>% 
           (function (s) s$diagnostics) %>% 
           (function (d) d["Sargan", "statistic"]))
  assign(paste("pvaloverident.e2.", i, sep = ""),
         get(i)$e2 %>% summary(diagnostics = TRUE) %>% 
           (function (s) s$diagnostics) %>% 
           (function (d) d["Sargan", "p-value"]))
  assign(paste("n.e2.", i, sep = ""),
         nobs(get(i)$e2))
  # pas de arsq puisque c'est un iv. 
  
}

# Sauvegarde --------------------------------------------------------------

save(list = ls()[str_detect(ls(), "pegiv") & 
                   ! str_detect(ls(), "^pegiv")],
     file = here("pegiv_def", "pegiv_statutreseau.rda"),
     version = 2)
