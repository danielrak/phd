# pegiv_cm2 (def). 
  # Version G2SLS de pepcml_cm2.R. 
  # On ne fait pas les régressions pour les sous-items. 
  # Sans 2009 car trop peu de covars. 
  # sexe et pcs seuls covars exo. 

library(here)
library(tidyverse)
library(ivpack)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions2.rda")
load("D:/00_phd/02_rectorat/04_resultats/41_age/01_donnees_def.rda")

c101112 <- filter(c, cohorte != "2009")

cov2 <- c("sexe_M", 
          "pcsR_Moy", "pcsR_Fav","pcsR_Tresfav", 
          "pcsR_Autres", "pcsR_NA")

cov3 <- c(cov2, "age_abs")

cov4 <- c(cov3, "position_Heure", "position_Avance")

# pegiv_cm2 ---------------------------------------------------------------

for (j in c("cov2", "cov3", "cov4")) {
  for (i in c("score_norm", "score_f_norm", "score_m_norm")) {
    assign(paste("pegiv_", j, "_", i, "_tous_c101112", sep = ""),
           bealer6(data = c101112, depvar = i, covars = get(j),
                   group = "classecoh"))
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
     file = here("pegiv_cm2_def", "pegiv_cm2.rda"),
     version = 2)
