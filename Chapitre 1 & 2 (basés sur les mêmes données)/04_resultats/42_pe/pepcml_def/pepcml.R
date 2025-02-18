# pepcml (def). 
  # Effets endogènes, PCML. 
  # Il faut sauvegarder d'abord les formules. 

library(here)
library(tidyverse)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions2.rda")
load(here("01_01_datcompl_def.rda"))
# load(here("01_01_donnees_def.rda"))
# load(here("datrestr_2_def", "datrestr_2_def.rda"))
source(here("pedepvars.R"))
source(here("pepcmlcovars.R"))
# load(here("peclniv.rda"))

# Filtre : sans cls_prob --------------------------------------------------

  # cls_prob : une classe avec quasiment que des NA de y. 

cls_prob <- group_by(datcompl, divconstatrneses) %>% 
  filter(sum(is.na(moy_ec1)) > 20) %>% pull(divconstatrneses) %>% 
  unique

datcompl <- filter(datcompl, divconstatrneses != cls_prob)

# pepcmlcovars ------------------------------------------------------------

  # uniquement quand pas de dummies. 
for (i in 2:length(pepcmlcovars)) {
  assign(paste("cov", i, sep = ""),
         pepcmlcovars[1:i])
}
  # MAJ 2021_12_09. 
  # 5 covariates. 
  # Il faut considérer l'enlèvement de lregime : il faut autres covars. 
cov3_noregime <- cov3[cov3 != "lregime_constat_g"]
cov4_noregime <- cov4[cov4 != "lregime_constat_g"]
  # Les covariates utilisées par Izaguirre et Di Capua 2020. 

# pepcml ------------------------------------------------------------------

for (k in c("datcompl")) {
  for (j in c(paste("cov", 2:length(pepcmlcovars), sep = ""),
                    "cov3_noregime", "cov4_noregime")) {
    for (i in pedepvars %>% 
         (function (p) p[str_detect(p, "_norm")])) {
      
      assign("ff", Formula::as.Formula(
        paste(i, " + factor(divconstatrneses)", " ~ ",
              paste(get(j)) %>% paste(collapse = " + "))
      ))
      
      assign(paste("pepcml_", j, "_", i, "_tous_", k, sep = ""),
             pseudo_cml3(ff, get(k) %>% 
                           filter(! is.na(eval(
                             parse(text = i)
                           ))) %>% 
                           group_by(divconstatrneses) %>% 
                           filter(n() > 1) %>% ungroup))
      
    }
  }
}

# Sauvegarde --------------------------------------------------------------

save(list = ls()[str_detect(ls(), "pepcml") & 
                   ls() != "pepcmlcovars"],
     file = here("pepcml_def", "pepcml.rda"),
     version = 2)
