# pes13hnolinq5corrnppsfe. (def). 
  # corr : score_norm en var expl. MAJ2022_02_09. 
  # MAJ2022_02_16 : des problèmes de collinéarités  pour sA uniquement. 
  # Manque des variables pnp dans les modèles. 
  # Pour avoir la liste des modèles qui comportent ces problèmes : 
    # 1 - loader le fichier rda enregistré par le script présent. 
    # 2 - lancer le code (avec package nécessaire) : ls()[str_detect(ls(), "^ct\\.pes13hnolinq5corrnppsfe") & 
       # ! str_detect(ls(), "sexe\\_mod|pcs\\_reg\\_mod")] %>% (function (l) lapply(l, get) %>% lapply(function (x) rownames(x) %>% str_extract("pnp|pop") %>% table %>% as.vector %>% paste(collapse = " ET ")) %>% setNames(l)) %>% unlist %>% (function (x) x[x == "16 ET 20"]) %>% names

library(here)
library(tidyverse)
library(fixest)
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
  assign(paste("pes13hnolinq5corrnppsfe_", j, "_",
               i,
               "_tous_", k,
               sep = ""),
         feols(as.formula(paste(
           i, " ~ ",
           c(peersindivsnp_q5, "score_norm",
             get(j)) %>% 
             paste(collapse = " + "),
           " | rneconstatses + ecolecoh"
         )),
         data = get(k) %>% filter(! is.na(score_norm))))
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
#   assign(paste("pes13hnolinq5corrnppsfe_", j, "_",
#                i,
#                "_tous_", k, "_wclnivsup",
#                sep = ""),
#          feols(as.formula(paste(
#            i, " ~ ",
#            c(peersindivsnp_q5, "score_norm",
#              get(j)) %>% 
#              paste(collapse = " + "),
#            " | rneconstatses + ecolecoh"
#          )),
#          data = get(k) %>% 
#            (function (d) rbind(datcompl_clniv_sup, d))))
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
#   assign(paste("pes13hnolinq5corrnppsfe_", j, "_",
#                i,
#                "_tous_", k, "_wclnivinf",
#                sep = ""),
#          feols(as.formula(paste(
#            i, " ~ ",
#            c(peersindivsnp_q5, "score_norm",
#              get(j)) %>% 
#              paste(collapse = " + "),
#            " | rneconstatses + ecolecoh"
#          )),
#          data = get(k) %>% 
#            (function (d) rbind(d, datcompl_clniv_inf))))
# }
# }
# }

# Inférence et narsq ------------------------------------------------------

for (i in ls()[str_detect(ls(), "^pes13")]) {
  assign(paste("ct.", i, sep = ""),
         get(i)$coeftable %>% as.matrix %>% 
           structure(class = "coeftest"))
  assign(paste("n.", i, sep = ""), nobs(get(i)))
  assign(paste("arsq.", i, sep = ""), r2(get(i))["war2"])
}

# Sauvegarde --------------------------------------------------------------

save(list = ls()[str_detect(ls(), "pes13hnolinq5corr") & 
                  ! str_detect(ls(), "^pes13hnolinq5corr")],
     file = here("pes13hnolinq5corrnppsfe_def", 
                 "pes13hnolinq5corrnppsfe.rda"),
     version = 2)
