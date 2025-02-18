# g20rftobit_hetero (def3). 

library(here)
library(tidyverse)
library(lmtest)
library(plm)
library(Formula)
library(tobit1)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions2.rda")
load(here("01_datg20_neo_def.rda"))
load(here("g20specs_def3.rda"))
load(here("g20depvars_def3.rda"))
# load(here("g20treats_def.rda"))

g20specs <- ls()[str_detect(ls(), "g20cov")]
g20dats <- ls()[str_detect(ls(), "datg20")]
g20depvars <- g20depvars[g20depvars %in% 
                           c("note_td", "note_ctqcm", "note_ue")]

# for (i in ls()[str_detect(ls(), "g20cov")]) {
#   assign(i, str_replace(get(i), "filiereR", "filiere") %>% 
#            str_replace("serie\\_diplome\\_psv4R",
#                        "serie\\_diplome\\_psv4"))
# }
g20cov0 <- NULL

# Réglage bug2 : serie_diplome_psv4_autre 1 seul effectif : ne mar --------

# datg20_red_noNAcovs_nomaxabi <- filter(datg20_red_noNAcovs_nomaxabi, 
#                                        serie_diplome_psv4 != "autre")

# Relevel sur les plus grands effectifs -----------------------------------

# for (i in g20dats) 
#   assign(i, mutate(get(i), 
#                    pays_nais_fr = relevel(pays_nais_fr, "oui"),
#                    serie_diplome_psv4R = relevel(serie_diplome_psv4R, 
#                                                  "es"),
#                    serie_diplome_psv4 = relevel(serie_diplome_psv4, "es")))

# covmax ------------------------------------------------------------------

g20specs <- 
  g20specs %>% (function (g) {
    g0 <- g
  g <- str_remove(g, "g20")
  g <- g[str_detect(g, "cov[:digit:]|cov[:digit:][:digit:]")]
  max <- str_remove(g, "cov") %>% as.numeric() %>% 
    max
  g <- g0[str_detect(g0, paste("g20cov", max, sep = ""))]
  c(g, "g20covnocor")
})

for (i in g20specs) assign(i, get(i) %>% 
                             str_replace("moy_bac", "mention_diplome_ps") %>% 
                             str_replace("serie_diplome_psv4R", 
                                         "serie_diplome_psv4")) 
  # exception : pas identifié sur R. 

# depvars utiles ----------------------------------------------------------

g20depvars <- g20depvars[! str_detect(g20depvars, "\\_sup")]

# hetero pertinents -------------------------------------------------------

g20heterocovars <- c("mention_diplome_ps", "serie_diplome_psv4", 
                         "filiere", "q5age_26aout", "sexe_ps",
                         "pays_nais_fr", # problématique : effectifs. (rappel)
                     "boursier")
for (i in g20specs) assign(i, get(i) %>% 
                             str_replace("moy_bac", "mention_diplome_ps"))

# Censure des deux côtés --------------------------------------------------

for (i in c("z") %>% sapply(function (x) 
  paste(x, g20heterocovars, sep = "_")) %>% 
  as.vector) {
  for (j in g20specs) {
    for (k in g20depvars) {
      for (l in g20dats) {
        assign(paste("g20rftobit_", i, "_", j %>% str_remove("g20"), 
                     "_", k, "_tous_", l, sep = ""),
               tobit1(as.Formula(
                 paste(
                   k, " ~ ", 
                   paste(c(i %>% str_replace("^z_", "z * "), 
                         get(j) %>% paste(collapse = " + ")),
                         collapse = " + ") %>% 
                     (function (x) ifelse(str_detect(str_trim(x), "\\+$"),
                                          str_remove(x, "\\+ $"), x)),
                   sep = ""
                   )
               ), 
               data = get(l) %>% mutate_if(is.factor, droplevels),
               left = 0, right = 20))
      }
    }
  }
  }

# Censure à gauche uniquement --------------------------------------------------

for (i in c("z") %>% sapply(function (x) 
  paste(x, g20heterocovars, sep = "_")) %>% 
  as.vector) {
  for (j in g20specs) {
    for (k in g20depvars) {
      for (l in g20dats) {
        assign(paste("g20rftobitl_", i, "_", j %>% str_remove("g20"), 
                     "_", k, "_tous_", l, sep = ""),
               tobit1(as.formula(
                 paste(
                   k, " ~ ", 
                   paste(c(i %>% str_replace("^z_", "z * "), 
                         get(j) %>% paste(collapse = " + ")),
                         collapse = " + ") %>% 
                     (function (x) ifelse(str_detect(str_trim(x), "\\+$"),
                                          str_remove(x, "\\+ $"), x)),
                   sep = ""
                   )
               ), 
               data = get(l) %>% mutate_if(is.factor, droplevels),
               left = 0, right = Inf))
      }
    }
  }
  }

# Censure à droite --------------------------------------------------------

  # Pas pertinent (?).

# Inférence simple et autres éléments -------------------------------------

for (i in ls()[str_detect(ls(), "^g20rftobit")]) {
  
  assign(paste("ct.", i, sep = ""),
         coef(summary(get(i)))) # à transformer en écart-types robustes. pour tobit.
  
  assign(paste("n.", i, sep = ""), nobs(get(i)))
  
  assign(paste("loglik.", i, sep = ""),
         get(i) %>% (function (x) x$logLik %>% as.numeric))
}

# Sauvegarde --------------------------------------------------------------

save(list = ls()[str_detect(ls(), "g20rftobit") & 
                   ! str_detect(ls(), "^g20rftobit")],
     file = here("g20rftobit_def3", "covmax",
                 "g20rftobitcovmax_hetero.rda"),
     version = 2)
