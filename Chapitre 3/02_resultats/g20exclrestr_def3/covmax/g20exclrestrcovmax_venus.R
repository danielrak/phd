# g20exclrestr_venus. def3. 

library(here)
library(tidyverse)
library(lmtest)
library(plm)
library(Formula)
library(mfx)
library(needs)
prioritize(dplyr)
library(AER)
library(ivpack)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions2.rda")
load(here("01_datg20_neo_def.rda"))
load(here("g20specs_def3.rda"))
load(here("g20depvars_def3.rda"))
load(here("g20treats_def3.rda"))

g20specs <- ls()[str_detect(ls(), "g20cov")]
g20dats <- ls()[str_detect(ls(), "datg20")]

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

# depvars venus --------------------------------------------------

g20depvars <- c("venu_td", "venu_ctqcm")

# datg20_neo uniquement ---------------------------------------------------

g20dats <- "datg20_neo"

# Régressions -------------------------------------------------------------

for (i in c("z")) {
  for (j in g20specs) {
    for (k in g20depvars) {
      for (l in g20dats) {
        assign(paste("g20exclrestr_", i, "_", j %>% str_remove("g20"), 
                     "_", k, "_tous_", l, sep = ""),
               lm(as.Formula(
                 paste(
                   k, " ~ ", 
                   paste(c(i, 
                         get(j) %>% paste(collapse = " + ")),
                         collapse = " + ") %>% 
                     (function (x) ifelse(str_detect(str_trim(x), "\\+$"),
                                          str_remove(x, "\\+ $"), x)),
                   sep = ""
                   )
               ), 
               get(l)))
      }
    }
  }
}

# Probit ------------------------------------------------------------------

for (i in c("z")) {
  for (j in g20specs) {
    for (k in g20depvars) {
      for (l in g20dats) {
        assign(paste("g20exclrestr_probit_", i, "_", j %>% str_remove("g20"), 
                     "_", k, "_tous_", l, sep = ""),
               probitmfx(as.Formula(
                 paste(
                   k, " ~ ", 
                   paste(c(i, 
                         get(j) %>% paste(collapse = " + ")),
                         collapse = " + ") %>% 
                     (function (x) ifelse(str_detect(str_trim(x), "\\+$"),
                                          str_remove(x, "\\+ $"), x)),
                   sep = ""
                   )
               ), 
               get(l)))
      }
    }
  }
}

# IV : pour regarder d'éventuels effets de la plateforme sur présence aux TD ----------------

for (m in c("z")) {
for (i in g20treats) {
  for (j in g20specs) {
    for (k in g20depvars) {
      for (l in g20dats) {
        assign(paste("g20ivexclrestr_venus_", i, "_", j %>% str_remove("g20"), 
                     "_", k, "_tous_", l, sep = ""),
               ivreg(as.formula(
                 paste(
                   k, " ~ ", 
                   paste(c(i, 
                         get(j) %>% paste(collapse = " + ")),
                         collapse = " + ") %>% 
                     (function (x) ifelse(str_detect(str_trim(x), "\\+$"),
                                          str_remove(x, "\\+ $"), x)),
                   "|. - ", i, " + ", m,
                   sep = ""
                   )
               ), 
               data = get(l)))
      }
    }
  }
}
}

# Inférence simple et autres éléments -------------------------------------

for (i in ls()[str_detect(ls(), "^g20exclrestr") & ! str_detect(ls(), "probit")]) {
  
  assign(paste("ct.", i, sep = ""),
         rsewhite(get(i))) # 0 warnings. 
  
  assign(paste("n.", i, sep = ""), nobs(get(i)))
  
  assign(paste("arsq.", i, sep = ""),
         get(i) %>% ext_adjrsq)
}

for (i in ls()[str_detect(ls(), "^g20exclrestr") & str_detect(ls(), "probit")]) {
  
  assign(paste("ct.", i, sep = ""),
         rsewhite(get(i))) # 0 warnings. 
  
  assign(paste("n.", i, sep = ""), nobs(get(i)))
  
  assign(paste("arsq.", i, sep = ""),
         get(i) %>% ext_adjrsq)
}

for (i in ls()[str_detect(ls(), "^g20iv")]) {
  
  assign(paste("ct.", i, sep = ""),
         robust.se(get(i)))
  
  assign(paste("n.", i, sep = ""), nobs(get(i)))
  
  assign(paste("arsq.", i, sep = ""),
         get(i) %>% ext_adjrsq)
}

# Sauvegarde --------------------------------------------------------------

save(list = ls()[str_detect(ls(), "g20exclrestr") & 
                   ! str_detect(ls(), "^g20exclrestr")],
     file = here("g20exclrestr_def3", "covmax",
                 "g20exclrestrcovmax_venus.rda"),
     version = 2)
