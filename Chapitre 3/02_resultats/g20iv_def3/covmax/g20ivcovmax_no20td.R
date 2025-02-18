# g20iv (def3). covmax
	# 2022_06_22 : no20td => pas de 20 en td. 

library(here)
library(tidyverse)
library(lmtest)
library(plm)
# library(Formula) # as.Formula pose problème avec ivreg. 
library(AER)
library(ivpack)

# Chargement --------------------------------------------------------------

load("C:/00_phd/00_fonctions/fonctions2.rda")
load(here("01_datg20_neo_def.rda"))
load(here("g20specs_def3.rda"))
load(here("g20depvars_def3.rda"))
load(here("g20treats_def3.rda"))
# load(here("g20z_hetero_def.rda"))

g20specs <- ls()[str_detect(ls(), "g20cov")]
# g20heterocovars <- sapply(g20specs, get) %>% unlist %>% unname %>% unique
g20dats <- ls()[str_detect(ls(), "datg20")]

# no20td ------------------------------------------------------------------

for (i in g20dats) {
  assign(paste(i, "_no20td", sep = ""), 
         filter(get(i), note_td != 20))
}
g20dats <- ls()[str_detect(ls(), "datg20") & 
                  str_detect(ls(), "no20td")]
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

# Régressions -------------------------------------------------------------

for (m in c("z")) {
for (i in g20treats) {
  for (j in g20specs) {
    for (k in g20depvars) {
      for (l in g20dats) {
        assign(paste("g20iv_", i, "_", j %>% str_remove("g20"), 
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

# Inférence simple et autres éléments --------------------------------------------

for (i in ls()[str_detect(ls(), "^g20iv")]) {
  
  assign(paste("ct.", i, sep = ""),
         robust.se(get(i)))
  
  assign(paste("n.", i, sep = ""), nobs(get(i)))
  
  assign(paste("arsq.", i, sep = ""),
         get(i) %>% ext_adjrsq)
}

# Sauvegarde --------------------------------------------------------------

save(list = ls()[str_detect(ls(), "g20iv") & 
                   ! str_detect(ls(), "^g20iv")],
     file = here("g20iv_def3", "covmax",
                 "g20ivcovmax_no20td.rda"),
     version = 2)
