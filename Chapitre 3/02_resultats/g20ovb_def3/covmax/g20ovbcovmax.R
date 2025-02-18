# g20ovbols_def3. covmax 

library(here)
library(tidyverse)
library(lmtest)
library(plm)
library(Formula)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions2.rda")
load(here("01_datg20_neo_def.rda"))
load(here("g20specs_def3.rda"))
# source(here("g20depvars_def.R"))
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

# z = 0 -------------------------------------------------------------------

for (i in g20dats) {
  assign(i, get(i) %>% filter(z == "0"))
}

# Régressions -------------------------------------------------------------

for (i in c("z")) {
  for (j in g20specs) {
    for (k in g20treats) {
      for (l in g20dats) {
        assign(paste("g20ovb_", i, "_", j %>% str_remove("g20"), 
                     "_", k, "_tous_", l, sep = ""),
               lm(as.Formula(
                 paste(
                   k, " ~ ", 
                   paste(c( 
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

# Inférence simple et autres éléments -------------------------------------

for (i in ls()[str_detect(ls(), "^g20ovb")]) {
  
  assign(paste("ct.", i, sep = ""),
         rsewhite(get(i)))
  
  assign(paste("f.", i, sep = ""),
         get(i) %>% summary %>% (function (s) s$fstatistic) %>%
           (function (f) c(f[1],
                           pf(f[1], f[2], f[3], lower.tail = FALSE)) %>%
              setNames(c("fstat", "pval"))))
  
  # assign(paste("f.", i, sep = ""),
  #        get(i) %>% summary %>%
  #          coef %>% (function (c) c["z1", 3]) %>% (function (x) x ^ 2))
  
  assign(paste("n.", i, sep = ""), nobs(get(i)))
  
  assign(paste("arsq.", i, sep = ""),
         get(i) %>% ext_adjrsq)
}

# Sauvegarde --------------------------------------------------------------

save(list = ls()[str_detect(ls(), "g20ovb") & 
                   ! str_detect(ls(), "^g20ovb")],
     file = here("g20ovb_def3", "covmax",
                 "g20ovbcovmax.rda"),
     version = 2)
