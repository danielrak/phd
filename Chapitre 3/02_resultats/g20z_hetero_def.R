# g20z_hetero. 

library(tidyverse)
library(here)
load(here("g20specs_def.rda"))
g20specs <- ls()[str_detect(ls(), "g20cov")]
g20heterocovars <- sapply(g20specs, get) %>% unlist %>% unname %>% unique
g20z_hetero <- c("z") %>% sapply(function (x) 
  paste(x, g20heterocovars, sep = "_")) %>% 
  as.vector

save(g20z_hetero, g20heterocovars, 
     file = here("g20z_hetero_def.rda"), version = 2)
