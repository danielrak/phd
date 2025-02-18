# g20depvars (def2). 

library(tidyverse)
library(here)

g20depvars <- c("note_td", "note_ctqcm", "note_ue") %>% 
  (function (x)
  c(x, sapply(x, function (x2) paste(x2, "_normpop", sep = "")) %>% as.vector)) %>% 
  (function (v) c(v, sapply(v[! str_detect(v, "_norm")], 
                            function (x) if (str_detect(x, "30")) 
                              paste(x, "_sup15", sep = "") else 
                                paste(x, "_sup10", sep = "")))) %>% unname

g20depvars <- c(g20depvars, c("note_td_v2", 
                              "note_td_v2_sup10"))

save(g20depvars,
     file = here("g20depvars_def3.rda"), 
     version = 2)