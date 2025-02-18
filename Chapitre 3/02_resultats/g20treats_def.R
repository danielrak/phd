# g20treats. (def). 

library(here)
library(tidyverse)

g20treats <- c("sessions", "videos_views", "videos_comp", 
               "sheets_views", "workpoints") %>% 
  (function (x) c(paste(x, "_b24", sep = ""),
                  paste(x, "_tot", sep = ""))) %>% 
  (function (x2) c(x2, "sessions_b24_sup1", "sessions_tot_sup1"))

save(g20treats, file = here("g20treats_def.rda"), version = 2)