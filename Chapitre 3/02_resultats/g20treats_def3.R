# g20treats (def2). 

library(here)
library(tidyverse)

  # changement : on ne prend que tot. 
g20treats <- c(c("sessions_tot", "videos_views_tot",
               "videos_comp_tot", 
               "sheets_views_tot", 
               # "workpoints_tot",
               "pratique_tot", "pratique_tot2",
               
               "sessions_tot_ori", "videos_views_tot_ori",
               "videos_comp_tot_ori", 
               "sheets_views_tot_ori", 
               "pratique_tot_ori", "pratique_tot2_ori") %>% 
                 (function (x) c(x, paste(x, "_norm", sep = ""))),
               
               "sessions_tot_sup1", "videos_views_tot_sup1",
               "videos_comp_tot_sup1", 
               "sheets_views_tot_sup1","pratique_tot_sup1",
               "pratique_tot2_sup1",
               
               "connect")
  # workpoints_tot_sup1 pas pertinent. 

save(g20treats,
     file = here("g20treats_def3.rda"), 
     version = 2)
