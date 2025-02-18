# lancement s13. 
# uniquement si j'ai besoin de tout relancer. 

library(here)
library(tidyverse)

list.files(here("res_datrestr_2", "s13h5")) %>% 
  (function (l) l[str_detect(l, "\\.R$") & 
                    ! str_detect(l, "lancement")]) %>% 
  (function (x) for (i in here("res_datrestr_2", "s13h5", x)) source(i))