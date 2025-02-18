# lancement s13ols. 
# uniquement si j'ai besoin de tout relancer. 

library(here)
library(tidyverse)

list.files(here("res_datrestr_2", "s13ols")) %>% 
  (function (l) l[str_detect(l, "\\.R$") & 
                    ! str_detect(l, "lancement")]) %>% 
  (function (x) for (i in here("res_datrestr_2", "s13ols", x)) source(i))