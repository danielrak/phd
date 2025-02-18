# lancement s13hnolinq5. 
# uniquement si j'ai besoin de tout relancer. 

library(here)
library(tidyverse)

list.files(here("res_datrestr_2", "s13hnolinq5")) %>% 
  (function (l) l[str_detect(l, "\\.R$") & 
                    ! str_detect(l, "lancement") & 
                    ! str_detect(l, "hnolintab")]) %>% 
  (function (x) for (i in here("res_datrestr_2", "s13hnolinq5", x)) source(i))