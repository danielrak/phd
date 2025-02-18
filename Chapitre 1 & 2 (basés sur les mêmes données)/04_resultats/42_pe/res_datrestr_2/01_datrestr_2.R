# Restriction des données, première version : on enlève une école dès qu'elle apparaît dans
# une année. 

library(tidyverse)
library(here)

# Chargement --------------------------------------------------------------

load(here("01_donnees.rda"))
load(here("01_03_kstest_score_r1000.rda"))


# Restriction -------------------------------------------------------------

datrestr_2 <- filter(dat, 
                    ! rneconstat %in% (filter(kstest_score$ksdata, pval < .1354) %>% 
                                         mutate(gsup0 = str_remove(gsup, " .*")) %>% 
                                         count(gsup0) %>% filter(n > 1) %>% 
                                         pull(gsup0)))
  # Voir BM18 pour la p-value (accept at most one rejection)

# Plus que 34210 observations. 95.08% de dat. 

# FILTRE : Sans ncl 1 à ce niveau --------------------------------------------------

  # Car n'apporte rien à l'identification des effets de pairs
  # si l'on prend un effet école-année.

datrestr_2 <- group_by(datrestr_2, rneconstatses) %>% 
  mutate(ncl = length(unique(divconstatrneses))) %>% 
  ungroup %>% filter(ncl != 1) %>% mutate(cg = droplevels(cg))
  # 34180 obs. 95% de dat. 

# Sauvegarde --------------------------------------------------------------

save(datrestr_2, file = here("res_datrestr_2", "01_datrestr_2.rda"))
