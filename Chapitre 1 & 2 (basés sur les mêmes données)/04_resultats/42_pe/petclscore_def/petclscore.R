# petclscore
  # Voir Section \@ref(peresfrlm) pour l'utilité de ceci. 

library(here)
library(tidyverse)
library(plm)
library(lmtest)

load(here("datrestr_2_def", "datrestr_2_def.rda"))

  # Concession nom. 
petclscorenofe <- lm(tdivconstatrneses ~ score_norm 
                   + sexe_mod + pcs_reg_mod 
                  + age_absdnb + positiondnb2
                  + lregime_constat_g, 
                  datrestr_2)

petclscore <- plm(tdivconstatrneses ~ score_norm 
                   + sexe_mod + pcs_reg_mod 
                  + age_absdnb + positiondnb2
                  + lregime_constat_g, 
                  datrestr_2, index = "rneconstatses")

save(petclscorenofe, petclscore,
     file = here("petclscore_def",
                 "petclscore.rda"),
     version = 2)