# g20rf_hetero_robcovs. 
# 2022_06_17 : pour regarder rapidement la robustesse 
# d'un ou plusieurs paramètres. 

library(here)
library(tidyverse)
library(texreg)
# library(mfx)
library(needs)
prioritize(dplyr)

load("C:/00_phd/00_fonctions/fonctions2.rda")
load(here("g20rf_def3", "g20rf_hetero.rda"))
load(here("01_datg20_neo_def.rda"))
load(here("g20specs_def3.rda"))

l <- ls()[str_detect(ls(), "^ct.") &
             str_detect(ls(), "cov[:digit:]") & 
            str_detect(ls(), "datg20_neo$") & 
           ! str_detect(ls(), "normpop")]

# robcovs_mention_diplome_ps ----------------------------------------------

  # note_td.
screenreg(list(ct.g20rf_z_mention_diplome_ps_cov0_note_td_tous_datg20_neo,
               ct.g20rf_z_mention_diplome_ps_cov1_note_td_tous_datg20_neo,
               ct.g20rf_z_mention_diplome_ps_cov2_note_td_tous_datg20_neo,
               ct.g20rf_z_mention_diplome_ps_cov3_note_td_tous_datg20_neo,
               ct.g20rf_z_mention_diplome_ps_cov4_note_td_tous_datg20_neo,
               ct.g20rf_z_mention_diplome_ps_cov5_note_td_tous_datg20_neo,
               ct.g20rf_z_mention_diplome_ps_cov6_note_td_tous_datg20_neo,
               ct.g20rf_z_mention_diplome_ps_cov7_note_td_tous_datg20_neo,
               ct.g20rf_z_mention_diplome_ps_cov8_note_td_tous_datg20_neo,
               ct.g20rf_z_mention_diplome_ps_cov9_note_td_tous_datg20_neo,
               ct.g20rf_z_mention_diplome_ps_cov10_note_td_tous_datg20_neo,
               ct.g20rf_z_mention_diplome_ps_cov11_note_td_tous_datg20_neo))
  # Pas robuste.  

  # note_td_sup10.
screenreg(list(ct.g20rf_z_mention_diplome_ps_cov0_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_mention_diplome_ps_cov1_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_mention_diplome_ps_cov2_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_mention_diplome_ps_cov3_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_mention_diplome_ps_cov4_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_mention_diplome_ps_cov5_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_mention_diplome_ps_cov6_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_mention_diplome_ps_cov7_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_mention_diplome_ps_cov8_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_mention_diplome_ps_cov9_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_mention_diplome_ps_cov10_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_mention_diplome_ps_cov11_note_td_sup10_tous_datg20_neo))
  # Pas robuste. 

  # note_ctqcm. 
screenreg(list(ct.g20rf_z_mention_diplome_ps_cov0_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_mention_diplome_ps_cov1_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_mention_diplome_ps_cov2_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_mention_diplome_ps_cov3_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_mention_diplome_ps_cov4_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_mention_diplome_ps_cov5_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_mention_diplome_ps_cov6_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_mention_diplome_ps_cov7_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_mention_diplome_ps_cov8_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_mention_diplome_ps_cov9_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_mention_diplome_ps_cov10_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_mention_diplome_ps_cov11_note_ctqcm_tous_datg20_neo))
  # Pas robuste. 

  # note_ctqcm_sup10. 
screenreg(list(ct.g20rf_z_mention_diplome_ps_cov0_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_mention_diplome_ps_cov1_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_mention_diplome_ps_cov2_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_mention_diplome_ps_cov3_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_mention_diplome_ps_cov4_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_mention_diplome_ps_cov5_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_mention_diplome_ps_cov6_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_mention_diplome_ps_cov7_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_mention_diplome_ps_cov8_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_mention_diplome_ps_cov9_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_mention_diplome_ps_cov10_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_mention_diplome_ps_cov11_note_ctqcm_sup10_tous_datg20_neo))
  # Robuste mais pas significatif. Faibles ampleurs.


# robcovs_serie_diplome_psv4 -----------------------------------------------------------------

  # note_td. 
screenreg(list(ct.g20rf_z_serie_diplome_psv4_cov0_note_td_tous_datg20_neo,
               ct.g20rf_z_serie_diplome_psv4_cov1_note_td_tous_datg20_neo,
               ct.g20rf_z_serie_diplome_psv4_cov2_note_td_tous_datg20_neo,
               ct.g20rf_z_serie_diplome_psv4_cov3_note_td_tous_datg20_neo,
               ct.g20rf_z_serie_diplome_psv4_cov4_note_td_tous_datg20_neo,
               ct.g20rf_z_serie_diplome_psv4_cov5_note_td_tous_datg20_neo,
               ct.g20rf_z_serie_diplome_psv4_cov6_note_td_tous_datg20_neo,
               ct.g20rf_z_serie_diplome_psv4_cov7_note_td_tous_datg20_neo,
               ct.g20rf_z_serie_diplome_psv4_cov8_note_td_tous_datg20_neo,
               ct.g20rf_z_serie_diplome_psv4_cov9_note_td_tous_datg20_neo,
               ct.g20rf_z_serie_diplome_psv4_cov10_note_td_tous_datg20_neo,
               ct.g20rf_z_serie_diplome_psv4_cov11_note_td_tous_datg20_neo))
  # L'effet de 4 points pour les S est hyper robuste. D'ailleurs tous les effets sont assez robustes. 

  # note_td_sup10. 
screenreg(list(ct.g20rf_z_serie_diplome_psv4_cov0_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_serie_diplome_psv4_cov1_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_serie_diplome_psv4_cov2_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_serie_diplome_psv4_cov3_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_serie_diplome_psv4_cov4_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_serie_diplome_psv4_cov5_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_serie_diplome_psv4_cov6_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_serie_diplome_psv4_cov7_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_serie_diplome_psv4_cov8_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_serie_diplome_psv4_cov9_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_serie_diplome_psv4_cov10_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_serie_diplome_psv4_cov11_note_td_sup10_tous_datg20_neo))
  # L'effet de .21 pour les S (prob lin) est robuste en ampleur mais jamais significatif. 

  # note_ctqcm. 
screenreg(list(ct.g20rf_z_serie_diplome_psv4_cov0_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_serie_diplome_psv4_cov1_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_serie_diplome_psv4_cov2_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_serie_diplome_psv4_cov3_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_serie_diplome_psv4_cov4_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_serie_diplome_psv4_cov5_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_serie_diplome_psv4_cov6_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_serie_diplome_psv4_cov7_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_serie_diplome_psv4_cov8_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_serie_diplome_psv4_cov9_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_serie_diplome_psv4_cov10_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_serie_diplome_psv4_cov11_note_ctqcm_tous_datg20_neo))
  # L'effet de 1.84 pour les séries S est robuste mais jamais précisément estimé. 
  
  # note_ctqcm_sup10. 
screenreg(list(ct.g20rf_z_serie_diplome_psv4_cov0_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_serie_diplome_psv4_cov1_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_serie_diplome_psv4_cov2_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_serie_diplome_psv4_cov3_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_serie_diplome_psv4_cov4_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_serie_diplome_psv4_cov5_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_serie_diplome_psv4_cov6_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_serie_diplome_psv4_cov7_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_serie_diplome_psv4_cov8_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_serie_diplome_psv4_cov9_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_serie_diplome_psv4_cov10_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_serie_diplome_psv4_cov11_note_ctqcm_sup10_tous_datg20_neo))
  # L'effet de .17 dans le papier est robuste (et c'est très fort) mais imprécisément estimé. 

# robcovs_q5age_26aout ----------------------------------------------------

  # note_td. 
screenreg(list(ct.g20rf_z_q5age_26aout_cov0_note_td_tous_datg20_neo,
               ct.g20rf_z_q5age_26aout_cov1_note_td_tous_datg20_neo,
               ct.g20rf_z_q5age_26aout_cov2_note_td_tous_datg20_neo,
               ct.g20rf_z_q5age_26aout_cov3_note_td_tous_datg20_neo,
               ct.g20rf_z_q5age_26aout_cov4_note_td_tous_datg20_neo,
               ct.g20rf_z_q5age_26aout_cov5_note_td_tous_datg20_neo,
               ct.g20rf_z_q5age_26aout_cov6_note_td_tous_datg20_neo,
               ct.g20rf_z_q5age_26aout_cov7_note_td_tous_datg20_neo,
               ct.g20rf_z_q5age_26aout_cov8_note_td_tous_datg20_neo,
               ct.g20rf_z_q5age_26aout_cov9_note_td_tous_datg20_neo,
               ct.g20rf_z_q5age_26aout_cov10_note_td_tous_datg20_neo,
               ct.g20rf_z_q5age_26aout_cov11_note_td_tous_datg20_neo))
  # Les ampleurs sont robustes mais jamais significatifs. 

  # note_td_sup10. 
screenreg(list(ct.g20rf_z_q5age_26aout_cov0_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_q5age_26aout_cov1_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_q5age_26aout_cov2_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_q5age_26aout_cov3_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_q5age_26aout_cov4_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_q5age_26aout_cov5_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_q5age_26aout_cov6_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_q5age_26aout_cov7_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_q5age_26aout_cov8_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_q5age_26aout_cov9_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_q5age_26aout_cov10_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_q5age_26aout_cov11_note_td_sup10_tous_datg20_neo))
  # Robustes mais jamais significatifs. 

  # note_ctqcm. 
screenreg(list(ct.g20rf_z_q5age_26aout_cov0_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_q5age_26aout_cov1_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_q5age_26aout_cov2_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_q5age_26aout_cov3_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_q5age_26aout_cov4_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_q5age_26aout_cov5_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_q5age_26aout_cov6_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_q5age_26aout_cov7_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_q5age_26aout_cov8_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_q5age_26aout_cov9_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_q5age_26aout_cov10_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_q5age_26aout_cov11_note_ctqcm_tous_datg20_neo))
  # Attention, l'effet de - 2 pour ceux en q2 d'âge à la rentrée n'est pas robuste en significativité. 
  
  # note_ctqcm_sup10. 
screenreg(list(ct.g20rf_z_q5age_26aout_cov0_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_q5age_26aout_cov1_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_q5age_26aout_cov2_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_q5age_26aout_cov3_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_q5age_26aout_cov4_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_q5age_26aout_cov5_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_q5age_26aout_cov6_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_q5age_26aout_cov7_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_q5age_26aout_cov8_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_q5age_26aout_cov9_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_q5age_26aout_cov10_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_q5age_26aout_cov11_note_ctqcm_sup10_tous_datg20_neo))
  # Les coefs sont robustes. L'effet de .26 pour les q4 d'âge à la rentrée est robuste en significativité. 
  # Ils ne sont pas cohérents avec les effets trouvés dans les MCO. 

# robcovs_sexe_ps ---------------------------------------------------------

  # note_td. 
screenreg(list(ct.g20rf_z_sexe_ps_cov0_note_td_tous_datg20_neo,
               ct.g20rf_z_sexe_ps_cov1_note_td_tous_datg20_neo,
               ct.g20rf_z_sexe_ps_cov2_note_td_tous_datg20_neo,
               ct.g20rf_z_sexe_ps_cov3_note_td_tous_datg20_neo,
               ct.g20rf_z_sexe_ps_cov4_note_td_tous_datg20_neo,
               ct.g20rf_z_sexe_ps_cov5_note_td_tous_datg20_neo,
               ct.g20rf_z_sexe_ps_cov6_note_td_tous_datg20_neo,
               ct.g20rf_z_sexe_ps_cov7_note_td_tous_datg20_neo,
               ct.g20rf_z_sexe_ps_cov8_note_td_tous_datg20_neo,
               ct.g20rf_z_sexe_ps_cov9_note_td_tous_datg20_neo,
               ct.g20rf_z_sexe_ps_cov10_note_td_tous_datg20_neo,
               ct.g20rf_z_sexe_ps_cov11_note_td_tous_datg20_neo))
  # Pas robuste. 

  # note_td_sup10. 
screenreg(list(ct.g20rf_z_sexe_ps_cov0_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_sexe_ps_cov1_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_sexe_ps_cov2_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_sexe_ps_cov3_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_sexe_ps_cov4_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_sexe_ps_cov5_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_sexe_ps_cov6_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_sexe_ps_cov7_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_sexe_ps_cov8_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_sexe_ps_cov9_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_sexe_ps_cov10_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_sexe_ps_cov11_note_td_sup10_tous_datg20_neo))
  # Pas robuste. 

  # note_ctqcm. 
screenreg(list(ct.g20rf_z_sexe_ps_cov0_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_sexe_ps_cov1_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_sexe_ps_cov2_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_sexe_ps_cov3_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_sexe_ps_cov4_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_sexe_ps_cov5_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_sexe_ps_cov6_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_sexe_ps_cov7_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_sexe_ps_cov8_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_sexe_ps_cov9_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_sexe_ps_cov10_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_sexe_ps_cov11_note_ctqcm_tous_datg20_neo))
  # Robustes mais pas significatifs. 
  
  # note_ctqcm_sup10. 
screenreg(list(ct.g20rf_z_sexe_ps_cov0_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_sexe_ps_cov1_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_sexe_ps_cov2_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_sexe_ps_cov3_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_sexe_ps_cov4_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_sexe_ps_cov5_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_sexe_ps_cov6_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_sexe_ps_cov7_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_sexe_ps_cov8_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_sexe_ps_cov9_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_sexe_ps_cov10_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_sexe_ps_cov11_note_ctqcm_sup10_tous_datg20_neo))
  # Robustes mais jamais significatifs. 

# robcovs_pays_nais_fr ----------------------------------------------------

  # note_td. 
screenreg(list(ct.g20rf_z_pays_nais_fr_cov0_note_td_tous_datg20_neo,
               ct.g20rf_z_pays_nais_fr_cov1_note_td_tous_datg20_neo,
               ct.g20rf_z_pays_nais_fr_cov2_note_td_tous_datg20_neo,
               ct.g20rf_z_pays_nais_fr_cov3_note_td_tous_datg20_neo,
               ct.g20rf_z_pays_nais_fr_cov4_note_td_tous_datg20_neo,
               ct.g20rf_z_pays_nais_fr_cov5_note_td_tous_datg20_neo,
               ct.g20rf_z_pays_nais_fr_cov6_note_td_tous_datg20_neo,
               ct.g20rf_z_pays_nais_fr_cov7_note_td_tous_datg20_neo,
               ct.g20rf_z_pays_nais_fr_cov8_note_td_tous_datg20_neo,
               ct.g20rf_z_pays_nais_fr_cov9_note_td_tous_datg20_neo,
               ct.g20rf_z_pays_nais_fr_cov10_note_td_tous_datg20_neo,
               ct.g20rf_z_pays_nais_fr_cov11_note_td_tous_datg20_neo))
  # Robustes. 

  # note_td_sup10. 
screenreg(list(ct.g20rf_z_pays_nais_fr_cov0_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_pays_nais_fr_cov1_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_pays_nais_fr_cov2_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_pays_nais_fr_cov3_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_pays_nais_fr_cov4_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_pays_nais_fr_cov5_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_pays_nais_fr_cov6_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_pays_nais_fr_cov7_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_pays_nais_fr_cov8_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_pays_nais_fr_cov9_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_pays_nais_fr_cov10_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_pays_nais_fr_cov11_note_td_sup10_tous_datg20_neo))
  # Robustes. 

  # note_ctqcm. 
screenreg(list(ct.g20rf_z_pays_nais_fr_cov0_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_pays_nais_fr_cov1_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_pays_nais_fr_cov2_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_pays_nais_fr_cov3_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_pays_nais_fr_cov4_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_pays_nais_fr_cov5_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_pays_nais_fr_cov6_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_pays_nais_fr_cov7_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_pays_nais_fr_cov8_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_pays_nais_fr_cov9_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_pays_nais_fr_cov10_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_pays_nais_fr_cov11_note_ctqcm_tous_datg20_neo))
  # Robustes. 
  
  # note_ctqcm_sup10. 
screenreg(list(ct.g20rf_z_pays_nais_fr_cov0_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_pays_nais_fr_cov1_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_pays_nais_fr_cov2_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_pays_nais_fr_cov3_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_pays_nais_fr_cov4_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_pays_nais_fr_cov5_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_pays_nais_fr_cov6_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_pays_nais_fr_cov7_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_pays_nais_fr_cov8_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_pays_nais_fr_cov9_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_pays_nais_fr_cov10_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_pays_nais_fr_cov11_note_ctqcm_sup10_tous_datg20_neo))
  # Robustes. 


# robcovs_boursier --------------------------------------------------------

  # note_td. 
screenreg(list(ct.g20rf_z_boursier_cov0_note_td_tous_datg20_neo,
               ct.g20rf_z_boursier_cov1_note_td_tous_datg20_neo,
               ct.g20rf_z_boursier_cov2_note_td_tous_datg20_neo,
               ct.g20rf_z_boursier_cov3_note_td_tous_datg20_neo,
               ct.g20rf_z_boursier_cov4_note_td_tous_datg20_neo,
               ct.g20rf_z_boursier_cov5_note_td_tous_datg20_neo,
               ct.g20rf_z_boursier_cov6_note_td_tous_datg20_neo,
               ct.g20rf_z_boursier_cov7_note_td_tous_datg20_neo,
               ct.g20rf_z_boursier_cov8_note_td_tous_datg20_neo,
               ct.g20rf_z_boursier_cov9_note_td_tous_datg20_neo,
               ct.g20rf_z_boursier_cov10_note_td_tous_datg20_neo,
               ct.g20rf_z_boursier_cov11_note_td_tous_datg20_neo))
  # Robustes si on met la série au bac.  

  # note_td_sup10. 
screenreg(list(ct.g20rf_z_boursier_cov0_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_boursier_cov1_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_boursier_cov2_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_boursier_cov3_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_boursier_cov4_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_boursier_cov5_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_boursier_cov6_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_boursier_cov7_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_boursier_cov8_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_boursier_cov9_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_boursier_cov10_note_td_sup10_tous_datg20_neo,
               ct.g20rf_z_boursier_cov11_note_td_sup10_tous_datg20_neo))
  # Robustes. 

  # note_ctqcm. 
screenreg(list(ct.g20rf_z_boursier_cov0_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_boursier_cov1_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_boursier_cov2_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_boursier_cov3_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_boursier_cov4_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_boursier_cov5_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_boursier_cov6_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_boursier_cov7_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_boursier_cov8_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_boursier_cov9_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_boursier_cov10_note_ctqcm_tous_datg20_neo,
               ct.g20rf_z_boursier_cov11_note_ctqcm_tous_datg20_neo))
  # Robustes. 
  
  # note_ctqcm_sup10. 
screenreg(list(ct.g20rf_z_boursier_cov0_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_boursier_cov1_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_boursier_cov2_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_boursier_cov3_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_boursier_cov4_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_boursier_cov5_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_boursier_cov6_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_boursier_cov7_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_boursier_cov8_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_boursier_cov9_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_boursier_cov10_note_ctqcm_sup10_tous_datg20_neo,
               ct.g20rf_z_boursier_cov11_note_ctqcm_sup10_tous_datg20_neo))
  # Robustes. 

          
          