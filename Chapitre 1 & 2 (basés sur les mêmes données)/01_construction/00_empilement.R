# Empilement. Toutes les bases. 

library(tidyverse)
library(fastDummies)
library(lubridate)
library(magrittr)
library(stringi)
library(here)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions.rda")
load(here("00_donnees_originales", "00_donnees_originales.rda"))

# Factors -> chars --------------------------------------------------------

for (i in ls()[!as.vector(sapply(ls(), function (x)
  is.function(get(x))))]) {
  assign(i, mutate_if(get(i), is.factor, as.character))
}

# tc ----------------------------------------------------------------------

tc10 <- rename(tc10,
               clef_cm2 = clefCm2,
               clef_6eme = clef6eme) %>%
  mutate(cohorte = "2010")

tc11 <- rename(tc11,
               clef_cm2 = clefCm2,
               clef_6eme = clef6eme) %>%
  mutate(cohorte = "2011")

  # tc12$clef_cm2 est déjà bien nommé

tc12 <- rename(tc12,
               clef_6eme = clef6eme) %>%
  mutate(cohorte = "2012")

tc <- do.call(rbind, lapply(list(tc10,
                                 tc11,
                                 tc12),
                            function (x)
                              select(x, sort(names(
                                x
                              )))))

# constatred10 ------------------------------------------------------------
  # Correction NA

constatred10 <- mutate(
  constatred10,
  reseau = str_trim(reseau),
  reseau = fct_recode(factor(reseau),
                      "ECLAIR" = "RAR") %>%
    as.character
)

constatred10$commune[constatred10$commune == "#N/A"] <- NA

# cm2 ---------------------------------------------------------------------

evalcm2_2009 <- left_join(evalcm2_2009,
                          select(evalcm2_2009supp, - session), 
                          by = "clefCm2")
  # 12321 obs.

evalcm2_2009 <- left_join(evalcm2_2009,
                          select(constatred10, - statut_constat),
                          by = "ceco")
  # pas d'augmentation du nombre d'observations

evalcm2_2010 <- left_join(evalcm2_2010,
                          select(constatred10, - statut_constat),
                          by = "ceco")
  # pas d'augmentation du nombre d'observations

evalcm2_2011 <- left_join(evalcm2_2011,
                          select(constatred11, - statut_constat),
                          by = "ceco")
  # pas d'augmentation du nombre d'observations

  # Il y a une école non retrouvée, on peut compléter manuellement
  # Source : https://www.etablissements-scolaires.fr/etablissement-scolaire-ecole-elementaire-publique-raymond-mondon-b.html

evalcm2_2011$reseau[evalcm2_2011$ceco == "9740175X"] <- "HEP"
evalcm2_2011$commune[evalcm2_2011$ceco == "9740175X"] <- "LE PORT"

evalcm2_2012 <- left_join(evalcm2_2012,
                          rename(constatred12,
                                 Statut = statut_constat),
                          by = "ceco")
  # pas d'augmentation du nombre d'observations

  # UNIFORMISATION DES VARIABLES 

# Pour 2009 uniquement
# On ramène les notes FR et MATHS de cm2 2009 sur 60 et 40. 
# Source : Livret

evalcm2_2009 <- rename(evalcm2_2009,
                       clef_cm2 = clefCm2) %>% 
  mutate(SF_ORI = SF,
         SF = round(SF * 60 / 100),
         
         SM_ORI = SM,
         SM = round(SM * 40 / 100),
         
         ST_ORI = ST,
         ST = SF + SM) %>% 
  select(- mois_naiss, - an_naiss, - dbl)

  # 2010
evalcm2_2010 <- rename(evalcm2_2010,
                       clef_cm2 = clefCm2) %>% 
  select(- mois_naiss, - an_naiss,  - dbl) %>% 
  mutate(SF_ORI = SF,
         SM_ORI = SM,
         ST_ORI = ST)

  # 2011
evalcm2_2011 <- rename(evalcm2_2011,
                       clef_cm2 = clefCm2) %>% 
  select(- dnaiss1, - mois_naiss, - an_naiss,  - dbl) %>% 
  mutate(SF_ORI = SF,
         SM_ORI = SM,
         ST_ORI = ST) 

  # 2012
evalcm2_2012 <- rename(evalcm2_2012, 
                       dnaiss = date_naissance,
                       sexe = sxe) %>%
  select(- sex, - clef) %>% 
  mutate(ST = SF + SM) %>% 
  mutate(SF_ORI = SF,
         SM_ORI = SM,
         ST_ORI = ST)


  # L'EMPILEMENT.

cm2 <- do.call(rbind, lapply(list(evalcm2_2009, 
                                  evalcm2_2010, 
                                  evalcm2_2011, 
                                  evalcm2_2012), 
                             function (x) select(x, sort(names(x)))))

# base6eme ----------------------------------------------------------------

  # 2010
base6eme2010 <- rename(base6eme2010, 
                       clef_6eme = clef6eme,
                       statut = secteur) %>% 
  mutate(coption1 = NA, loption1 = NA,
         coption2 = NA, loption2 = NA, 
         coption3 = NA, loption3 = NA,
         annee = NA,
         mois = NA,
         comnat = NA, lacommune = NA,
         ctransport = NA, cregime = NA, regime = NA, lregime = NA,
         cstatut = NA, lstatut = NA,
         age6eme = NA)

  # 2011
base6eme2011 <- rename(base6eme2011, 
                       clef_6eme = clef6eme,
                       statut = secteur,
                       denom = denomination, 
                       div = division,
                       llpcs = lpcs) %>% 
  select(- DDMMYY10) %>% 
  mutate(bourse = NA, lbourse = NA, llbourse = NA,
         codcomm = NA, nomcomm = NA, 
         nenfant = NA,
         lpcs = NA, # car déjà changé en llpcs
         
         comnat = NA, lacommune = NA,
         ctransport = NA, cregime = NA, regime = NA, lregime = NA,
         cstatut = NA, lstatut = NA,
         age6eme = NA)


  # 2012
base6eme2012 <- rename(base6eme2012, 
                       clef_6eme = clef6eme,
                       sexe = sex,
                       bourse = cbourse, llbourse = lbourse,
                       codcomm = ccom, nomcomm = commune,
                       llpcs = lpcs,
                       nenfant = nbenft,
                       div = division,
                       lpcs = pcs) %>% 
  mutate(annee = NA, mois = NA, lbourse = NA)

  # L'EMPILEMENT. 
base6eme <- do.call(rbind, lapply(list(base6eme2010,
                                       base6eme2011,
                                       base6eme2012),
                                  function (x)
                                    select(x,
                                           sort(names(x))))) %>%
  mutate(cohorte = c(rep("2010", dim(base6eme2010)[1]),
                     rep("2011", dim(base6eme2011)[1]),
                     rep("2012", dim(base6eme2012)[1])))

# dnb ---------------------------------------------------------------------

dnb14 <- mutate(
  dnb14,
  mat_nivA2_reg = NA,
  notes_nivA2_reg = NA,
  
  educiv_cc = NA,
  higeo_cc = NA,
  
  coef_Lingu_cc = NA,
  coef_nonLingu_cc = NA,
  mat_Lingu_cc = NA,
  mat_nonLingu_cc = NA,
  moy_Lingu_cc = NA,
  moy_nonLingu_cc = NA,
  notes_Lingu_cc = NA,
  notes_nonLingu_cc = NA
)

dnb15 <- mutate(
  dnb15,
  coef_socio_cc = NA,
  mat_socio_cc = NA,
  moy_socio_cc = NA,
  notes_socio_cc = NA,
  
  bassin = NA,
  coef_edart_cc = NA,
  coef_ensart_cc = NA,
  coef_higeo_cc = NA,
  coef_lv_cc = NA,
  coef_lve_cc = NA,
  coef_scbio_cc = NA,
  coef_viesco_cc = NA,
  mat_edart_cc = NA,
  mat_ensart_cc = NA,
  mat_higeo_cc = NA,
  mat_lv_cc = NA,
  mat_lve_cc = NA,
  mat_scbio_cc = NA,
  mat_viesco_cc = NA,
  moy_edart_cc = NA,
  moy_ensart_cc = NA,
  moy_higeo_cc = NA,
  moy_lv_cc = NA,
  moy_lve_cc = NA,
  moy_scbio_cc = NA,
  moy_viesco_cc = NA,
  notes_higeo_cc = NA,
  
  coef_Lingu_cc = NA,
  coef_nonLingu_cc = NA,
  mat_Lingu_cc = NA,
  mat_nonLingu_cc = NA,
  moy_Lingu_cc = NA,
  moy_nonLingu_cc = NA,
  notes_Lingu_cc = NA,
  notes_nonLingu_cc = NA
  
)

dnb16 <- mutate(
  dnb16,
  mois_naiss = NA,
  moy_red_ec1 = NA,
  notes_eco_cc = NA,
  notes_edart_cc = NA,
  notes_ensart_cc = NA,
  notes_lv_cc = NA,
  notes_lve_cc = NA,
  notes_scbio_cc = NA,
  notes_viesco_cc = NA,
  notes_vsocpro_cc = NA,
  
  
  bassin = NA,
  coef_edart_cc = NA,
  coef_ensart_cc = NA,
  coef_higeo_cc = NA,
  coef_lv_cc = NA,
  coef_lve_cc = NA,
  coef_scbio_cc = NA,
  coef_viesco_cc = NA,
  mat_edart_cc = NA,
  mat_ensart_cc = NA,
  mat_higeo_cc = NA,
  mat_lv_cc = NA,
  mat_lve_cc = NA,
  mat_scbio_cc = NA,
  mat_viesco_cc = NA,
  moy_edart_cc = NA,
  moy_ensart_cc = NA,
  moy_higeo_cc = NA,
  moy_lv_cc = NA,
  moy_lve_cc = NA,
  moy_scbio_cc = NA,
  moy_viesco_cc = NA,
  notes_higeo_cc = NA,
  
  educiv_cc = NA,
  higeo_cc = NA
)

  # L'EMPILEMENT. 

dnb <- do.call(rbind, lapply(list(dnb14,
                                  dnb15,
                                  dnb16),
                             function (x)
                               select(x,
                                      sort(names(
                                        x
                                      ))))) %>%
  mutate(session = c(rep("2014", dim(dnb14)[1]),
                     rep("2015", dim(dnb15)[1]),
                     rep("2016", dim(dnb16)[1])))

# constat1316 -------------------------------------------------------------

  # Directement empilable. 

constat1316 <-
  rbind(
    mutate(
      constat13,
      rne_origine = NA,
      denom_origine = NA,
      cohorte = "2013"
    ),
    mutate(constat14, cohorte = "2014"),
    mutate(constat15, cohorte = "2015"),
    mutate(constat16, cohorte = "2016")
  )

# Sauvegarde --------------------------------------------------------------

save(tc, cm2, base6eme, dnb, constat1316, 
     file = here("01_construction",
                 "00_empilement.rda"),
     version = 2)
