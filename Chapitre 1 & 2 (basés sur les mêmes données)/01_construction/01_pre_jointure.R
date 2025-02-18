# Pré-jointures. 

library(tidyverse)
library(fastDummies)
library(lubridate)
library(magrittr)
library(stringi)
library(here)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions.rda")
load(here("01_construction", "00_empilement.rda"))

# tc ----------------------------------------------------------------------

tc$ine[nchar(tc$ine) == 0] <- NA

tc <- mutate(tc, 
             clef_6eme = if_else(str_detect(clef_6eme, "6eme"),
                                 str_replace(clef_6eme, "6eme_", "") %>% 
                                   str_replace("RS2011", "RS2011_"),
                                 clef_6eme))

tc$clef_6eme[nchar(tc$clef_6eme) == 0] <- NA

tc <- mutate(
  tc,
  clef_cm2coh = paste(clef_cm2, cohorte),
  clef_6emecoh = ifelse(!is.na(clef_6eme),
                        paste(clef_6eme, cohorte),
                        NA),
  inecoh = ifelse(!is.na(ine),
                  paste(ine, cohorte),
                  NA)
)

tc <- filter(tc, ! duplicated(clef_cm2coh))

  # le code ci-dessous enlève les inecoh qui ne sont pas des NA mais qui sont des doublons
  # avant : 40206
tc <- filter(tc,!inecoh %in% (filter(tc,!is.na(inecoh) &
                                       duplicated(inecoh)) %>%
                                pull(inecoh)))
  # après le retrait des 3 doublons : 40200

# cm2 ---------------------------------------------------------------------

  # Les items. 
  # Source : Livrets

cm2 <- mutate_at(cm2, paste("i", 1:100, sep = ""), as.numeric)

# les scores originales 
# en 2012, les données sous-items sont déjà ok 
cm2 <- rbind(
  mutate(
    filter(cm2, session == "2009"),
    ecrire_ori = ecrire * 10 / 100,
    lire_ori = lire * 15 / 100,
    grammaire_ori = grammaire * 15 / 100,
    ortho_ori = ortho * 10 / 100,
    voca_ori = voca * 10 / 100,
    
    calcul_ori = calcul * 12 / 100,
    geometrie_ori = geometrie * 7 / 100,
    grand_mes_ori = grand_mes * 7 / 100,
    nombre_ori = nombre * 8 / 100,
    org_donnee_ori = org_donnee * 6 / 100
  ),
  
  mutate(
    filter(cm2, session == "2010"),
    ecrire_ori = ecrire * 10 / 100,
    lire_ori = lire * 15 / 100,
    grammaire_ori = grammaire * 15 / 100,
    ortho_ori = ortho * 10 / 100,
    voca_ori = voca * 10 / 100,
    
    calcul_ori = calcul * 12 / 100,
    geometrie_ori = geometrie * 7 / 100,
    grand_mes_ori = grand_mes * 7 / 100,
    nombre_ori = nombre * 8 / 100,
    org_donnee_ori = org_donnee * 6 / 100
  ),
  
  mutate(
    filter(cm2, session == "2011"),
    ecrire_ori = ecrire * 10 / 100,
    lire_ori = lire * 15 / 100,
    grammaire_ori = grammaire * 15 / 100,
    ortho_ori = ortho * 10 / 100,
    voca_ori = voca * 10 / 100,
    
    calcul_ori = calcul * 13 / 100,
    geometrie_ori = geometrie * 7 / 100,
    grand_mes_ori = grand_mes * 6 / 100,
    nombre_ori = nombre * 7 / 100,
    org_donnee_ori = org_donnee * 7 / 100
  ),
  
  mutate(
    filter(cm2, session == "2012"),
    ecrire_ori = ecrire,
    lire_ori = lire,
    grammaire_ori = grammaire,
    ortho_ori = ortho,
    voca_ori = voca,
    
    calcul_ori = calcul,
    geometrie_ori = geometrie,
    grand_mes_ori = grand_mes,
    nombre_ori = nombre,
    org_donnee_ori = org_donnee
  )
) %>%
  mutate(
    score_f_ori = ecrire_ori + lire_ori +
      grammaire_ori + ortho_ori + voca_ori,
    
    score_m_ori = calcul_ori + geometrie_ori +
      grand_mes_ori + nombre_ori + org_donnee_ori,
    
    score_ori = score_f_ori + score_m_ori
  )
  # après vérification, seuls les scores ori de 2012 sont exacts
  # les scores réaggrégés sont plus cohérents
  # pas de doute là-dessus
  # (23/10/2020)

cm2 <- rbind(
  # 2009
  mutate(
    filter(cm2, session == "2009"),
    lire = sumer(
      filter(cm2, session == "2009"),
      c(27,
        1, 58,
        2, 3, 4, 5, 6,
        21, 24, 59, 60,
        22, 23, 25)
    ),
    ecrire = sumer(
      filter(cm2, session == "2009"),
      c(50, 51, 52,
        10, 11, 12, 13,
        14, 15, 26)
    ),
    voca = sumer(filter(cm2, session == "2009"),
                 c(8,
                   7, 9, 48, 49,
                   55, 56, 57,
                   53, 54)),
    grammaire = sumer(
      filter(cm2, session == "2009"),
      c(37, 38, 39, 40,
        16, 17, 18, 19, 20,
        32, 33, 34, 41,
        42, 43)
    ),
    ortho = sumer(
      filter(cm2, session == "2009"),
      c(28, 30,
        35, 36,
        29, 31, 44, 45,
        46, 47)
    ),
    nombre = sumer(filter(cm2, session == "2009"),
                   c(64, 65,
                     66, 67, 68,
                     71, 72, 73)),
    calcul = sumer(
      filter(cm2, session == "2009"),
      c(74, 75,
        69, 70,
        78, 79, 80, 81,
        82, 83,
        76, 77)
    ),
    geometrie = sumer(filter(cm2, session == "2009"),
                      c(88, 89,
                        87,
                        90, 91, 92, 93)),
    grand_mes = sumer(filter(cm2, session == "2009"),
                      c(84, 85,
                        94, 95,
                        86, 96, 97)),
    org_donnee = sumer(filter(cm2, session == "2009"),
                       c(62, 63,
                         61, 98, 99, 100))
  ) %>%
    mutate(
      SF = lire + ecrire + voca + grammaire + ortho,
      SM = nombre + calcul + geometrie + grand_mes + org_donnee,
      ST = SF + SM
    ),
  
  # 2010
  mutate(
    filter(cm2, session == "2010"),
    lire = sumer(
      filter(cm2, session == "2010"),
      c(27,
        1, 58,
        2, 3, 4, 5, 6, 21, 24,
        59, 60,
        8, 22,
        25)
    ),
    ecrire = sumer(
      filter(cm2, session == "2010"),
      c(50, 51,
        10, 11, 12, 13, 14,
        15, 16, 26)
    ),
    voca = sumer(filter(cm2, session == "2010"), c(9, 23,
                                                   7, 47,
                                                   54, 55, 56, 57,
                                                   52, 53)),
    grammaire = sumer(
      filter(cm2, session == "2010"),
      c(38, 39, 40, 41, 42,
        17, 18, 19, 20,
        34, 35, 43, 44, 45,
        46)
    ),
    ortho = sumer(
      filter(cm2, session == "2010"),
      c(28, 29, 30,
        36, 37,
        31, 32, 33, 48, 49)
    ),
    
    nombre = sumer(filter(cm2, session == "2010"), c(64, 65,
                                                     66, 67, 68,
                                                     71, 72, 73)),
    calcul = sumer(
      filter(cm2, session == "2010"),
      c(74, 75,
        69, 70,
        78, 79, 80, 81,
        82, 83,
        76, 77)
    ),
    geometrie = sumer(filter(cm2, session == "2010"), c(88, 89,
                                                        87,
                                                        90, 91, 92, 93)),
    grand_mes = sumer(filter(cm2, session == "2010"), c(84, 85,
                                                        94, 95,
                                                        86, 96, 97)),
    org_donnee = sumer(filter(cm2, session == "2010"), c(61, 62,
                                                         63, 98,
                                                         99, 100))
  ) %>%
    mutate(
      SF = lire + ecrire + voca + grammaire + ortho,
      SM = nombre + calcul + geometrie + grand_mes + org_donnee,
      ST = SF + SM
    ),
  
  # 2011
  
  mutate(
    filter(cm2, session == "2011"),
    lire = sumer(
      filter(cm2, session == "2011"),
      c(1, 27, 58,
        2, 3, 4, 5, 21, 24,
        59, 60,
        7, 8, 22, 25)
    ),
    ecrire = sumer(
      filter(cm2, session == "2011"),
      c(50, 51,
        10, 11, 12, 13, 14,
        15, 16, 26)
    ),
    voca = sumer(filter(cm2, session == "2011"), c(6, 23,
                                                   9, 43,
                                                   54, 55, 56, 57,
                                                   52, 53)),
    grammaire = sumer(
      filter(cm2, session == "2011"),
      c(36, 37, 38, 39, 40,
        17, 18, 19, 20,
        34, 35, 41, 42, 44, 45)
    ),
    ortho = sumer(
      filter(cm2, session == "2011"),
      c(28, 29, 30,
        46, 47,
        31, 32, 33, 48, 49)
    ),
    
    nombre = sumer(filter(cm2, session == "2011"), c(64, 65,
                                                     66, 67, 68,
                                                     69,
                                                     76)),
    calcul = sumer(
      filter(cm2, session == "2011"),
      c(72, 73,
        94, 95,
        81, 82, 83, 84,
        85, 86,
        78,
        74, 75)
    ),
    geometrie = sumer(filter(cm2, session == "2011"), c(90,
                                                        70, 71,
                                                        99, 100,
                                                        92, 93)),
    grand_mes = sumer(filter(cm2, session == "2011"), c(77, 79,
                                                        80,
                                                        91, 98,
                                                        87)),
    org_donnee = sumer(filter(cm2, session == "2011"), c(61, 62,
                                                         63, 96, 97,
                                                         88, 89))
  ) %>%
    mutate(
      SF = lire + ecrire + voca + grammaire + ortho,
      SM = nombre + calcul + geometrie + grand_mes + org_donnee,
      ST = SF + SM
    ),
  
  # 2012
  
  mutate(
    filter(cm2, session == "2012"),
    lire = sumer(
      filter(cm2, session == "2012"),
      c(1, 26,
        2, 4, 5, 7,
        21, 24, 57,
        58, 59,
        3, 8, 22, 25)
    ),
    ecrire = sumer(
      filter(cm2, session == "2012"),
      c(48, 49,
        20, 45,
        10, 11, 12,
        13, 14, 15)
    ),
    voca = sumer(filter(cm2, session == "2012"), c(6, 23,
                                                   9,
                                                   52,
                                                   53, 54,
                                                   55, 56,
                                                   50, 51)),
    grammaire = sumer(
      filter(cm2, session == "2012"),
      c(36, 37, 38,
        39, 40,
        16, 17, 18,
        19,
        34, 35, 41,
        42, 44,
        60)
    ),
    ortho = sumer(
      filter(cm2, session == "2012"),
      c(27, 28, 29,
        33, 43,
        30, 31, 32,
        46, 47)
    ),
    
    nombre = sumer(filter(cm2, session == "2012"), c(61,
                                                     64,
                                                     62, 63,
                                                     80, 81)),
    calcul = sumer(
      filter(cm2, session == "2012"),
      c(78,
        65, 66, 67,
        72, 73, 74,
        79,
        88, 89, 90,
        91,
        92, 93,
        85)
    ),
    geometrie = sumer(filter(cm2, session == "2012"), c(83,
                                                        82, 94,
                                                        97, 96)),
    grand_mes = sumer(filter(cm2, session == "2012"), c(68, 69,
                                                        99, 100,
                                                        95,
                                                        86, 87, 98)),
    org_donnee = sumer(filter(cm2, session == "2012"), c(84,
                                                         70, 71,
                                                         75, 76, 77))
  ) %>%
    mutate(
      SF = lire + ecrire + voca + grammaire + ortho,
      SM = nombre + calcul + geometrie + grand_mes + org_donnee,
      ST = SF + SM
    )
)

  # Renommage général. 

cm2 <- select(
  cm2,
  paste("i", 1:100, sep = ""),
  clef_cm2,
  commune,
  reseau,
  rnecirc,
  ecole = ceco,
  classe = cl,
  statut = Statut,
  nais = dnaiss,
  cohorte = session,
  
  score = ST,
  score_f = SF,
  score_m = SM,
  ecrire,
  lire,
  grammaire,
  ortho,
  voca,
  calcul,
  geometrie,
  grand_mes,
  nombre,
  org_donnee,
  
  score_ori,
  score_f_ori,
  score_m_ori,
  ecrire_ori,
  lire_ori,
  grammaire_ori,
  ortho_ori,
  voca_ori,
  calcul_ori,
  geometrie_ori,
  grand_mes_ori,
  nombre_ori,
  org_donnee_ori,
  
  sexe
)

  # Toutes les variables reliées à la date de naissance. 

cm2 <- mutate(
  cm2,
  nais = as.Date(nais, origin = "1960-01-01"),
  age_abs = if_else(
    cohorte == "2009",
    int_length(interval(nais,
                        as.Date("2009-02-04"))) /
      ((3600 * 24) * 365.25),
    if_else(
      cohorte == "2010",
      int_length(interval(nais,
                          as.Date("2010-01-20"))) /
        ((3600 * 24) * 365.25),
      if_else(
        cohorte == "2011",
        int_length(interval(nais,
                            as.Date("2011-01-20"))) /
          ((3600 * 24) * 365.25),
        int_length(interval(nais,
                            as.Date("2012-05-25"))) /
          ((3600 * 24) * 365.25)
      )
    )
  ),
  
  position_num = 11 - as.numeric(cohorte) + year(nais),
  position = if_else(
    position_num < 0,
    "Redoublant",
    if_else(position_num == 0, "Heure",
            "Avance")
  )
)

  # z, quinze, bim et trim. 

cm2 <- mutate(
  cm2,
  
  z = (int_length(interval(
    nais,
    as.Date(paste(year(nais), "-12-31", sep = ""))
  ) / 365.25)) / (3600 * 24),
  # j'ai vérifié jour_dnb ici, ça devrait être bon
  jour = round((1 - z) * 365.25),
  
  quinze = as.character(if_else(as.numeric(substr(
    nais, 9, 10
  )) <= 15, 1, 2) +
    (month(nais) - 1) * 2),
  
  mois = as.character(month(nais)),
  
  bim = as.character(if_else(
    month(nais) %in% c(1, 2), 1,
    if_else(month(nais) %in% c(3, 4), 2,
            if_else(
              month(nais) %in% c(5, 6), 3,
              if_else(month(nais) %in% c(7, 8), 4,
                      if_else(month(nais) %in% c(9, 10), 5, 6))
            ))
  )),
  
  trim = as.character(if_else(
    month(nais) %in% c(1:3), 1,
    if_else(month(nais) %in% c(4:6), 2,
            if_else(month(nais) %in% c(7:9), 3, 4))
  ))
)

  # Une valeur impossible de l'année de naissance : 2099 à enlever. 

cm2 <- filter(cm2, year(nais) != 2099)

   # Récupération des pcs. 

cm2 <- left_join(mutate(cm2,
                        clef_cm2coh = paste(clef_cm2, cohorte)),
                 select(tc, clef_cm2coh, clef_6emecoh, inecoh),
                 by = "clef_cm2coh") %>%
  left_join(transmute(
    base6eme,
    clef_6emecoh = paste(clef_6eme, cohorte),
    pcs6eme = cpcs,
    libpcs6eme = lpcs
  ),
  by = "clef_6emecoh")

  # INE uniquement, afin de récupérer les CSP au DNB 

cm2 <- mutate(cm2, 
              ine = substr(inecoh, 1, nchar(inecoh) - 5))

  # NB : 5 doublons d'INE au CM2 : des élèves ayant passé deux examens.

# Int : corr pcs dupl dnb et constat1316 ----------------------------------

  # Correction des CSP dupliquées au DNB et constat1316. 
  # Nécessaire pour les mettre dans cm2


dnb <- left_join(
  dnb,
  group_by(dnb, ine, session, pcsdnb_corr = pcs) %>% summarise %>%
    arrange(ine, session) %>% as.data.frame %>%
    filter(!duplicated(ine)) %>% select(-session),
  by = "ine"
)
  # nobs gardé

constat1316 <- left_join(
  constat1316,
  group_by(constat1316, ine, cohorte, pcsconstat_corr = cpcs) %>%
    summarise %>% arrange(ine, cohorte) %>% as.data.frame %>%
    filter(!duplicated(ine)) %>% select(-cohorte),
  by = "ine"
)
  # nobs gardé

  # La récupération. 
  # Rappel : pour 2011, pcs6eme pose problème.
  # Ainsi, pour cette cohorte, la CSP sera à aller chercher dans constat1316

  ## depuis constat
cm2 <- left_join(cm2, 
                 summarise(group_by(constat1316, ine, pcsconstat_corr)),
                 by = "ine")
  # nobs gardé : 42060 personnes

  # Création des CSP. 
  # pcs6eme pour 2010 et 2012 et pcsconstat_corr pour 2011.

cm2 <- mutate(cm2,
              pcsn2 = ifelse(cohorte == "2011", pcsconstat_corr, pcs6eme))

# à un chiffre
cm2 <- mutate(cm2,
              pcs = as.character(
                fct_recode(
                  factor(substr(pcsn2, 1, 1)),
                  "Agriculteurs" = "1",
                  "ArtComChefEnt" = "2",
                  "CadrPrIntSup" = "3",
                  "ProfInt" = "4",
                  "Employes" = "5",
                  "Ouvriers" = "6",
                  "Retraites" = "7",
                  "Inactifs" = "8",
                  "Autres" = "9"
                )
              ))
# cm2$pcs[is.na(cm2$pcs)] <- "Autres"

# Regroupement de Grenet 2009.
cm2 <- mutate(cm2,
              pcs_g = as.character(fct_collapse(
                pcs,
                "unpriv" = c(
                  "Agriculteurs",
                  "Employes",
                  "Ouvriers",
                  "Retraites",
                  "Inactifs"
                ),
                "priv" = c("ArtComChefEnt",
                           "CadrPrIntSup",
                           "ProfInt"),
                "Autres" = "Autres"
              )))

# Regroupement de l'INSEE.
cm2 <- mutate(cm2,
              pcs_g2 = as.character(
                fct_collapse(
                  pcsn2,
                  "Defav" = c("61", "66", "69",
                              "76", "81", "82"),
                  "Moy" = c("10", "21", "22", "52", "53",
                            "54", "55", "56", "71", "72"),
                  "Fav" = c("43", "44", "45", "46",
                            "47", "48", "73"),
                  "Tresfav" = c("23", "31", "33",
                                "34", "35", "37",
                                "38", "42"),
                  "Autres" = "99"
                )
              ))

# cm2$pcs_g2[cm2$pcs_g == "Autres"] <- "Autres"

  # pcsv2 pour montrer la robustesse des résultats
  # à un chiffre
cm2 <- mutate(cm2,
              pcsv2 = as.character(
                fct_recode(
                  factor(substr(pcsn2, 1, 1)),
                  "Agriculteurs" = "1",
                  "ArtComChefEnt" = "2",
                  "CadrPrIntSup" = "3",
                  "ProfInt" = "4",
                  "Employes" = "5",
                  "Ouvriers" = "6",
                  "Retraites" = "7",
                  "Inactifs" = "8",
                  "Autres" = "9"
                )
              ))
cm2$pcsv2[is.na(cm2$pcsv2)] <- "Autres"

  # Regroupement de Grenet 2009.
cm2 <- mutate(cm2,
              pcsv2_g = as.character(
                fct_collapse(
                  pcsv2,
                  "unpriv" = c(
                    "Agriculteurs",
                    "Employes",
                    "Ouvriers",
                    "Retraites",
                    "Inactifs"
                  ),
                  "priv" = c("ArtComChefEnt",
                             "CadrPrIntSup",
                             "ProfInt"),
                  "Autres" = "Autres"
                )
              ))

  # Regroupement de l'INSEE.
cm2 <- mutate(cm2,
              pcsv2_g2 = as.character(
                fct_collapse(
                  pcs6eme,
                  "Defav" = c("61", "66", "69",
                              "76", "81", "82"),
                  "Moy" = c("10", "21", "22", "52", "53",
                            "54", "55", "56", "71", "72"),
                  "Fav" = c("43", "44", "45", "46",
                            "47", "48", "73"),
                  "Tresfav" = c("23", "31", "33",
                                "34", "35", "37",
                                "38", "42"),
                  "Autres" = "99"
                )
              ))

cm2$pcsv2_g2[cm2$pcsv2_g == "Autres"] <- "Autres"

  # Les arrondissements. 

cm2 <- mutate(cm2,
              arrondissement = as.character(
                fct_collapse(
                  factor(commune),
                  "saint_benoit" = c(
                    "BRAS-PANON",
                    "LA PLAINE-DES-PALMISTES",
                    "SAINT-ANDRE",
                    "SAINT-BENOIT",
                    "SAINTE-ROSE",
                    "SALAZIE"
                  ),
                  "saint_denis" = c("SAINT-DENIS", "SAINTE-MARIE", "SAINTE-SUZANNE"),
                  "saint_paul" = c(
                    "LE PORT",
                    "LA POSSESSION",
                    "SAINT-LEU",
                    "SAINT-PAUL",
                    "LES TROIS-BASSINS"
                  ),
                  "saint_pierre" = c(
                    "LES AVIRONS",
                    "CILAOS",
                    "ENTRE-DEUX",
                    "L'ETANG-SALE",
                    "PETITE-ILE",
                    "SAINT-JOSEPH",
                    "SAINT-LOUIS",
                    "SAINT-PHILIPPE",
                    "SAINT-PIERRE",
                    "LE TAMPON"
                  )
                )
              ))

  # reseau_bin. 

cm2 <- mutate(cm2,
              reseau_bin = if_else(reseau == "HEP", "non", "oui"))

# cm2 - enlever pos impossibles -------------------------------------------

cm2_wvextp <- cm2
cm2 <- filter(cm2, position_num %in% c(- 2:2))

  # classecoh
cm2 <- mutate(cm2, classecoh = paste(classe, cohorte))
  # tclassecoh à construire spécifiquement dans le contexte de l'étude. 


  # ecolecoh
cm2 <- mutate(cm2, ecolecoh = paste(ecole, cohorte))

# Distinction entre cm2_w09 et cm2. Le premier étant la base avec les 2009. 
# En effet dans l'analyse principale, on ne prend pas les 2009. 
# Justification : pas de CSP dans les 2009. 

cm2_w09 <- cm2
cm2 <- filter(cm2, cohorte != "2009")
cm209 <- filter(cm2_w09, cohorte == "2009")

# rdcm2 -------------------------------------------------------------------

rdcm2 <- filter(cm2_w09,
                year(nais) %in% c(1999, 2000)) %>%
  mutate(
    dist = difftime(nais, as.Date("2000-01-01"), unit = "days") %>% as.numeric,
    old = 1 * (dist >= 0),
    dist_m = if_else(year(nais) == 1999,
                     as.numeric(mois) - 13,
                     as.numeric(mois) - 1)
  )

# base6eme ----------------------------------------------------------------

# Juste une copie de ce qui est dans 31_const_prejoin. Quasiment pas exploité.

base6eme <- select(base6eme,-age6eme,-annee,-mois)

base6eme <- mutate(base6eme,
                   clefgestion = as.character(
                     fct_collapse(
                       clefgestion,
                       "6BILNG" = c("6BILN", "6BILNG"),
                       "6EUROP" = c("6EURO", "6EUROP"),
                       "6HIART" = c("6HIAR", "6HIART"),
                       "6INTE" = c("6 INT", "6 INTE"),
                       "6SPOR" = c("6 SPO", "6 SPOR"),
                       "6THEAT" = c("6THEA", "6THEAT")
                     )
                   ))

base6eme <- cbind(base6eme,
                  rename_all(base6eme, function (x)
                    paste(x,
                          "_mod", sep = "")))

base6eme <- mutate(
  base6eme,
  bourse_mod = fct_collapse(
    bourse,
    "non" = "0",
    "bcollege" = "8",
    "nationale" = "1",
    "enssup" = "4",
    "departementale" = "2"
  ) %>%
    factor(
      levels = c("non", "bcollege", "nationale", "enssup", "departementale")
    ),
  # je ne touche pas à clefgestion pour le moment (06 dec 2019)
  # ne touche pas à codcomm/nomcomm et comnat/lacommune pour le moment
  
  # coption1 : absent en 2010
  coption1_mod = fct_collapse(
    coption1,
    "chinois" = "030401",
    "allemand" = "030101",
    "espagnol" = "030601",
    "anglais" = "030201"
  ) %>%
    factor(levels = c(
      "chinois", "allemand", "espagnol", "anglais"
    ))
)


# coption2 et loption2, contient des nchar = 0, il faut corriger cela

base6eme$coption2_mod[nchar(as.character(base6eme$coption2_mod)) == 0] <-
  NA
base6eme$loption2_mod[nchar(as.character(base6eme$loption2_mod)) == 0] <-
  NA


base6eme <- mutate(base6eme, 
                   # je vais regrouper pour que cela soit plus facile
                   coption2_mod = fct_collapse(coption2_mod,
                                               "allemand" = c("030101", "030102"),
                                               "anglais" = c("030201", "030202", "030204"),
                                               "chinois" = c("030401", "030402"),
                                               "espagnol" = c("030601", "030602")) %>% 
                     as.character,
                   coption2_mod = if_else(! is.na(coption2_mod) & 
                                            ! coption2_mod %in% c("allemand", "anglais", "chinois", "espagnol"),
                                          "autres", coption2_mod) %>% 
                     factor(levels = c("autres", "chinois", "allemand", "espagnol", "anglais"))
                   #ok
                   # pour coption3, il faudra encore enlever les nchar = 0 (16h59)
)


base6eme$coption3_mod[nchar(as.character(base6eme$coption3_mod)) == 0] <- NA
base6eme$loption3_mod[nchar(as.character(base6eme$loption3_mod)) == 0] <- NA


# il est encore difficile de regrouper la variable coption3, de plus qu'il n'y a pas beaucoup d'élèves qui l'ont pris
# => je vais m'en tenir à ce qui est mentionné dans cption3 original

base6eme <- mutate(
  base6eme,
  coption3_mod = as.character(coption3_mod),
  coption3_mod = fct_collapse(
    factor(coption3_mod),
    "soutien" = "00140",
    "trait.diff.sco" = "003800",
    "ae.aid.dev.lecon" = "009800",
    "lang.cult.reg" = "02300",
    "aid.indiv.fr" = "023100",
    "sect.chin" = "030D09",
    "aid.indiv.maths" = "068300",
    "ae.pratiq.artcult" = "099800",
    "rugby" = "10160",
    "escalade" = "102200",
    "ae.pratiq.sport" = "109800"
  ) %>%
    factor(
      levels = c(
        "soutien",
        "trait.diff.sco",
        "ae.aid.dev.lecon",
        "lang.cult.reg",
        "aid.indiv.fr",
        "sect.chin",
        "aid.indiv.maths",
        "ae.pratiq.artcult",
        "rugby",
        "escalade",
        "ae.pratiq.sport"
      )
      # ici, je ne sais pas encore s'il est nécessaire de mettre des levels
    )
)


# 09 dec 2019 19h30 : pcs on laisse pour plus tard


# cregime (uniquement en 2012)

base6eme <- mutate(
  base6eme,
  cregime_mod = fct_collapse(
    cregime,
    "ext.lib" = "0",
    "ext.surv" = "1",
    "demipens.etab" = "2",
    "interne" = "3",
    "demipens.hors" = "6"
  ) %>%
    factor(
      levels = c(
        "interne",
        "ext.surv",
        "demipens.hors",
        "ext.lib",
        "demipens.etab"
      )
    )
)

# ok

# cstatut : inutile

# ctransport (uniquement en 2012) : inconnu mais on peut deviner que 1 = oui et 2 = non
# à partir du doc de panel : À CONFIRMER, PAS SÛR DU TOUT

base6eme <- mutate(base6eme,
                   ctransport_mod = fct_collapse(ctransport,
                                                 "oui" = "1",
                                                 "non" = "2") %>%
                     factor(levels = c("non", "oui")))

# ok (mais à vérifier)

# denom : pas à modifier

# div : pas à modifier

# lacommune : pas à modifier
# lbourse mais pas à modifier

# mef et libmef : à étudier longuement

# attention : div n'est pas la classe et il y a des genres de regroupement, tronc commun, etc.
# à comprendre

# également à comprendre : le mefstat4 => le plus important c'est le mef
# mais pour la compréhension du système, vaut mieux connaître le mefstat4


base6eme <- mutate(
  base6eme,
  mef_mod = fct_collapse(
    mef,
    "6e.hist.art" = "1001000G11A",
    "6e" = "10010012110",
    "6e.inter" = "10010012111",
    "6e.sect.eur.lang.ori" = "10010012112",
    "6e.sect.sport" = "10010012117",
    "6e.cl.bilang" = "10010012119",
    "6e.theatre" = "10010023110"
  ) %>%
    factor(
      levels = c(
        "6e.theatre",
        "6e.inter",
        "6e.hist.art",
        "6e.sect.eur.lang.ori",
        "6e.sect.sport",
        "6e.cl.bilang",
        "6e"
      )
    )
)

# ok


# en fait, on peut filtrer ceux qui sont en 6eme avec le mefstat4 / llmefstat4


# nenfant (absent en 2011) : inconnu


# sigle : clg et clg p uniquement saut en 2010

base6eme <- mutate(
  base6eme,
  sigle_mod = fct_collapse(
    sigle,
    "college" = "CLG",
    "college privé" = c("CLG P", "CLG PR"),
    "lgt" = "LGT",
    "lgt_pr" = "LGT PR",
    "lp" = "LP",
    "lp_metiers" = "LP LYC",
    "lpo" = "LPO",
    "lpo_metiers" = "LPO LY",
    "lpo_pr" = "LPO PR",
    "lp_pr" = "LP PR"
  )
)

# !!
# c'est bon pour base6eme


# correction générale des classes
# il a été vérifié que les fct_collapse marchent

base6eme <- mutate_if(base6eme, is.factor, as.character)

# constat1316 -------------------------------------------------------------

constat1316 <- mutate(constat1316,
                      session = as.character(as.numeric(cohorte) + 1))

constat1316 <- cbind(constat1316,
                     constat1316 %>%
                       set_colnames(paste(names(constat1316),
                                          "_mod", sep = "")))

# le 16 dec 2019 à 13h22 : problème d'encodage classique sur loption4
constat1316$loption4_mod <-
  stri_encode(constat1316$loption4, "", "latin1")
constat1316$loption4_mod <-
  str_replace_all(constat1316$loption4, "\032", "..")


# un problème d'encodage sur denom
constat1316$denom_mod <-
  stri_encode(constat1316$denom, "", "latin1")

# on va anticiper tous les problèmes d'encodage possible
constat1316$lpcs_mod <- stri_encode(constat1316$lpcs, "", "latin1")
constat1316$lpcs_mod <-
  str_replace_all(constat1316$lpcs_mod, "\032", "..")

constat1316$llpcs_mod <-
  stri_encode(constat1316$llpcs, "", "latin1")
constat1316$llpcs_mod <-
  str_replace_all(constat1316$llpcs_mod, "\032", "..")


constat1316$denom_mod <-
  stri_encode(constat1316$denom, "", "latin1")

constat1316$libmef_mod <-
  stri_encode(constat1316$libmef, "", "latin1")

constat1316$lmefstat4_mod <-
  stri_encode(constat1316$lmefstat4, "", "latin1")
constat1316$lmefstat4_mod <-
  str_replace_all(constat1316$lmefstat4_mod, "\032", "..")

constat1316$llmefstat4_mod <-
  stri_encode(constat1316$llmefstat4, "", "latin1")
constat1316$llmefstat4_mod <-
  str_replace_all(constat1316$llmefstat4_mod, "\032", "..")


constat1316$libmeforigine_mod <-
  stri_encode(constat1316$libmeforigine, "", "latin1")


constat1316$lmefstat4origine_mod <-
  stri_encode(constat1316$lmefstat4origine, "", "latin1")
constat1316$lmefstat4origine_mod <-
  str_replace_all(constat1316$lmefstat4origine_mod, "\032", "..")


constat1316$denom_origine_mod <-
  stri_encode(constat1316$denom_origine, "", "latin1")


# sexe : 1 = M et 2 = F (vérifié grâce à des jointures et comptage)
constat1316 <- mutate(constat1316,
                      sexe_mod = if_else(sexe == "1", "M", "F"))

# les modalités ont été vérifiées une à une


# Probème d'encodage sur les denom des écoles. 
# 04 Mars 2021. 

constat1316 <- mutate_if(constat1316, is.character,
                         
                         function (x)
                           gsub("\\xc7", "C", x) %>%
                           (function (y)
                             gsub("\\xc8", "E", y)) %>%
                           (function (y)
                             gsub("\\xd4", "O", y)) %>%
                           (function (y)
                             gsub("\\xc2", "A", y)) %>%
                           (function (y)
                             gsub("\\xca", "E", y)) %>%
                           str_replace("  ", " "))

constat1316 <- mutate(
  constat1316,
  pcsconstat = substr(cpcs_mod, 1, 1),
  pcsconstat = as.character(
    fct_recode(
      pcsconstat,
      "Agriculteurs" = "1",
      "ArtComChefEnt" = "2",
      "CadrPrIntSup" = "3",
      "ProfInt" = "4",
      "Employes" = "5",
      "Ouvriers" = "6",
      "Retraites" = "7",
      "Inactifs" = "8",
      "Autres" = "9"
    )
  ),
  divconstatrne = paste(rne_mod, div_mod),
  divconstatrneses = paste(rne_mod, div_mod, session_mod),
  rneconstatses = paste(rne_mod, session_mod),
  rnelmefstat4modses = paste(rne_mod, lmefstat4_mod, session_mod)
)

constat1316 <- group_by(constat1316, divconstatrneses) %>% 
  mutate(tdivconstatrneses = n()) %>% ungroup

constat1316 <- mutate(constat1316,
                      lmefstat4_mod = as.character(
                        fct_collapse(
                          factor(lmefstat4),
                          "6eme" = "6EME",
                          "6eme_segpa" = "6E SEGPA",
                          "5eme" = "5EME",
                          "5eme_segpa" = "5E SEGPA",
                          "4eme" = c("4EME GENE", "4EME"),
                          "4eme_segpa" = "4E SEGPA",
                          "3eme" = c("3EME GENE", "3EME"),
                          "3eme_segpa" = "3E SEGPA",
                          "2nde_gt" = "2NDE G-T",
                          "2nde_pro" = "2NDE PRO",
                          "niveau3_autre" = c("1BMA2",  "1CAP2", "DIMA",
                                              "ULIS", "MLDS", "MC",
                                              "PDPREBAC1"),
                          "1ere_gt" = "1ERE G-T",
                          "1ere_pro" = "1ERE PRO",
                          "term_gt" = "TERM G-T",
                          "term_pro" = "TLEPRO"
                        )
                      ))

constat1316 <- mutate(constat1316,
                      lmefstat4_mod = if_else(
                        as.character(lmefstat4_mod) %in%
                          c(
                            "6eme",
                            "6eme_segpa",
                            "5eme",
                            "5eme_segpa",
                            "4eme",
                            "4eme_segpa",
                            "3eme",
                            "3eme_segpa",
                            "2nde_gt",
                            "2nde_pro",
                            "niveau3_autre",
                            "1ere_gt",
                            "1ere_pro",
                            "term_gt",
                            "term_pro"
                          ),
                        as.character(lmefstat4_mod),
                        "autres"
                      ))


  # Variables liées à la date de naissance. 


constat1316 <-
  mutate(
    constat1316,
    dnais_mod = as.Date(dnais_mod, format = "%d/%m/%Y"),
    age1sep = int_length(interval(dnais_mod,
                                  as.Date(
                                    paste(cohorte_mod, "-09-01",
                                          sep = "")
                                  )) / 365.25) /
      (3600 * 24),
    
    niveauconstat = as.character(
      fct_collapse(
        lmefstat4_mod,
        "6" = c("6eme", "6eme_segpa"),
        "5" = c("5eme", "5eme_segpa"),
        "4" = c("4eme", "4eme_segpa"),
        "3" = c("3eme", "3eme_segpa"),
        "2" = c("2nde_gt", "2nde_pro", "niveau3_autre"),
        "1" = c("1ere_gt", "1ere_pro"),
        "0" = c("term_gt", "term_pro"),
        "autres" = "autres"
      )
    ),
    
    positionconstat_num = if_else(
      niveauconstat == "6",
      year(dnais_mod) + 12 - as.numeric(session_mod),
      if_else(
        niveauconstat == "5",
        year(dnais_mod) + 13 -
          as.numeric(session_mod),
        if_else(
          niveauconstat == "4",
          year(dnais_mod) + 14 -
            as.numeric(session_mod),
          if_else(
            niveauconstat == "3",
            year(dnais_mod) + 15 - as.numeric(session_mod),
            if_else(
              niveauconstat == "2",
              year(dnais_mod) + 16 - as.numeric(session_mod),
              if_else(
                niveauconstat == "1",
                year(dnais_mod) + 17 - as.numeric(session_mod),
                if_else(
                  niveauconstat == "0",
                  year(dnais_mod) + 18 - as.numeric(session_mod),
                  10000
                )
              )
            )
          )
        )
      )
    )
  )




constat1316 <- mutate(constat1316,
                      
                      positionconstat = ifelse(
                        positionconstat_num <= -1,
                        "Redoublant",
                        ifelse(
                          positionconstat_num == 0,
                          "Heure",
                          ifelse(positionconstat_num >=  1,
                                 "Avance", NA)
                        )
                      ))


# reste lbourses et pcs_g à créer
constat1316 <- mutate(
  constat1316,
  lbourses_mod = as.character(
    fct_recode(
      factor(lbourses),
      "non_boursier" = "NON BOURSIER",
      "bourse_nationale" = "BOURSE NATIONALE",
      "bourse_departementale" = "BOURSE DEPARTEMENTALE",
      "bourse_des_colleges" = "BOURSE DES COLLEGES",
      "bourse_ens_sup" = "BOURSE ENSEIGNEMENT SUPER"
    )
  ),
  pcs_gconstat = as.character(
    fct_collapse(
      factor(pcsconstat),
      "unpriv" = c(
        "Agriculteurs",
        "Employes",
        "Ouvriers",
        "Retraites",
        "Inactifs"
      ),
      "priv" = c("ArtComChefEnt", "CadrPrIntSup", "ProfInt"),
      "Autres" = "Autres"
    )
  )
)

# rajouté le 24 juin 2020
constat1316 <- mutate(constat1316,
                      pcs_g2constat = as.character(
                        fct_collapse(
                          pcsconstat_corr,
                          "Defav" = c("61", "66", "69",
                                      "76", "81", "82"),
                          "Moy" = c("10", "21", "22", "52", "53",
                                    "54", "55", "56", "71", "72"),
                          "Fav" = c("43", "44", "45", "46",
                                    "47", "48", "73"),
                          "Tresfav" = c("23", "31", "33",
                                        "34", "35", "37",
                                        "38", "42"),
                          "Autres" = "99"
                        )
                      ))

# pas besoin de pcsv2. Dans le cas contraire, il sera toujours possible de
# revenir en arrière. 



# Le 04 mars 2021 : remplacement des variables nchar = 0 en NA
# déjà vérifié que ces variables sont les seules avec nchar 0
# dans constat, que je vais utiliser. (à ce jour, actualisable)
constat1316 <- mutate(
  constat1316,
  coption1_mod = coption1_mod %>%
    (function (x) {
      x[nchar(x) == 0] <- NA
      x
    }) ,
  
  loption1_mod = loption1_mod %>%
    (function (x) {
      x[nchar(x) == 0] <- NA
      x
    }),
  
  coption2_mod = coption2_mod %>%
    (function (x) {
      x[nchar(x) == 0] <- NA
      x
    }),
  
  loption2_mod = loption2_mod %>%
    (function (x) {
      x[nchar(x) == 0] <- NA
      x
    }),
  
  coption3_mod = coption3_mod %>%
    (function (x) {
      x[nchar(x) == 0] <- NA
      x
    }),
  
  loption3_mod = loption3_mod %>%
    (function (x) {
      x[nchar(x) == 0] <- NA
      x
    }),
  
  coption4_mod = coption4_mod %>%
    (function (x) {
      x[nchar(x) == 0] <- NA
      x
    }),
  
  loption4_mod = loption4_mod %>%
    (function (x) {
      x[nchar(x) == 0] <- NA
      x
    }),
  
  coption5_mod = coption5_mod %>%
    (function (x) {
      x[nchar(x) == 0] <- NA
      x
    }),
  
  loption5_mod = loption5_mod %>%
    (function (x) {
      x[nchar(x) == 0] <- NA
      x
    }),
  
  coption6_mod = coption6_mod %>%
    (function (x) {
      x[nchar(x) == 0] <- NA
      x
    }),
  
  loption6_mod = loption6_mod %>%
    (function (x) {
      x[nchar(x) == 0] <- NA
      x
    }),
  
  coption7_mod = coption7_mod %>%
    (function (x) {
      x[nchar(x) == 0] <- NA
      x
    }),
  
  loption7_mod = loption7_mod %>%
    (function (x) {
      x[nchar(x) == 0] <- NA
      x
    }),
  
  coption8_mod = coption8_mod %>%
    (function (x) {
      x[nchar(x) == 0] <- NA
      x
    }),
  
  loption8_mod = loption8_mod %>%
    (function (x) {
      x[nchar(x) == 0] <- NA
      x
    }),
  
  denom_origine_mod = denom_origine_mod %>%
    (function (x) {
      x[nchar(x) == 0] <- NA
      x
    })
)
# vérifié.

# Le 16 mars 2021 : variables régime
constat1316 <- mutate(
  constat1316,
  lregime_mod = fct_recode(
    cregime,
    "dp_dan" = "2",
    "dp_hor" = "6",
    "int_dan" = "3",
    "int_heb" = "5",
    "int_ext" = "4",
    "ext_sur" = "1",
    "ext_lib" = "0"
  ) %>%
    factor(
      levels = c(
        "dp_dan",
        "dp_hor",
        "int_dan",
        "int_heb",
        "int_ext",
        "ext_sur",
        "ext_lib"
      )
    ),
  
  lregime_mod_g =
    fct_collapse(
      lregime_mod,
      "dp" = c("dp_dan", "dp_hor"),
      "int" = c("int_dan", "int_heb", "int_ext"),
      "ext" = c("ext_sur", "ext_lib")
    ) %>%
    factor(levels = c("dp", "int", "ext"))
)

# dnb ---------------------------------------------------------------------

dnb <- select(dnb, - ydnais, - mois_naiss)

dnb <- mutate(dnb,
              typ_etab = ifelse(nchar(as.character(typ_etab)) == 0,
                                NA, as.character(typ_etab)))

dnb <- mutate(dnb, typ_etab = as.character(factor(
  typ_etab,
  levels = c("1PUB", "CNED", "C_PR",
             "L_PR", "MFR")
)))

dnb <- mutate(dnb, reseau = as.character(
  fct_collapse(
    reseau,
    "HEP" = c("HEP", "OUT"),
    "REP/ex-ECL" = c("REP", "ECL"),
    "REP+/ex-RRS" = c("REP+", "RRS")
  )
))

dnb <- mutate(dnb, reseauR = if_else(reseau == "HEP", 0, 1))

dnb <- mutate(dnb, serie_c = str_replace_all(serie_c, " ", ""))

dnb <- cbind(dnb,
             rename_all(dnb, function (x)
               paste(x, "_mod", sep = "")))

# attention ici : prendre les pcs non dupliquées pour un même INE
dnb <- mutate(dnb,
              pcs_mod = as.character(
                fct_recode(
                  substr(pcs, 1, 1),
                  "Agriculteurs" = "1",
                  "ArtComChefEnt" = "2",
                  "CadrPrIntSup" = "3",
                  "ProfInt" = "4",
                  "Employes" = "5",
                  "Ouvriers" = "6",
                  "Retraites" = "7",
                  "Inactifs" = "8",
                  "Autres" = "9"
                )
              ),
              pcsdnb_corr_mod = as.character(
                fct_recode(
                  substr(pcsdnb_corr, 1, 1),
                  "Agriculteurs" = "1",
                  "ArtComChefEnt" = "2",
                  "CadrPrIntSup" = "3",
                  "ProfInt" = "4",
                  "Employes" = "5",
                  "Ouvriers" = "6",
                  "Retraites" = "7",
                  "Inactifs" = "8",
                  "Autres" = "9"
                )
              ))
dnb <- mutate(
  dnb,
  pcs_gdnb = as.character(
    fct_collapse(
      pcs_mod,
      "unpriv" = c(
        "Agriculteurs",
        "Employes",
        "Ouvriers",
        "Retraites",
        "Inactifs"
      ),
      "priv" = c("ArtComChefEnt",
                 "CadrPrIntSup",
                 "ProfInt"),
      "Autres" = "Autres"
    )
  ),
  pcsdnb_corrgdnb = as.character(
    fct_collapse(
      pcsdnb_corr_mod,
      "unpriv" = c(
        "Agriculteurs",
        "Employes",
        "Ouvriers",
        "Retraites",
        "Inactifs"
      ),
      "priv" = c("ArtComChefEnt",
                 "CadrPrIntSup",
                 "ProfInt"),
      "Autres" = "Autres"
    )
  )
)

dnb <- mutate(dnb,
              pcs_reg_mod = as.character(
                fct_recode(
                  pcs_reg,
                  "Defav" = "4Defav",
                  "Moy" = "3Moy",
                  "Fav" = "2Fav",
                  "Tresfav" = "1Tresfav"
                )
              ))

dnb$pcs_reg_mod[dnb$pcs_mod == "Autres"] <- "Autres"
# pas besoin de faire comme dans cm2 car ces regroupements sont déjà bon
# vérifié le 13 oct 2020 que cela correspond bien au regroupement de la depp
# (Metayer el al.)

# récupération des vars utiles depuis constat
# les variables utiles sont : ine, session_mod, div_mod,
# rne_mod, tdivconstatrneses,
# denom_mod, sigle_mod, statut_mod, libmef_mod, clefgestion_mod,
# mefstat4_mod, lmefstat4_mod, llmefstat4_mod,
# rne_origine_mod, "denom_origine_mod, meforigine_mod,
# lmefstat4origine_mod, llmefstat4origine_mod,

# coption1_mod, loption1_mod, coption2_mod, loption2_mod,
# coption3_mod, loption3_mod, coption4_mod, loption4_mod,
# coption5_mod, loption5_mod, coption6_mod, loption6_mod,
# coption7_mod, loption7_mod, coption8_mod, loption8_mod,

# cbourses_mod, lbourses_mod

# dnais_mod

dnb <- left_join(
  dnb,
  transmute(
    constat1316,
    ine,
    session_mod,
    div_mod,
    rne_mod,
    # depuis constat, à ne pas confondre avec cetab_mod
    # denom_constat = denom_mod,
    sigle_constat = sigle_mod,
    statut_constat = statut_mod,
    # libmef_constat = libmef_mod,
    clefgestion_constat = clefgestion_mod,
    niveauconstat,
    mefstat4_constat = mefstat4_mod,
    lmefstat4_constat = lmefstat4_mod,
    llmefstat4_constat = llmefstat4_mod,
    rne_origine_constat = rne_origine_mod,
    # denom_origine_constat = denom_origine_mod,
    meforigine_constat = meforigine_mod,
    lmefstat4origine_constat = lmefstat4origine_mod,
    coption1_constat = coption1_mod,
    loption1_constat = loption1_mod,
    coption2_constat = coption2_mod,
    loption2_constat = loption2_mod,
    coption3_constat = coption3_mod,
    loption3_constat = loption3_mod,
    coption4_constat = coption4_mod,
    loption4_constat = loption4_mod,
    coption5_constat = coption5_mod,
    loption5_constat = loption5_mod,
    coption6_constat = coption6_mod,
    loption6_constat = loption6_mod,
    coption7_constat = coption7_mod,
    loption7_constat = loption7_mod,
    coption8_constat = coption8_mod,
    loption8_constat = loption8_mod,
    
    cbourses_constat = cbourses_mod,
    lbourses_constat = lbourses_mod,
    
    dnais_constat = dnais_mod,
    
    tdivdnbrneses = tdivconstatrneses
    
  ),
  by = c("ine", "session_mod")
)
# le nombre de lignes n'a pas bougé : 43705 lignes


# les écoles dont on n'a pas pu obtenir les statuts (correction manuelle)

dnb$statut_constat[dnb$cetab_mod == "0761295V"] <- "PU"
dnb$statut_constat[dnb$cetab_mod == "9740006N"] <- "PU"
dnb$statut_constat[dnb$cetab_mod == "9740027L"] <- "PU"
dnb$statut_constat[dnb$cetab_mod == "9740036W"] <- "PU"
dnb$statut_constat[dnb$cetab_mod == "9740072K"] <- "PR"
dnb$statut_constat[dnb$cetab_mod == "9740572D"] <- "PU"
dnb$statut_constat[dnb$cetab_mod == "9740574F"] <- "PU"
dnb$statut_constat[dnb$cetab_mod == "9740576H"] <- "PU"
dnb$statut_constat[dnb$cetab_mod == "9740595D"] <- "PU"
dnb$statut_constat[dnb$cetab_mod == "9740599H"] <- "PU"
dnb$statut_constat[dnb$cetab_mod == "9740645H"] <- "PU"
dnb$statut_constat[dnb$cetab_mod == "9740652R"] <- "PU"
dnb$statut_constat[dnb$cetab_mod == "9740679V"] <- "PR"
dnb$statut_constat[dnb$cetab_mod == "9740702V"] <- "PU"
dnb$statut_constat[dnb$cetab_mod == "9740735F"] <- "PU"
dnb$statut_constat[dnb$cetab_mod == "9740841W"] <- "PU"
dnb$statut_constat[dnb$cetab_mod == "9740932V"] <- "PU"
dnb$statut_constat[dnb$cetab_mod == "9741048W"] <- "PU"
dnb$statut_constat[dnb$cetab_mod == "9741308D"] <- "PR"
dnb$statut_constat[dnb$cetab_mod == "9741387P"] <- "PU"
dnb$statut_constat[dnb$cetab_mod == "9741546M"] <- "PR"


# il y a encre 74 autres écoles dont on ne connaît pas le statut, il faut les injecter
# on pourrait utiliser base6eme : on en retrouve 60, rne_mod et statut_mod sont oneone (6eme)

dnb <- left_join(
  dnb,
  transmute(base6eme, rne_mod, statut6eme = statut_mod) %>%
    group_by(rne_mod, statut6eme) %>% summarise,
  by = c("cetab_mod" = "rne_mod")
) %>%
  mutate(statut_constat = if_else(is.na(statut_constat), statut6eme, statut_constat))
# les dimensions sont gardées : 43705 observations

# il reste 14 écoles dont les statuts sont à identifier
# elles ne sont pas toutes retrouvables sur internet ci-dessous celles identifiables
# autre source d'info : typ_etab (dnb)
# liste : filter(dnb, is.na(statut_constat)) %>% group_by(cetab_mod, typ_etab) %>% summarise %>% arrange(cetab_mod)

dnb$statut_constat[dnb$cetab_mod == "9741056E"] <- "PU"
dnb$statut_constat[dnb$cetab_mod == "9741057F"] <- "PU"
dnb$statut_constat[dnb$cetab_mod == "9741059H"] <- "PU"
dnb$statut_constat[dnb$cetab_mod == "9741060J"] <- "PR"
dnb$statut_constat[dnb$cetab_mod == "9741243H"] <- "PU"
dnb$statut_constat[dnb$cetab_mod == "9741306B"] <- "PU"


# création des variables liées à la date de naissance (il faut précisément les dates de DNB cette fois)
# Sources pour les dates de DNB : 
# 2014 : https://www.france-examen.com/actus/2014-01/dates-retenir-epreuves-brevet-2014-586667.html
# 2015 : https://www.ac-reunion.fr/academie/actualites-de-lacademie-de-la-reunion/article-dactualite/news/detail/News/cest-parti-pour-les-epreuves-du-brevet-2015.html?tx_news_pi1%5B%40widget_0%5D%5BcurrentPage%5D=15&cHash=005878517ea620931da8c2ba0db7451b
# 2016 : https://www.rtl.fr/actu/debats-societe/brevet-2016-les-dates-officielles-des-epreuves-7781907516
   # Pour 2016 : petite erreur qui ne change rien : la date devrait être le 24 juin (source ci-dessus). 

dnb <- mutate(dnb,
              age_absdnb = if_else(session_mod == "2014",
                                   int_length(interval(dnais_constat,
                                                       as.Date("2014-06-26"))) / (365.25 * 3600 * 24),
                                   if_else(session_mod == "2015",
                                           int_length(interval(dnais_constat,
                                                               as.Date("2015-06-26"))) / (365.25 * 3600 * 24),
                                           int_length(interval(dnais_constat,
                                                               as.Date("2016-06-26"))) / (365.25 * 3600 * 24))))

dnb <- mutate(dnb, 
              positiondnb2_num = (as.numeric(as.character(year(dnais_constat))) + 15) - 
                as.numeric(session_mod),
              positiondnb2 = if_else(positiondnb2_num <= - 1, "Redoublant",
                                     if_else(positiondnb2_num == 0, "Heure", "Avance")))


# z_dnb, jour_dnb, quinze_dnb, mois_dnb, bim_dnb, trim_dnb

dnb <- mutate(dnb,
              z_dnb = (int_length(interval(dnais_constat, 
                                           as.Date(paste(year(dnais_constat), "-12-31", sep = ""))) / 365.25)) / (3600 * 24),
              # j'ai vérifié jour_dnb ici, ça devrait être bon
              jour_dnb = round((1 - z_dnb) * 365.25),
              
              # quinze
              quinze_dnb = relevel(factor(if_else(as.numeric(substr(dnais_constat, 9, 10)) <= 15, 1, 2) + 
                                            (month(dnais_constat)-1)*2), "24"),
              
              # mois
              mois_dnb = month(dnais_constat) %>% factor %>% relevel("12"), 
              
              # bim
              bim_dnb = if_else(month(dnais_constat) %in% c(1, 2), 1,
                                if_else(month(dnais_constat) %in% c(3, 4), 2, 
                                        if_else(month(dnais_constat) %in% c(5, 6), 3,
                                                if_else(month(dnais_constat) %in% c(7, 8), 4, 
                                                        if_else(month(dnais_constat) %in% c(9, 10), 5, 6))))) %>% 
                factor %>% relevel("6"),
              
              # trim
              trim_dnb = if_else(month(dnais_constat) %in% c(1:3), 1,
                                 if_else(month(dnais_constat) %in% c(4:6), 2,
                                         if_else(month(dnais_constat) %in% c(7:9), 3, 4))) %>% factor %>% relevel("4")
)
# les valeurs extrêmes d'âge sont à exclure à ce stade

dnb_wvextp <- dnb
dnb <- filter(dnb, positiondnb2_num %in% c(-2:2))

  # groupes (pas besoin des tailles de groupe)
dnb <- mutate(dnb,
              divdnbrne = paste(div_mod, cetab_mod),
              divdnbrneses = paste(div_mod, cetab_mod, session_mod),
              rneses = paste(cetab_mod, session_mod))

dnb <- mutate(dnb,
              statutreseauR = paste(statut_constat, reseauR_mod))

# 19 juin 2020 : récupération des parcours de l'année suivante (2nde_gt, 2nde_pro, niveau3_autre, 3eme (?))
dnb <- rbind(
  filter(dnb, session_mod == "2014") %>%
    left_join(
      filter(constat1316, cohorte_mod == "2014") %>%
        select(ine, parcours_suiv = lmefstat4_mod),
      "ine"
    ),
  filter(dnb, session_mod == "2015") %>%
    left_join(
      filter(constat1316, cohorte_mod == "2015") %>%
        select(ine, parcours_suiv = lmefstat4_mod),
      "ine"
    ),
  filter(dnb, session_mod == "2016") %>%
    left_join(
      filter(constat1316, cohorte_mod == "2016") %>%
        select(ine, parcours_suiv = lmefstat4_mod),
      "ine"
    )
)

# 2021_02_05 : récupération du régime.
dnb <- left_join(
  dnb,
  transmute(constat1316, ine_mod, session_mod,
            cregime_constat = cregime),
  by = c("session_mod", "ine_mod")
)
# nobs ok.

dnb <- mutate(
  dnb,
  lregime_constat = fct_recode(
    cregime_constat,
    "dp_dan" = "2",
    "dp_hor" = "6",
    "int_dan" = "3",
    "int_heb" = "5",
    "int_ext" = "4",
    "ext_sur" = "1",
    "ext_lib" = "0"
  ) %>%
    factor(
      levels = c(
        "dp_dan",
        "dp_hor",
        "int_dan",
        "int_heb",
        "int_ext",
        "ext_sur",
        "ext_lib"
      )
    )
)

dnb <- mutate(
  dnb,
  lregime_constat_g =
    fct_collapse(
      lregime_constat,
      "dp" = c("dp_dan", "dp_hor"),
      "int" = c("int_dan", "int_heb", "int_ext"),
      "ext" = c("ext_sur", "ext_lib")
    ) %>%
    factor(levels = c("dp", "int", "ext"))
)

dnb <- dummy_cols(dnb, c("lregime_constat",
                         "lregime_constat_g"))


# 2021-03-10 : récupération de libmef (libellé clefgestion)
# depuis constat.
dnb <- left_join(
  dnb,
  transmute(constat1316, ine_mod, session_mod,
            libmef_constat = libmef_mod),
  by = c("ine_mod", "session_mod")
)
# nobs ok

# PAS DE CONSTRUCTION A FAIRE SUR DNB17.

# Sauvegarde --------------------------------------------------------------

save(list = ls()["function" != ls() %>% 
            sapply(function (x) class(get(x))[1])],
     file = here("01_construction", "01_pre_jointure.rda"),
     version = 2)
