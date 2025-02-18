# 01_donnees_autres (def2). 
  # 2022_03_30. 
  # Autres données pour comparaisons : ps, g20_tous (tous les élèves du S1), 
    # pg20 (les 500 intéressés). 
  # À sourcer. 

library(here)
library(tidyverse)
library(gtools)
library(fastDummies)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions2.rda")
load("D:/00_phd/03_g20/01_v2_construction_def/04_pre_analyse.rda")
load("D:/00_phd/03_g20/01_v2_construction_def/06_pre_analyse_ps.rda")
load("D:/00_phd/03_g20/01_v2_construction_def/07_construction_autres_notes.rda")

# Filtre : neo et 40 ------------------------------------------------------

# for (i in c("g20_tous", "pg20", "ps")) {
#   assign(i, 
#          filter(get(i), annee_bacv2 == "2020") %>% 
#            mutate_if(is.factor, droplevels))
# }
# 
#   # 40 uniquement pour g20_tous. 
# g20_tous <- filter(g20_tous, str_sub(id_univ, 1, 2) == "40") %>% 
#   mutate_if(is.factor, droplevels)

# Filtre : sans doublons de personnes -------------------------------------

g20_tous <- filter(g20_tous, duplicated(id_univ)) %>% pull(id_univ) %>% 
  (function (i) filter(g20_tous, ! id_univ %in% i))

# Recup2 : g20_tous ~ ps --------------------------------------------------

ps_recup2 <- select(ps,
                    nom_bdd, prenom_bdd, prenom2_bdd, prenom3_bdd, 
                    nais_ps,
                    moy_bac, moy_fran_ec, moy_maths, 
                    serie_diplome_psv4, serie_diplome_psv4R, 
                    campus, filiere_ps, age_26aout, 
                    pays_nais_fr, nationalite, boursier, 
                    statut_etab, statut_etabR, 
                    type_etabv2, type_etabv2R,
                    dep_etabv2, dep_etabv2R) %>% 
  rename_all(function (x) paste(x, "_recup2", sep = ""))

g20_tous <- left_join(g20_tous %>% 
                        mutate(campus_nm = 
                                 if_else(str_detect(fichier, "sud"), 
                                         "tampon", "stdenis"))
                      # %>% 
                      #   mutate(prenom = str_to_lower(prenom) %>% 
                      #            unaccent2)
                      ,
                      ps_recup2 
                      # %>% 
                      #   mutate(prenom_bdd_recup2 = 
                      #            unaccent2(prenom_bdd_recup2))
                      , 
                      by = c("nom" = "nom_bdd_recup2",
                             # "prenom" = "prenom2_bdd_recup2",
                             "nais_nm" = "nais_ps_recup2")) %>% 
  filter(! duplicated(id_univ)) %>% 
  mutate(campus_nm = factor(campus_nm, levels = c("tampon", "stdenis")))
  # Il faut refilter puisqu'il y a des numéros étudiants qui reviennent
  # (mineures). 
  # nobs ok. Mais je perds 39% d'indivs de ps. (30% sur les néo bacheliers)
  # On va s'arrêter là. 
# Jointure : notes autres matières ----------------------------------------

g20_tous <- mutate(g20_tous, id_univ = as.character(id_univ))
g20_tous <- left_join(g20_tous,
                      note %>% select(- c(nom, prenom, nais_an,
                                          campus_an)),
                      by = c("id_univ", "filiere")) # nobs ok.

# Refactorisation filiere -------------------------------------------------

  # Le left_join casse le factor.
g20_tous <- mutate(g20_tous, 
                 filiere = factor(filiere, levels = c("aes", "eco")))

# Normalisation des notes de td par groupe (pas à faire dans neo) ---------

g20_tous <- mutate(g20_tous,
                   groupe_tdR = if_else(is.na(groupe_td), "NA",
                                        groupe_td))
g20_tous <- group_by(g20_tous, groupe_tdR) %>% 
  mutate(note_td_normpop = (note_td - mean(note_td, na.rm = TRUE)) / 
           sd(note_td, na.rm = TRUE)) %>% 
  ungroup
  # Ensuite il faut insérer cette variable dans : 
    # datg20 neo def en priorité
    # datg20 def2
    # donnees autres neo.

# Normalisation des notes par filière campus (pop) ------------------------

g20_tous <- group_by(g20_tous, filiere) %>% 
  mutate(note_ctqcm_normpop = 
           (note_ctqcm - mean(note_ctqcm, na.rm = TRUE)) / 
           sd(note_ctqcm, na.rm = TRUE),
         note_gestion_normpop = 
           (note_gestion - mean(note_gestion, na.rm = TRUE)) / 
           sd(note_gestion, na.rm = TRUE),
         note_economie_normpop = 
           (note_economie - mean(note_economie, na.rm = TRUE)) / 
           sd(note_economie, na.rm = TRUE),
         note_ue_normpop = 
           (note_ue - mean(note_ue, na.rm = TRUE)) / 
           sd(note_ue, na.rm = TRUE)
         ) %>% 
  ungroup

# Dummy cols --------------------------------------------------------------

  # Certaines n'ont pas spécialement de sens mais pratique pour les stats.
    # les variables oui/non.  
# lapply(list("g20_tous", "pg20", "ps"), function (x) {
#   assign(x, dummy_cols(get(x), 
#                        c("serie_diplome_psv3",
#                          "serie_diplome_psv4",
#                          "mention_diplome_ps",
#                          "sexe_ps", "pays_nais_fr", "nationalite",
#                        "boursier", "statut_etab", "type_etabv2",
#                        "dep_etabv2", "campus", "filiere",
#                        "filiere_ps",
#                        "mineure_sante",
#                        "niv_classe", "avis_ce")), 
#          pos = sys.frame())
#   invisible()
# })

for (i in c("g20_tous", "pg20", "ps")) {
  assign(i, dummy_cols(get(i),
                       c("serie_diplome_psv3",
                         "serie_diplome_psv4",
                         "mention_diplome_ps",
                         "sexe_ps", "pays_nais_fr", "nationalite",
                       "boursier", "statut_etab", "type_etabv2",
                       "dep_etabv2", "campus", "filiere",
                       "filiere_ps",
                       "mineure_sante",
                       "niv_classe", "avis_ce")))
}

# age_26aout : regroupement -----------------------------------------------

# lapply(list("g20_tous", "pg20", "ps"), function (x) {
#   assign(x, 
#          mutate(get(x), q5age_26aout = quantcut(age_26aout, 5) %>% 
#            factor(labels = paste("q", 1:5, sep = ""))), 
#          pos = sys.frame())
#   invisible()
# })

for (i in c("g20_tous", "pg20", "ps")) {
  assign(i, 
         mutate(get(i), 
                q5age_26aout = quantcut(age_26aout, 5) %>% 
                  factor(labels = paste("q", 1:5, sep = ""))))
}

# id_univ (g20_tous uniquement) -------------------------------------------

g20_tous <- mutate(g20_tous, 
                     id_univ2 = substr(id_univ, 1, 2),
                     id_univ2 = if_else(as.numeric(id_univ2) < 39,
                                        "39m", if_else(id_univ2 == "39",
                                                       "39", "40")) %>% 
                   factor(levels = c("40", "39", "39m"))) %>% 
  mutate_if(is.factor, droplevels)

# mention diplome ps rattrapage -> sans -----------------------------------

pg20 <- mutate(pg20,
               mention_diplome_ps = 
                 fct_collapse(mention_diplome_ps, 
                              "sans" = c("sans", "rattrapage")))

ps <- mutate(ps,
               mention_diplome_ps = 
                 fct_collapse(mention_diplome_ps, 
                              "sans" = c("sans", "rattrapage", 
                                         "oral_juillet")))

# lycee_postbac -> universite ---------------------------------------------
  # Incohérent. Trop peu d'obs dans universite. 
  # à ne pas lancer dans les fichiers sans neo. 

pg20 <- mutate(pg20, 
               type_etabv2 = 
                 fct_collapse(type_etabv2, 
                              "lycee_postbac" = c("lycee_postbac",
                                                  "universite")))

ps <- mutate(ps, 
               type_etabv2 = 
                 fct_collapse(type_etabv2, 
                              "lycee_postbac" = c("lycee_postbac",
                                                  "universite")))


# Filtre : sans boursier ens_sup sur ps -----------------------------------

  # Pas à faire ici. 
# ps <- filter(ps, boursier != "ens_sup") %>% 
#   mutate_if(is.factor, droplevels)

# is factor droplevels ----------------------------------------------------

pg20 <- mutate_if(pg20, is.factor, droplevels)
ps <- mutate_if(ps, is.factor, droplevels)

# neo ---------------------------------------------------------------------

# g20_tous_neo <- g20_tous
# pg20_neo <- pg20
# ps_neo <- ps

# Sauvegarde --------------------------------------------------------------

save(g20_tous, pg20, ps, 
     file = here("01_donnees_autres_def2.rda"),
     version = 2)
