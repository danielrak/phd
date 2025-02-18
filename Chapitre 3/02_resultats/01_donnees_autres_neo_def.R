# 01_donnees_autres_neo (def). 
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

# réglage même noms
g20_tous0 <- g20_tous

  # pour récup note_td_normpop (après avoir lancé le script).
load(here("01_donnees_autres_def2.rda"))

g20_tous_def2 <- g20_tous # avec note_td_normpop.
g20_tous <- g20_tous0
rm(g20_tous0)

  # pour mettre la variable inscrit. 
load(here("01_datg20_neo_def.rda"))

# connect dans pg20 et g20_tous -------------------------------------------

# pg20 <- mutate(pg20)

# Traitements sup1 dans pg20 ----------------------------------------------

pg20 <- mutate(pg20, 
               pratique_tot2 = videos_views_tot + sheets_views_tot,
               
               pratique_tot_sup1 = as.numeric(pratique_tot >= 1),
               pratique_tot2_sup1 = as.numeric(pratique_tot2 >= 1),
               videos_comp_tot_sup1 = as.numeric(videos_comp_tot >= 1),
               videos_views_tot_sup1 = as.numeric(videos_views_tot >= 1),
               sheets_views_tot_sup1 = as.numeric(sheets_views_tot >= 1))

# Filtre : neo et 40 ------------------------------------------------------

for (i in c("g20_tous", "pg20", "ps")) {
  assign(i, 
         filter(get(i), annee_bacv2 == "2020") %>% 
           mutate_if(is.factor, droplevels))
}

  # 40 uniquement pour g20_tous. 
g20_tous <- filter(g20_tous, str_sub(id_univ, 1, 2) == "40") %>% 
  mutate_if(is.factor, droplevels)

# Filtre : sans doublons de personnes -------------------------------------

g20_tous <- filter(g20_tous, duplicated(id_univ)) %>% pull(id_univ) %>% 
  (function (i) filter(g20_tous, ! id_univ %in% i))

# Récupération note_td_normpop --------------------------------------------

  # voir cahier rouge.
g20_tous <- left_join(g20_tous %>% mutate(id_univ = as.character(id_univ)), 
                    select(g20_tous_def2, id_univ, filiere, note_td_normpop),
                    by = c("id_univ", "filiere")) # nobs ok.

# Récupération notes normpop ----------------------------------------------

g20_tous <- left_join(g20_tous, 
                    select(g20_tous_def2, id_univ, filiere, 
                           note_ctqcm_normpop, note_ue_normpop,
                           note_gestion_normpop, note_economie_normpop),
                    by = c("id_univ", "filiere")) # nobs ok.

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

# Filtre : sans boursier ens_sup sur ps -----------------------------------

ps <- filter(ps, boursier != "ens_sup") %>% 
  mutate_if(is.factor, droplevels)

# Variable inscrit dans pg20 ----------------------------------------------

# 
# pg20 <- mutate(pg20, inscrit = as.numeric(
#   paste(nom_bdd, nais_ps) %in% 
#     paste(datg20_neo$nom, datg20_neo$nais_nm)
# ))

  # Hyper complexe. Légère erreur à laisser et à expliquer. 

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

# neo ---------------------------------------------------------------------

g20_tous_neo <- g20_tous
pg20_neo <- pg20
ps_neo <- ps

# Sauvegarde --------------------------------------------------------------

save(g20_tous_neo, pg20_neo, ps_neo, 
     file = here("01_donnees_autres_neo_def.rda"),
     version = 2)
