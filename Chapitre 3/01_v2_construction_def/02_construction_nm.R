# 02_construction_nm (def). 
  # lancer en exécution et pas avec job. 
  # UE = .5 * CT/QCM + .5 * TD. 
  # seuls les fichiers ue contient la colonne apoL_c0004 (ADM, etc.).
  # But : base id_univ, nom, prenom, nais_nm, note_ctqcm, note_td, note_ue.
  # ABI -> 0, ABJ -> ?, autres modalités (il ne semble pas y en avoir) ?
  # est-ce que le ct ne serait pas un qcm ? 
  
library(here)
library(tidyverse)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions2.rda")
load(here("00_donnees_originales", "nm.rda"))

# Nom des variables et constructions mineures - UE -----------------------------

lapply(list(
  "maths_ue_l1_aes_s1",
  "maths_ue_l1_eco_s1",
  "maths_ue_l1_aes_sud_s1",
  "maths_ue_l1_eco_sud_s1"
  ), function (x) {
    assign(x,
           transmute(get(x), 
              id_univ = apoL_a01_code, 
              nom = apoL_a02_nom,
              prenom = apoL_a03_prenom,
              nais_nm = str_trim(apoL_a04_naissance) %>% 
                as.Date(format = "%d/%m/%Y"),
              note_nm = apoL_c0001, 
              bareme_nm = apoL_c0002,
              # rien dans apoL_c0003.
              resultat_ue = apoL_c0004,
              resultat_ue = str_to_lower(resultat_ue),
              resultat_ue_adm = if_else(resultat_ue == "adm", 
                                        "oui", "non"),
              note_notdbl_nm = note_nm %>% 
                # str_extract("[:alpha:].*") %>% 
                str_to_lower, # là où note_nm char
                # code ci-dessus : vérifié. 
              note_nm = as.numeric(note_nm),
              # note_nm = if_else(note_notdbl_nm == "abi", 0, note_nm),
              fichier = x,
              filiere = str_extract(fichier, "aes|eco"),
                site = if_else(str_detect(fichier, "sud"), "sud",
                               "nord"),
              ) %>% 
    (function (t) {
      names(t)[names(t) == "note_nm"] <- 
        paste("note_", str_extract(x, "\\_ct|^qcm|\\_td|\\_ue") %>% 
                str_remove("_"), sep = "")
      
      names(t)[names(t) == "bareme_nm"] <- 
        paste("bareme_", str_extract(x, "\\_ct|^qcm|\\_td|\\_ue") %>% 
                str_remove("_"), sep = "")
      
      names(t)[names(t) == "note_notdbl_nm"] <- 
        paste("note_notdbl_", str_extract(x, "\\_ct|^qcm|\\_td|\\_ue") %>% 
                str_remove("_"), sep = "")
      
      t
    }) %>% mutate_if(is.character, str_trim), 
    pos = sys.frame())
    invisible()
    })


# Nom des variables et constructions mineures -----------------------------

lapply(list(
  "qcm_maths_cc_l1_aes", 
  "maths_td_l1_aes_s1",
  
  "qcm_maths_cc_l1_eco_s1",
  "maths_td_l1_eco_s1",
  
  "maths_ct_l1_aes_sud_s1",
  "maths_td_l1_aes_sud_s1",
  
  "maths_ct_l1_eco_sud_s1",
  "maths_td_l1_eco_sud_s1"), function (x) {
    assign(x,
           transmute(get(x), 
              id_univ = apoL_a01_code, 
              nom = apoL_a02_nom,
              prenom = apoL_a03_prenom,
              nais_nm = str_trim(apoL_a04_naissance) %>% 
                as.Date(format = "%d/%m/%Y"),
              note_nm = apoL_c0001, 
              bareme_nm = apoL_c0002,
              # rien dans apoL_c0003.
              note_notdbl_nm = note_nm %>% 
                # str_extract("[:alpha:].*") %>% 
                str_to_lower, # là où note_nm char
                # code ci-dessus : vérifié. 
              note_nm = as.numeric(note_nm),
              # note_nm = if_else(note_notdbl_nm == "abi", 0, note_nm),
              fichier = x,
              filiere = str_extract(fichier, "aes|eco"),
                site = if_else(str_detect(fichier, "sud"), "sud",
                               "nord"),
              ) %>% 
    (function (t) {
      names(t)[names(t) == "note_nm"] <- 
        paste("note_", str_extract(x, "\\_ct|^qcm|\\_td|\\_ue") %>% 
                str_remove("_"), sep = "")
      
      names(t)[names(t) == "bareme_nm"] <- 
        paste("bareme_", str_extract(x, "\\_ct|^qcm|\\_td|\\_ue") %>% 
                str_remove("_"), sep = "")
      
      names(t)[names(t) == "note_notdbl_nm"] <- 
        paste("note_notdbl_", str_extract(x, "\\_ct|^qcm|\\_td|\\_ue") %>% 
                str_remove("_"), sep = "")
      
      t
    }) %>% mutate_if(is.character, str_trim), 
    pos = sys.frame())
    invisible()
    })

# Sauvegarde --------------------------------------------------------------

save(list = ls()[str_detect(ls(), "maths\\_")],
     file = here("01_v2_construction_def", "02_construction_nm.rda"),
     version = 2)
