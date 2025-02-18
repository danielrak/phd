# g20preps. (def).
  # Préparation aux régressions. 

g20depvars <- c("cg_20_norm", "fran1_20_norm", "fran2_20_norm", "maths_30_norm",
                "note_ue_norm", "note_td_norm", "note_ctqcm_norm") %>% 
  (function (c) c(c, paste(c, "_norm", sep = "")))

g20treatments <- c("sessions_b24", 
                   "sheets_views_b24",
                   "videos_comp_b24")

g20insts <- c("z", "z_msg1", "z_rel1", "z_rel2")

g20covars <- c("age_26aout", "sexe_ps", 
               "pays_nais_fr", "nationalite",
               "boursier", 
               "type_etabv2R",
               "statut_etabR", 
               "pays_etab_frR",
               "voie")
