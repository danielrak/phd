# pepeersindivs. 
# Sert à sauvegarder les interactions p_score score. hnolinq5. 
# Inspiré de ce qui a été déjà fait dans les scripts de res_datrestr_2. 



# peersindivs -------------------------------------------------------------

peersindivs_q3 <- paste("p_q3score_q", (1:3)[- 2],
                        " : p : ",
                        sep = "") %>% 
  (function (p) sapply(paste("q3score_q", 1:3, sep = ""),
                       function (x) paste(p, x, sep = "")) %>% 
     as.vector %>% sort)


peersindivs_q5 <- paste("p_q5score_q", (1:5)[- 3],
                        " : p : ",
                        sep = "") %>% 
  (function (p) sapply(paste("q5score_q", 1:5, sep = ""),
                       function (x) paste(p, x, sep = "")) %>% 
     as.vector %>% sort)


peersindivs_qb <- paste("p_qbscore_", c("low", "high"),
                        " : p : ",
                        sep = "") %>% 
  (function (p) sapply(paste("qbscore_", c("low", "middle", "high"), 
                             sep = ""),
                       function (x) paste(p, x, sep = "")) %>% 
     as.vector %>% sort)

# MAJ2022_01_15. 
peersindivs_q20 <- paste("p_q20score_q", (1:20)[-10],
                         " : p : ",
                         sep = "") %>% 
  (function (p) sapply(paste("q20score_q", 1:20, sep = ""),
                       function (x) 
                         paste(p, x, sep = "")) %>% 
     as.vector %>% sort)

# peersindivs_sexe_mod ----------------------------------------------------

peersindivs_q3_sexe_mod <- paste("(", 
                                 peersindivs_q3, 
                                 ") * sexe_mod",
                                 sep = "")

peersindivs_q5_sexe_mod <- paste("(", 
                                 peersindivs_q5, 
                                 ") * sexe_mod",
                                 sep = "")

peersindivs_qb_sexe_mod <- paste("(", 
                                 peersindivs_qb, 
                                 ") * sexe_mod",
                                 sep = "")

# peersindivs_pcs_reg_mod -------------------------------------------------

peersindivs_q3_pcs_reg_mod <- paste("(", 
                                 peersindivs_q3, 
                                 ") * pcs_reg_mod",
                                 sep = "")

peersindivs_q5_pcs_reg_mod <- paste("(", 
                                 peersindivs_q5, 
                                 ") * pcs_reg_mod",
                                 sep = "")

peersindivs_qb_pcs_reg_mod <- paste("(", 
                                 peersindivs_qb, 
                                 ") * pcs_reg_mod",
                                 sep = "")

# peersindivsnp -----------------------------------------------------------

  # MAJ2022_01_06 : concerne les nouveaux pairs - anciens pairs.

peersindivsnp_q3 <- paste("pnp_q3score_q", (1:3)[- 2],
                        " : p : ",
                        sep = "") %>% 
  (function (p) sapply(paste("q3score_q", 1:3, sep = ""),
                       function (x) paste(p, x, sep = "")) %>% 
     as.vector %>% sort) %>% 
  (function (p) 
    c(p, 
      paste("pop_q3score_q", (1:3)[- 2],
                        " : p : ",
                        sep = "") %>% 
  (function (p) sapply(paste("q3score_q", 1:3, sep = ""),
                       function (x) paste(p, x, sep = "")) %>% 
     as.vector %>% sort)))
  # vérifié : ok. 

peersindivsnp_q5 <- paste("pnp_q5score_q", (1:5)[- 3],
                        " : p : ",
                        sep = "") %>% 
  (function (p) sapply(paste("q5score_q", 1:5, sep = ""),
                       function (x) paste(p, x, sep = "")) %>% 
     as.vector %>% sort) %>% 
  (function (p) 
    c(p,
      paste("pop_q5score_q", (1:5)[- 3],
                        " : p : ",
                        sep = "") %>% 
  (function (p) sapply(paste("q5score_q", 1:5, sep = ""),
                       function (x) paste(p, x, sep = "")) %>% 
     as.vector %>% sort)))


peersindivsnp_qb <- paste("pnp_qbscore_", c("low", "high"),
                        " : p : ",
                        sep = "") %>% 
  (function (p) sapply(paste("qbscore_", c("low", "middle", "high"), 
                             sep = ""),
                       function (x) paste(p, x, sep = "")) %>% 
     as.vector %>% sort) %>% 
  (function (p) 
    c(p, 
      paste("pop_qbscore_", c("low", "high"),
                        " : p : ",
                        sep = "") %>% 
  (function (p) sapply(paste("qbscore_", c("low", "middle", "high"), 
                             sep = ""),
                       function (x) paste(p, x, sep = "")) %>% 
     as.vector %>% sort)))

# peersindivsnp_sexe_mod ----------------------------------------------------

peersindivsnp_q3_sexe_mod <- paste("(", 
                                 peersindivsnp_q3, 
                                 ") * sexe_mod",
                                 sep = "")

peersindivsnp_q5_sexe_mod <- paste("(", 
                                 peersindivsnp_q5, 
                                 ") * sexe_mod",
                                 sep = "")

peersindivsnp_qb_sexe_mod <- paste("(", 
                                 peersindivsnp_qb, 
                                 ") * sexe_mod",
                                 sep = "")

# peersindivs_pcs_reg_mod -------------------------------------------------

peersindivsnp_q3_pcs_reg_mod <- paste("(", 
                                 peersindivsnp_q3, 
                                 ") * pcs_reg_mod",
                                 sep = "")

peersindivsnp_q5_pcs_reg_mod <- paste("(", 
                                 peersindivsnp_q5, 
                                 ") * pcs_reg_mod",
                                 sep = "")

peersindivsnp_qb_pcs_reg_mod <- paste("(", 
                                 peersindivsnp_qb, 
                                 ") * pcs_reg_mod",
                                 sep = "")
