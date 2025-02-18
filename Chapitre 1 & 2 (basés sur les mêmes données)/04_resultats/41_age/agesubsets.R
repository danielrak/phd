# agesubsets. 
  
  # Synthétise les modalités de sous-échantillons. 
  # Concerne les paramètres d'un même dossier. 

agesexe <- c("F", "M")

agepcs_g2 <- c("Defav", "Moy", "Fav", "Tresfav", "Autres")

agesexepcs_g2 <- sapply(agesexe, 
                     function (x) paste(x, agepcs_g2, 
                                        sep = "_AND_")) %>% 
  as.vector

agereseau_bin <- c("non", "oui")

agesexereseau_bin <- sapply(agesexe,
                            function (x) paste(x, agereseau_bin,
                                               sep = "_AND_")) %>% 
  as.vector

agepcs_g2reseau_bin <- sapply(agepcs_g2,
                           function (x) paste(x, agereseau_bin,
                                              sep = "_AND_")) %>% 
  as.vector

agesexepcs_g2reseau_bin <- sapply(agesexepcs_g2, 
                                  function (x) paste(x, agereseau_bin,
                                                     sep = "_AND2_")) %>% 
  as.vector()
