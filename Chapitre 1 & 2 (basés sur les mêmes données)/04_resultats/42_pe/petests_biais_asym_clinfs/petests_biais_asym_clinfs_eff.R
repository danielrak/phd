# petests_biais_asym_clinfs_eff. 
  # 2022_02_23. 


# Packages ----------------------------------------------------------------

library(here)
library(tidyverse)
library(fixest)

# Chargement --------------------------------------------------------------

load(here("01_01_datcompl_def.rda"))
load(here("datrestr_2_def",
          "datrestr_2_def.rda"))
source(here("pespecs.R"))

# Mise en oeuvre ----------------------------------------------------------

datcompl_wclnivinf <- filter(datcompl, type_classe == "atypique_inf")

clnivinfs <- group_by(datcompl_wclnivinf, divconstatrneses, tdivconstatrneses) %>% 
  summarise() %>% ungroup


  # On choisit de prendre 1000 échantillons aléatoires de 46 classes atypiques inférieures. 
  # Pourquoi 46 ? Parce que ça permet d'avoir un nombre d'élèves d'environ 955 (proche de 949). 
  # J'ai essayé pour 45. On a des nobs moins proches de 955 qu'avec 46. 

# Vecteur gardant le nombre d'élèves pour chaque combinaison (dans l'ordre). 
nobs_comb46_clinfs <- c()

# Matrice telle qu'une ligne correspond à une combinaison. Il y a 1000 lignes. 
# Une colonne ne veut rien dire. 

comb46_clinfs <- matrix(NA, nrow = 1000, ncol = 46)
  
for (i in 1:1000) {
  
  set.seed(i) 
  
  comb <- sample(clnivinfs$divconstatrneses, 46)
  comb46_clinfs[i, ] <- comb
  
  t <- sum(filter(clnivinfs, divconstatrneses %in%
                     comb)$tdivconstatrneses)
  nobs_comb46_clinfs[i] <- t
}

# Régressions -------------------------------------------------------------

petests_biais_asym_clinfs_eff_coefs <- data.frame(num_comb_clinfs = 1:1000,
                                                  peer_coef = NA, 
                                                  peer_sd = NA) %>% as_tibble

for (i in 1:1000) {
  reg <- feols(as.formula(paste("moy_ec1_norm ~ ", 
                                c("p_score_norm : p", "score_norm", get("sDexo_s13")) %>% 
                                  paste(collapse = " + "), 
                                "| rneconstatses")),
               data = rbind(datrestr_2, 
                            filter(datcompl_wclnivinf, divconstatrneses %in%
                                     comb46_clinfs[i, ])))
  reg <- reg$coeftable %>% as.matrix %>% structure(class = "coeftest")
  petests_biais_asym_clinfs_eff_coefs$peer_coef[i] <- reg[str_detect(rownames(reg), "p_score_norm"), 1]
  petests_biais_asym_clinfs_eff_coefs$peer_sd[i] <- reg[str_detect(rownames(reg), "p_score_norm"), 2]
}
  # Un peu moins de 10 min de calcul. 

petests_biais_asym_clinfs_eff_coefs <- mutate(petests_biais_asym_clinfs_eff_coefs,
                                              low = peer_coef - 1.96 * peer_sd,
                                              up = peer_coef + 1.96 * peer_sd)

# Sauvegarde --------------------------------------------------------------

save(petests_biais_asym_clinfs_eff_coefs, 
     file = here("petests_biais_asym_clinfs", 
                 "petests_biais_asym_clinfs_eff_coefs.rda"),
     version = 2)
