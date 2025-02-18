# datrestr_2_def. 
# Refait le test KS comme décrit dans BG16 ou encore CW10. 
# Légèrement différent du test présenté dans la thèse de Brodaty. 

# Packages ----------------------------------------------------------------

library(here)
library(tidyverse)

# Chargement --------------------------------------------------------------

load(here("01_donnees_def.rda")) # Données sans classes de niveau. 

# d -----------------------------------------------------------------------

d0 <- d
d <- select(d, rneconstat, 
            rneconstatses, divconstatrneses, z_dnb) %>%
  na.omit

# KS-Test -----------------------------------------------------------------

  # 2021_10_27. Revérifié que c'est la bonne méthode. 
  # Relecture de CW10 et BG16. 
  # Les résultats diffèrent peu selon le nombre de réplication choisie.
  # La majorité des collèges sont retrouvées.   
  # Il y a bien une classification non aléatoire des notes au CM2 dans
  # ces classes. 

split(d, d$rneconstatses) %>% 
  lapply(function (x) {
    m_obs = tapply(x$z_dnb, x$divconstatrneses, mean) 
    # ci-dessus : mean ou sum ne change rien. 
    # Ils appellent àa "values". 
    m_sim <- lapply(1:1000, function (y) {
      set.seed(y)
      tapply(x$z_dnb, sample(x$divconstatrneses), mean)
    }) %>% do.call(what = rbind)
    rbind <- rbind(m_obs, m_sim)
    emp_pval <- apply(rbind, 2, 
                      function (x) sum(x[2:1001] < x[1]) / 1000)
    ks.test(emp_pval, "punif")$p.value
  }) %>% unlist -> pval_ecoles
  # moins d'une minute. 
  # Flexible, mais tous les régressions reposent sur ces données.

ecoles_clniv <- pval_ecoles[pval_ecoles < .1354] %>% names %>% 
  substr(1, 8) %>% unique
  # .1354 dans BM18. On décide d'enlever une école si elle
  # fait les classes de niveau sur deux années. 
  # On enlève 19 écoles. 
  # C'est beaucoup par rapport à la première version.
  # Il faut savoir quel test faire. 

# datrestr_2_def ----------------------------------------------------------

d_restr_2_z_dnb <- filter(d0, 
                     ! rneconstat %in% ecoles_clniv)
  # Que 4 écoles détectées faisant du regroupement par jour de naissance
  # dans l'année. 
  # En plus, les régressions sur d et sur d_restr_2_z_dnb
  # sont quasiment identifques. 

# Sauvegarde --------------------------------------------------------------

save(datrestr_2, 
     file = here("d_restr_2_z_dnb_def2", 
                 "d_restr_2_z_dnb_def.rda"),
     version = 2)
