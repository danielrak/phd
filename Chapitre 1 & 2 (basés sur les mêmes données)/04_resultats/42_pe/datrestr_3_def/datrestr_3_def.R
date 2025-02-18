# datrestr_3_def. 
# Refait le test KS comme décrit dans BG16 ou encore CW10. 
# Légèrement différent du test présenté dans la thèse de Brodaty. 

# Packages ----------------------------------------------------------------

library(here)
library(tidyverse)

# Chargement --------------------------------------------------------------

load(here("01_01_donnees_def.rda")) # Données sans classes de niveau. 

# d -----------------------------------------------------------------------

d <- select(dat, rneconstatses, divconstatrneses, score_norm) %>%
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
    m_obs = tapply(x$score_norm, x$divconstatrneses, mean) 
    # ci-dessus : mean ou sum ne change rien. 
    # Ils appellent àa "values". 
    m_sim <- lapply(1:1000, function (y) {
      set.seed(y)
      tapply(x$score_norm, sample(x$divconstatrneses), mean)
    }) %>% do.call(what = rbind)
    rbind <- rbind(m_obs, m_sim)
    emp_pval <- apply(rbind, 2, 
                      function (x) sum(x[2:1001] < x[1]) / 1000)
    ks.test(emp_pval, "punif")$p.value
  }) %>% unlist -> pval_ecoles
  # moins d'une minute. 
  # Flexible, mais tous les régressions reposent sur ces données.

ecoles_clniv <- pval_ecoles[pval_ecoles < .3684] %>% names %>%
  substr(1, 8) %>% table %>% 
  (function (t) t[t == 3]) %>% names
  # Accept at most 2 rejections. 
  # .3684 dans BM18. On décide d'enlever une école si elle
  # fait les classes de niveau sur deux années. 
  # On enlève 9 écoles. 
  # C'est beaucoup par rapport à la première version.
  # Il faut savoir quel test faire. 

# datrestr_3_def ----------------------------------------------------------

datrestr_3 <- filter(dat, 
                     ! rneconstat %in% ecoles_clniv)
  # 26402 individus. 
  # Environ 9000 individus par cohorte. 

# Sauvegarde --------------------------------------------------------------

save(datrestr_3, 
     file = here("datrestr_3_def", "datrestr_3_def.rda"),
     version = 2)
