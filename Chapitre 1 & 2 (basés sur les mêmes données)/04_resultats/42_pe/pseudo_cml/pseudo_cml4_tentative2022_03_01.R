# Tentative de création pseudo_cml4 : une version qui corrige proprement les
# valeurs manquantes. 
# Base : pseudo_cml (première version de M. Croissant). 

# Packages ----------------------------------------------------------------

library(here)
library(tidyverse)
library(numDeriv)
library(maxLik)

# Chargement --------------------------------------------------------------

load("D:/00_phd/00_fonctions/fonctions2.rda")
load(here("01_01_datcompl_def.rda"))
source(here("pedepvars.R"))
source(here("pepcmlcovars.R"))
for (i in 2:length(pepcmlcovars)) {
  assign(paste("cov", i, sep = ""),
         pepcmlcovars[1:i])
}

  # cls_prob : une classe avec quasiment que des NA de y. 

cls_prob <- group_by(datcompl, divconstatrneses) %>% 
  filter(sum(is.na(moy_ec1)) > 20) %>% pull(divconstatrneses) %>% 
  unique

datcompl <- filter(datcompl, divconstatrneses != cls_prob)


# Vérification 1 : résultats pseudo_cml = résultats pseudo_cml3 ----------- 
  # Attention : un problème principal sur cette fonction : 
    # S'il y a une taille de classe qui vaut 1 après enlèvement des valeurs
    # manquantes de la variable dépendante, le programme ne tournera pas. 
    # C'est pour ça que j'ai enlevé la classe ci-dessous. 
    # C'est une justification en plus pour corriger cette fonction. 

p1 <- pseudo_cml(datcompl %>% 
                   filter(divconstatrneses != "9740651P 304 2016"), 
                 depvar = "moy_ec1",
                 covars = c("sexe_mod_M", "age_absdnb"),
                 group = "divconstatrneses")

p2 <- pseudo_cml3(moy_ec1 + factor(divconstatrneses) ~ 
                    sexe_mod + age_absdnb,
                  datcompl %>% 
                   filter(divconstatrneses != "9740651P 304 2016"))
  # Pas exactement les mêmes mais les coefficients sont très comparables. 
  # J'ai vérifié que p1 et p2 donnent bien la même chose peu importe le nombre de fois
  # qu'on les lance. 

  # Vérification 1 : ça va. 

# Vérification 2 : si j'utilise pseudo_cml, je dois avoir les mêmes --------

p1 <- pseudo_cml3(moy_ec1_norm + factor(divconstatrneses) ~ 
                   sexe_mod + pcs_reg_mod, 
                 filter(datcompl, 
                        ! is.na(moy_ec1_norm)) %>% 
                   group_by(divconstatrneses) %>% 
                   filter(n() > 1) %>% ungroup)

  # Vérification 2 : ok. Les effets endogènes sont à actualiser. 

# Tentative d'appliquer correction y_manquantes Boucher et al. 2014 -------

pseudo_cml4 <- function (data, depvar, covars, group) {
  # data <- na.omit(data)
  y <- data[[depvar]]
  X <- select(data, covars) %>% as.matrix
  group0 <- group
  group <- data[[group]]
  # ymean <- tapply(y, group, mean)
  ymean <- tapply(y, group, function (x) mean(x, na.rm = TRUE))
    # code juste ci-dessus : vérifié avec dplyr : ok. 
  # stop(return(ymean));
  ystar <- y - ymean[as.character(group)]
  # code ci-dessus : vérifié avec dplyr : ok. 
  # stop(return(ystar)) ; 
  # Xmean <- apply(X, 2, function (x) tapply(x, group, mean))
  Xmean <- apply(X, 2, function (x)
    tapply(x, group, function (x) mean(x, na.rm = TRUE))) - 
    apply(X %>% apply(2, function (x2) {
      x2[! is.na(y)] <- 0 # je ne garde que les NA.
      x2
    }), 2, function (x) 
      tapply(x, group, function (x) mean(x, na.rm = TRUE)))
    # code ci-dessus : vérifié avec dplyr.
  # stop(return(Xmean)) ; 
  Xstar <- X - Xmean[as.character(group), ]
    # code ci-dessus : vérifié avec dplyr. 
  # stop(return(Xstar)) ; 
  N <- length(y)
  R <- length(unique(group))
  K <- ncol(X)
  mr <- table(group)
  nmr <- names(mr)
  mr <- as.numeric(mr)
  names(mr) <- nmr
    # rajout nr : 
  nr <- table(filter(data, ! is.na(eval(parse(text = depvar)))) %>% 
                pull(eval(parse(text = group0))))
  nnr <- names(nr)
  nr <- as.numeric(nr)
  names(nr) <- nnr
    # codes nr ci-dessus : vérifié avec dplyr. 
  # stop(return(list(mr, nr)));
  ystar <- ystar[! is.na(ystar)]
  Xstar <- Xstar[! is.na(y), ] # déjà vérfié plus haut, pas de soucis.
  
  # ystarl <- split(ystar, group)
  ystarl <- split(ystar, group[! is.na(y)])
  # stop(return(group))
  # Xstarl <- split(as.data.frame(Xstar), group)
  Xstarl <- split(as.data.frame(Xstar), group[! is.na(y)])
  Xstarl <- lapply(Xstarl, as.matrix)
  
  
  
  lnl <- function(x){
    beta <-  x[1]
    gamma <- x[2:(K+1)]
    delta <- x[(K+2):(2*K+1)]
    sig2 <-  x[2*K+2]
    coefX <- lapply(mr, function(x) ( (x - 1) * gamma - delta) / (x - 1))
    coefY <- lapply(mr, function(x)  (x - 1 + beta) / (x - 1))
    eps <- lapply(seq_along(mr), function(i) as.numeric(ystarl[[i]] * coefY[[i]] - Xstarl[[i]] %*% coefX[[i]]))
    # logl <- sum((mr - 1) * log(mr - 1 + beta)) - (N - R) / 2 * log(sig2) - sum(unlist(eps) ^ 2) / (2 * sig2)
    logl <- sum((nr - 1) * log(mr - 1 + beta)) - (N - R) / 2 * log(sig2) - sum(unlist(eps) ^ 2) / (2 * sig2)
    logl
  }
  mygrad <- function(x){
    beta <-  x[1]
    gamma <- x[2:(K+1)]
    delta <- x[(K+2):(2*K+1)]
    sig2 <-  x[2*K+2]
    coefX <- lapply(mr, function(x) ( (x - 1) * gamma - delta) / (x - 1))
    coefY <- lapply(mr, function(x)  (x - 1 + beta) / (x - 1))
    eps <- lapply(seq_along(mr), function(i) as.numeric(ystarl[[i]] * coefY[[i]] - Xstarl[[i]] %*% coefX[[i]]))
    # logl_beta <- sum( (mr - 1) / (mr - 1 + beta)) -
    #   1 / sig2 * sum(unlist(lapply(seq_along(mr), function(i) eps[[i]] * ystarl[[i]] / (mr[[i]]-1))))
    logl_beta <- sum( (nr - 1) / (mr - 1 + beta)) -
      1 / sig2 * sum(unlist(lapply(seq_along(mr), function(i) eps[[i]] * ystarl[[i]] / (mr[[i]]-1))))
    logl_gamma <-  1 / sig2 *
      apply(Reduce("rbind",
                   lapply(seq_along(mr), function(i) Xstarl[[i]] * eps[[i]])),
            2, sum)
    logl_delta <- 1 / sig2 *
      apply(Reduce("rbind",
                   lapply(seq_along(mr), function(i) - 1 / (mr[[i]] - 1) * Xstarl[[i]] * eps[[i]])),
            2, sum)
    log_sig2 <-  - (N - R) / 2 / sig2 + 1 / (2 * sig2 ^ 2) * sum(unlist(eps) ^ 2)
    c(beta = logl_beta, logl_gamma, logl_delta, sig2 = log_sig2)
  }
  Xmeanl <- split(as.data.frame(X), group) # à quoi ça sert ?
  # Xpeer <- (mr / (mr - 1) * Xmean)[as.character(group), ] - 1 / (mr[as.character(group)]) * X # erreur non ? 
  Xpeer <- (mr / (mr - 1) * Xmean)[as.character(group), ] - 1 / (mr[as.character(group)] - 1) * X
    # Code ci-dessus : vérifié avec calculs dplyr : ok. Il y avait bien une erreur. 
  # ypeer <- (mr / (mr - 1) * ymean)[as.character(group)] - 1 / (mr[as.character(group)]) * y # erreur non ?
  ypeer <- (nr / (mr - 1) * ymean)[as.character(group)] - 1 / (mr[as.character(group)] - 1) * y
    # Code ci-dessus : vérifié avec calculs dplyr : ok. Il y avait bien une erreur. 
  # stop(return(list(mr, nr, Xpeer, ypeer))) ; 
  
  # data <- na.omit(data)
  X <- X[! is.na(y), ]
  Xpeer <- Xpeer[! is.na(y), ]
  y <- y[! is.na(y)]
  ypeer <- ypeer[! is.na(ypeer)]
  # ystar <- ystar[! is.na(ystar)]
  
  lmest <- lm(y ~ ypeer + X + Xpeer)
  starting_values <- c(coef(lmest)[-1], sig2 = sigma(lmest) ^ 2)
  # stop(return(list(starting_values, mygrad(starting_values)))) ; 
  # stop(return(lmest));
  # stop(return(list = ls(envir = environment()) %>% (function (l)
  # l[sapply(l, function (x) sum(is.na(get(x))) > 0)])))
  
  # for (i in ls(envir = environment()) %>% (function (l)
  #   l[sapply(l, function (x) sum(is.na(get(x))) > 0)])) {
  #   assign(i, get(i) %>% (function (g) g[! is.na(g)]), pos = environment())
  #     # na.omit ne marche pas sur les array (découverte). 
  # }
  

  print(cbind(mygrad(starting_values), grad(lnl, starting_values)))
  ra2 <- maxLik(lnl, mygrad, start = starting_values, 
                control=list(printLevel=2), method = "BFGS")
  ra2
  
}

  # nb : pour l'essai, je ne dois pas enlever les y manquantes avant.
p4 <- pseudo_cml4(datcompl, 
                 depvar = "moy_ec1",
                 covars = c("sexe_mod_M", "pcs_reg_mod_Moy", "pcs_reg_mod_Fav", "pcs_reg_mod_Tresfav", "pcs_reg_mod_Autres"),
                 group = "divconstatrneses")

p5 <- pseudo_cml4(datcompl, 
                 depvar = "moy_maths_ec",
                 covars = c("sexe_mod_M", "pcs_reg_mod_Moy", "pcs_reg_mod_Fav", "pcs_reg_mod_Tresfav", "pcs_reg_mod_Autres"),
                 group = "divconstatrneses")

# JE VIENS DE TROUVER UNE CHOSE (A PRIORI, A CONDITION DE VERIFIER LA VERSION CORRIGEE DES DONNEES MANQUANTES). 2022_03_01. 
  # Si je corrige sur les données manquantes, j'ai des effets négatifs mais d'ampleur raisonnable (- 0.7/- 0.8). 
  # Si je ne corrige pas les données manquantes, j'ai des effets énormes (dans le papier avec les effets de 3 et plus). 
  # Ce qui veut dire qu'il faut corriger les y manquantes. Et discuter avec Croissant sur la technique. 
  # Il y a un souci : ça n'explique pas pourquoi je trouve les effets de 1.4 au CM2. Ou bien ce sont juste les vrais effets. 
  # Rassurant : si je filtre les données en enlevant les y manquantes puis les tailles n() > 1 restantes, je retombe exactement sur les effets de 3 environs
    # documentés dans mon papier. 

# Vérification 3 : avec pseudo_pcml4, les deux régressions doivent --------
  # être exactement les mêmes (tout en haut). 

p1 <- pseudo_cml4(datcompl %>% 
                   filter(divconstatrneses != "9740651P 304 2016") %>% 
                    filter(! is.na(moy_ec1)) %>% group_by(divconstatrneses) %>% 
                             filter(n() > 1) %>% ungroup, 
                 depvar = "moy_ec1",
                 covars = c("sexe_mod_M", "age_absdnb"),
                 group = "divconstatrneses")

p2 <- pseudo_cml3(moy_ec1 + factor(divconstatrneses) ~ 
                    sexe_mod + age_absdnb,
                  datcompl %>% 
                   filter(divconstatrneses != "9740651P 304 2016") %>% 
                    filter(! is.na(moy_ec1)) %>% group_by(divconstatrneses) %>% 
                    filter(n() > 1) %>% ungroup)
  # Pas exactement la même chose. Je ne sais pas pourquoi du coup. 


# Robustesse à travers les covars -----------------------------------------

  # J'ai remarqué que les résultats étaient plutôt robustes aux covariates, mais je vais confirmer ça ici. 
pcov2 <- pseudo_cml4(datcompl, 
                     depvar = "moy_ec1_norm", 
                     covars = c("sexe_mod_M", "pcs_reg_mod_Moy", "pcs_reg_mod_Fav", "pcs_reg_mod_Tresfav", "pcs_reg_mod_Autres"),
                     group = "divconstatrneses")

pcov3 <- pseudo_cml4(datcompl, 
                     depvar = "moy_ec1_norm", 
                     covars = c("sexe_mod_M", "pcs_reg_mod_Moy", "pcs_reg_mod_Fav", "pcs_reg_mod_Tresfav", "pcs_reg_mod_Autres",
                                "lregime_constat_g_int", "lregime_constat_g_ext"),
                     group = "divconstatrneses")

pcov4 <- pseudo_cml4(datcompl, 
                     depvar = "moy_ec1_norm", 
                     covars = c("sexe_mod_M", "pcs_reg_mod_Moy", "pcs_reg_mod_Fav", "pcs_reg_mod_Tresfav", "pcs_reg_mod_Autres",
                                "lregime_constat_g_int", "lregime_constat_g_ext", "age_absdnb"),
                     group = "divconstatrneses")

pcov5 <- pseudo_cml4(datcompl, 
                     depvar = "moy_ec1_norm", 
                     covars = c("sexe_mod_M", "pcs_reg_mod_Moy", "pcs_reg_mod_Fav", "pcs_reg_mod_Tresfav", "pcs_reg_mod_Autres",
                                "lregime_constat_g_int", "lregime_constat_g_ext", "age_absdnb",
                                "positiondnb2_Heure", "positiondnb2_Avance"),
                     group = "divconstatrneses")

  # Observation : très robuste : environ - 0.7 UET (note totale). 
  # Autres observations : si on enlève les classes atypiques => robuste à - 4 UET. (non montré mais j'ai regardé rigoureusement). 
  # ça veut dire que l'effet de pair endogène dans les classes atypiques est très fort. Est-ce que c'est intéressant ?. 
    # Oui => .5, très robuste. MAIS PAS SIGNIFICATIF. 
  # Et je trouve 0.7 dans les classes atypiques. 

# Sauvegarde des tests de robustesse (gain de temps) ----------------------

save(pcov2, pcov3, pcov4, pcov5, 
     file = here("pseudo_cml",
                 "pseudo_cml4_tentative2022_03_01_robustesse_correction_ymanquantes.rda"),
     version = 2)

  # Prochaine étape : essayer de faire la correction pour G2SLS. Regarder ce que ça donne. 

