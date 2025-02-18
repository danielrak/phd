# 01_01_donnees_def_pnp_supp. 
  # 2022_01_07. 
  # Script externe pour économiser l'espace. 


# pnp_q5score_q1 ----------------------------------------------------------

  # sog : sum old group. 
dat_nonaine <- group_by(dat_nonaine, clcm2_cldnb) %>% 
  mutate(sog_q5score_q1 = sum(q5score_q1)) %>% 
  ungroup

dat_nonaine <- group_by(dat_nonaine, ecm2_cldnb) %>% 
  mutate(soge_q5score_q1 = sum(q5score_q1)) %>% 
  ungroup

  # pnp. 
dat_nonaine <- group_by(dat_nonaine, divconstatrneses) %>% 
  mutate(pnp_q5score_q1 = 
           (sum(q5score_q1) - sog_q5score_q1) /
      (nona - tclcm2_cldnb),
    
    pnpe_q5score_q1 = 
      (sum(q5score_q1) - soge_q5score_q1) /
      (nona - tecm2_cldnb)) %>% 
  ungroup %>% 
  (function (d) {
    d$pnp_q5score_q1[d$nona <= 1] <- NA
    d$pnpe_q5score_q1[d$nona <= 1] <- NA
    d$pnp_q5score_q1[d$nona == d$tclcm2_cldnb & 
                       d$nona > 1] <- 0
    d$pnpe_q5score_q1[d$nona == d$tclcm2_cldnb & 
                        d$nona > 1] <- 0
    d
  })
  # Pour les n() = 1 ou 2, les pnp sont infinies. 
  # On les remplace par des valeurs manquantes. 
  # 9 valeurs manquantes chez pnp_q5score_q1. 
  # 9 valeurs manquantes chez les pnpe_q5score_q1. 

  # pop. 
dat_nonaine <- group_by(dat_nonaine, divconstatrneses) %>% 
  mutate(
    pop_q5score_q1 = 
           (sog_q5score_q1 - q5score_q1) / 
               (tclcm2_cldnb - 1),
    
    pope_q5score_q1 = 
      (soge_q5score_q1 - q5score_q1) / 
      (tecm2_cldnb - 1)
    ) %>% 
  ungroup %>% 
  (function (d) {
    d$pop_q5score_q1[d$tclcm2_cldnb == 1] <- 0
    d$pope_q5score_q1[d$tecm2_cldnb == 1] <- 0
    d
  })
  # Pour les tclcm2_cldnb ou tecm2_cldnb = 1, 
  # pop = 0 (logique). 

  # Vérifications : 

  # Montre que pop = p si pnp = 0 : 

  # dat_nonaine %>% 
  #   select(pnp_q5score_q1, nona, pop_q5score_q1, 
  #          tclcm2_cldnb, p_q5score_q1) %>% 
  #   filter(pnp_q5score_q1 == 0)

  # Montre que pnp = p si pop = 0, 
  # c'est le cas si tclcm2_cldnb = 1 : 

  # dat_nonaine %>% 
#   select(pnp_q5score_q1, nona, pop_q5score_q1, 
#          tclcm2_cldnb, p_q5score_q1) %>% 
#   filter(tclcm2_cldnb == 1)

  # Les code ci-dessous vérifie bien que la moyenne pondérée
  # des pnp et pop est égale à p_score. 

# dat_nonaine %>% 
#   select(pnp_q5score_q1, nona,
#          pop_q5score_q1, tclcm2_cldnb, p_q5score_q1) %>% 
#   mutate(v = (tclcm2_cldnb - 1) * pop_q5score_q1 / 
#            (nona - 1) + (nona - tclcm2_cldnb) *
#            pnp_q5score_q1 / (nona - 1)) %>% 
#   mutate(v2 = v - p_q5score_q1) %>% pull(v2) %>% summary()

# dat_nonaine %>% 
#   select(pnpe_q5score_q1, nona,
#          pope_q5score_q1, tecm2_cldnb, p_q5score_q1) %>% 
#   mutate(v = (tecm2_cldnb - 1) * pope_q5score_q1 / 
#            (nona - 1) + (nona - tecm2_cldnb) *
#            pnpe_q5score_q1 / (nona - 1)) %>% 
#   mutate(v2 = v - p_q5score_q1) %>% pull(v2) %>% summary()


# pnp_q5score_q2 ----------------------------------------------------------

  # sog : sum old group. 
dat_nonaine <- group_by(dat_nonaine, clcm2_cldnb) %>% 
  mutate(sog_q5score_q2 = sum(q5score_q2)) %>% 
  ungroup

dat_nonaine <- group_by(dat_nonaine, ecm2_cldnb) %>% 
  mutate(soge_q5score_q2 = sum(q5score_q2)) %>% 
  ungroup

  # pnp. 
dat_nonaine <- group_by(dat_nonaine, divconstatrneses) %>% 
  mutate(pnp_q5score_q2 = 
           (sum(q5score_q2) - sog_q5score_q2) /
      (nona - tclcm2_cldnb),
    
    pnpe_q5score_q2 = 
      (sum(q5score_q2) - soge_q5score_q2) /
      (nona - tecm2_cldnb)) %>% 
  ungroup %>% 
  (function (d) {
    d$pnp_q5score_q2[d$nona <= 1] <- NA
    d$pnpe_q5score_q2[d$nona <= 1] <- NA
    d$pnp_q5score_q2[d$nona == d$tclcm2_cldnb & 
                       d$nona > 1] <- 0
    d$pnpe_q5score_q2[d$nona == d$tclcm2_cldnb & 
                        d$nona > 1] <- 0
    d
  })
  # Pour les n() = 1 ou 2, les pnp sont infinies. 
  # On les remplace par des valeurs manquantes. 
  # 9 valeurs manquantes chez pnp_q5score_q2. 
  # 9 valeurs manquantes chez les pnpe_q5score_q2. 

  # pop. 
dat_nonaine <- group_by(dat_nonaine, divconstatrneses) %>% 
  mutate(
    pop_q5score_q2 = 
           (sog_q5score_q2 - q5score_q2) / 
               (tclcm2_cldnb - 1),
    
    pope_q5score_q2 = 
      (soge_q5score_q2 - q5score_q2) / 
      (tecm2_cldnb - 1)
    ) %>% 
  ungroup %>% 
  (function (d) {
    d$pop_q5score_q2[d$tclcm2_cldnb == 1] <- 0
    d$pope_q5score_q2[d$tecm2_cldnb == 1] <- 0
    d
  })
  # Pour les tclcm2_cldnb ou tecm2_cldnb = 1, 
  # pop = 0 (logique). 

  # Vérifications : 

  # Montre que pop = p si pnp = 0 : 

  # dat_nonaine %>% 
  #   select(pnp_q5score_q2, nona, pop_q5score_q2, 
  #          tclcm2_cldnb, p_q5score_q2) %>% 
  #   filter(pnp_q5score_q2 == 0)

  # Montre que pnp = p si pop = 0, 
  # c'est le cas si tclcm2_cldnb = 1 : 

  # dat_nonaine %>% 
#   select(pnp_q5score_q2, nona, pop_q5score_q2, 
#          tclcm2_cldnb, p_q5score_q2) %>% 
#   filter(tclcm2_cldnb == 1)

  # Les code ci-dessous vérifie bien que la moyenne pondérée
  # des pnp et pop est égale à p_score. 

# dat_nonaine %>% 
#   select(pnp_q5score_q2, nona,
#          pop_q5score_q2, tclcm2_cldnb, p_q5score_q2) %>% 
#   mutate(v = (tclcm2_cldnb - 1) * pop_q5score_q2 / 
#            (nona - 1) + (nona - tclcm2_cldnb) *
#            pnp_q5score_q2 / (nona - 1)) %>% 
#   mutate(v2 = v - p_q5score_q2) %>% pull(v2) %>% summary()

# dat_nonaine %>% 
#   select(pnpe_q5score_q2, nona,
#          pope_q5score_q2, tecm2_cldnb, p_q5score_q2) %>% 
#   mutate(v = (tecm2_cldnb - 1) * pope_q5score_q2 / 
#            (nona - 1) + (nona - tecm2_cldnb) *
#            pnpe_q5score_q2 / (nona - 1)) %>% 
#   mutate(v2 = v - p_q5score_q2) %>% pull(v2) %>% summary()

# pnp_q5score_q3 ----------------------------------------------------------

  # sog : sum old group. 
dat_nonaine <- group_by(dat_nonaine, clcm2_cldnb) %>% 
  mutate(sog_q5score_q3 = sum(q5score_q3)) %>% 
  ungroup

dat_nonaine <- group_by(dat_nonaine, ecm2_cldnb) %>% 
  mutate(soge_q5score_q3 = sum(q5score_q3)) %>% 
  ungroup

  # pnp. 
dat_nonaine <- group_by(dat_nonaine, divconstatrneses) %>% 
  mutate(pnp_q5score_q3 = 
           (sum(q5score_q3) - sog_q5score_q3) /
      (nona - tclcm2_cldnb),
    
    pnpe_q5score_q3 = 
      (sum(q5score_q3) - soge_q5score_q3) /
      (nona - tecm2_cldnb)) %>% 
  ungroup %>% 
  (function (d) {
    d$pnp_q5score_q3[d$nona <= 1] <- NA
    d$pnpe_q5score_q3[d$nona <= 1] <- NA
    d$pnp_q5score_q3[d$nona == d$tclcm2_cldnb & 
                       d$nona > 1] <- 0
    d$pnpe_q5score_q3[d$nona == d$tclcm2_cldnb & 
                        d$nona > 1] <- 0
    d
  })
  # Pour les n() = 1 ou 2, les pnp sont infinies. 
  # On les remplace par des valeurs manquantes. 
  # 9 valeurs manquantes chez pnp_q5score_q3. 
  # 9 valeurs manquantes chez les pnpe_q5score_q3. 

  # pop. 
dat_nonaine <- group_by(dat_nonaine, divconstatrneses) %>% 
  mutate(
    pop_q5score_q3 = 
           (sog_q5score_q3 - q5score_q3) / 
               (tclcm2_cldnb - 1),
    
    pope_q5score_q3 = 
      (soge_q5score_q3 - q5score_q3) / 
      (tecm2_cldnb - 1)
    ) %>% 
  ungroup %>% 
  (function (d) {
    d$pop_q5score_q3[d$tclcm2_cldnb == 1] <- 0
    d$pope_q5score_q3[d$tecm2_cldnb == 1] <- 0
    d
  })
  # Pour les tclcm2_cldnb ou tecm2_cldnb = 1, 
  # pop = 0 (logique). 

  # Vérifications : 

  # Montre que pop = p si pnp = 0 : 

  # dat_nonaine %>% 
  #   select(pnp_q5score_q3, nona, pop_q5score_q3, 
  #          tclcm2_cldnb, p_q5score_q3) %>% 
  #   filter(pnp_q5score_q3 == 0)

  # Montre que pnp = p si pop = 0, 
  # c'est le cas si tclcm2_cldnb = 1 : 

  # dat_nonaine %>% 
#   select(pnp_q5score_q3, nona, pop_q5score_q3, 
#          tclcm2_cldnb, p_q5score_q3) %>% 
#   filter(tclcm2_cldnb == 1)

  # Les code ci-dessous vérifie bien que la moyenne pondérée
  # des pnp et pop est égale à p_score. 

# dat_nonaine %>% 
#   select(pnp_q5score_q3, nona,
#          pop_q5score_q3, tclcm2_cldnb, p_q5score_q3) %>% 
#   mutate(v = (tclcm2_cldnb - 1) * pop_q5score_q3 / 
#            (nona - 1) + (nona - tclcm2_cldnb) *
#            pnp_q5score_q3 / (nona - 1)) %>% 
#   mutate(v2 = v - p_q5score_q3) %>% pull(v2) %>% summary()

# dat_nonaine %>% 
#   select(pnpe_q5score_q3, nona,
#          pope_q5score_q3, tecm2_cldnb, p_q5score_q3) %>% 
#   mutate(v = (tecm2_cldnb - 1) * pope_q5score_q3 / 
#            (nona - 1) + (nona - tecm2_cldnb) *
#            pnpe_q5score_q3 / (nona - 1)) %>% 
#   mutate(v2 = v - p_q5score_q3) %>% pull(v2) %>% summary()

# pnp_q5score_q4 ----------------------------------------------------------

  # sog : sum old group. 
dat_nonaine <- group_by(dat_nonaine, clcm2_cldnb) %>% 
  mutate(sog_q5score_q4 = sum(q5score_q4)) %>% 
  ungroup

dat_nonaine <- group_by(dat_nonaine, ecm2_cldnb) %>% 
  mutate(soge_q5score_q4 = sum(q5score_q4)) %>% 
  ungroup

  # pnp. 
dat_nonaine <- group_by(dat_nonaine, divconstatrneses) %>% 
  mutate(pnp_q5score_q4 = 
           (sum(q5score_q4) - sog_q5score_q4) /
      (nona - tclcm2_cldnb),
    
    pnpe_q5score_q4 = 
      (sum(q5score_q4) - soge_q5score_q4) /
      (nona - tecm2_cldnb)) %>% 
  ungroup %>% 
  (function (d) {
    d$pnp_q5score_q4[d$nona <= 1] <- NA
    d$pnpe_q5score_q4[d$nona <= 1] <- NA
    d$pnp_q5score_q4[d$nona == d$tclcm2_cldnb & 
                       d$nona > 1] <- 0
    d$pnpe_q5score_q4[d$nona == d$tclcm2_cldnb & 
                        d$nona > 1] <- 0
    d
  })
  # Pour les n() = 1 ou 2, les pnp sont infinies. 
  # On les remplace par des valeurs manquantes. 
  # 9 valeurs manquantes chez pnp_q5score_q4. 
  # 9 valeurs manquantes chez les pnpe_q5score_q4. 

  # pop. 
dat_nonaine <- group_by(dat_nonaine, divconstatrneses) %>% 
  mutate(
    pop_q5score_q4 = 
           (sog_q5score_q4 - q5score_q4) / 
               (tclcm2_cldnb - 1),
    
    pope_q5score_q4 = 
      (soge_q5score_q4 - q5score_q4) / 
      (tecm2_cldnb - 1)
    ) %>% 
  ungroup %>% 
  (function (d) {
    d$pop_q5score_q4[d$tclcm2_cldnb == 1] <- 0
    d$pope_q5score_q4[d$tecm2_cldnb == 1] <- 0
    d
  })
  # Pour les tclcm2_cldnb ou tecm2_cldnb = 1, 
  # pop = 0 (logique). 

  # Vérifications : 

  # Montre que pop = p si pnp = 0 : 

  # dat_nonaine %>% 
  #   select(pnp_q5score_q4, nona, pop_q5score_q4, 
  #          tclcm2_cldnb, p_q5score_q4) %>% 
  #   filter(pnp_q5score_q4 == 0)

  # Montre que pnp = p si pop = 0, 
  # c'est le cas si tclcm2_cldnb = 1 : 

  # dat_nonaine %>% 
#   select(pnp_q5score_q4, nona, pop_q5score_q4, 
#          tclcm2_cldnb, p_q5score_q4) %>% 
#   filter(tclcm2_cldnb == 1)

  # Les code ci-dessous vérifie bien que la moyenne pondérée
  # des pnp et pop est égale à p_score. 

# dat_nonaine %>% 
#   select(pnp_q5score_q4, nona,
#          pop_q5score_q4, tclcm2_cldnb, p_q5score_q4) %>% 
#   mutate(v = (tclcm2_cldnb - 1) * pop_q5score_q4 / 
#            (nona - 1) + (nona - tclcm2_cldnb) *
#            pnp_q5score_q4 / (nona - 1)) %>% 
#   mutate(v2 = v - p_q5score_q4) %>% pull(v2) %>% summary()

# dat_nonaine %>% 
#   select(pnpe_q5score_q4, nona,
#          pope_q5score_q4, tecm2_cldnb, p_q5score_q4) %>% 
#   mutate(v = (tecm2_cldnb - 1) * pope_q5score_q4 / 
#            (nona - 1) + (nona - tecm2_cldnb) *
#            pnpe_q5score_q4 / (nona - 1)) %>% 
#   mutate(v2 = v - p_q5score_q4) %>% pull(v2) %>% summary()

# pnp_q5score_q5 ----------------------------------------------------------

  # sog : sum old group. 
dat_nonaine <- group_by(dat_nonaine, clcm2_cldnb) %>% 
  mutate(sog_q5score_q5 = sum(q5score_q5)) %>% 
  ungroup

dat_nonaine <- group_by(dat_nonaine, ecm2_cldnb) %>% 
  mutate(soge_q5score_q5 = sum(q5score_q5)) %>% 
  ungroup

  # pnp. 
dat_nonaine <- group_by(dat_nonaine, divconstatrneses) %>% 
  mutate(pnp_q5score_q5 = 
           (sum(q5score_q5) - sog_q5score_q5) /
      (nona - tclcm2_cldnb),
    
    pnpe_q5score_q5 = 
      (sum(q5score_q5) - soge_q5score_q5) /
      (nona - tecm2_cldnb)) %>% 
  ungroup %>% 
  (function (d) {
    d$pnp_q5score_q5[d$nona <= 1] <- NA
    d$pnpe_q5score_q5[d$nona <= 1] <- NA
    d$pnp_q5score_q5[d$nona == d$tclcm2_cldnb & 
                       d$nona > 1] <- 0
    d$pnpe_q5score_q5[d$nona == d$tclcm2_cldnb & 
                        d$nona > 1] <- 0
    d
  })
  # Pour les n() = 1 ou 2, les pnp sont infinies. 
  # On les remplace par des valeurs manquantes. 
  # 9 valeurs manquantes chez pnp_q5score_q5. 
  # 9 valeurs manquantes chez les pnpe_q5score_q5. 

  # pop. 
dat_nonaine <- group_by(dat_nonaine, divconstatrneses) %>% 
  mutate(
    pop_q5score_q5 = 
           (sog_q5score_q5 - q5score_q5) / 
               (tclcm2_cldnb - 1),
    
    pope_q5score_q5 = 
      (soge_q5score_q5 - q5score_q5) / 
      (tecm2_cldnb - 1)
    ) %>% 
  ungroup %>% 
  (function (d) {
    d$pop_q5score_q5[d$tclcm2_cldnb == 1] <- 0
    d$pope_q5score_q5[d$tecm2_cldnb == 1] <- 0
    d
  })
  # Pour les tclcm2_cldnb ou tecm2_cldnb = 1, 
  # pop = 0 (logique). 

  # Vérifications : 

  # Montre que pop = p si pnp = 0 : 

  # dat_nonaine %>% 
  #   select(pnp_q5score_q5, nona, pop_q5score_q5, 
  #          tclcm2_cldnb, p_q5score_q5) %>% 
  #   filter(pnp_q5score_q5 == 0)

  # Montre que pnp = p si pop = 0, 
  # c'est le cas si tclcm2_cldnb = 1 : 

  # dat_nonaine %>% 
#   select(pnp_q5score_q5, nona, pop_q5score_q5, 
#          tclcm2_cldnb, p_q5score_q5) %>% 
#   filter(tclcm2_cldnb == 1)

  # Les code ci-dessous vérifie bien que la moyenne pondérée
  # des pnp et pop est égale à p_score. 

# dat_nonaine %>% 
#   select(pnp_q5score_q5, nona,
#          pop_q5score_q5, tclcm2_cldnb, p_q5score_q5) %>% 
#   mutate(v = (tclcm2_cldnb - 1) * pop_q5score_q5 / 
#            (nona - 1) + (nona - tclcm2_cldnb) *
#            pnp_q5score_q5 / (nona - 1)) %>% 
#   mutate(v2 = v - p_q5score_q5) %>% pull(v2) %>% summary()

# dat_nonaine %>% 
#   select(pnpe_q5score_q5, nona,
#          pope_q5score_q5, tecm2_cldnb, p_q5score_q5) %>% 
#   mutate(v = (tecm2_cldnb - 1) * pope_q5score_q5 / 
#            (nona - 1) + (nona - tecm2_cldnb) *
#            pnpe_q5score_q5 / (nona - 1)) %>% 
#   mutate(v2 = v - p_q5score_q5) %>% pull(v2) %>% summary()

# pnp_qbscore_low ----------------------------------------------------------

  # sog : sum old group. 
dat_nonaine <- group_by(dat_nonaine, clcm2_cldnb) %>% 
  mutate(sog_qbscore_low = sum(qbscore_low)) %>% 
  ungroup

dat_nonaine <- group_by(dat_nonaine, ecm2_cldnb) %>% 
  mutate(soge_qbscore_low = sum(qbscore_low)) %>% 
  ungroup

  # pnp. 
dat_nonaine <- group_by(dat_nonaine, divconstatrneses) %>% 
  mutate(pnp_qbscore_low = 
           (sum(qbscore_low) - sog_qbscore_low) /
      (nona - tclcm2_cldnb),
    
    pnpe_qbscore_low = 
      (sum(qbscore_low) - soge_qbscore_low) /
      (nona - tecm2_cldnb)) %>% 
  ungroup %>% 
  (function (d) {
    d$pnp_qbscore_low[d$nona <= 1] <- NA
    d$pnpe_qbscore_low[d$nona <= 1] <- NA
    d$pnp_qbscore_low[d$nona == d$tclcm2_cldnb & 
                       d$nona > 1] <- 0
    d$pnpe_qbscore_low[d$nona == d$tclcm2_cldnb & 
                        d$nona > 1] <- 0
    d
  })
  # Pour les n() = 1 ou 2, les pnp sont infinies. 
  # On les remplace par des valeurs manquantes. 
  # 9 valeurs manquantes chez pnp_qbscore_low. 
  # 9 valeurs manquantes chez les pnpe_qbscore_low. 

  # pop. 
dat_nonaine <- group_by(dat_nonaine, divconstatrneses) %>% 
  mutate(
    pop_qbscore_low = 
           (sog_qbscore_low - qbscore_low) / 
               (tclcm2_cldnb - 1),
    
    pope_qbscore_low = 
      (soge_qbscore_low - qbscore_low) / 
      (tecm2_cldnb - 1)
    ) %>% 
  ungroup %>% 
  (function (d) {
    d$pop_qbscore_low[d$tclcm2_cldnb == 1] <- 0
    d$pope_qbscore_low[d$tecm2_cldnb == 1] <- 0
    d
  })
  # Pour les tclcm2_cldnb ou tecm2_cldnb = 1, 
  # pop = 0 (logique). 

  # Vérifications : 

  # Montre que pop = p si pnp = 0 : 

  # dat_nonaine %>% 
  #   select(pnp_qbscore_low, nona, pop_qbscore_low, 
  #          tclcm2_cldnb, p_qbscore_low) %>% 
  #   filter(pnp_qbscore_low == 0)

  # Montre que pnp = p si pop = 0, 
  # c'est le cas si tclcm2_cldnb = 1 : 

  # dat_nonaine %>% 
#   select(pnp_qbscore_low, nona, pop_qbscore_low, 
#          tclcm2_cldnb, p_qbscore_low) %>% 
#   filter(tclcm2_cldnb == 1)

  # Les code ci-dessous vérifie bien que la moyenne pondérée
  # des pnp et pop est égale à p_score. 

# dat_nonaine %>% 
#   select(pnp_qbscore_low, nona,
#          pop_qbscore_low, tclcm2_cldnb, p_qbscore_low) %>% 
#   mutate(v = (tclcm2_cldnb - 1) * pop_qbscore_low / 
#            (nona - 1) + (nona - tclcm2_cldnb) *
#            pnp_qbscore_low / (nona - 1)) %>% 
#   mutate(v2 = v - p_qbscore_low) %>% pull(v2) %>% summary()

# dat_nonaine %>% 
#   select(pnpe_qbscore_low, nona,
#          pope_qbscore_low, tecm2_cldnb, p_qbscore_low) %>% 
#   mutate(v = (tecm2_cldnb - 1) * pope_qbscore_low / 
#            (nona - 1) + (nona - tecm2_cldnb) *
#            pnpe_qbscore_low / (nona - 1)) %>% 
#   mutate(v2 = v - p_qbscore_low) %>% pull(v2) %>% summary()

# pnp_qbscore_middle ----------------------------------------------------------

  # sog : sum old group. 
dat_nonaine <- group_by(dat_nonaine, clcm2_cldnb) %>% 
  mutate(sog_qbscore_middle = sum(qbscore_middle)) %>% 
  ungroup

dat_nonaine <- group_by(dat_nonaine, ecm2_cldnb) %>% 
  mutate(soge_qbscore_middle = sum(qbscore_middle)) %>% 
  ungroup

  # pnp. 
dat_nonaine <- group_by(dat_nonaine, divconstatrneses) %>% 
  mutate(pnp_qbscore_middle = 
           (sum(qbscore_middle) - sog_qbscore_middle) /
      (nona - tclcm2_cldnb),
    
    pnpe_qbscore_middle = 
      (sum(qbscore_middle) - soge_qbscore_middle) /
      (nona - tecm2_cldnb)) %>% 
  ungroup %>% 
  (function (d) {
    d$pnp_qbscore_middle[d$nona <= 1] <- NA
    d$pnpe_qbscore_middle[d$nona <= 1] <- NA
    d$pnp_qbscore_middle[d$nona == d$tclcm2_cldnb & 
                       d$nona > 1] <- 0
    d$pnpe_qbscore_middle[d$nona == d$tclcm2_cldnb & 
                        d$nona > 1] <- 0
    d
  })
  # Pour les n() = 1 ou 2, les pnp sont infinies. 
  # On les remplace par des valeurs manquantes. 
  # 9 valeurs manquantes chez pnp_qbscore_middle. 
  # 9 valeurs manquantes chez les pnpe_qbscore_middle. 

  # pop. 
dat_nonaine <- group_by(dat_nonaine, divconstatrneses) %>% 
  mutate(
    pop_qbscore_middle = 
           (sog_qbscore_middle - qbscore_middle) / 
               (tclcm2_cldnb - 1),
    
    pope_qbscore_middle = 
      (soge_qbscore_middle - qbscore_middle) / 
      (tecm2_cldnb - 1)
    ) %>% 
  ungroup %>% 
  (function (d) {
    d$pop_qbscore_middle[d$tclcm2_cldnb == 1] <- 0
    d$pope_qbscore_middle[d$tecm2_cldnb == 1] <- 0
    d
  })
  # Pour les tclcm2_cldnb ou tecm2_cldnb = 1, 
  # pop = 0 (logique). 

  # Vérifications : 

  # Montre que pop = p si pnp = 0 : 

  # dat_nonaine %>% 
  #   select(pnp_qbscore_middle, nona, pop_qbscore_middle, 
  #          tclcm2_cldnb, p_qbscore_middle) %>% 
  #   filter(pnp_qbscore_middle == 0)

  # Montre que pnp = p si pop = 0, 
  # c'est le cas si tclcm2_cldnb = 1 : 

  # dat_nonaine %>% 
#   select(pnp_qbscore_middle, nona, pop_qbscore_middle, 
#          tclcm2_cldnb, p_qbscore_middle) %>% 
#   filter(tclcm2_cldnb == 1)

  # Les code ci-dessous vérifie bien que la moyenne pondérée
  # des pnp et pop est égale à p_score. 

# dat_nonaine %>% 
#   select(pnp_qbscore_middle, nona,
#          pop_qbscore_middle, tclcm2_cldnb, p_qbscore_middle) %>% 
#   mutate(v = (tclcm2_cldnb - 1) * pop_qbscore_middle / 
#            (nona - 1) + (nona - tclcm2_cldnb) *
#            pnp_qbscore_middle / (nona - 1)) %>% 
#   mutate(v2 = v - p_qbscore_middle) %>% pull(v2) %>% summary()

# dat_nonaine %>% 
#   select(pnpe_qbscore_middle, nona,
#          pope_qbscore_middle, tecm2_cldnb, p_qbscore_middle) %>% 
#   mutate(v = (tecm2_cldnb - 1) * pope_qbscore_middle / 
#            (nona - 1) + (nona - tecm2_cldnb) *
#            pnpe_qbscore_middle / (nona - 1)) %>% 
#   mutate(v2 = v - p_qbscore_middle) %>% pull(v2) %>% summary()

# pnp_qbscore_high ----------------------------------------------------------

  # sog : sum old group. 
dat_nonaine <- group_by(dat_nonaine, clcm2_cldnb) %>% 
  mutate(sog_qbscore_high = sum(qbscore_high)) %>% 
  ungroup

dat_nonaine <- group_by(dat_nonaine, ecm2_cldnb) %>% 
  mutate(soge_qbscore_high = sum(qbscore_high)) %>% 
  ungroup

  # pnp. 
dat_nonaine <- group_by(dat_nonaine, divconstatrneses) %>% 
  mutate(pnp_qbscore_high = 
           (sum(qbscore_high) - sog_qbscore_high) /
      (nona - tclcm2_cldnb),
    
    pnpe_qbscore_high = 
      (sum(qbscore_high) - soge_qbscore_high) /
      (nona - tecm2_cldnb)) %>% 
  ungroup %>% 
  (function (d) {
    d$pnp_qbscore_high[d$nona <= 1] <- NA
    d$pnpe_qbscore_high[d$nona <= 1] <- NA
    d$pnp_qbscore_high[d$nona == d$tclcm2_cldnb & 
                       d$nona > 1] <- 0
    d$pnpe_qbscore_high[d$nona == d$tclcm2_cldnb & 
                        d$nona > 1] <- 0
    d
  })
  # Pour les n() = 1 ou 2, les pnp sont infinies. 
  # On les remplace par des valeurs manquantes. 
  # 9 valeurs manquantes chez pnp_qbscore_high. 
  # 9 valeurs manquantes chez les pnpe_qbscore_high. 

  # pop. 
dat_nonaine <- group_by(dat_nonaine, divconstatrneses) %>% 
  mutate(
    pop_qbscore_high = 
           (sog_qbscore_high - qbscore_high) / 
               (tclcm2_cldnb - 1),
    
    pope_qbscore_high = 
      (soge_qbscore_high - qbscore_high) / 
      (tecm2_cldnb - 1)
    ) %>% 
  ungroup %>% 
  (function (d) {
    d$pop_qbscore_high[d$tclcm2_cldnb == 1] <- 0
    d$pope_qbscore_high[d$tecm2_cldnb == 1] <- 0
    d
  })
  # Pour les tclcm2_cldnb ou tecm2_cldnb = 1, 
  # pop = 0 (logique). 

  # Vérifications : 

  # Montre que pop = p si pnp = 0 : 

  # dat_nonaine %>% 
  #   select(pnp_qbscore_high, nona, pop_qbscore_high, 
  #          tclcm2_cldnb, p_qbscore_high) %>% 
  #   filter(pnp_qbscore_high == 0)

  # Montre que pnp = p si pop = 0, 
  # c'est le cas si tclcm2_cldnb = 1 : 

  # dat_nonaine %>% 
#   select(pnp_qbscore_high, nona, pop_qbscore_high, 
#          tclcm2_cldnb, p_qbscore_high) %>% 
#   filter(tclcm2_cldnb == 1)

  # Les code ci-dessous vérifie bien que la moyenne pondérée
  # des pnp et pop est égale à p_score. 

# dat_nonaine %>% 
#   select(pnp_qbscore_high, nona,
#          pop_qbscore_high, tclcm2_cldnb, p_qbscore_high) %>% 
#   mutate(v = (tclcm2_cldnb - 1) * pop_qbscore_high / 
#            (nona - 1) + (nona - tclcm2_cldnb) *
#            pnp_qbscore_high / (nona - 1)) %>% 
#   mutate(v2 = v - p_qbscore_high) %>% pull(v2) %>% summary()

# dat_nonaine %>% 
#   select(pnpe_qbscore_high, nona,
#          pope_qbscore_high, tecm2_cldnb, p_qbscore_high) %>% 
#   mutate(v = (tecm2_cldnb - 1) * pope_qbscore_high / 
#            (nona - 1) + (nona - tecm2_cldnb) *
#            pnpe_qbscore_high / (nona - 1)) %>% 
#   mutate(v2 = v - p_qbscore_high) %>% pull(v2) %>% summary()

# pnp_q5score_f_q1 ----------------------------------------------------------

  # sog : sum old group. 
dat_nonaine <- group_by(dat_nonaine, clcm2_cldnb) %>% 
  mutate(sog_q5score_f_q1 = sum(q5score_f_q1)) %>% 
  ungroup

dat_nonaine <- group_by(dat_nonaine, ecm2_cldnb) %>% 
  mutate(soge_q5score_f_q1 = sum(q5score_f_q1)) %>% 
  ungroup

  # pnp. 
dat_nonaine <- group_by(dat_nonaine, divconstatrneses) %>% 
  mutate(pnp_q5score_f_q1 = 
           (sum(q5score_f_q1) - sog_q5score_f_q1) /
      (nona - tclcm2_cldnb),
    
    pnpe_q5score_f_q1 = 
      (sum(q5score_f_q1) - soge_q5score_f_q1) /
      (nona - tecm2_cldnb)) %>% 
  ungroup %>% 
  (function (d) {
    d$pnp_q5score_f_q1[d$nona <= 1] <- NA
    d$pnpe_q5score_f_q1[d$nona <= 1] <- NA
    d$pnp_q5score_f_q1[d$nona == d$tclcm2_cldnb & 
                       d$nona > 1] <- 0
    d$pnpe_q5score_f_q1[d$nona == d$tclcm2_cldnb & 
                        d$nona > 1] <- 0
    d
  })
  # Pour les n() = 1 ou 2, les pnp sont infinies. 
  # On les remplace par des valeurs manquantes. 
  # 9 valeurs manquantes chez pnp_q5score_f_q1. 
  # 9 valeurs manquantes chez les pnpe_q5score_f_q1. 

  # pop. 
dat_nonaine <- group_by(dat_nonaine, divconstatrneses) %>% 
  mutate(
    pop_q5score_f_q1 = 
           (sog_q5score_f_q1 - q5score_f_q1) / 
               (tclcm2_cldnb - 1),
    
    pope_q5score_f_q1 = 
      (soge_q5score_f_q1 - q5score_f_q1) / 
      (tecm2_cldnb - 1)
    ) %>% 
  ungroup %>% 
  (function (d) {
    d$pop_q5score_f_q1[d$tclcm2_cldnb == 1] <- 0
    d$pope_q5score_f_q1[d$tecm2_cldnb == 1] <- 0
    d
  })
  # Pour les tclcm2_cldnb ou tecm2_cldnb = 1, 
  # pop = 0 (logique). 

  # Vérifications : 

  # Montre que pop = p si pnp = 0 : 

  # dat_nonaine %>% 
  #   select(pnp_q5score_f_q1, nona, pop_q5score_f_q1, 
  #          tclcm2_cldnb, p_q5score_f_q1) %>% 
  #   filter(pnp_q5score_f_q1 == 0)

  # Montre que pnp = p si pop = 0, 
  # c'est le cas si tclcm2_cldnb = 1 : 

  # dat_nonaine %>% 
#   select(pnp_q5score_f_q1, nona, pop_q5score_f_q1, 
#          tclcm2_cldnb, p_q5score_f_q1) %>% 
#   filter(tclcm2_cldnb == 1)

  # Les code ci-dessous vérifie bien que la moyenne pondérée
  # des pnp et pop est égale à p_score. 

# dat_nonaine %>% 
#   select(pnp_q5score_f_q1, nona,
#          pop_q5score_f_q1, tclcm2_cldnb, p_q5score_f_q1) %>% 
#   mutate(v = (tclcm2_cldnb - 1) * pop_q5score_f_q1 / 
#            (nona - 1) + (nona - tclcm2_cldnb) *
#            pnp_q5score_f_q1 / (nona - 1)) %>% 
#   mutate(v2 = v - p_q5score_f_q1) %>% pull(v2) %>% summary()

# dat_nonaine %>% 
#   select(pnpe_q5score_f_q1, nona,
#          pope_q5score_f_q1, tecm2_cldnb, p_q5score_f_q1) %>% 
#   mutate(v = (tecm2_cldnb - 1) * pope_q5score_f_q1 / 
#            (nona - 1) + (nona - tecm2_cldnb) *
#            pnpe_q5score_f_q1 / (nona - 1)) %>% 
#   mutate(v2 = v - p_q5score_f_q1) %>% pull(v2) %>% summary()


# pnp_q5score_f_q2 ----------------------------------------------------------

  # sog : sum old group. 
dat_nonaine <- group_by(dat_nonaine, clcm2_cldnb) %>% 
  mutate(sog_q5score_f_q2 = sum(q5score_f_q2)) %>% 
  ungroup

dat_nonaine <- group_by(dat_nonaine, ecm2_cldnb) %>% 
  mutate(soge_q5score_f_q2 = sum(q5score_f_q2)) %>% 
  ungroup

  # pnp. 
dat_nonaine <- group_by(dat_nonaine, divconstatrneses) %>% 
  mutate(pnp_q5score_f_q2 = 
           (sum(q5score_f_q2) - sog_q5score_f_q2) /
      (nona - tclcm2_cldnb),
    
    pnpe_q5score_f_q2 = 
      (sum(q5score_f_q2) - soge_q5score_f_q2) /
      (nona - tecm2_cldnb)) %>% 
  ungroup %>% 
  (function (d) {
    d$pnp_q5score_f_q2[d$nona <= 1] <- NA
    d$pnpe_q5score_f_q2[d$nona <= 1] <- NA
    d$pnp_q5score_f_q2[d$nona == d$tclcm2_cldnb & 
                       d$nona > 1] <- 0
    d$pnpe_q5score_f_q2[d$nona == d$tclcm2_cldnb & 
                        d$nona > 1] <- 0
    d
  })
  # Pour les n() = 1 ou 2, les pnp sont infinies. 
  # On les remplace par des valeurs manquantes. 
  # 9 valeurs manquantes chez pnp_q5score_f_q2. 
  # 9 valeurs manquantes chez les pnpe_q5score_f_q2. 

  # pop. 
dat_nonaine <- group_by(dat_nonaine, divconstatrneses) %>% 
  mutate(
    pop_q5score_f_q2 = 
           (sog_q5score_f_q2 - q5score_f_q2) / 
               (tclcm2_cldnb - 1),
    
    pope_q5score_f_q2 = 
      (soge_q5score_f_q2 - q5score_f_q2) / 
      (tecm2_cldnb - 1)
    ) %>% 
  ungroup %>% 
  (function (d) {
    d$pop_q5score_f_q2[d$tclcm2_cldnb == 1] <- 0
    d$pope_q5score_f_q2[d$tecm2_cldnb == 1] <- 0
    d
  })
  # Pour les tclcm2_cldnb ou tecm2_cldnb = 1, 
  # pop = 0 (logique). 

  # Vérifications : 

  # Montre que pop = p si pnp = 0 : 

  # dat_nonaine %>% 
  #   select(pnp_q5score_f_q2, nona, pop_q5score_f_q2, 
  #          tclcm2_cldnb, p_q5score_f_q2) %>% 
  #   filter(pnp_q5score_f_q2 == 0)

  # Montre que pnp = p si pop = 0, 
  # c'est le cas si tclcm2_cldnb = 1 : 

  # dat_nonaine %>% 
#   select(pnp_q5score_f_q2, nona, pop_q5score_f_q2, 
#          tclcm2_cldnb, p_q5score_f_q2) %>% 
#   filter(tclcm2_cldnb == 1)

  # Les code ci-dessous vérifie bien que la moyenne pondérée
  # des pnp et pop est égale à p_score. 

# dat_nonaine %>% 
#   select(pnp_q5score_f_q2, nona,
#          pop_q5score_f_q2, tclcm2_cldnb, p_q5score_f_q2) %>% 
#   mutate(v = (tclcm2_cldnb - 1) * pop_q5score_f_q2 / 
#            (nona - 1) + (nona - tclcm2_cldnb) *
#            pnp_q5score_f_q2 / (nona - 1)) %>% 
#   mutate(v2 = v - p_q5score_f_q2) %>% pull(v2) %>% summary()

# dat_nonaine %>% 
#   select(pnpe_q5score_f_q2, nona,
#          pope_q5score_f_q2, tecm2_cldnb, p_q5score_f_q2) %>% 
#   mutate(v = (tecm2_cldnb - 1) * pope_q5score_f_q2 / 
#            (nona - 1) + (nona - tecm2_cldnb) *
#            pnpe_q5score_f_q2 / (nona - 1)) %>% 
#   mutate(v2 = v - p_q5score_f_q2) %>% pull(v2) %>% summary()

# pnp_q5score_f_q3 ----------------------------------------------------------

  # sog : sum old group. 
dat_nonaine <- group_by(dat_nonaine, clcm2_cldnb) %>% 
  mutate(sog_q5score_f_q3 = sum(q5score_f_q3)) %>% 
  ungroup

dat_nonaine <- group_by(dat_nonaine, ecm2_cldnb) %>% 
  mutate(soge_q5score_f_q3 = sum(q5score_f_q3)) %>% 
  ungroup

  # pnp. 
dat_nonaine <- group_by(dat_nonaine, divconstatrneses) %>% 
  mutate(pnp_q5score_f_q3 = 
           (sum(q5score_f_q3) - sog_q5score_f_q3) /
      (nona - tclcm2_cldnb),
    
    pnpe_q5score_f_q3 = 
      (sum(q5score_f_q3) - soge_q5score_f_q3) /
      (nona - tecm2_cldnb)) %>% 
  ungroup %>% 
  (function (d) {
    d$pnp_q5score_f_q3[d$nona <= 1] <- NA
    d$pnpe_q5score_f_q3[d$nona <= 1] <- NA
    d$pnp_q5score_f_q3[d$nona == d$tclcm2_cldnb & 
                       d$nona > 1] <- 0
    d$pnpe_q5score_f_q3[d$nona == d$tclcm2_cldnb & 
                        d$nona > 1] <- 0
    d
  })
  # Pour les n() = 1 ou 2, les pnp sont infinies. 
  # On les remplace par des valeurs manquantes. 
  # 9 valeurs manquantes chez pnp_q5score_f_q3. 
  # 9 valeurs manquantes chez les pnpe_q5score_f_q3. 

  # pop. 
dat_nonaine <- group_by(dat_nonaine, divconstatrneses) %>% 
  mutate(
    pop_q5score_f_q3 = 
           (sog_q5score_f_q3 - q5score_f_q3) / 
               (tclcm2_cldnb - 1),
    
    pope_q5score_f_q3 = 
      (soge_q5score_f_q3 - q5score_f_q3) / 
      (tecm2_cldnb - 1)
    ) %>% 
  ungroup %>% 
  (function (d) {
    d$pop_q5score_f_q3[d$tclcm2_cldnb == 1] <- 0
    d$pope_q5score_f_q3[d$tecm2_cldnb == 1] <- 0
    d
  })
  # Pour les tclcm2_cldnb ou tecm2_cldnb = 1, 
  # pop = 0 (logique). 

  # Vérifications : 

  # Montre que pop = p si pnp = 0 : 

  # dat_nonaine %>% 
  #   select(pnp_q5score_f_q3, nona, pop_q5score_f_q3, 
  #          tclcm2_cldnb, p_q5score_f_q3) %>% 
  #   filter(pnp_q5score_f_q3 == 0)

  # Montre que pnp = p si pop = 0, 
  # c'est le cas si tclcm2_cldnb = 1 : 

  # dat_nonaine %>% 
#   select(pnp_q5score_f_q3, nona, pop_q5score_f_q3, 
#          tclcm2_cldnb, p_q5score_f_q3) %>% 
#   filter(tclcm2_cldnb == 1)

  # Les code ci-dessous vérifie bien que la moyenne pondérée
  # des pnp et pop est égale à p_score. 

# dat_nonaine %>% 
#   select(pnp_q5score_f_q3, nona,
#          pop_q5score_f_q3, tclcm2_cldnb, p_q5score_f_q3) %>% 
#   mutate(v = (tclcm2_cldnb - 1) * pop_q5score_f_q3 / 
#            (nona - 1) + (nona - tclcm2_cldnb) *
#            pnp_q5score_f_q3 / (nona - 1)) %>% 
#   mutate(v2 = v - p_q5score_f_q3) %>% pull(v2) %>% summary()

# dat_nonaine %>% 
#   select(pnpe_q5score_f_q3, nona,
#          pope_q5score_f_q3, tecm2_cldnb, p_q5score_f_q3) %>% 
#   mutate(v = (tecm2_cldnb - 1) * pope_q5score_f_q3 / 
#            (nona - 1) + (nona - tecm2_cldnb) *
#            pnpe_q5score_f_q3 / (nona - 1)) %>% 
#   mutate(v2 = v - p_q5score_f_q3) %>% pull(v2) %>% summary()

# pnp_q5score_f_q4 ----------------------------------------------------------

  # sog : sum old group. 
dat_nonaine <- group_by(dat_nonaine, clcm2_cldnb) %>% 
  mutate(sog_q5score_f_q4 = sum(q5score_f_q4)) %>% 
  ungroup

dat_nonaine <- group_by(dat_nonaine, ecm2_cldnb) %>% 
  mutate(soge_q5score_f_q4 = sum(q5score_f_q4)) %>% 
  ungroup

  # pnp. 
dat_nonaine <- group_by(dat_nonaine, divconstatrneses) %>% 
  mutate(pnp_q5score_f_q4 = 
           (sum(q5score_f_q4) - sog_q5score_f_q4) /
      (nona - tclcm2_cldnb),
    
    pnpe_q5score_f_q4 = 
      (sum(q5score_f_q4) - soge_q5score_f_q4) /
      (nona - tecm2_cldnb)) %>% 
  ungroup %>% 
  (function (d) {
    d$pnp_q5score_f_q4[d$nona <= 1] <- NA
    d$pnpe_q5score_f_q4[d$nona <= 1] <- NA
    d$pnp_q5score_f_q4[d$nona == d$tclcm2_cldnb & 
                       d$nona > 1] <- 0
    d$pnpe_q5score_f_q4[d$nona == d$tclcm2_cldnb & 
                        d$nona > 1] <- 0
    d
  })
  # Pour les n() = 1 ou 2, les pnp sont infinies. 
  # On les remplace par des valeurs manquantes. 
  # 9 valeurs manquantes chez pnp_q5score_f_q4. 
  # 9 valeurs manquantes chez les pnpe_q5score_f_q4. 

  # pop. 
dat_nonaine <- group_by(dat_nonaine, divconstatrneses) %>% 
  mutate(
    pop_q5score_f_q4 = 
           (sog_q5score_f_q4 - q5score_f_q4) / 
               (tclcm2_cldnb - 1),
    
    pope_q5score_f_q4 = 
      (soge_q5score_f_q4 - q5score_f_q4) / 
      (tecm2_cldnb - 1)
    ) %>% 
  ungroup %>% 
  (function (d) {
    d$pop_q5score_f_q4[d$tclcm2_cldnb == 1] <- 0
    d$pope_q5score_f_q4[d$tecm2_cldnb == 1] <- 0
    d
  })
  # Pour les tclcm2_cldnb ou tecm2_cldnb = 1, 
  # pop = 0 (logique). 

  # Vérifications : 

  # Montre que pop = p si pnp = 0 : 

  # dat_nonaine %>% 
  #   select(pnp_q5score_f_q4, nona, pop_q5score_f_q4, 
  #          tclcm2_cldnb, p_q5score_f_q4) %>% 
  #   filter(pnp_q5score_f_q4 == 0)

  # Montre que pnp = p si pop = 0, 
  # c'est le cas si tclcm2_cldnb = 1 : 

  # dat_nonaine %>% 
#   select(pnp_q5score_f_q4, nona, pop_q5score_f_q4, 
#          tclcm2_cldnb, p_q5score_f_q4) %>% 
#   filter(tclcm2_cldnb == 1)

  # Les code ci-dessous vérifie bien que la moyenne pondérée
  # des pnp et pop est égale à p_score. 

# dat_nonaine %>% 
#   select(pnp_q5score_f_q4, nona,
#          pop_q5score_f_q4, tclcm2_cldnb, p_q5score_f_q4) %>% 
#   mutate(v = (tclcm2_cldnb - 1) * pop_q5score_f_q4 / 
#            (nona - 1) + (nona - tclcm2_cldnb) *
#            pnp_q5score_f_q4 / (nona - 1)) %>% 
#   mutate(v2 = v - p_q5score_f_q4) %>% pull(v2) %>% summary()

# dat_nonaine %>% 
#   select(pnpe_q5score_f_q4, nona,
#          pope_q5score_f_q4, tecm2_cldnb, p_q5score_f_q4) %>% 
#   mutate(v = (tecm2_cldnb - 1) * pope_q5score_f_q4 / 
#            (nona - 1) + (nona - tecm2_cldnb) *
#            pnpe_q5score_f_q4 / (nona - 1)) %>% 
#   mutate(v2 = v - p_q5score_f_q4) %>% pull(v2) %>% summary()

# pnp_q5score_f_q5 ----------------------------------------------------------

  # sog : sum old group. 
dat_nonaine <- group_by(dat_nonaine, clcm2_cldnb) %>% 
  mutate(sog_q5score_f_q5 = sum(q5score_f_q5)) %>% 
  ungroup

dat_nonaine <- group_by(dat_nonaine, ecm2_cldnb) %>% 
  mutate(soge_q5score_f_q5 = sum(q5score_f_q5)) %>% 
  ungroup

  # pnp. 
dat_nonaine <- group_by(dat_nonaine, divconstatrneses) %>% 
  mutate(pnp_q5score_f_q5 = 
           (sum(q5score_f_q5) - sog_q5score_f_q5) /
      (nona - tclcm2_cldnb),
    
    pnpe_q5score_f_q5 = 
      (sum(q5score_f_q5) - soge_q5score_f_q5) /
      (nona - tecm2_cldnb)) %>% 
  ungroup %>% 
  (function (d) {
    d$pnp_q5score_f_q5[d$nona <= 1] <- NA
    d$pnpe_q5score_f_q5[d$nona <= 1] <- NA
    d$pnp_q5score_f_q5[d$nona == d$tclcm2_cldnb & 
                       d$nona > 1] <- 0
    d$pnpe_q5score_f_q5[d$nona == d$tclcm2_cldnb & 
                        d$nona > 1] <- 0
    d
  })
  # Pour les n() = 1 ou 2, les pnp sont infinies. 
  # On les remplace par des valeurs manquantes. 
  # 9 valeurs manquantes chez pnp_q5score_f_q5. 
  # 9 valeurs manquantes chez les pnpe_q5score_f_q5. 

  # pop. 
dat_nonaine <- group_by(dat_nonaine, divconstatrneses) %>% 
  mutate(
    pop_q5score_f_q5 = 
           (sog_q5score_f_q5 - q5score_f_q5) / 
               (tclcm2_cldnb - 1),
    
    pope_q5score_f_q5 = 
      (soge_q5score_f_q5 - q5score_f_q5) / 
      (tecm2_cldnb - 1)
    ) %>% 
  ungroup %>% 
  (function (d) {
    d$pop_q5score_f_q5[d$tclcm2_cldnb == 1] <- 0
    d$pope_q5score_f_q5[d$tecm2_cldnb == 1] <- 0
    d
  })
  # Pour les tclcm2_cldnb ou tecm2_cldnb = 1, 
  # pop = 0 (logique). 

  # Vérifications : 

  # Montre que pop = p si pnp = 0 : 

  # dat_nonaine %>% 
  #   select(pnp_q5score_f_q5, nona, pop_q5score_f_q5, 
  #          tclcm2_cldnb, p_q5score_f_q5) %>% 
  #   filter(pnp_q5score_f_q5 == 0)

  # Montre que pnp = p si pop = 0, 
  # c'est le cas si tclcm2_cldnb = 1 : 

  # dat_nonaine %>% 
#   select(pnp_q5score_f_q5, nona, pop_q5score_f_q5, 
#          tclcm2_cldnb, p_q5score_f_q5) %>% 
#   filter(tclcm2_cldnb == 1)

  # Les code ci-dessous vérifie bien que la moyenne pondérée
  # des pnp et pop est égale à p_score. 

# dat_nonaine %>% 
#   select(pnp_q5score_f_q5, nona,
#          pop_q5score_f_q5, tclcm2_cldnb, p_q5score_f_q5) %>% 
#   mutate(v = (tclcm2_cldnb - 1) * pop_q5score_f_q5 / 
#            (nona - 1) + (nona - tclcm2_cldnb) *
#            pnp_q5score_f_q5 / (nona - 1)) %>% 
#   mutate(v2 = v - p_q5score_f_q5) %>% pull(v2) %>% summary()

# dat_nonaine %>% 
#   select(pnpe_q5score_f_q5, nona,
#          pope_q5score_f_q5, tecm2_cldnb, p_q5score_f_q5) %>% 
#   mutate(v = (tecm2_cldnb - 1) * pope_q5score_f_q5 / 
#            (nona - 1) + (nona - tecm2_cldnb) *
#            pnpe_q5score_f_q5 / (nona - 1)) %>% 
#   mutate(v2 = v - p_q5score_f_q5) %>% pull(v2) %>% summary()

# pnp_qbscore_f_low ----------------------------------------------------------

  # sog : sum old group. 
dat_nonaine <- group_by(dat_nonaine, clcm2_cldnb) %>% 
  mutate(sog_qbscore_f_low = sum(qbscore_f_low)) %>% 
  ungroup

dat_nonaine <- group_by(dat_nonaine, ecm2_cldnb) %>% 
  mutate(soge_qbscore_f_low = sum(qbscore_f_low)) %>% 
  ungroup

  # pnp. 
dat_nonaine <- group_by(dat_nonaine, divconstatrneses) %>% 
  mutate(pnp_qbscore_f_low = 
           (sum(qbscore_f_low) - sog_qbscore_f_low) /
      (nona - tclcm2_cldnb),
    
    pnpe_qbscore_f_low = 
      (sum(qbscore_f_low) - soge_qbscore_f_low) /
      (nona - tecm2_cldnb)) %>% 
  ungroup %>% 
  (function (d) {
    d$pnp_qbscore_f_low[d$nona <= 1] <- NA
    d$pnpe_qbscore_f_low[d$nona <= 1] <- NA
    d$pnp_qbscore_f_low[d$nona == d$tclcm2_cldnb & 
                       d$nona > 1] <- 0
    d$pnpe_qbscore_f_low[d$nona == d$tclcm2_cldnb & 
                        d$nona > 1] <- 0
    d
  })
  # Pour les n() = 1 ou 2, les pnp sont infinies. 
  # On les remplace par des valeurs manquantes. 
  # 9 valeurs manquantes chez pnp_qbscore_f_low. 
  # 9 valeurs manquantes chez les pnpe_qbscore_f_low. 

  # pop. 
dat_nonaine <- group_by(dat_nonaine, divconstatrneses) %>% 
  mutate(
    pop_qbscore_f_low = 
           (sog_qbscore_f_low - qbscore_f_low) / 
               (tclcm2_cldnb - 1),
    
    pope_qbscore_f_low = 
      (soge_qbscore_f_low - qbscore_f_low) / 
      (tecm2_cldnb - 1)
    ) %>% 
  ungroup %>% 
  (function (d) {
    d$pop_qbscore_f_low[d$tclcm2_cldnb == 1] <- 0
    d$pope_qbscore_f_low[d$tecm2_cldnb == 1] <- 0
    d
  })
  # Pour les tclcm2_cldnb ou tecm2_cldnb = 1, 
  # pop = 0 (logique). 

  # Vérifications : 

  # Montre que pop = p si pnp = 0 : 

  # dat_nonaine %>% 
  #   select(pnp_qbscore_f_low, nona, pop_qbscore_f_low, 
  #          tclcm2_cldnb, p_qbscore_f_low) %>% 
  #   filter(pnp_qbscore_f_low == 0)

  # Montre que pnp = p si pop = 0, 
  # c'est le cas si tclcm2_cldnb = 1 : 

  # dat_nonaine %>% 
#   select(pnp_qbscore_f_low, nona, pop_qbscore_f_low, 
#          tclcm2_cldnb, p_qbscore_f_low) %>% 
#   filter(tclcm2_cldnb == 1)

  # Les code ci-dessous vérifie bien que la moyenne pondérée
  # des pnp et pop est égale à p_score. 

# dat_nonaine %>% 
#   select(pnp_qbscore_f_low, nona,
#          pop_qbscore_f_low, tclcm2_cldnb, p_qbscore_f_low) %>% 
#   mutate(v = (tclcm2_cldnb - 1) * pop_qbscore_f_low / 
#            (nona - 1) + (nona - tclcm2_cldnb) *
#            pnp_qbscore_f_low / (nona - 1)) %>% 
#   mutate(v2 = v - p_qbscore_f_low) %>% pull(v2) %>% summary()

# dat_nonaine %>% 
#   select(pnpe_qbscore_f_low, nona,
#          pope_qbscore_f_low, tecm2_cldnb, p_qbscore_f_low) %>% 
#   mutate(v = (tecm2_cldnb - 1) * pope_qbscore_f_low / 
#            (nona - 1) + (nona - tecm2_cldnb) *
#            pnpe_qbscore_f_low / (nona - 1)) %>% 
#   mutate(v2 = v - p_qbscore_f_low) %>% pull(v2) %>% summary()

# pnp_qbscore_f_middle ----------------------------------------------------------

  # sog : sum old group. 
dat_nonaine <- group_by(dat_nonaine, clcm2_cldnb) %>% 
  mutate(sog_qbscore_f_middle = sum(qbscore_f_middle)) %>% 
  ungroup

dat_nonaine <- group_by(dat_nonaine, ecm2_cldnb) %>% 
  mutate(soge_qbscore_f_middle = sum(qbscore_f_middle)) %>% 
  ungroup

  # pnp. 
dat_nonaine <- group_by(dat_nonaine, divconstatrneses) %>% 
  mutate(pnp_qbscore_f_middle = 
           (sum(qbscore_f_middle) - sog_qbscore_f_middle) /
      (nona - tclcm2_cldnb),
    
    pnpe_qbscore_f_middle = 
      (sum(qbscore_f_middle) - soge_qbscore_f_middle) /
      (nona - tecm2_cldnb)) %>% 
  ungroup %>% 
  (function (d) {
    d$pnp_qbscore_f_middle[d$nona <= 1] <- NA
    d$pnpe_qbscore_f_middle[d$nona <= 1] <- NA
    d$pnp_qbscore_f_middle[d$nona == d$tclcm2_cldnb & 
                       d$nona > 1] <- 0
    d$pnpe_qbscore_f_middle[d$nona == d$tclcm2_cldnb & 
                        d$nona > 1] <- 0
    d
  })
  # Pour les n() = 1 ou 2, les pnp sont infinies. 
  # On les remplace par des valeurs manquantes. 
  # 9 valeurs manquantes chez pnp_qbscore_f_middle. 
  # 9 valeurs manquantes chez les pnpe_qbscore_f_middle. 

  # pop. 
dat_nonaine <- group_by(dat_nonaine, divconstatrneses) %>% 
  mutate(
    pop_qbscore_f_middle = 
           (sog_qbscore_f_middle - qbscore_f_middle) / 
               (tclcm2_cldnb - 1),
    
    pope_qbscore_f_middle = 
      (soge_qbscore_f_middle - qbscore_f_middle) / 
      (tecm2_cldnb - 1)
    ) %>% 
  ungroup %>% 
  (function (d) {
    d$pop_qbscore_f_middle[d$tclcm2_cldnb == 1] <- 0
    d$pope_qbscore_f_middle[d$tecm2_cldnb == 1] <- 0
    d
  })
  # Pour les tclcm2_cldnb ou tecm2_cldnb = 1, 
  # pop = 0 (logique). 

  # Vérifications : 

  # Montre que pop = p si pnp = 0 : 

  # dat_nonaine %>% 
  #   select(pnp_qbscore_f_middle, nona, pop_qbscore_f_middle, 
  #          tclcm2_cldnb, p_qbscore_f_middle) %>% 
  #   filter(pnp_qbscore_f_middle == 0)

  # Montre que pnp = p si pop = 0, 
  # c'est le cas si tclcm2_cldnb = 1 : 

  # dat_nonaine %>% 
#   select(pnp_qbscore_f_middle, nona, pop_qbscore_f_middle, 
#          tclcm2_cldnb, p_qbscore_f_middle) %>% 
#   filter(tclcm2_cldnb == 1)

  # Les code ci-dessous vérifie bien que la moyenne pondérée
  # des pnp et pop est égale à p_score. 

# dat_nonaine %>% 
#   select(pnp_qbscore_f_middle, nona,
#          pop_qbscore_f_middle, tclcm2_cldnb, p_qbscore_f_middle) %>% 
#   mutate(v = (tclcm2_cldnb - 1) * pop_qbscore_f_middle / 
#            (nona - 1) + (nona - tclcm2_cldnb) *
#            pnp_qbscore_f_middle / (nona - 1)) %>% 
#   mutate(v2 = v - p_qbscore_f_middle) %>% pull(v2) %>% summary()

# dat_nonaine %>% 
#   select(pnpe_qbscore_f_middle, nona,
#          pope_qbscore_f_middle, tecm2_cldnb, p_qbscore_f_middle) %>% 
#   mutate(v = (tecm2_cldnb - 1) * pope_qbscore_f_middle / 
#            (nona - 1) + (nona - tecm2_cldnb) *
#            pnpe_qbscore_f_middle / (nona - 1)) %>% 
#   mutate(v2 = v - p_qbscore_f_middle) %>% pull(v2) %>% summary()

# pnp_qbscore_f_high ----------------------------------------------------------

  # sog : sum old group. 
dat_nonaine <- group_by(dat_nonaine, clcm2_cldnb) %>% 
  mutate(sog_qbscore_f_high = sum(qbscore_f_high)) %>% 
  ungroup

dat_nonaine <- group_by(dat_nonaine, ecm2_cldnb) %>% 
  mutate(soge_qbscore_f_high = sum(qbscore_f_high)) %>% 
  ungroup

  # pnp. 
dat_nonaine <- group_by(dat_nonaine, divconstatrneses) %>% 
  mutate(pnp_qbscore_f_high = 
           (sum(qbscore_f_high) - sog_qbscore_f_high) /
      (nona - tclcm2_cldnb),
    
    pnpe_qbscore_f_high = 
      (sum(qbscore_f_high) - soge_qbscore_f_high) /
      (nona - tecm2_cldnb)) %>% 
  ungroup %>% 
  (function (d) {
    d$pnp_qbscore_f_high[d$nona <= 1] <- NA
    d$pnpe_qbscore_f_high[d$nona <= 1] <- NA
    d$pnp_qbscore_f_high[d$nona == d$tclcm2_cldnb & 
                       d$nona > 1] <- 0
    d$pnpe_qbscore_f_high[d$nona == d$tclcm2_cldnb & 
                        d$nona > 1] <- 0
    d
  })
  # Pour les n() = 1 ou 2, les pnp sont infinies. 
  # On les remplace par des valeurs manquantes. 
  # 9 valeurs manquantes chez pnp_qbscore_f_high. 
  # 9 valeurs manquantes chez les pnpe_qbscore_f_high. 

  # pop. 
dat_nonaine <- group_by(dat_nonaine, divconstatrneses) %>% 
  mutate(
    pop_qbscore_f_high = 
           (sog_qbscore_f_high - qbscore_f_high) / 
               (tclcm2_cldnb - 1),
    
    pope_qbscore_f_high = 
      (soge_qbscore_f_high - qbscore_f_high) / 
      (tecm2_cldnb - 1)
    ) %>% 
  ungroup %>% 
  (function (d) {
    d$pop_qbscore_f_high[d$tclcm2_cldnb == 1] <- 0
    d$pope_qbscore_f_high[d$tecm2_cldnb == 1] <- 0
    d
  })
  # Pour les tclcm2_cldnb ou tecm2_cldnb = 1, 
  # pop = 0 (logique). 

  # Vérifications : 

  # Montre que pop = p si pnp = 0 : 

  # dat_nonaine %>% 
  #   select(pnp_qbscore_f_high, nona, pop_qbscore_f_high, 
  #          tclcm2_cldnb, p_qbscore_f_high) %>% 
  #   filter(pnp_qbscore_f_high == 0)

  # Montre que pnp = p si pop = 0, 
  # c'est le cas si tclcm2_cldnb = 1 : 

  # dat_nonaine %>% 
#   select(pnp_qbscore_f_high, nona, pop_qbscore_f_high, 
#          tclcm2_cldnb, p_qbscore_f_high) %>% 
#   filter(tclcm2_cldnb == 1)

  # Les code ci-dessous vérifie bien que la moyenne pondérée
  # des pnp et pop est égale à p_score. 

# dat_nonaine %>% 
#   select(pnp_qbscore_f_high, nona,
#          pop_qbscore_f_high, tclcm2_cldnb, p_qbscore_f_high) %>% 
#   mutate(v = (tclcm2_cldnb - 1) * pop_qbscore_f_high / 
#            (nona - 1) + (nona - tclcm2_cldnb) *
#            pnp_qbscore_f_high / (nona - 1)) %>% 
#   mutate(v2 = v - p_qbscore_f_high) %>% pull(v2) %>% summary()

# dat_nonaine %>% 
#   select(pnpe_qbscore_f_high, nona,
#          pope_qbscore_f_high, tecm2_cldnb, p_qbscore_f_high) %>% 
#   mutate(v = (tecm2_cldnb - 1) * pope_qbscore_f_high / 
#            (nona - 1) + (nona - tecm2_cldnb) *
#            pnpe_qbscore_f_high / (nona - 1)) %>% 
#   mutate(v2 = v - p_qbscore_f_high) %>% pull(v2) %>% summary()

# pnp_q5score_m_q1 ----------------------------------------------------------

  # sog : sum old group. 
dat_nonaine <- group_by(dat_nonaine, clcm2_cldnb) %>% 
  mutate(sog_q5score_m_q1 = sum(q5score_m_q1)) %>% 
  ungroup

dat_nonaine <- group_by(dat_nonaine, ecm2_cldnb) %>% 
  mutate(soge_q5score_m_q1 = sum(q5score_m_q1)) %>% 
  ungroup

  # pnp. 
dat_nonaine <- group_by(dat_nonaine, divconstatrneses) %>% 
  mutate(pnp_q5score_m_q1 = 
           (sum(q5score_m_q1) - sog_q5score_m_q1) /
      (nona - tclcm2_cldnb),
    
    pnpe_q5score_m_q1 = 
      (sum(q5score_m_q1) - soge_q5score_m_q1) /
      (nona - tecm2_cldnb)) %>% 
  ungroup %>% 
  (function (d) {
    d$pnp_q5score_m_q1[d$nona <= 1] <- NA
    d$pnpe_q5score_m_q1[d$nona <= 1] <- NA
    d$pnp_q5score_m_q1[d$nona == d$tclcm2_cldnb & 
                       d$nona > 1] <- 0
    d$pnpe_q5score_m_q1[d$nona == d$tclcm2_cldnb & 
                        d$nona > 1] <- 0
    d
  })
  # Pour les n() = 1 ou 2, les pnp sont infinies. 
  # On les remplace par des valeurs manquantes. 
  # 9 valeurs manquantes chez pnp_q5score_m_q1. 
  # 9 valeurs manquantes chez les pnpe_q5score_m_q1. 

  # pop. 
dat_nonaine <- group_by(dat_nonaine, divconstatrneses) %>% 
  mutate(
    pop_q5score_m_q1 = 
           (sog_q5score_m_q1 - q5score_m_q1) / 
               (tclcm2_cldnb - 1),
    
    pope_q5score_m_q1 = 
      (soge_q5score_m_q1 - q5score_m_q1) / 
      (tecm2_cldnb - 1)
    ) %>% 
  ungroup %>% 
  (function (d) {
    d$pop_q5score_m_q1[d$tclcm2_cldnb == 1] <- 0
    d$pope_q5score_m_q1[d$tecm2_cldnb == 1] <- 0
    d
  })
  # Pour les tclcm2_cldnb ou tecm2_cldnb = 1, 
  # pop = 0 (logique). 

  # Vérifications : 

  # Montre que pop = p si pnp = 0 : 

  # dat_nonaine %>% 
  #   select(pnp_q5score_m_q1, nona, pop_q5score_m_q1, 
  #          tclcm2_cldnb, p_q5score_m_q1) %>% 
  #   filter(pnp_q5score_m_q1 == 0)

  # Montre que pnp = p si pop = 0, 
  # c'est le cas si tclcm2_cldnb = 1 : 

  # dat_nonaine %>% 
#   select(pnp_q5score_m_q1, nona, pop_q5score_m_q1, 
#          tclcm2_cldnb, p_q5score_m_q1) %>% 
#   filter(tclcm2_cldnb == 1)

  # Les code ci-dessous vérifie bien que la moyenne pondérée
  # des pnp et pop est égale à p_score. 

# dat_nonaine %>% 
#   select(pnp_q5score_m_q1, nona,
#          pop_q5score_m_q1, tclcm2_cldnb, p_q5score_m_q1) %>% 
#   mutate(v = (tclcm2_cldnb - 1) * pop_q5score_m_q1 / 
#            (nona - 1) + (nona - tclcm2_cldnb) *
#            pnp_q5score_m_q1 / (nona - 1)) %>% 
#   mutate(v2 = v - p_q5score_m_q1) %>% pull(v2) %>% summary()

# dat_nonaine %>% 
#   select(pnpe_q5score_m_q1, nona,
#          pope_q5score_m_q1, tecm2_cldnb, p_q5score_m_q1) %>% 
#   mutate(v = (tecm2_cldnb - 1) * pope_q5score_m_q1 / 
#            (nona - 1) + (nona - tecm2_cldnb) *
#            pnpe_q5score_m_q1 / (nona - 1)) %>% 
#   mutate(v2 = v - p_q5score_m_q1) %>% pull(v2) %>% summary()


# pnp_q5score_m_q2 ----------------------------------------------------------

  # sog : sum old group. 
dat_nonaine <- group_by(dat_nonaine, clcm2_cldnb) %>% 
  mutate(sog_q5score_m_q2 = sum(q5score_m_q2)) %>% 
  ungroup

dat_nonaine <- group_by(dat_nonaine, ecm2_cldnb) %>% 
  mutate(soge_q5score_m_q2 = sum(q5score_m_q2)) %>% 
  ungroup

  # pnp. 
dat_nonaine <- group_by(dat_nonaine, divconstatrneses) %>% 
  mutate(pnp_q5score_m_q2 = 
           (sum(q5score_m_q2) - sog_q5score_m_q2) /
      (nona - tclcm2_cldnb),
    
    pnpe_q5score_m_q2 = 
      (sum(q5score_m_q2) - soge_q5score_m_q2) /
      (nona - tecm2_cldnb)) %>% 
  ungroup %>% 
  (function (d) {
    d$pnp_q5score_m_q2[d$nona <= 1] <- NA
    d$pnpe_q5score_m_q2[d$nona <= 1] <- NA
    d$pnp_q5score_m_q2[d$nona == d$tclcm2_cldnb & 
                       d$nona > 1] <- 0
    d$pnpe_q5score_m_q2[d$nona == d$tclcm2_cldnb & 
                        d$nona > 1] <- 0
    d
  })
  # Pour les n() = 1 ou 2, les pnp sont infinies. 
  # On les remplace par des valeurs manquantes. 
  # 9 valeurs manquantes chez pnp_q5score_m_q2. 
  # 9 valeurs manquantes chez les pnpe_q5score_m_q2. 

  # pop. 
dat_nonaine <- group_by(dat_nonaine, divconstatrneses) %>% 
  mutate(
    pop_q5score_m_q2 = 
           (sog_q5score_m_q2 - q5score_m_q2) / 
               (tclcm2_cldnb - 1),
    
    pope_q5score_m_q2 = 
      (soge_q5score_m_q2 - q5score_m_q2) / 
      (tecm2_cldnb - 1)
    ) %>% 
  ungroup %>% 
  (function (d) {
    d$pop_q5score_m_q2[d$tclcm2_cldnb == 1] <- 0
    d$pope_q5score_m_q2[d$tecm2_cldnb == 1] <- 0
    d
  })
  # Pour les tclcm2_cldnb ou tecm2_cldnb = 1, 
  # pop = 0 (logique). 

  # Vérifications : 

  # Montre que pop = p si pnp = 0 : 

  # dat_nonaine %>% 
  #   select(pnp_q5score_m_q2, nona, pop_q5score_m_q2, 
  #          tclcm2_cldnb, p_q5score_m_q2) %>% 
  #   filter(pnp_q5score_m_q2 == 0)

  # Montre que pnp = p si pop = 0, 
  # c'est le cas si tclcm2_cldnb = 1 : 

  # dat_nonaine %>% 
#   select(pnp_q5score_m_q2, nona, pop_q5score_m_q2, 
#          tclcm2_cldnb, p_q5score_m_q2) %>% 
#   filter(tclcm2_cldnb == 1)

  # Les code ci-dessous vérifie bien que la moyenne pondérée
  # des pnp et pop est égale à p_score. 

# dat_nonaine %>% 
#   select(pnp_q5score_m_q2, nona,
#          pop_q5score_m_q2, tclcm2_cldnb, p_q5score_m_q2) %>% 
#   mutate(v = (tclcm2_cldnb - 1) * pop_q5score_m_q2 / 
#            (nona - 1) + (nona - tclcm2_cldnb) *
#            pnp_q5score_m_q2 / (nona - 1)) %>% 
#   mutate(v2 = v - p_q5score_m_q2) %>% pull(v2) %>% summary()

# dat_nonaine %>% 
#   select(pnpe_q5score_m_q2, nona,
#          pope_q5score_m_q2, tecm2_cldnb, p_q5score_m_q2) %>% 
#   mutate(v = (tecm2_cldnb - 1) * pope_q5score_m_q2 / 
#            (nona - 1) + (nona - tecm2_cldnb) *
#            pnpe_q5score_m_q2 / (nona - 1)) %>% 
#   mutate(v2 = v - p_q5score_m_q2) %>% pull(v2) %>% summary()

# pnp_q5score_m_q3 ----------------------------------------------------------

  # sog : sum old group. 
dat_nonaine <- group_by(dat_nonaine, clcm2_cldnb) %>% 
  mutate(sog_q5score_m_q3 = sum(q5score_m_q3)) %>% 
  ungroup

dat_nonaine <- group_by(dat_nonaine, ecm2_cldnb) %>% 
  mutate(soge_q5score_m_q3 = sum(q5score_m_q3)) %>% 
  ungroup

  # pnp. 
dat_nonaine <- group_by(dat_nonaine, divconstatrneses) %>% 
  mutate(pnp_q5score_m_q3 = 
           (sum(q5score_m_q3) - sog_q5score_m_q3) /
      (nona - tclcm2_cldnb),
    
    pnpe_q5score_m_q3 = 
      (sum(q5score_m_q3) - soge_q5score_m_q3) /
      (nona - tecm2_cldnb)) %>% 
  ungroup %>% 
  (function (d) {
    d$pnp_q5score_m_q3[d$nona <= 1] <- NA
    d$pnpe_q5score_m_q3[d$nona <= 1] <- NA
    d$pnp_q5score_m_q3[d$nona == d$tclcm2_cldnb & 
                       d$nona > 1] <- 0
    d$pnpe_q5score_m_q3[d$nona == d$tclcm2_cldnb & 
                        d$nona > 1] <- 0
    d
  })
  # Pour les n() = 1 ou 2, les pnp sont infinies. 
  # On les remplace par des valeurs manquantes. 
  # 9 valeurs manquantes chez pnp_q5score_m_q3. 
  # 9 valeurs manquantes chez les pnpe_q5score_m_q3. 

  # pop. 
dat_nonaine <- group_by(dat_nonaine, divconstatrneses) %>% 
  mutate(
    pop_q5score_m_q3 = 
           (sog_q5score_m_q3 - q5score_m_q3) / 
               (tclcm2_cldnb - 1),
    
    pope_q5score_m_q3 = 
      (soge_q5score_m_q3 - q5score_m_q3) / 
      (tecm2_cldnb - 1)
    ) %>% 
  ungroup %>% 
  (function (d) {
    d$pop_q5score_m_q3[d$tclcm2_cldnb == 1] <- 0
    d$pope_q5score_m_q3[d$tecm2_cldnb == 1] <- 0
    d
  })
  # Pour les tclcm2_cldnb ou tecm2_cldnb = 1, 
  # pop = 0 (logique). 

  # Vérifications : 

  # Montre que pop = p si pnp = 0 : 

  # dat_nonaine %>% 
  #   select(pnp_q5score_m_q3, nona, pop_q5score_m_q3, 
  #          tclcm2_cldnb, p_q5score_m_q3) %>% 
  #   filter(pnp_q5score_m_q3 == 0)

  # Montre que pnp = p si pop = 0, 
  # c'est le cas si tclcm2_cldnb = 1 : 

  # dat_nonaine %>% 
#   select(pnp_q5score_m_q3, nona, pop_q5score_m_q3, 
#          tclcm2_cldnb, p_q5score_m_q3) %>% 
#   filter(tclcm2_cldnb == 1)

  # Les code ci-dessous vérifie bien que la moyenne pondérée
  # des pnp et pop est égale à p_score. 

# dat_nonaine %>% 
#   select(pnp_q5score_m_q3, nona,
#          pop_q5score_m_q3, tclcm2_cldnb, p_q5score_m_q3) %>% 
#   mutate(v = (tclcm2_cldnb - 1) * pop_q5score_m_q3 / 
#            (nona - 1) + (nona - tclcm2_cldnb) *
#            pnp_q5score_m_q3 / (nona - 1)) %>% 
#   mutate(v2 = v - p_q5score_m_q3) %>% pull(v2) %>% summary()

# dat_nonaine %>% 
#   select(pnpe_q5score_m_q3, nona,
#          pope_q5score_m_q3, tecm2_cldnb, p_q5score_m_q3) %>% 
#   mutate(v = (tecm2_cldnb - 1) * pope_q5score_m_q3 / 
#            (nona - 1) + (nona - tecm2_cldnb) *
#            pnpe_q5score_m_q3 / (nona - 1)) %>% 
#   mutate(v2 = v - p_q5score_m_q3) %>% pull(v2) %>% summary()

# pnp_q5score_m_q4 ----------------------------------------------------------

  # sog : sum old group. 
dat_nonaine <- group_by(dat_nonaine, clcm2_cldnb) %>% 
  mutate(sog_q5score_m_q4 = sum(q5score_m_q4)) %>% 
  ungroup

dat_nonaine <- group_by(dat_nonaine, ecm2_cldnb) %>% 
  mutate(soge_q5score_m_q4 = sum(q5score_m_q4)) %>% 
  ungroup

  # pnp. 
dat_nonaine <- group_by(dat_nonaine, divconstatrneses) %>% 
  mutate(pnp_q5score_m_q4 = 
           (sum(q5score_m_q4) - sog_q5score_m_q4) /
      (nona - tclcm2_cldnb),
    
    pnpe_q5score_m_q4 = 
      (sum(q5score_m_q4) - soge_q5score_m_q4) /
      (nona - tecm2_cldnb)) %>% 
  ungroup %>% 
  (function (d) {
    d$pnp_q5score_m_q4[d$nona <= 1] <- NA
    d$pnpe_q5score_m_q4[d$nona <= 1] <- NA
    d$pnp_q5score_m_q4[d$nona == d$tclcm2_cldnb & 
                       d$nona > 1] <- 0
    d$pnpe_q5score_m_q4[d$nona == d$tclcm2_cldnb & 
                        d$nona > 1] <- 0
    d
  })
  # Pour les n() = 1 ou 2, les pnp sont infinies. 
  # On les remplace par des valeurs manquantes. 
  # 9 valeurs manquantes chez pnp_q5score_m_q4. 
  # 9 valeurs manquantes chez les pnpe_q5score_m_q4. 

  # pop. 
dat_nonaine <- group_by(dat_nonaine, divconstatrneses) %>% 
  mutate(
    pop_q5score_m_q4 = 
           (sog_q5score_m_q4 - q5score_m_q4) / 
               (tclcm2_cldnb - 1),
    
    pope_q5score_m_q4 = 
      (soge_q5score_m_q4 - q5score_m_q4) / 
      (tecm2_cldnb - 1)
    ) %>% 
  ungroup %>% 
  (function (d) {
    d$pop_q5score_m_q4[d$tclcm2_cldnb == 1] <- 0
    d$pope_q5score_m_q4[d$tecm2_cldnb == 1] <- 0
    d
  })
  # Pour les tclcm2_cldnb ou tecm2_cldnb = 1, 
  # pop = 0 (logique). 

  # Vérifications : 

  # Montre que pop = p si pnp = 0 : 

  # dat_nonaine %>% 
  #   select(pnp_q5score_m_q4, nona, pop_q5score_m_q4, 
  #          tclcm2_cldnb, p_q5score_m_q4) %>% 
  #   filter(pnp_q5score_m_q4 == 0)

  # Montre que pnp = p si pop = 0, 
  # c'est le cas si tclcm2_cldnb = 1 : 

  # dat_nonaine %>% 
#   select(pnp_q5score_m_q4, nona, pop_q5score_m_q4, 
#          tclcm2_cldnb, p_q5score_m_q4) %>% 
#   filter(tclcm2_cldnb == 1)

  # Les code ci-dessous vérifie bien que la moyenne pondérée
  # des pnp et pop est égale à p_score. 

# dat_nonaine %>% 
#   select(pnp_q5score_m_q4, nona,
#          pop_q5score_m_q4, tclcm2_cldnb, p_q5score_m_q4) %>% 
#   mutate(v = (tclcm2_cldnb - 1) * pop_q5score_m_q4 / 
#            (nona - 1) + (nona - tclcm2_cldnb) *
#            pnp_q5score_m_q4 / (nona - 1)) %>% 
#   mutate(v2 = v - p_q5score_m_q4) %>% pull(v2) %>% summary()

# dat_nonaine %>% 
#   select(pnpe_q5score_m_q4, nona,
#          pope_q5score_m_q4, tecm2_cldnb, p_q5score_m_q4) %>% 
#   mutate(v = (tecm2_cldnb - 1) * pope_q5score_m_q4 / 
#            (nona - 1) + (nona - tecm2_cldnb) *
#            pnpe_q5score_m_q4 / (nona - 1)) %>% 
#   mutate(v2 = v - p_q5score_m_q4) %>% pull(v2) %>% summary()

# pnp_q5score_m_q5 ----------------------------------------------------------

  # sog : sum old group. 
dat_nonaine <- group_by(dat_nonaine, clcm2_cldnb) %>% 
  mutate(sog_q5score_m_q5 = sum(q5score_m_q5)) %>% 
  ungroup

dat_nonaine <- group_by(dat_nonaine, ecm2_cldnb) %>% 
  mutate(soge_q5score_m_q5 = sum(q5score_m_q5)) %>% 
  ungroup

  # pnp. 
dat_nonaine <- group_by(dat_nonaine, divconstatrneses) %>% 
  mutate(pnp_q5score_m_q5 = 
           (sum(q5score_m_q5) - sog_q5score_m_q5) /
      (nona - tclcm2_cldnb),
    
    pnpe_q5score_m_q5 = 
      (sum(q5score_m_q5) - soge_q5score_m_q5) /
      (nona - tecm2_cldnb)) %>% 
  ungroup %>% 
  (function (d) {
    d$pnp_q5score_m_q5[d$nona <= 1] <- NA
    d$pnpe_q5score_m_q5[d$nona <= 1] <- NA
    d$pnp_q5score_m_q5[d$nona == d$tclcm2_cldnb & 
                       d$nona > 1] <- 0
    d$pnpe_q5score_m_q5[d$nona == d$tclcm2_cldnb & 
                        d$nona > 1] <- 0
    d
  })
  # Pour les n() = 1 ou 2, les pnp sont infinies. 
  # On les remplace par des valeurs manquantes. 
  # 9 valeurs manquantes chez pnp_q5score_m_q5. 
  # 9 valeurs manquantes chez les pnpe_q5score_m_q5. 

  # pop. 
dat_nonaine <- group_by(dat_nonaine, divconstatrneses) %>% 
  mutate(
    pop_q5score_m_q5 = 
           (sog_q5score_m_q5 - q5score_m_q5) / 
               (tclcm2_cldnb - 1),
    
    pope_q5score_m_q5 = 
      (soge_q5score_m_q5 - q5score_m_q5) / 
      (tecm2_cldnb - 1)
    ) %>% 
  ungroup %>% 
  (function (d) {
    d$pop_q5score_m_q5[d$tclcm2_cldnb == 1] <- 0
    d$pope_q5score_m_q5[d$tecm2_cldnb == 1] <- 0
    d
  })
  # Pour les tclcm2_cldnb ou tecm2_cldnb = 1, 
  # pop = 0 (logique). 

  # Vérifications : 

  # Montre que pop = p si pnp = 0 : 

  # dat_nonaine %>% 
  #   select(pnp_q5score_m_q5, nona, pop_q5score_m_q5, 
  #          tclcm2_cldnb, p_q5score_m_q5) %>% 
  #   filter(pnp_q5score_m_q5 == 0)

  # Montre que pnp = p si pop = 0, 
  # c'est le cas si tclcm2_cldnb = 1 : 

  # dat_nonaine %>% 
#   select(pnp_q5score_m_q5, nona, pop_q5score_m_q5, 
#          tclcm2_cldnb, p_q5score_m_q5) %>% 
#   filter(tclcm2_cldnb == 1)

  # Les code ci-dessous vérifie bien que la moyenne pondérée
  # des pnp et pop est égale à p_score. 

# dat_nonaine %>% 
#   select(pnp_q5score_m_q5, nona,
#          pop_q5score_m_q5, tclcm2_cldnb, p_q5score_m_q5) %>% 
#   mutate(v = (tclcm2_cldnb - 1) * pop_q5score_m_q5 / 
#            (nona - 1) + (nona - tclcm2_cldnb) *
#            pnp_q5score_m_q5 / (nona - 1)) %>% 
#   mutate(v2 = v - p_q5score_m_q5) %>% pull(v2) %>% summary()

# dat_nonaine %>% 
#   select(pnpe_q5score_m_q5, nona,
#          pope_q5score_m_q5, tecm2_cldnb, p_q5score_m_q5) %>% 
#   mutate(v = (tecm2_cldnb - 1) * pope_q5score_m_q5 / 
#            (nona - 1) + (nona - tecm2_cldnb) *
#            pnpe_q5score_m_q5 / (nona - 1)) %>% 
#   mutate(v2 = v - p_q5score_m_q5) %>% pull(v2) %>% summary()

# pnp_qbscore_m_low ----------------------------------------------------------

  # sog : sum old group. 
dat_nonaine <- group_by(dat_nonaine, clcm2_cldnb) %>% 
  mutate(sog_qbscore_m_low = sum(qbscore_m_low)) %>% 
  ungroup

dat_nonaine <- group_by(dat_nonaine, ecm2_cldnb) %>% 
  mutate(soge_qbscore_m_low = sum(qbscore_m_low)) %>% 
  ungroup

  # pnp. 
dat_nonaine <- group_by(dat_nonaine, divconstatrneses) %>% 
  mutate(pnp_qbscore_m_low = 
           (sum(qbscore_m_low) - sog_qbscore_m_low) /
      (nona - tclcm2_cldnb),
    
    pnpe_qbscore_m_low = 
      (sum(qbscore_m_low) - soge_qbscore_m_low) /
      (nona - tecm2_cldnb)) %>% 
  ungroup %>% 
  (function (d) {
    d$pnp_qbscore_m_low[d$nona <= 1] <- NA
    d$pnpe_qbscore_m_low[d$nona <= 1] <- NA
    d$pnp_qbscore_m_low[d$nona == d$tclcm2_cldnb & 
                       d$nona > 1] <- 0
    d$pnpe_qbscore_m_low[d$nona == d$tclcm2_cldnb & 
                        d$nona > 1] <- 0
    d
  })
  # Pour les n() = 1 ou 2, les pnp sont infinies. 
  # On les remplace par des valeurs manquantes. 
  # 9 valeurs manquantes chez pnp_qbscore_m_low. 
  # 9 valeurs manquantes chez les pnpe_qbscore_m_low. 

  # pop. 
dat_nonaine <- group_by(dat_nonaine, divconstatrneses) %>% 
  mutate(
    pop_qbscore_m_low = 
           (sog_qbscore_m_low - qbscore_m_low) / 
               (tclcm2_cldnb - 1),
    
    pope_qbscore_m_low = 
      (soge_qbscore_m_low - qbscore_m_low) / 
      (tecm2_cldnb - 1)
    ) %>% 
  ungroup %>% 
  (function (d) {
    d$pop_qbscore_m_low[d$tclcm2_cldnb == 1] <- 0
    d$pope_qbscore_m_low[d$tecm2_cldnb == 1] <- 0
    d
  })
  # Pour les tclcm2_cldnb ou tecm2_cldnb = 1, 
  # pop = 0 (logique). 

  # Vérifications : 

  # Montre que pop = p si pnp = 0 : 

  # dat_nonaine %>% 
  #   select(pnp_qbscore_m_low, nona, pop_qbscore_m_low, 
  #          tclcm2_cldnb, p_qbscore_m_low) %>% 
  #   filter(pnp_qbscore_m_low == 0)

  # Montre que pnp = p si pop = 0, 
  # c'est le cas si tclcm2_cldnb = 1 : 

  # dat_nonaine %>% 
#   select(pnp_qbscore_m_low, nona, pop_qbscore_m_low, 
#          tclcm2_cldnb, p_qbscore_m_low) %>% 
#   filter(tclcm2_cldnb == 1)

  # Les code ci-dessous vérifie bien que la moyenne pondérée
  # des pnp et pop est égale à p_score. 

# dat_nonaine %>% 
#   select(pnp_qbscore_m_low, nona,
#          pop_qbscore_m_low, tclcm2_cldnb, p_qbscore_m_low) %>% 
#   mutate(v = (tclcm2_cldnb - 1) * pop_qbscore_m_low / 
#            (nona - 1) + (nona - tclcm2_cldnb) *
#            pnp_qbscore_m_low / (nona - 1)) %>% 
#   mutate(v2 = v - p_qbscore_m_low) %>% pull(v2) %>% summary()

# dat_nonaine %>% 
#   select(pnpe_qbscore_m_low, nona,
#          pope_qbscore_m_low, tecm2_cldnb, p_qbscore_m_low) %>% 
#   mutate(v = (tecm2_cldnb - 1) * pope_qbscore_m_low / 
#            (nona - 1) + (nona - tecm2_cldnb) *
#            pnpe_qbscore_m_low / (nona - 1)) %>% 
#   mutate(v2 = v - p_qbscore_m_low) %>% pull(v2) %>% summary()

# pnp_qbscore_m_middle ----------------------------------------------------------

  # sog : sum old group. 
dat_nonaine <- group_by(dat_nonaine, clcm2_cldnb) %>% 
  mutate(sog_qbscore_m_middle = sum(qbscore_m_middle)) %>% 
  ungroup

dat_nonaine <- group_by(dat_nonaine, ecm2_cldnb) %>% 
  mutate(soge_qbscore_m_middle = sum(qbscore_m_middle)) %>% 
  ungroup

  # pnp. 
dat_nonaine <- group_by(dat_nonaine, divconstatrneses) %>% 
  mutate(pnp_qbscore_m_middle = 
           (sum(qbscore_m_middle) - sog_qbscore_m_middle) /
      (nona - tclcm2_cldnb),
    
    pnpe_qbscore_m_middle = 
      (sum(qbscore_m_middle) - soge_qbscore_m_middle) /
      (nona - tecm2_cldnb)) %>% 
  ungroup %>% 
  (function (d) {
    d$pnp_qbscore_m_middle[d$nona <= 1] <- NA
    d$pnpe_qbscore_m_middle[d$nona <= 1] <- NA
    d$pnp_qbscore_m_middle[d$nona == d$tclcm2_cldnb & 
                       d$nona > 1] <- 0
    d$pnpe_qbscore_m_middle[d$nona == d$tclcm2_cldnb & 
                        d$nona > 1] <- 0
    d
  })
  # Pour les n() = 1 ou 2, les pnp sont infinies. 
  # On les remplace par des valeurs manquantes. 
  # 9 valeurs manquantes chez pnp_qbscore_m_middle. 
  # 9 valeurs manquantes chez les pnpe_qbscore_m_middle. 

  # pop. 
dat_nonaine <- group_by(dat_nonaine, divconstatrneses) %>% 
  mutate(
    pop_qbscore_m_middle = 
           (sog_qbscore_m_middle - qbscore_m_middle) / 
               (tclcm2_cldnb - 1),
    
    pope_qbscore_m_middle = 
      (soge_qbscore_m_middle - qbscore_m_middle) / 
      (tecm2_cldnb - 1)
    ) %>% 
  ungroup %>% 
  (function (d) {
    d$pop_qbscore_m_middle[d$tclcm2_cldnb == 1] <- 0
    d$pope_qbscore_m_middle[d$tecm2_cldnb == 1] <- 0
    d
  })
  # Pour les tclcm2_cldnb ou tecm2_cldnb = 1, 
  # pop = 0 (logique). 

  # Vérifications : 

  # Montre que pop = p si pnp = 0 : 

  # dat_nonaine %>% 
  #   select(pnp_qbscore_m_middle, nona, pop_qbscore_m_middle, 
  #          tclcm2_cldnb, p_qbscore_m_middle) %>% 
  #   filter(pnp_qbscore_m_middle == 0)

  # Montre que pnp = p si pop = 0, 
  # c'est le cas si tclcm2_cldnb = 1 : 

  # dat_nonaine %>% 
#   select(pnp_qbscore_m_middle, nona, pop_qbscore_m_middle, 
#          tclcm2_cldnb, p_qbscore_m_middle) %>% 
#   filter(tclcm2_cldnb == 1)

  # Les code ci-dessous vérifie bien que la moyenne pondérée
  # des pnp et pop est égale à p_score. 

# dat_nonaine %>% 
#   select(pnp_qbscore_m_middle, nona,
#          pop_qbscore_m_middle, tclcm2_cldnb, p_qbscore_m_middle) %>% 
#   mutate(v = (tclcm2_cldnb - 1) * pop_qbscore_m_middle / 
#            (nona - 1) + (nona - tclcm2_cldnb) *
#            pnp_qbscore_m_middle / (nona - 1)) %>% 
#   mutate(v2 = v - p_qbscore_m_middle) %>% pull(v2) %>% summary()

# dat_nonaine %>% 
#   select(pnpe_qbscore_m_middle, nona,
#          pope_qbscore_m_middle, tecm2_cldnb, p_qbscore_m_middle) %>% 
#   mutate(v = (tecm2_cldnb - 1) * pope_qbscore_m_middle / 
#            (nona - 1) + (nona - tecm2_cldnb) *
#            pnpe_qbscore_m_middle / (nona - 1)) %>% 
#   mutate(v2 = v - p_qbscore_m_middle) %>% pull(v2) %>% summary()

# pnp_qbscore_m_high ----------------------------------------------------------

  # sog : sum old group. 
dat_nonaine <- group_by(dat_nonaine, clcm2_cldnb) %>% 
  mutate(sog_qbscore_m_high = sum(qbscore_m_high)) %>% 
  ungroup

dat_nonaine <- group_by(dat_nonaine, ecm2_cldnb) %>% 
  mutate(soge_qbscore_m_high = sum(qbscore_m_high)) %>% 
  ungroup

  # pnp. 
dat_nonaine <- group_by(dat_nonaine, divconstatrneses) %>% 
  mutate(pnp_qbscore_m_high = 
           (sum(qbscore_m_high) - sog_qbscore_m_high) /
      (nona - tclcm2_cldnb),
    
    pnpe_qbscore_m_high = 
      (sum(qbscore_m_high) - soge_qbscore_m_high) /
      (nona - tecm2_cldnb)) %>% 
  ungroup %>% 
  (function (d) {
    d$pnp_qbscore_m_high[d$nona <= 1] <- NA
    d$pnpe_qbscore_m_high[d$nona <= 1] <- NA
    d$pnp_qbscore_m_high[d$nona == d$tclcm2_cldnb & 
                       d$nona > 1] <- 0
    d$pnpe_qbscore_m_high[d$nona == d$tclcm2_cldnb & 
                        d$nona > 1] <- 0
    d
  })
  # Pour les n() = 1 ou 2, les pnp sont infinies. 
  # On les remplace par des valeurs manquantes. 
  # 9 valeurs manquantes chez pnp_qbscore_m_high. 
  # 9 valeurs manquantes chez les pnpe_qbscore_m_high. 

  # pop. 
dat_nonaine <- group_by(dat_nonaine, divconstatrneses) %>% 
  mutate(
    pop_qbscore_m_high = 
           (sog_qbscore_m_high - qbscore_m_high) / 
               (tclcm2_cldnb - 1),
    
    pope_qbscore_m_high = 
      (soge_qbscore_m_high - qbscore_m_high) / 
      (tecm2_cldnb - 1)
    ) %>% 
  ungroup %>% 
  (function (d) {
    d$pop_qbscore_m_high[d$tclcm2_cldnb == 1] <- 0
    d$pope_qbscore_m_high[d$tecm2_cldnb == 1] <- 0
    d
  })
  # Pour les tclcm2_cldnb ou tecm2_cldnb = 1, 
  # pop = 0 (logique). 

  # Vérifications : 

  # Montre que pop = p si pnp = 0 : 

  # dat_nonaine %>% 
  #   select(pnp_qbscore_m_high, nona, pop_qbscore_m_high, 
  #          tclcm2_cldnb, p_qbscore_m_high) %>% 
  #   filter(pnp_qbscore_m_high == 0)

  # Montre que pnp = p si pop = 0, 
  # c'est le cas si tclcm2_cldnb = 1 : 

  # dat_nonaine %>% 
#   select(pnp_qbscore_m_high, nona, pop_qbscore_m_high, 
#          tclcm2_cldnb, p_qbscore_m_high) %>% 
#   filter(tclcm2_cldnb == 1)

  # Les code ci-dessous vérifie bien que la moyenne pondérée
  # des pnp et pop est égale à p_score. 

# dat_nonaine %>% 
#   select(pnp_qbscore_m_high, nona,
#          pop_qbscore_m_high, tclcm2_cldnb, p_qbscore_m_high) %>% 
#   mutate(v = (tclcm2_cldnb - 1) * pop_qbscore_m_high / 
#            (nona - 1) + (nona - tclcm2_cldnb) *
#            pnp_qbscore_m_high / (nona - 1)) %>% 
#   mutate(v2 = v - p_qbscore_m_high) %>% pull(v2) %>% summary()

# dat_nonaine %>% 
#   select(pnpe_qbscore_m_high, nona,
#          pope_qbscore_m_high, tecm2_cldnb, p_qbscore_m_high) %>% 
#   mutate(v = (tecm2_cldnb - 1) * pope_qbscore_m_high / 
#            (nona - 1) + (nona - tecm2_cldnb) *
#            pnpe_qbscore_m_high / (nona - 1)) %>% 
#   mutate(v2 = v - p_qbscore_m_high) %>% pull(v2) %>% summary()