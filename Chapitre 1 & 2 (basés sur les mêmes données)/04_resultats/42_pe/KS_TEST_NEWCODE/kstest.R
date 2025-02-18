# ks test, thèse de brodaty, échantillon d'un bien meilleur code. 
load("01_donnees.rda")
d <- select(dat, rneconstatses, divconstatrneses, score_ori) %>% na.omit
split(d, d$rneconstatses) %>% head %>% 
  lapply(function (x) {
    m_obs = tapply(x$score_ori, x$divconstatrneses, mean)
    m_sim <- lapply(1:100, function (y) {
      set.seed(y)
      tapply(x$score_ori, sample(x$divconstatrneses), mean)
    }) %>% do.call(what = rbind)
    rbind(m_obs, m_sim) %>% 
      (function (r) ks.test(r[1, ], as.vector(r[2:101, ]))$p.value)
  }) %>% unlist
  # ok : lorsque paramétré comme dans kstester : donne exactement les mêmes résultats. 
  # sauf que ça va 10 fois plus vite !! 

# ks test, version brodaty et gurgand 2016 ou encore carell et west 2010 ou les deux livres que ces derniers ont mentionné. 
# Ces deux livres sont : resampling methds de Good et testing statistical hypothesis de Lehmann et Romano. 
split(d, d$rneconstatses) %>% 
  lapply(function (x) {
    m_obs = tapply(x$score_ori, x$divconstatrneses, mean)
    m_sim <- lapply(1:1000, function (y) {
      set.seed(y)
      tapply(x$score_ori, sample(x$divconstatrneses), mean)
    }) %>% do.call(what = rbind)
    rbind <- rbind(m_obs, m_sim)
    emp_pval <- apply(rbind, 2, function (x) sum(x[2:1001] < x[1]) / 1000)
    ks.test(emp_pval, "punif")$p.value
  }) %>% unlist -> z

