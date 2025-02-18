# pedepvars. 
# Synthétise les variables dépendantes. 
# À sourcer. 


pedepvars <- c("moy_ec1", "moy_cc",
                    "moy_fran_ec", "moy_fran_cc",
                    "moy_maths_ec", "moy_maths_cc",
                    "moy_hist_ec", 
                    "moy_dic_ec",
                    "moy_red_ec") %>% 
  (function (x) c(x,
                  paste(x, "_norm", sep = ""),
                  paste(x, "_rp", sep = "")))