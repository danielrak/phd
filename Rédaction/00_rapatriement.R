# Modifie les parts de texte nécessaires. 

# Packages ----------------------------------------------------------------

library(here)
library(tidyverse)

# age ---------------------------------------------------------------------

age <- readLines("D:/00_phd/02_rectorat/04_resultats/41_age/WP/wp_age_sept2021_def2.Rmd")
age2 <- str_replace(age, "#", "##") %>% str_replace("#eq", "eq")
writeLines(age2, here("investigations", "02_pdf_book",
                      "chap1_age.Rmd"))

age_appendix <- 
  readLines("D:/00_phd/02_rectorat/04_resultats/41_age/WP/appendix_age_def2.Rmd")
age_appendix2 <- str_replace(age_appendix, "#", "##") %>% str_replace("#eq", "eq")
writeLines(age_appendix2, here("investigations", "02_pdf_book", 
                               "chap1_age_appendix.Rmd")) # Un warning. 

# pe ----------------------------------------------------------------------

pe <- 
  readLines("D:/00_phd/02_rectorat/04_resultats/42_pe/WP/wp_pe_juin2021_def.Rmd")
pe2 <- str_replace(pe, "#", "##") %>% str_replace("#eq", "eq")
writeLines(pe2, here("investigations", "02_pdf_book", "chap2_pe.Rmd"))

pe_appendix <- 
  readLines("D:/00_phd/02_rectorat/04_resultats/42_pe/WP/appendix_pe_def.Rmd")
pe_appendix2 <- str_replace(pe_appendix, "#", "##") %>% str_replace("#eq", "eq")
writeLines(pe_appendix2, here("investigations", "02_pdf_book",
                              "chap2_pe_appendix.Rmd"))

# g20 ---------------------------------------------------------------------

g20 <- readLines("D:/00_phd/03_g20/02_resultats/WP/wp_g20_mars2022_def3.Rmd")
g202 <- str_replace(g20, "#", "##") %>% str_replace("#eq", "eq")  
writeLines(g202, here("investigations", "02_pdf_book", "chap3_g20.Rmd"))

g20_appendix <- 
  readLines("D:/00_phd/03_g20/02_resultats/WP/appendix_g20_def3.Rmd")
g20_appendix2 <- str_replace(g20_appendix, "#", "##") %>% str_replace("#eq", "eq")
writeLines(g20_appendix2, here("investigations", "02_pdf_book",
                               "chap3_g20_appendix.Rmd"))

