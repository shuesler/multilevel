
# *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*

imput <- function(x) {
  dplyr::coalesce(x,
                  dplyr::lag(x,1),
                  dplyr::lag(x,2),
                  dplyr::lag(x,3),
                  dplyr::lag(x,4),
                  dplyr::lag(x,5),
                  dplyr::lag(x,6),
                  dplyr::lag(x,7),
                  dplyr::lag(x,8),
                  dplyr::lag(x,9),
                  dplyr::lag(x,10),
                  dplyr::lag(x,11))
}

# *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*

library("pdftools")
library("pdfsearch")


file <- system.file('pdf', '1501.00450.pdf', package = 'pdfsearch')

result <- keyword_search(file,
                         ignore_case = TRUE,
                         keyword     = c(
                           'repeated measures', 
                           'mixed effects'),
                         path        = TRUE, 
                         surround_lines = 1)
head(result)

# *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*

# ichtige Funktion zur "Listenbenennung"
naming_lists <- function(oldname, newname) {
  
  
  as.lazy_dots(set_names(list(oldname), newname), env = globalenv())
  
  
}

# *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*

# Merge

setwd("quellenordner")

d <- dir()

sink("zielfile.R")

for (i in 1:length(d)) {
  
  xxx <- readLines(dir()[i])
  
  purrr::map(xxx, cat, sep="\n", append=TRUE)
  
  cat("\n")
  cat("# +*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+", sep="\n")
  cat("# +*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+", sep="\n")
  cat("# +*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+", sep="\n")
  cat("# +*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+", sep="\n")
  cat("# +*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+", sep="\n")
  cat("\n")
  cat("\n")
  
}

sink()

# *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*

# Neu in dplyr_dev (5.9) 

ti %>% mutate(ti2=
                case_when(
                  x==y ~ 1L,
                  TRUE ~ x
                ))


iris %>% 
  mutate(Petal.Width = 
                  na_if(
                    Petal.Width,rep(0.2, length(Petal.Width))))


# *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*

# Extraktion Pdf-Inhaltsverzeichnis

toc <- pdftools::pdf_toc("lav.pdf")

x   <- unname(unlist(toc))

cat(x, sep="\n", file = "toc.txt")


# *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*

















