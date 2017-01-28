

library("tidyverse")
library("stringr")
library("gtools")
library("lazyeval")
options(tibble.print_min = 100)
library(AzureML)



# Funktionen 
# *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*

.tab <- function (x, ..., wt = NULL, sort = FALSE) 

{
    vars <- lazyeval::lazy_dots(...)
    wt   <- substitute(wt)
    count_(x, vars, wt, sort = sort) %>% 
        mutate(Percent = round(         100*prop.table(n),  2), 
               Cum.    = round( cumsum( 100*prop.table(n)), 2)) %>% 
        rename(Freq = n)  
}


# *+*+*+*+*+*+*+*+*+*+*


.helper <- function(anzahl_wellen) {
  .tmp1 <- gtools::permutations(2, 
                       anzahl_wellen, 
                       c(0,1), 
                       repeats = TRUE) %>% 
    t %>% 
    tbl_df %>% 
    purrr::map(purrr::as_vector)
  
  .tmp2 <- .tmp1 %>% map(`*`, 1:6) %>% 
    map(str_c, collapse="") %>% 
    map(str_replace_all, "0", "")
  
  .tmp3 <- .tmp1 %>% map(str_c, collapse="")
  
  lookup <- tibble(Teiln=unname(unlist(.tmp2)), Muster=unname(unlist(.tmp3)))

  lookup
}


# *+*+*+*+*+*+*+*+*+*+*


.muster <- function(.data, .wave, ...) {
  .wave      <- lazyeval::lazy(.wave)
  
  .wave_char <- as.character(.wave)[1]
  
  .gruppvar  <- lazyeval::lazy_dots(...)
  
  .tmp4 <- .data %>% select_(.dots = .wave) %>% 
    distinct() %>% 
    arrange() %>% mutate(.counter = row_number())
   
   anzahl_wellen <- nrow(.tmp4)
   
  .tmp5 <- left_join(.data, .tmp4, by =.wave_char)
  
  .tmp6 <- .tmp5 %>% group_by_(.dots = .gruppvar) %>% 
    mutate(Teiln = stringr::str_c(.counter, collapse=""))
   
  .lookup <- .helper(anzahl_wellen)
  
   left_join(.tmp6, .lookup, by="Teiln") %>% ungroup %>% filter(occ==1) %>%  
     .tab(Muster, sort=T)
}

# *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*


# Daten
# *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*

ws <- workspace()

daten <- download.datasets(
  dataset = ws, 
  name    = "stata15.1.csv")

daten <- tbl_df(daten)


daten2 <- daten %>% 
  gather(key, value, -id, -female, -grade) %>% 
  arrange(id)

daten3 <- daten2 %>% 
  separate(key, c("tmp", "occ"), sep = 3, remove = TRUE)

daten4 <- daten3 %>% spread(tmp, value) %>% drop_na()


# *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*


# Funktionsanwendung
# *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*

daten4 %>% .muster(occ, id)









