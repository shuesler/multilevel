
# 6.4 EXAMPLE: ANALYZING PROPORTIONS

iglis_ml1  <- list(debugmode=F, drop.levels=T, EstM = 0, Meth = 1, nonlinear = c(N = 0 , M = 1)) # iglis_ml1
iglis_ml2  <- list(debugmode=F, drop.levels=T, EstM = 0, Meth = 1, nonlinear = c(N = 0 , M = 2)) # iglis_ml2
iglis_ql1  <- list(debugmode=F, drop.levels=T, EstM = 0, Meth = 1, nonlinear = c(N = 1 , M = 1)) # iglis_ql1
iglis_ql2  <- list(debugmode=F, drop.levels=T, EstM = 0, Meth = 1, nonlinear = c(N = 1 , M = 2)) # iglis_ql2

riglis_ml1 <- list(debugmode=F, drop.levels=T, EstM = 0, Meth = 0, nonlinear = c(N = 0 , M = 1)) # riglis_ml1
riglis_ml2 <- list(debugmode=F, drop.levels=T, EstM = 0, Meth = 0, nonlinear = c(N = 0 , M = 2)) # riglis_ml2
riglis_ql1 <- list(debugmode=F, drop.levels=T, EstM = 0, Meth = 0, nonlinear = c(N = 1 , M = 1)) # riglis_ql1
riglis_ql2 <- list(debugmode=F, drop.levels=T, EstM = 0, Meth = 0, nonlinear = c(N = 1 , M = 2)) # riglis_ql2


library("tidyverse")
library("R2MLwiN")
library("doBy")
library("haven")
library("AzureML")
options(tibble.print_min = 100)
options(MLwiN_path = "C:/Program Files (x86)/MLwiN trial/i386/")


ws    <- workspace()
daten <- download.datasets(
  dataset = ws, 
  name    = "MetaResp.csv")
daten <- tbl_df(daten)

daten <- daten %>% mutate(year_zent = year - mean(year)) 



m1_f <- logit(response, denom) ~ 1 + respisrr + (1 | source)
m2_f <- logit(response, denom) ~ 1 + respisrr + teldum + maildum + (1 | source)
m3_f <- logit(response, denom) ~ 1 + respisrr + teldum + maildum + (1 + teldum + maildum | source)
m4_f <- logit(response, denom) ~ 1 + 
  respisrr + teldum*year_zent + maildum*year_zent + year_zent + saliency + (1 + teldum + maildum | source)


# RIGLS nehmen!!
(m1 <- runMLwiN(m1_f, D = "Binomial", data = daten, estoptions = riglis_ql2))
(m2 <- runMLwiN(m2_f, D = "Binomial", data = daten, estoptions = riglis_ql2))
(m3 <- runMLwiN(m3_f, D = "Binomial", data = daten, estoptions = riglis_ql2))
(m4 <- runMLwiN(m4_f, D = "Binomial", data = daten, estoptions = riglis_ql2))


lb <- function(x) {
  exp(x)/(1+exp(x))
}



# -------------------------------
co_l <- list(
coef(m4)["FP_Intercept"],
coef(m4)["FP_respisrr"],
coef(m4)["FP_teldum"],
coef(m4)["FP_maildum"],
coef(m4)["FP_year_zent"],
coef(m4)["FP_saliency"],
coef(m4)["FP_teldum:year_zent"],
coef(m4)["FP_year_zent:maildum"],
coef(m4)["RP2_var_Intercept"],
coef(m4)["RP2_var_maildum" ],
coef(m4)["RP2_var_teldum"])
# -------------------------------


# map_dbl(co, as.numeric)
# map_chr(co, names)
# m4["RP"][["RP2_var_Intercept"]]
# m4["FP"][["FP_Intercept"]]
# lmtest::lrtest(m4)
# vcov(m4)
# m4@xlevels


co_df <- data_frame(
  one = map_chr(co_l, names),
  two = map_dbl(co_l, as.numeric))


##############################################################
# Betrachtungen zur Zeitvariable

tmp <- daten %>% 
  dplyr::select(year, year_zent) %>% 
  mutate(year2       = as.numeric(year),
         year_zent2  = as.numeric(year_zent),
         jahr        = year2+1947)


tmp    <- tmp %>% distinct(year_zent2, jahr) %>% arrange(year_zent2)
y      <- tmp$year_zent2
jahr   <- tmp$jahr
##############################################################

# Empirie-Werte: f2f
# -------------

tel      <- rep(0, 27)
mail     <- rep(0, 27)
saliency <- rep(1, 27)

f2f <- data_frame(
  inter                 = 1,
  respisrr              = 1,
  year                  = y,
  tel                   = tel,
  mail                  = mail,
  saliency              = saliency,
  FP_teldumXXyear_zent  = tel*y,
  FP_year_zentXXmaildum = mail*y
)


f2f <- f2f %>% mutate( 
    inter.c                 = inter                  * coef(m4)["FP_Intercept"],
    respisrr.c              = respisrr               * coef(m4)["FP_respisrr"],
    tel.c                   = tel                    * coef(m4)["FP_teldum"],                 
    mail.c                  = mail                   * coef(m4)["FP_maildum"],                  
    year.c                  = year                   * coef(m4)["FP_year_zent"],                 
    saliency.c              = saliency               * coef(m4)["FP_saliency"],             
    FP_teldumXXyear_zent.c  = FP_teldumXXyear_zent   * coef(m4)["FP_teldum:year_zent"], 
    FP_year_zentXXmaildum.c = FP_year_zentXXmaildum  * coef(m4)["FP_year_zent:maildum"]
)


f2f_b <- f2f %>% 
  dplyr::select(ends_with(".c")) %>% 
  mutate(logit = rowSums(.),
         proba = lb(logit))



##############################################################

# Empirie-Werte: tel_mode
# -------------

tel      <- rep(1, 27)
mail     <- rep(0, 27)
saliency <- rep(1, 27)

tel_mode <- data_frame(
  inter                 = 1,
  respisrr              = 1,
  year                  = y,
  tel                   = tel,
  mail                  = mail,
  saliency              = saliency,
  FP_teldumXXyear_zent  = tel*y,
  FP_year_zentXXmaildum = mail*y
)


tel_mode <- tel_mode %>% mutate( 
  inter.c                 = inter                  * coef(m4)["FP_Intercept"],
  respisrr.c              = respisrr               * coef(m4)["FP_respisrr"],
  tel.c                   = tel                    * coef(m4)["FP_teldum"],                 
  mail.c                  = mail                   * coef(m4)["FP_maildum"],                  
  year.c                  = year                   * coef(m4)["FP_year_zent"],                 
  saliency.c              = saliency               * coef(m4)["FP_saliency"],             
  FP_teldumXXyear_zent.c  = FP_teldumXXyear_zent   * coef(m4)["FP_teldum:year_zent"], 
  FP_year_zentXXmaildum.c = FP_year_zentXXmaildum  * coef(m4)["FP_year_zent:maildum"]
)


tel_mode_b <- tel_mode %>% 
  dplyr::select(ends_with(".c")) %>% 
  mutate(logit = rowSums(.),
         proba = lb(logit))

##############################################################

# Empirie-Werte: mail_mode
# -------------

tel      <- rep(0, 27)
mail     <- rep(1, 27)
saliency <- rep(1, 27)

mail_mode <- data_frame(
  inter                 = 1,
  respisrr              = 1,
  year                  = y,
  tel                   = tel,
  mail                  = mail,
  saliency              = saliency,
  FP_teldumXXyear_zent  = tel*y,
  FP_year_zentXXmaildum = mail*y
)


mail_mode <- mail_mode %>% mutate( 
  inter.c                 = inter                  * coef(m4)["FP_Intercept"],
  respisrr.c              = respisrr               * coef(m4)["FP_respisrr"],
  tel.c                   = tel                    * coef(m4)["FP_teldum"],                 
  mail.c                  = mail                   * coef(m4)["FP_maildum"],                  
  year.c                  = year                   * coef(m4)["FP_year_zent"],                 
  saliency.c              = saliency               * coef(m4)["FP_saliency"],             
  FP_teldumXXyear_zent.c  = FP_teldumXXyear_zent   * coef(m4)["FP_teldum:year_zent"], 
  FP_year_zentXXmaildum.c = FP_year_zentXXmaildum  * coef(m4)["FP_year_zent:maildum"]
)


mail_mode_b <- mail_mode %>% 
  dplyr::select(ends_with(".c")) %>% 
  mutate(logit = rowSums(.),
         proba = lb(logit))

##############################################################

schluss_df <- data_frame(
  Jahr           = jahr,
  F2F            = f2f_b$proba,
  telefonisch    = tel_mode_b$proba,
  postal         = mail_mode_b$proba
)

##############################################################


schluss_df2 <- gather(schluss_df, Methode, Antwortrate, F2F:postal) %>% 
  arrange(Jahr, Methode)

ggplot(schluss_df2, aes(x=Jahr, y=Antwortrate, colour=Methode)) +
  geom_line() 

##############################################################

