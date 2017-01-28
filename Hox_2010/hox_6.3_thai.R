


# 6.3 EXAMPLE: ANALYZING DICHOTOMOUS DATA

library("haven")
library("lme4")
library("broom")
library("modelr")
library("R2MLwiN")
library("tidyverse")
library("AzureML")

options(MLwiN_path = "C:/Program Files (x86)/MLwiN trial/i386/")

# Meth = 0 -> RIGLIS
# Meth = 1 -> IGLIS

# nonlinear = c(N = 0 , M = 1) MQL1
# nonlinear = c(N = 0 , M = 2) MQL2

# nonlinear = c(N = 1 , M = 1) PQL1
# nonlinear = c(N = 1 , M = 2) PQL2

iglis_mql1  <- list(debugmode=F, drop.levels=T, EstM = 0, Meth = 1, nonlinear = c(N = 0 , M = 1)) 
iglis_mql2  <- list(debugmode=F, drop.levels=T, EstM = 0, Meth = 1, nonlinear = c(N = 0 , M = 2)) 
iglis_pql1  <- list(debugmode=F, drop.levels=T, EstM = 0, Meth = 1, nonlinear = c(N = 1 , M = 1)) 
iglis_pql2  <- list(debugmode=F, drop.levels=T, EstM = 0, Meth = 1, nonlinear = c(N = 1 , M = 2)) 

riglis_mql1 <- list(debugmode=F, drop.levels=T, EstM = 0, Meth = 0, nonlinear = c(N = 0 , M = 1)) 
riglis_mql2 <- list(debugmode=F, drop.levels=T, EstM = 0, Meth = 0, nonlinear = c(N = 0 , M = 2)) 
riglis_pql1 <- list(debugmode=F, drop.levels=T, EstM = 0, Meth = 0, nonlinear = c(N = 1 , M = 1)) 
riglis_pql2 <- list(debugmode=F, drop.levels=T, EstM = 0, Meth = 0, nonlinear = c(N = 1 , M = 2)) 

################################################################################################

# Daten
ws    <- workspace()
daten <- download.datasets(
  dataset = ws, 
  name    = "UTHAI1.csv")

daten <- drop_na(daten) %>% 
  mutate(schueler = row_number())


# Modell
fo <- logit(rep1) ~ 1 + male + pped + msesc + (1 | schoolid)


# Ausführung mit den gespeicherten Parameter
IGLIS_MQL1   <- runMLwiN(fo, D = "Binomial", data = daten, estoptions = iglis_mql1) 
RIGLIS_MQL1  <- runMLwiN(fo, D = "Binomial", data = daten, estoptions = riglis_mql1)

IGLIS_PQL2   <- runMLwiN(fo, D = "Binomial", data = daten, estoptions = iglis_pql2)
RIGLIS_PQL2  <- runMLwiN(fo, D = "Binomial", data = daten, estoptions = riglis_pql2)


# Ausführung mit MCMC
mcmc_einst <- list(debugmode=F, drop.levels=T, EstM = 1)
mcmc_mod   <- runMLwiN(fo, D = "Binomial", data = daten, estoptions = mcmc_einst)



#############################################################

# lme4

fo <- rep1 ~ 1 + male + pped + msesc + (1 | schoolid)

a <- glmer(fo, family="binomial", data=daten)

summary(a)

#############################################################






