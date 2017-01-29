


library("tidyverse")
library("haven")
library("broom")
library("modelr")
options(tibble.print_min = 100)


library(AzureML)
ws <- workspace()
daten <- tbl_df(download.datasets(
  dataset = ws, 
  name    = "8Netzwerk_Bleiben.csv"))

glm(Bleiben ~ Freunde, data = daten, family = binomial(link = "logit"), trace=T)
fit <- glm(Bleiben ~ Freunde, data = daten, family = binomial(link = "logit"), trace=T)

summary(fit)

tidy(fit)
glance(fit)

daten <- augment(fit, daten)

#########################################

fit2 <- glm(Bleiben ~ factor(Freunde), data = daten, family = binomial(link = "logit"), trace=T)

tidy(fit2)

#########################################

daten <- daten %>% arrange(Freunde)

model_matrix(daten, Bleiben ~ factor(Freunde))

model_matrix(daten, Bleiben ~ factor(Freunde)) %>% summarise_all(funs(sum)) %>% glimpse


#########################################
library("lme4")

Data <- tbl_df(download.datasets(
  dataset = ws, 
  name    = "BeetlesMale.csv"))
# Fit null model without fixed effects (but including all random effects)

m0 <- glmer(Colour ~ 1 + (1 | Population) + (1 | Container),
            family = "binomial", data = Data)
summary(m0)



#########################################
library("R2MLwiN")
options(MLwiN_path = "C:/Program Files (x86)/MLwiN trial/i386/")
data(bang)

(mymodel1 <- runMLwiN(logit(use) ~ 1 + lc, D = "Binomial", data = bang))



#########################################



rabash <- tribble(
  ~Score, ~School, ~ID,
  3,       10,       1,
  2,       10,       2,
 -1,       20,       3, 
 -4,       20,       4
)


model_matrix(rabash, Score ~ 1 + (1 | School))

eee <- lmer(Score ~ 1 + (1 | School), data=rabash)

eee

summary(eee)

augment(eee)




VarCompModel <- runMLwiN(Formula = Score ~ 1 + (1 | School) + (1 | ID), data = rabash)

slotNames(VarCompModel)

print(VPC <- VarCompModel["RP"][["RP2_var_Intercept"]]/(VarCompModel["RP"][["RP1_var_Intercept"]] + 
                                                          VarCompModel["RP"][["RP2_var_Intercept"]]))



#########################################
data("tutorial")

ssq1 <- function(x) {
  
  (x - mean(x))^2
  
}

ttt <- tutorial %>% group_by(school) %>% 
  dplyr::summarise(ssq1=sum(ssq1(normexam)), 
                   n=n(), 
                   ssq1/n, 
                   var=var(normexam), 
                   ssq1/(n-1), 
                   sqrt(ssq1/(n-1)),
                   sd=sd(normexam)) %>% ungroup()

mean(ttt$var)

F1 <- normexam ~ 1 + (1 | school) + (1 | student)

VarCompResid <- runMLwiN(Formula = F1, data = tutorial, estoptions = list(resi.store = TRUE))

VarCompModel["RP"][["RP1_var_Intercept"]]
VarCompModel["RP"][["RP2_var_Intercept"]]

print(VPC <- VarCompModel["RP"][["RP2_var_Intercept"]]/(VarCompModel["RP"][["RP1_var_Intercept"]] + 
                                                          VarCompModel["RP"][["RP2_var_Intercept"]]))

VarCompResid <- runMLwiN(F1, data = tutorial, estoptions = list(resi.store = TRUE))

residuals <- VarCompResid@residual$lev_2_resi_est_Intercept
residualsCI <- 1.96 * sqrt(VarCompResid@residual$lev_2_resi_var_Intercept)
residualsRank <- rank(residuals)
rankno <- order(residualsRank)

caterpillar(y = residuals[rankno], x = 1:65, qtlow = (residuals - residualsCI)[rankno], 
            qtup = (residuals + residualsCI)[rankno], xlab = "Rank", ylab = "Intercept")

#########################################











