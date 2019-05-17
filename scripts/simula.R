##======================================================================
## Simulações para modelos de ANOVA
##======================================================================

##======================================================================
## Modelos com um fator
##======================================================================

## Seja o modelo
## y_ij = \mu + \tau_i + e_ij
## i = 1, ..., t tratamentos
## j = 1, ..., r repetições
## e_ij ~ N(0, \sigma^2)
## Dessa forma,
## Y_ij ~ N(\mu, \sigma^2)

##----------------------------------------------------------------------
## Especificação dos componentes do modelo
## Considerando t = 2 e r = 1000
t <- 2
r <- 1000
## Média geral \mu
mu <- 50
## Efeito do primeiro tratamento \tau_1
tau1 <- 10
## Efeito do segundo tratamento \tau_2
tau2 <- -15
## Variância comum \sigma^2
sigma2 <- 12
## Valores de Y para o tratamento 1
set.seed(11)
tratA <- rnorm(r, mu + tau1, sqrt(sigma2))
mean(tratA); var(tratA)
## Valores de Y para o tratamento 2
tratB <- rnorm(r, mu + tau2, sqrt(sigma2))
mean(tratB); var(tratB)
## Média geral
mean(c(tratA, tratB))

## Monta a base de dados
da1 <- data.frame(Trat = rep(c("A", "B"), each = r),
                  Resp = c(tratA, tratB))
str(da1)
summary(da1)

## Gráficos
boxplot(Resp  ~ Trat, data = da1)
abline(h = mean(da1$Resp), col = 2, lty = 2)

## Ajuste dos modelos
## ANOVA usando aov()
m1aov <- aov(Resp ~ Trat, data = da1)
summary(m1aov)
## ANOVA usando lm() - contr.treatment
m1lm1 <- lm(Resp ~ Trat, data = da1,
            contrasts = list(Trat = contr.treatment))
anova(m1lm1)
## ANOVA usando lm() - contr.sum
m1lm2 <- lm(Resp ~ Trat, data = da1,
            contrasts = list(Trat = contr.sum))
anova(m1lm2)

##----------------------------------------------------------------------
## Calculo dos efeitos

## contraste padrão ----------------------------------------------------
summary(m1lm1)
mean(da1$Resp)
with(da1, tapply(Resp, Trat, mean))
with(da1, diff(tapply(Resp, Trat, mean)))
## É a média do segundo nível menos a média do primeiro nível
## Fazendo na mão
mean(da1$Resp[da1$Trat == "A"])
mean(da1$Resp[da1$Trat == "B"]) - mean(da1$Resp[da1$Trat == "A"])
mean(da1$Resp[da1$Trat == "A"]) + coef(m1lm1)[2]
## Portanto, aqui estima-se o \tau_2, pois assume-se que o \tau_1 é
## zero.

## contraste soma zero -------------------------------------------------
summary(m1lm2)
mean(da1$Resp)
with(da1, tapply(Resp, Trat, mean))
mean(da1$Resp) + coef(m1lm2)[2] # média + tau1
mean(da1$Resp) - coef(m1lm2)[2] # média - tau1
mean(da1$Resp[da1$Trat == "A"]) - mean(da1$Resp)
## É a media do primeiro menos a média geral, porque:
## \mu_1 = \mu + \tau_1  =>  \tau_1 = \mu_1 - \mu
## \mu_2 = \mu - \tau_1  =>  \tau_1 = \mu - \mu_2
mean(da1$Resp) - mean(da1$Resp[da1$Trat == "B"])
## Portanto aqui estima-se o \tau_1 apenas, pois assume-se que o \tau_2
## é o negativo do \tau_1
with(da1, tapply(Resp, Trat, mean)) - mean(da1$Resp)
model.tables(m1aov, type = "means")
model.tables(m1aov, type = "effects")
## Fazendo na mão
mean(da1$Resp)
X <- model.matrix(m1lm2)
t(X) %*% X
MASS::fractions(solve(t(X) %*% X))
solve(t(X) %*% X) %*% t(X) %*% da1$Resp
## Média geral
sum(da1$Resp)/2000
## tau1
sum(da1$Resp[da1$Trat == "A"])/2000 -
    sum(da1$Resp[da1$Trat == "B"])/2000
## tau2 = -tau1
sum(da1$Resp[da1$Trat == "B"])/2000 -
    sum(da1$Resp[da1$Trat == "A"])/2000

## O que são os efeitos então?
model.tables(m1aov, type = "effects")
#library(dae)
yates.effects(m1aov, data = da1) # é o tau2 do contr.treatment!!
yates.effects(m1aov, data = da1)/2

stop
##----------------------------------------------------------------------
## Especificação dos componentes do modelo
## Considerando t = 3 e r = 100
t <- 3
r <- 1000
## Média geral \mu
mu <- 50
## Efeito do primeiro tratamento \tau_1
tau1 <- 10
## Efeito do segundo tratamento \tau_2
tau2 <- -15
## Efeito do terceiro tratamento \tau_3
tau3 <- 5
## Variância comum \sigma^2
sigma2 <- 12
## Valores de Y para o tratamento 1
set.seed(11)
tratA <- rnorm(r, mu + tau1, sqrt(sigma2))
mean(tratA); var(tratA)
## Valores de Y para o tratamento 2
tratB <- rnorm(r, mu + tau2, sqrt(sigma2))
mean(tratB); var(tratB)
## Valores de Y para o tratamento 3
tratC <- rnorm(r, mu + tau3, sqrt(sigma2))
mean(tratC); var(tratC)

## Monta a base de dados
da2 <- data.frame(Trat = rep(c("A", "B", "C"), each = r),
                  Resp = c(tratA, tratB, tratC))
str(da2)
summary(da2)

## Gráficos
boxplot(Resp  ~ Trat, data = da2)
abline(h = mean(da2$Resp), col = 2, lty = 2)

## Ajuste dos modelos
## ANOVA usando aov()
m2aov <- aov(Resp ~ Trat, data = da2)
summary(m2aov)
## ANOVA usando lm() - contr.treatment
m2lm1 <- lm(Resp ~ Trat, data = da2,
            contrasts = list(Trat = contr.treatment))
anova(m2lm1)
## ANOVA usando lm() - contr.sum
m2lm2 <- lm(Resp ~ Trat, data = da2,
            contrasts = list(Trat = contr.sum))
anova(m2lm2)

##----------------------------------------------------------------------
## Calculo dos efeitos

## contraste padrão ----------------------------------------------------
summary(m2lm1)
mean(da2$Resp)
with(da2, tapply(Resp, Trat, mean))
with(da2, diff(tapply(Resp, Trat, mean)))
with(da2, diff(tapply(Resp, Trat, mean), lag = 2))
## É a média do i-ésimo nível menos a média do primeiro nível
## Fazendo na mão
mean(da2$Resp[da2$Trat == "A"])
mean(da2$Resp[da2$Trat == "B"]) - mean(da2$Resp[da2$Trat == "A"])
mean(da2$Resp[da2$Trat == "C"]) - mean(da2$Resp[da2$Trat == "A"])
mean(da2$Resp[da2$Trat == "A"]) + coef(m2lm1)[2]
mean(da2$Resp[da2$Trat == "A"]) + coef(m2lm1)[3]
## Portanto, aqui estima-se o \tau_2 e \tau_3, pois assume-se que o
## \tau_1 é zero.

## contraste soma zero -------------------------------------------------
summary(m2lm2)
mean(da2$Resp)
with(da2, tapply(Resp, Trat, mean))
mean(da2$Resp) + coef(m2lm2)[2] # média + tau1
mean(da2$Resp) + coef(m2lm2)[3] # média + tau2
mean(da2$Resp) - coef(m2lm2)[2] - coef(m2lm2)[3]  # média - tau1 - tau2
mean(da2$Resp[da2$Trat == "A"]) - mean(da2$Resp)
mean(da2$Resp[da2$Trat == "B"]) - mean(da2$Resp)
## É a media do i-ésimo menos a média geral, porque:
## \mu_1 = \mu + \tau_1  =>  \tau_1 = \mu_1 - \mu
## \mu_2 = \mu + \tau_2  =>  \tau_2 = \mu_2 - \mu
## \mu_3 = \mu - \tau_3  =>  \tau_3 = \mu - \mu_3
## onde \tau_3 = \tau_1 + \tau_2
mean(da2$Resp) - mean(da2$Resp[da2$Trat == "C"])
## Portanto aqui estima-se o \tau_1 e o \tau_2, pois assume-se que o
## \tau_3 é o negativo do \tau_1 mais o \tau_2.
## NOTE QUE aqui se estima os taus mesmo!!!
with(da2, tapply(Resp, Trat, mean)) - mean(da2$Resp)
model.tables(m2aov, type = "means")
model.tables(m2aov, type = "effects")
## Fazendo na mão
mean(da2$Resp)
X <- model.matrix(m2lm2)
t(X) %*% X
MASS::fractions(solve(t(X) %*% X))
solve(t(X) %*% X) %*% t(X) %*% da2$Resp
## Média geral
sum(da2$Resp)/300
## tau1
sum(da2$Resp[da2$Trat == "A"])/1500 -
    sum(da2$Resp[da2$Trat == "B"])/3000 -
    sum(da2$Resp[da2$Trat == "C"])/3000
## 9.976569
## tau2
sum(da2$Resp[da2$Trat == "B"])/1500 -
    sum(da2$Resp[da2$Trat == "A"])/3000 -
    sum(da2$Resp[da2$Trat == "C"])/3000
## -15.06292
## tau3
sum(da2$Resp[da2$Trat == "C"])/1500 -
    sum(da2$Resp[da2$Trat == "A"])/3000 -
    sum(da2$Resp[da2$Trat == "B"])/3000
## - 9.976569 - (-15.06292)
- 9.976569 - (-15.06292)

## O que são os efeitos então?
model.tables(m2aov, type = "effects")
yates.effects(m2aov, data = da2)

##======================================================================
## Modelos com DOIS fator
##======================================================================

## Seja o modelo
## y_ij = \mu + \alpha_i + \beta_j + \gamma_ij +  e_ij
## i = 1, ..., a
## j = 1, ..., b
## e_ij ~ N(0, \sigma^2)
## Dessa forma,
## Y_ij ~ N(\mu, \sigma^2)

##----------------------------------------------------------------------
## Especificação dos componentes do modelo
## Considerando a = 2, b = 2 e r = a*b (sem repetição)
a <- 2
b <- 2
r <- a*b
## Média geral \mu
mu <- 50
## Efeito do primeiro nível do primeiro tratamento \alpha_1
alpha1 <- 4
## Efeito do segundo nível do primeiro tratamento \alpha_2
alpha2 <- -2
## Efeito do primeiro nível do segundo tratamento \beta_1
beta1 <- 9
## Efeito do segundo nível do segundo tratamento \beta_2
beta2 <- -5
## Variância comum \sigma^2
sigma2 <- 12
## Valores de Y para o tratamento 1
set.seed(11)
tratA <- rnorm(r, mu + tau1, sqrt(sigma2))
mean(tratA); var(tratA)
## Valores de Y para o tratamento 2
tratB <- rnorm(r, mu + tau2, sqrt(sigma2))
mean(tratB); var(tratB)
## Média geral
mean(c(tratA, tratB))

## Monta a base de dados
da1 <- data.frame(Trat = rep(c("A", "B"), each = r),
                  Resp = c(tratA, tratB))
str(da1)
summary(da1)

## Gráficos
boxplot(Resp  ~ Trat, data = da1)
abline(h = mean(da1$Resp), col = 2, lty = 2)

## Ajuste dos modelos
## ANOVA usando aov()
m1aov <- aov(Resp ~ Trat, data = da1)
summary(m1aov)
## ANOVA usando lm() - contr.treatment
m1lm1 <- lm(Resp ~ Trat, data = da1,
            contrasts = list(Trat = contr.treatment))
anova(m1lm1)
## ANOVA usando lm() - contr.sum
m1lm2 <- lm(Resp ~ Trat, data = da1,
            contrasts = list(Trat = contr.sum))
anova(m1lm2)
