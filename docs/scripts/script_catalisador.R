##======================================================================
## Interpretação de efeitos estimados em modelos lineares
##======================================================================

##----------------------------------------------------------------------
## Exemplo do catalisador

url <- "http://leg.ufpr.br/~fernandomayer/data/temp_cat.csv"
dados <- read.csv(url)
str(dados)
dados$Temperatura <- as.factor(dados$Temperatura)

##----------------------------------------------------------------------
## Separa em um fator
da <- aggregate(Rendimento ~ Temperatura + Catalisador,
                data = dados, FUN = mean)
da1 <- da[, -1]
plot(Rendimento ~ as.numeric(Catalisador), data = da1)

## Contraste treatment = zerar primeiro nivel --------------------------
m1 <- lm(Rendimento ~ Catalisador, data = da1,
         contrasts = list(Catalisador = contr.treatment))
summary(m1)
tapply(da1$Rendimento, da1$Catalisador, mean)
diff(tapply(da1$Rendimento, da1$Catalisador, mean))
## É a média do segundo nível menos a média do primeiro nível

## Fazendo na mão
mean(da1$Rendimento[da1$Catalisador == "A"])
mean(da1$Rendimento[da1$Catalisador == "B"]) -
    mean(da1$Rendimento[da1$Catalisador == "A"])
mean(da1$Rendimento[da1$Catalisador == "A"]) + coef(m1)[2]
## Portanto, aqui estima-se o \tau_2, pois assume-se que o \tau_1 é
## zero.
model.matrix(m1)

## Contraste soma zero -------------------------------------------------
m2 <- lm(Rendimento ~ Catalisador, data = da1,
         contrasts = list(Catalisador = contr.sum))
summary(m2)
tapply(da1$Rendimento, da1$Catalisador, mean)
mean(da1$Rendimento) # media geral = intercepto
mean(da1$Rendimento) + coef(m2)[2] # média + tau1
mean(da1$Rendimento) - coef(m2)[2] # média - tau1
## É a media do primeiro menos a média geral, porque:
## \mu_1 = \mu + \tau_1  =>  \tau_1 = \mu_1 - \mu
mean(da1$Rendimento[da1$Catalisador == "A"]) - mean(da1$Rendimento)
## Ou a média geral menos a média do segundo, porque:
## \mu_2 = \mu - \tau_1  =>  \tau_1 = \mu - \mu_2
mean(da1$Rendimento) - mean(da1$Rendimento[da1$Catalisador == "B"])

## Note que é de fato a diferença entre as médias sobre 2:
## Ef_A = (1/2) (\bar{y_A+} - \bar{y_A-})
diff(tapply(da1$Rendimento, da1$Catalisador, mean))/2

## Portanto aqui estima-se o \tau_1 apenas, pois assume-se que o \tau_2
## é o negativo do \tau_1
with(da1, tapply(Rendimento, Catalisador, mean)) - mean(da1$Rendimento)
model.matrix(m2)

## ANOVA usando aov() para poder acessar alguns métodos específicos
m2aov <- aov(Rendimento ~ Catalisador, data = da1)
model.tables(m2aov, type = "means")
model.tables(m2aov, type = "effects")
#dae::yates.effects(m2aov, data = da1) # é o tau2 do contr.treatment!!
#dae::yates.effects(m2aov, data = da1)/2

## Na mão
y <- da1$Rendimento
X <- rbind(c(1, 1),
           c(1, 1),
           c(1, -1),
           c(1, -1))
(Xt <- t(X))
Xt %*% X
solve(Xt %*% X)
MASS::fractions(solve(Xt %*% X))
solve(Xt %*% X) %*% Xt %*% y

##======================================================================
## Modelo com 2 fatores
##======================================================================

## Agrega dados brutos
da <- aggregate(Rendimento ~ Temperatura + Catalisador,
                data = dados, FUN = mean)
str(da)
da$Temperatura <- as.factor(da$Temperatura)

## Gráfico de interação
with(da,
     interaction.plot(Catalisador, Temperatura, Rendimento, mean))
wireframe(Rendimento ~ Catalisador * Temperatura, data = da)
levelplot(Rendimento ~ Catalisador * Temperatura, data = da)

## Contraste treatment -------------------------------------------------
m1 <- lm(Rendimento ~ Catalisador * Temperatura, data = da,
         contrasts = list(Temperatura = contr.treatment,
                          Catalisador = contr.treatment))
summary(m1)
## Aqui não existem médias, pois só há uma observação por combinação de
## tratamento
da
## Os efeitos são então:
da$Rendimento[1] # média do primeiro nível dos dois fatores
da$Rendimento[3] - da$Rendimento[1]
da$Rendimento[2] - da$Rendimento[1]
da$Rendimento[1] - da$Rendimento[2] - da$Rendimento[3] +
    da$Rendimento[4]
## Ver exercícios

## Contraste soma zero -------------------------------------------------
m3 <- lm(Rendimento ~ Catalisador * Temperatura, data = da,
         contrasts = list(Temperatura = contr.sum,
                          Catalisador = contr.sum))
summary(m3)
da
mean(da$Rendimento) # media geral = intercepto

## Note que é de fato a diferença entre as médias sobre 2:
## Ef_A = (1/2) (\bar{y_A+} - \bar{y_A-})
diff(tapply(da$Rendimento, da$Catalisador, mean))
diff(tapply(da$Rendimento, da$Catalisador, mean))/2
diff(tapply(da$Rendimento, da$Temperatura, mean))
diff(tapply(da$Rendimento, da$Temperatura, mean))/2

## Porque os sinais são trocados?
model.matrix(m3)

## ANOVA usando aov() para poder acessar alguns métodos específicos
m3aov <- aov(Rendimento ~ Catalisador * Temperatura, data = da)
model.tables(m3aov, type = "means")
model.tables(m3aov, type = "effects")
#dae::yates.effects(m3aov, data = da)
#dae::yates.effects(m3aov, data = da)/2

## Catalisador
(mean(da$Rendimento[da$Catalisador == "A"]) -
 mean(da$Rendimento[da$Catalisador == "B"]))/2

mean(da$Rendimento) + coef(m3)[2] # media do catalisador A
mean(da$Rendimento[da$Catalisador == "A"])
mean(da$Rendimento) - coef(m3)[2] # media do catalisador B
mean(da$Rendimento[da$Catalisador == "B"])

## Temperatura
(mean(da$Rendimento[da$Temperatura == "40"]) -
 mean(da$Rendimento[da$Temperatura == "60"]))/2

mean(da$Rendimento) + coef(m3)[3] # media da temperatura BAIXA
mean(da$Rendimento[da$Temperatura == "40"])
mean(da$Rendimento) - coef(m3)[3] # media do temperatura ALTA
mean(da$Rendimento[da$Temperatura == "60"])

## Catalisador:Temperatura
(mean(c(da$Rendimento[da$Catalisador == "A" & da$Temperatura == "40"],
       da$Rendimento[da$Catalisador == "B" & da$Temperatura == "60"])) -
    mean(c(da$Rendimento[da$Catalisador == "A" & da$Temperatura == "60"],
           da$Rendimento[da$Catalisador == "B" & da$Temperatura == "40"])))/2

mean(da$Rendimento) + coef(m3)[4] # media de A,40:B,60
mean(c(da$Rendimento[da$Catalisador == "A" & da$Temperatura == "40"],
       da$Rendimento[da$Catalisador == "B" & da$Temperatura == "60"]))
mean(da$Rendimento) - coef(m3)[4] # media de A,60:B,40
mean(c(da$Rendimento[da$Catalisador == "A" & da$Temperatura == "60"],
       da$Rendimento[da$Catalisador == "B" & da$Temperatura == "40"]))

##----------------------------------------------------------------------
## Predição
## Cria novo data frame com codificação -1 e 1
da.new <- data.frame(
    Temperatura = ifelse(da$Temperatura == "40", -1, 1),
    Catalisador = ifelse(da$Catalisador == "A", -1, 1),
    Rendimento = da$Rendimento)
## Ajusta o modelo
m3.new <- lm(Rendimento ~ Catalisador * Temperatura, data = da.new)
summary(m3.new) # Exatamente igual, mas com os sinais trocados
## Predição e gráficos
pred <- expand.grid(Catalisador = seq(-1, 1, length = 20),
                    Temperatura = seq(-1, 1, length = 20))
pred$y <- predict(m3.new, newdata = pred)
wireframe(y ~ Catalisador + Temperatura, data = pred)
wireframe(y ~ Catalisador + Temperatura, data = pred, drape = TRUE)
levelplot(y ~ Catalisador + Temperatura, data = pred)
levelplot(y ~ Catalisador + Temperatura, data = pred, cuts = 90)
levelplot(y ~ Catalisador + Temperatura, data = pred, cuts = 90,
          col.regions = heat.colors)

##======================================================================
## Usando dados completos

##----------------------------------------------------------------------
## Gráfico de interação
with(dados,
     interaction.plot(Catalisador, Temperatura, Rendimento, mean))
wireframe(Rendimento ~ Catalisador * Temperatura, data = dados)
levelplot(Rendimento ~ Catalisador * Temperatura, data = dados)

## Contraste soma zero -------------------------------------------------
m4 <- lm(Rendimento ~ Catalisador * Temperatura, data = dados,
         contrasts = list(Temperatura = contr.sum,
                          Catalisador = contr.sum))
summary(m4)
m4aov <- aov(Rendimento ~ Catalisador * Temperatura, data = dados)
summary(m4aov)
model.tables(m3aov, type = "means")
model.tables(m3aov, type = "effects")
#dae::yates.effects(m3aov, data = da)
#dae::yates.effects(m3aov, data = da)/2

##----------------------------------------------------------------------
## Predição
## Cria novo data frame com codificação -1 e 1
dados.new <- data.frame(
    Temperatura = ifelse(dados$Temperatura == "40", -1, 1),
    Catalisador = ifelse(dados$Catalisador == "A", -1, 1),
    Rendimento = dados$Rendimento)
## Ajusta o modelo
m4.new <- lm(Rendimento ~ Catalisador * Temperatura, data = dados.new)
summary(m4.new) # Exatamente igual, mas com os sinais trocados
## Predição e gráfico
pred <- expand.grid(Catalisador = seq(-1, 1, length = 20),
                    Temperatura = seq(-1, 1, length = 20))
pred$y <- predict(m4.new, newdata = pred)
wireframe(y ~ Catalisador + Temperatura, data = pred)
wireframe(y ~ Catalisador + Temperatura, data = pred, drape = TRUE)
levelplot(y ~ Catalisador + Temperatura, data = pred)
levelplot(y ~ Catalisador + Temperatura, data = pred, cuts = 90)
levelplot(y ~ Catalisador + Temperatura, data = pred, cuts = 90,
          col.regions = heat.colors)
