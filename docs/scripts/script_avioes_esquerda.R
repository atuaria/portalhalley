##======================================================================
## Análise do experimento com aviões de papel realizado em sala
##======================================================================

##----------------------------------------------------------------------
## Pacotes necessários
library(lattice)
#library(dae)
##----------------------------------------------------------------------

##======================================================================
## Dados do grupo da esquerda
##======================================================================

##----------------------------------------------------------------------
## Importação e conferência
url <- "http://leg.ufpr.br/~fernandomayer/aulas/ce074/dados/avioes_esquerda.csv"
desq <- read.table(url, header = TRUE, sep = ",", dec = ".")
str(desq)
desq$BLOCO <- as.factor(desq$BLOCO)
str(desq)

##----------------------------------------------------------------------
## Visualização

## Relação TEMPO x Distância
plot(TEMPO ~ DISTANCIA, data = desq)
mcor <- lm(TEMPO ~ DISTANCIA, data = desq)
abline(mcor)
summary(mcor)
par(mfrow = c(1, 2))
hist(desq$TEMPO)
hist(desq$DISTANCIA)
par(mfrow = c(1, 1))

## Usando TEMPO
with(desq,
     interaction.plot(x.factor = PAPEL, trace.factor = DESIGN,
                      response = TEMPO))
interaction.ABC.plot(response = TEMPO, x.factor = PAPEL,
                     groups.factor = DESIGN, trace.factor = BLOCO,
                     data = desq)
wireframe(TEMPO ~ PAPEL + DESIGN, data = desq)
levelplot(TEMPO ~ PAPEL + DESIGN, data = desq)

## Usando DISTANCIA
with(desq,
     interaction.plot(x.factor = PAPEL, trace.factor = DESIGN,
                      response = DISTANCIA))
interaction.ABC.plot(response = DISTANCIA, x.factor = PAPEL,
                     groups.factor = DESIGN, trace.factor = BLOCO,
                     data = desq)
wireframe(DISTANCIA ~ PAPEL + DESIGN, data = desq)
levelplot(TEMPO ~ PAPEL + DESIGN, data = desq)

##----------------------------------------------------------------------
## Modelo para TEMPO
mt0aov <- aov(1/TEMPO ~ BLOCO + PAPEL * DESIGN + Error(BLOCO), data = desq)
summary(mt0aov)
## Nada é significativo
model.tables(mt0aov, type = "means")
model.tables(mt0aov, type = "effects")
yates.effects(mt0aov, data = desq)
yates.effects(mt0aov, data = desq)/2
## Usando lm
mt0 <- lm(TEMPO ~ BLOCO + PAPEL * DESIGN, data = desq,
          contrasts = list(BLOCO = contr.sum,
                           PAPEL = contr.sum,
                           DESIGN = contr.sum))
coef(mt0)
summary(mt0)
## Análise dos resíduos
par(mfrow = c(2, 2))
plot(mt0)
par(mfrow = c(1, 1))

##----------------------------------------------------------------------
## Modelo para DISTANCIA
md0aov <- aov(DISTANCIA ~ BLOCO + PAPEL * DESIGN + Error(BLOCO),
              data = desq)
summary(md0aov)
## Nada é significativo
model.tables(md0aov, type = "means")
model.tables(md0aov, type = "effects")
yates.effects(md0aov, data = desq)
yates.effects(md0aov, data = desq)/2
## Usando lm
md0 <- lm(DISTANCIA ~ BLOCO + PAPEL * DESIGN, data = desq,
          contrasts = list(BLOCO = contr.sum,
                           PAPEL = contr.sum,
                           DESIGN = contr.sum))
coef(md0)
summary(md0)
## Análise dos resíduos
par(mfrow = c(2, 2))
plot(md0)
par(mfrow = c(1, 1))

##----------------------------------------------------------------------
## SE fosse fazer...
## Predição
## Cria novo data frame com codificação -1 e 1
desq.new <- data.frame(
    PAPEL = ifelse(desq$PAPEL == "cartolina", -1, 1),
    DESIGN = ifelse(desq$DESIGN == "com", -1, 1),
    BLOCO = desq$BLOCO,
    DISTANCIA = desq$DISTANCIA)
## Ajusta o modelo
md0.new <- lm(DISTANCIA ~ BLOCO + PAPEL * DESIGN, data = desq.new)
summary(md0.new) # Exatamente igual, mas com os sinais trocados
## Predição e gráficos
pred <- expand.grid(PAPEL = seq(-1, 1, length = 20),
                    DESIGN = seq(-1, 1, length = 20),
                    BLOCO = levels(desq$BLOCO))
pred$y <- predict(md0.new, newdata = pred)
wireframe(y ~ PAPEL + DESIGN, data = pred)
wireframe(y ~ PAPEL + DESIGN, data = pred, drape = TRUE)
levelplot(y ~ PAPEL + DESIGN, data = pred)
levelplot(y ~ PAPEL + DESIGN, data = pred, cuts = 90)
levelplot(y ~ PAPEL + DESIGN, data = pred, cuts = 90,
          col.regions = heat.colors)
