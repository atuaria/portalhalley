##======================================================================
## Análise do experimento com aviões de papel realizado em sala
##======================================================================

##----------------------------------------------------------------------
## Pacotes necessários
library(lattice)
#library(dae)
##----------------------------------------------------------------------

##======================================================================
## Dados do grupo da direita
##======================================================================

##----------------------------------------------------------------------
## Importação e conferência
url <- "http://leg.ufpr.br/~fernandomayer/aulas/ce074/dados/avioes_direita.csv"
ddir <- read.table(url, header = TRUE, sep = ";", dec = ",")
str(ddir)
names(ddir) <- toupper(names(ddir))
## Converte distancia para centimetros (por compatibilidade com outro
## grupo)
ddir$DISTANCIA <- ddir$DISTANCIA * 100
str(ddir)
## Quem são A1, A2; B1, B2?

##----------------------------------------------------------------------
## Visualização

## Relação TEMPO x Distância
plot(TEMPO ~ DISTANCIA, data = ddir)
mcor <- lm(TEMPO ~ DISTANCIA, data = ddir)
abline(mcor)
summary(mcor)
par(mfrow = c(1, 2))
hist(ddir$TEMPO)
hist(ddir$DISTANCIA)
par(mfrow = c(1, 1))

## Usando TEMPO
with(ddir,
     interaction.plot(x.factor = PAPEL, trace.factor = DESIGN,
                      response = TEMPO))
interaction.ABC.plot(response = TEMPO, x.factor = PAPEL,
                     groups.factor = DESIGN, trace.factor = BLOCO,
                     data = ddir)
wireframe(TEMPO ~ PAPEL + DESIGN, data = ddir)
levelplot(TEMPO ~ PAPEL + DESIGN, data = ddir)

## Usando DISTANCIA
with(ddir,
     interaction.plot(x.factor = PAPEL, trace.factor = DESIGN,
                      response = DISTANCIA))
interaction.ABC.plot(response = DISTANCIA, x.factor = PAPEL,
                     groups.factor = DESIGN, trace.factor = BLOCO,
                     data = ddir)
wireframe(DISTANCIA ~ PAPEL + DESIGN, data = ddir)
levelplot(TEMPO ~ PAPEL + DESIGN, data = ddir)

##----------------------------------------------------------------------
## Modelo para TEMPO
mt0aov <- aov(TEMPO ~ BLOCO + PAPEL * DESIGN + Error(BLOCO), data = ddir)
summary(mt0aov)
## Nada é significativo
model.tables(mt0aov, type = "means")
model.tables(mt0aov, type = "effects")
yates.effects(mt0aov, data = ddir)
yates.effects(mt0aov, data = ddir)/2
## Usando lm
mt0 <- lm(TEMPO ~ BLOCO + PAPEL * DESIGN, data = ddir,
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
              data = ddir)
summary(md0aov)
## Nada é significativo
model.tables(md0aov, type = "means")
model.tables(md0aov, type = "effects")
yates.effects(md0aov, data = ddir)
yates.effects(md0aov, data = ddir)/2
## Usando lm
md0 <- lm(DISTANCIA ~ BLOCO + PAPEL * DESIGN, data = ddir,
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
ddir.new <- data.frame(
    PAPEL = ifelse(ddir$PAPEL == "A1", -1, 1),
    DESIGN = ifelse(ddir$DESIGN == "B1", -1, 1),
    BLOCO = ddir$BLOCO,
    DISTANCIA = ddir$DISTANCIA)
## Ajusta o modelo
md0.new <- lm(DISTANCIA ~ BLOCO + PAPEL * DESIGN, data = ddir.new)
summary(md0.new) # Exatamente igual, mas com os sinais trocados
## Predição e gráficos
pred <- expand.grid(PAPEL = seq(-1, 1, length = 20),
                    DESIGN = seq(-1, 1, length = 20),
                    BLOCO = levels(ddir$BLOCO))
pred$y <- predict(md0.new, newdata = pred)
wireframe(y ~ PAPEL + DESIGN, data = pred)
wireframe(y ~ PAPEL + DESIGN, data = pred, drape = TRUE)
levelplot(y ~ PAPEL + DESIGN, data = pred)
levelplot(y ~ PAPEL + DESIGN, data = pred, cuts = 90)
levelplot(y ~ PAPEL + DESIGN, data = pred, cuts = 90,
          col.regions = heat.colors)
