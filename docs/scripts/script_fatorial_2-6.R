##======================================================================
## Análise completa de um experimento fatorial 2^6
##======================================================================

##----------------------------------------------------------------------
## Dados disponíveis em
url <- "http://leg.ufpr.br/~fernandomayer/data/fatorial_2-6.txt"
dados <- read.table(url, header = TRUE)
str(dados)

## Definições
k <- 6
n <- 1

##----------------------------------------------------------------------
## Visualização
xyplot(y ~ factor(A) | B * C, groups = interaction(D, E, F), data = dados,
       type = c("p", "a"), auto.key = TRUE)

##----------------------------------------------------------------------
## Montando a tabela de sinais
tab <- model.matrix(~ A * B * C * D * E * F, data = dados)
tab

##----------------------------------------------------------------------
## Estimativa dos efeitos
## Podemos usar as colunas da tabela de sinais para calcular os
## contrastes
y <- dados$y
## Contrastes
(contr <- t(tab[, -1]) %*% y)
## Efeitos = contraste/(n2^{k-1})
(ef <- contr/(n * 2^(k-1)))

##----------------------------------------------------------------------
## Gráfico de probabilidade normal dos efeitos estimados
qqaux <- qqnorm(ef, col = 2, pch = 19, ylim = c(-10, 4)); qqline(ef)
text(qqaux$x, qqaux$y, rownames(qqaux$y), cex = 0.8, pos = 3)

##----------------------------------------------------------------------
## Ajuste do modelo
## De acordo com o gráfico de probabilidade normla, efeitos de até
## terceira ordem podem ser importantes. Para sermos conservadores,
## vamos ajustar o primeiro modelo com efeitos de até quarta ordem.
m0 <- lm(y ~ (A + B + C + D + E + F)^4, data = dados)
anova(m0)
## Apenas efeitos principais parecem importantes, mas podemos ir
## abaixando os graus de interação gradativamente
m1 <- update(m0, . ~ (A + B + C + D + E + F)^3)
anova(m1)
## Considera apenas de segunda ordem
m2 <- update(m1, . ~ (A + B + C + D + E + F)^2)
anova(m2)
## Apenas efeitos principais D e F são importantes. A interação C:E
## parece marginalmente significativa, por isso vamos mantê-la no modelo
## (incluindo efeitos principais C e E para manter a marginalidade)
m3 <- update(m2, . ~ D + F + (C + E)^2)
anova(m3)
## Ajusta um último modelo apenas com os efeitos principais
m4 <- update(m3, . ~ D + F)
anova(m4)
## Fazendo o TRV para confirmar o modelo mais adequado
anova(m4, m3, m2, m1, m0)
## Como nenhum modelo difere significativamente do outro, então adotamos
## aquele que possui menos parâmetros, pelo princípio da parcimônia.

## NOTE que com esse resultado, passamos de um experimento fatorial 2^6
## sem repetição para um experimento fatorial 2^2 com 4 repetições!
## Assim, dizemos que o experimento fatorial 2^6 foi **projetado** em um
## fatorial 2^2.

##----------------------------------------------------------------------
## Análise dos efeitos
## Teste t para os coeficientes dos efeitos
summary(m4)
## Efeitos estimados (qual a interpretação)
c(coef(m4)[1], 2*coef(m4)[-1])
## Veja a interação entre os fatores. Por que ela não é significativa?
with(dados, interaction.plot(D, E, y))
## Arruma a escala para visualizar a diferença
with(dados, interaction.plot(D, E, y,
                             ylim = c(0, 35)))

##----------------------------------------------------------------------
## Residuos do modelo
res <- residuals(m4)
## Quantis normais
qqnorm(res); qqline(res)
## Resíduo vs preditor
par(mfrow = c(1, 2))
with(dados, {
    plot(res ~ D)
    abline(h = 0, lty = 2, col = 2)
    plot(res ~ E)
    abline(h = 0, lty = 2, col = 2)
})
par(mfrow = c(1, 1))

##----------------------------------------------------------------------
## Predições

## Predição para as combinações únicas dos fatores
pred <- data.frame(D = dados$D,
                   F = dados$F)
pred$y <- predict(m4, newdata = pred)
pred
## Verifica a projeção
proj(m4)
cbind(pred, yproj = apply(proj(m4)[,-4], 1, sum))

## Predição para um intervalo de valores entre os níveis baixo e alto
## dos fatores
pred <- expand.grid(D = seq(-1, 1, length.out = 30),
                    F = seq(-1, 1 ,length.out = 30))
pred$y <- predict(m4, newdata = pred)
## Vários formas de visualizar
wireframe(y ~ D + F, data = pred, drape = TRUE)
levelplot(y ~ D + F, data = pred)
levelplot(y ~ D + F, data = pred, cuts = 90,
          col.regions = heat.colors)
