#-----------------------------------------------------------------------
# Carregando pacotes e funções.

library(lattice)
library(latticeExtra)

# Para criar uma sequência de valores.
eseq <- function(x, n = 20) {
    r <- extendrange(x)
    s <- seq(r[1], r[2], length.out = n[1])
    return(s)
}

# Para gear a superfície teórica de um fenômeno. O ponto estácionário é
# em x1 = 8 e x2 = 8 e o valor no ponto estacionário é y = 10.
superficie <- function(x1, x2) {
    x1 <- x1 - 8
    x2 <- x2 - 8
    y <- 10 +
        -0.02 * (x1^2) +
         -0.02 * (x2^2) +
         0.01 * x1 * x2
    return(y)
}

# Para adicionar um erro aos valores simulados.
erro <- function(y, sd = 0.05) {
    y <- y + rnorm(length(y), 0, sd = sd)
    return(y)
}

# Visualizando a superfície teórica.
da <- expand.grid(x1 = seq(-1, 10, length.out = 20),
                  x2 = seq(-1, 10, length.out = 20))
da$y <- with(da, superficie(x1, x2))

levelplot(y ~ x1 + x2, data = da, contour = TRUE, aspect = "iso") +
    layer(panel.abline(v = 8, h = 8, lty = 2))

#-----------------------------------------------------------------------
# Primeiro experimento: fatorial 2^k com pontos centrais.

# Centro do experimento.
ct1 <- c(0, 0)

# Pontos fatoriais.
exp1 <- expand.grid(x1 = c(-1, 1),
                    x2 = c(-1, 1))

# Pontos centrais.
exp1 <- rbind(exp1, matrix(ct1,
                           byrow = TRUE,
                           nrow = 4,
                           ncol = 2,
                           dimnames = list(NULL, names(exp1))))

# Classificando observações do centro para estimar efeito de curvatura.
exp1$centro <- gl(n = 2, k = 4)

# Plano experimental.
exp1

# Simulando valores da resposta.
set.seed(123)
exp1$y <- with(exp1, erro(superficie(x1, x2)))

# Ajustando o modelo.
m1 <- lm(y ~ x1 * x2 + centro, data = exp1)
anova(m1)

# Reduzindo o modelo.
m1 <- lm(y ~ x1 + x2, data = exp1)
anova(m1)

# Estimativas dos coeficientes.
summary(m1)

# Efeitos de x1 e x2 para obter a direção do próximo plano.
b1 <- coef(m1)[-1]
b1

# Predição.
pred <- with(exp1,
             expand.grid(x1 = eseq(x1),
                         x2 = eseq(x2)))
pred$y <- predict(m1, newdata = pred)

wireframe(y ~ x1 + x2, data = pred, drape = TRUE)
levelplot(y ~ x1 + x2, data = pred, contour = TRUE, aspect = "iso")

levelplot(y ~ x1 + x2, data = pred, contour = TRUE, aspect = "iso") +
    layer(panel.arrows(ct1[1],
                       ct1[2],
                       ct1[1] + b1[1],
                       ct1[2] + b1[2],
                       length = 0.1))

#-----------------------------------------------------------------------
# Experimento 2.

# Direção de maior inclinação.
b1[1]/b1[2]

levelplot(y ~ x1 + x2, data = da, contour = TRUE, aspect = "iso") +
    layer(panel.levelplot(x = pred$x1, y = pred$x2, z = pred$y,
                          subscripts = 1:length(pred$x1),
                          contour = TRUE,
                          col.regions = grey.colors)) +
    layer(panel.rect(min(x1), min(x2), max(x1), max(x2)),
          data = exp1) +
    layer(panel.arrows(ct1[1],
                       ct1[2],
                       ct1[1] + 10 * b1[1],
                       ct1[2] + 10 * b1[2],
                       length = 0.1))

# Centro do novo plano experimental é ponta da seta.
ct2 <- 10 * b1

# Plano experimental centrado no novo centro.
exp2 <- expand.grid(x1 = ct2[1] + c(-1, 1),
                    x2 = ct2[2] + c(-1, 1))
exp2 <- rbind(exp2, matrix(ct2,
                           byrow = TRUE,
                           nrow = 4,
                           ncol = 2,
                           dimnames = list(NULL, names(exp2))))
exp2$centro <- gl(n = 2, k = 4)

# Resposta observada.
set.seed(234)
exp2$y <- with(exp2, erro(superficie(x1, x2)))
exp2

# Ajuste do modelo.
m2 <- lm(y ~ x1 * x2 + centro, data = exp2)
anova(m2)

# Ajuste do modelo reduzido.
m2 <- lm(y ~ x1 + x2, data = exp2)
anova(m2)

# Estimativas dos efeitos.
summary(m2)

# Efeitos de x1 e x2.
b2 <- coef(m2)[-1]
b2

# Predição.
pred <- with(exp2,
             expand.grid(x1 = eseq(x1),
                         x2 = eseq(x2)))
pred$y <- predict(m2, newdata = pred)

levelplot(y ~ x1 + x2, data = da, contour = TRUE, aspect = "iso") +
    layer(panel.levelplot(x = pred$x1, y = pred$x2, z = pred$y,
                          subscripts = 1:length(pred$x1),
                          contour = TRUE,
                          col.regions = grey.colors)) +
    layer(panel.rect(min(x1), min(x2), max(x1), max(x2)),
          data = exp2) +
    layer(panel.arrows(ct2[1],
                       ct2[2],
                       ct2[1] + 10 * b2[1],
                       ct2[2] + 10 * b2[2],
                       length = 0.1)) +
    layer(panel.rect(min(x1), min(x2), max(x1), max(x2)),
          data = exp1) +
    layer(panel.arrows(ct1[1],
                       ct1[2],
                       ct1[1] + 10 * b1[1],
                       ct1[2] + 10 * b1[2],
                       length = 0.1))

#-----------------------------------------------------------------------
# Experimento 3.

# Direção de maior inclinação.
b2[1]/b2[2]

# Centro do novo plano
ct3 <- ct2 + 10 * c(b2[1], b2[2])

# Novo plano experimental.
exp3 <- expand.grid(x1 = ct3[1] + c(-1, 1),
                    x2 = ct3[2] + c(-1, 1))
exp3 <- rbind(exp3, matrix(ct3,
                           byrow = TRUE,
                           nrow = 4,
                           ncol = 2,
                           dimnames = list(NULL, names(exp3))))
exp3$centro <- gl(n = 2, k = 4)

# Obtendo a resposta.
set.seed(234)
exp3$y <- with(exp3, erro(superficie(x1, x2)))
exp3

# Ajuste do modelo.
m3 <- lm(y ~ x1 * x2 + centro, data = exp3)
anova(m3)

# Ajuste do modelo reduzido.
m3 <- lm(y ~ x1 + x2, data = exp3)
anova(m3)

# Estimativas dos efeitos.
summary(m3)

# NOTE: x1 e x2 não foram significativos (à 5%) nesse experimento e isso
# é um indicatico de proximidade da região estacionária.

# Efeitos de x1 e x2.
b3 <- coef(m3)[-1]
b3

# Predição.
pred <- with(exp3,
             expand.grid(x1 = eseq(x1),
                         x2 = eseq(x2)))
pred$y <- predict(m3, newdata = pred)

levelplot(y ~ x1 + x2, data = da, contour = TRUE, aspect = "iso") +
    layer(panel.levelplot(x = pred$x1, y = pred$x2, z = pred$y,
                          subscripts = 1:length(pred$x1),
                          contour = TRUE,
                          col.regions = grey.colors)) +
    layer(panel.rect(min(x1), min(x2), max(x1), max(x2)),
          data = exp3) +
    layer(panel.arrows(ct3[1],
                       ct3[2],
                       ct3[1] + 20 * b3[1],
                       ct3[2] + 20 * b3[2],
                       length = 0.1)) +
    layer(panel.rect(min(x1), min(x2), max(x1), max(x2)),
          data = exp2) +
    layer(panel.arrows(ct2[1],
                       ct2[2],
                       ct2[1] + 10 * b2[1],
                       ct2[2] + 10 * b2[2],
                       length = 0.1)) +
    layer(panel.rect(min(x1), min(x2), max(x1), max(x2)),
          data = exp1) +
    layer(panel.arrows(ct1[1],
                       ct1[2],
                       ct1[1] + 10 * b1[1],
                       ct1[2] + 10 * b1[2],
                       length = 0.1))


#-----------------------------------------------------------------------
# Experimento 3.

# Direção de maior inclinação.
b3[1]/b3[2]

# Centro do novo plano
ct4 <- ct3 + 10 * c(b2[1], b2[2])

# Novo plano experimental.
exp3 <- expand.grid(x1 = ct3[1] + c(-1, 1),
                    x2 = ct3[2] + c(-1, 1))
exp3 <- rbind(exp3, matrix(ct3,
                           byrow = TRUE,
                           nrow = 4,
                           ncol = 2,
                           dimnames = list(NULL, names(exp3))))
exp3$centro <- gl(n = 2, k = 4)

# Obtendo a resposta.
set.seed(234)
exp3$y <- with(exp3, erro(superficie(x1, x2)))
exp3

# Ajuste do modelo.
m3 <- lm(y ~ x1 * x2 + centro, data = exp3)
anova(m3)

# Ajuste do modelo reduzido.
m3 <- lm(y ~ x1 + x2, data = exp3)
anova(m3)

# Estimativas dos efeitos.
summary(m3)

# NOTE: x1 e x2 não foram significativos (à 5%) nesse experimento e isso
# é um indicatico de proximidade da região estacionária.

# Efeitos de x1 e x2.
b3 <- coef(m3)[-1]
b3

# Predição.
pred <- with(exp3,
             expand.grid(x1 = eseq(x1),
                         x2 = eseq(x2)))
pred$y <- predict(m3, newdata = pred)

levelplot(y ~ x1 + x2, data = da, contour = TRUE, aspect = "iso") +
    layer(panel.levelplot(x = pred$x1, y = pred$x2, z = pred$y,
                          subscripts = 1:length(pred$x1),
                          contour = TRUE,
                          col.regions = grey.colors)) +
    layer(panel.rect(min(x1), min(x2), max(x1), max(x2)),
          data = exp3) +
    layer(panel.arrows(ct3[1],
                       ct3[2],
                       ct3[1] + 20 * b3[1],
                       ct3[2] + 20 * b3[2],
                       length = 0.1)) +
    layer(panel.rect(min(x1), min(x2), max(x1), max(x2)),
          data = exp2) +
    layer(panel.arrows(ct2[1],
                       ct2[2],
                       ct2[1] + 10 * b2[1],
                       ct2[2] + 10 * b2[2],
                       length = 0.1)) +
    layer(panel.rect(min(x1), min(x2), max(x1), max(x2)),
          data = exp1) +
    layer(panel.arrows(ct1[1],
                       ct1[2],
                       ct1[1] + 10 * b1[1],
                       ct1[2] + 10 * b1[2],
                       length = 0.1))


#-----------------------------------------------------------------------
# Experimento 4 - Entrar com o Planejamento Composto central.

# Direção de maior inclinação.
b3[1]/b3[2]

# Centro do novo plano
ct4 <- ct3 + 20 * c(b3[1], b3[2])

# Planejamento composto central.
r <- sqrt(2)
exp4 <- expand.grid(x1 = ct4[1] + c(-1, 1),
                    x2 = ct4[2] + c(-1, 1))
exp4 <- rbind(exp4,
              data.frame(x1 = ct4[1] + c(r, -r, 0, 0),
                         x2 = ct4[2] + c(0, 0, r, -r)))
exp4 <- rbind(exp4, matrix(ct4,
                           byrow = TRUE,
                           nrow = 4,
                           ncol = 2,
                           dimnames = list(NULL, names(exp4))))
exp4$centro <- rep(1:2, c(nrow(exp4) - 4, 4))

# Visualizando o planejamento.
xyplot(x1 ~ x2, data = exp4, aspect = "iso", pch = 19) +
    layer(panel.segments(ct4[2] + c(-r, 0),
                         ct4[1] + c(0, -r),
                         ct4[2] + c(r, 0),
                         ct4[1] + c(0, r),
                         col = 1, lty = 2)) +
    layer(panel.lines(ct4[2] + c(-1, 1, 1, -1, -1),
                      ct4[1] + c(-1, -1, 1, 1, -1),
                      col = 1, lty = 2))

# Obtendo os resultados.
set.seed(301)
exp4$y <- with(exp4, erro(superficie(x1, x2)))
exp4

# m4 <- lm(y ~ x1 * x2 + I(x1^2) + I(x2^2) + centro, data = exp4)
m4 <- lm(y ~ x1 * x2 + I(x1^2) + I(x2^2), data = exp4)
anova(m4)

summary(m4)

# Predição.
pred <- with(exp4,
             expand.grid(x1 = eseq(x1),
                         x2 = eseq(x2)))
pred$y <- predict(m4, newdata = pred)

levelplot(y ~ x1 + x2, data = pred, contour = TRUE, aspect = "iso") +
    layer(panel.points(8, 8, pch = 19, col = "red"))

levelplot(y ~ x1 + x2, data = da, contour = TRUE, aspect = "iso") +
    layer(panel.levelplot(x = pred$x1, y = pred$x2, z = pred$y,
                          subscripts = 1:length(pred$x1),
                          contour = TRUE,
                          col.regions = grey.colors)) +
    layer(panel.points(x1, x2),
          data = exp4) +
    layer(panel.points(x1, x2),
          data = exp3) +
    layer(panel.points(x1, x2),
          data = exp2) +
    layer(panel.points(x1, x2),
          data = exp1) +
    layer(panel.rect(min(x1), min(x2), max(x1), max(x2)),
          data = exp3) +
    layer(panel.arrows(ct3[1],
                       ct3[2],
                       ct3[1] + 20 * b3[1],
                       ct3[2] + 20 * b3[2],
                       length = 0.1)) +
    layer(panel.rect(min(x1), min(x2), max(x1), max(x2)),
          data = exp2) +
    layer(panel.arrows(ct2[1],
                       ct2[2],
                       ct2[1] + 10 * b2[1],
                       ct2[2] + 10 * b2[2],
                       length = 0.1)) +
    layer(panel.rect(min(x1), min(x2), max(x1), max(x2)),
          data = exp1) +
    layer(panel.arrows(ct1[1],
                       ct1[2],
                       ct1[1] + 10 * b1[1],
                       ct1[2] + 10 * b1[2],
                       length = 0.1))

#-----------------------------------------------------------------------
