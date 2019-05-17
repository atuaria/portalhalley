## ----setup, include=FALSE------------------------------------------------
library(knitr, quietly = TRUE)
opts_chunk$set(size = "small",
               comment = "#",
               tidy = FALSE,
               fig.align = "center"
               )

## ------------------------------------------------------------------------
## Dessa forma, temos 3 x 4 = 12 UEs. Associamos os valores de 1 a 12
## para cada UE, e sorteamos os níveis dos fatores para cada uma
nf <- 3
r <- 4
n <- nf * r
fator <- rep(c("A", "B", "C"), each = r)
set.seed(123)
(DIC <- data.frame(UE = 1:n, Fator = sample(fator)))
## Croqui do experimento
matrix(DIC$Fator, ncol = nf, dimnames = list(1:r, 1:nf))

## ------------------------------------------------------------------------
## Nesse caso, temos 6 x 4 = 24 UEs. Associamos os valores de 1 a 24
## para cada UE, e alocamos cada 6 UEs em cada um dos 4 blocos. Os
## níveis dos fatores devem ser sorteados dentro de cada bloco
nf <- 6
b <- 4
n <- nf * b
fator <- rep(LETTERS[1:6], each = b)
blocos <- rep(c("I", "II", "III", "IV"), each = nf)
set.seed(123)
(DBC <- data.frame(UE = 1:n, Blocos = blocos, Fator = sample(fator)))
## Está certo o que foi feito até aqui?
set.seed(123)
(DBC <- data.frame(UE = 1:n, Blocos = blocos,
                   Fator = as.vector(
                       replicate(b, sample(unique(fator)))
                   )))
## Croqui do experimento
matrix(DBC$Fator, ncol = b, dimnames = list(1:nf, paste("Bloco", 1:b)))

## ------------------------------------------------------------------------
## Nesse caso temos 2 níveis A x 2 níveis B  = 4 possibilidades de
## combinação. Criamos os níveis dos fatores e fazemos todas as
## combinações possíveis. Depois aleatorizamos as combinações para cada
## UE.
a <- 2
b <- 2
r <- 1 # sem repetição
n <- a * b * r
fatores <- expand.grid(A = c("A1", "A2"),
                       B = c("B1", "B2"))
fatores$AB <- paste(fatores$A, fatores$B, sep = ":")
fatores
set.seed(123)
(DICf <- data.frame(UE = 1:n, Fatores = sample(fatores$AB)))
table(DICf$Fatores)
## Croqui para este experimento
matrix(DICf$Fatores, ncol = a*b,
       dimnames = list(1:r, 1:(a*b)))

## ------------------------------------------------------------------------
a <- 2
b <- 2
r <- 4
n <- a * b * r
fatores <- expand.grid(A = rep(c("A1", "A2"), each = a),
                       B = rep(c("B1", "B2"), each = b))
fatores$AB <- paste(fatores$A, fatores$B, sep = ":")
fatores
set.seed(123)
(DICf <- data.frame(UE = 1:n, Fatores = sample(fatores$AB)))
table(DICf$Fatores)
## Croqui para este experimento
matrix(DICf$Fatores, ncol = a*b,
       dimnames = list(1:r, 1:(a*b)))

## ---- out.width='60%'----------------------------------------------------
## Um exemplo com cars
set.seed(123)
(carros <- cars[sample(1:nrow(cars), size = 15), ])
plot(dist ~ speed, data = carros)
model.matrix(dist ~ speed, data = carros)
## Um fator com 3 níveis e 4 repetições
fator <- factor(rep(c("A", "B", "C"), each = 4))
X <- matrix(0, nrow = 12, ncol = 3)
X[cbind(seq_along(fator), fator)] <- 1
(X <- cbind(1, X))

## ---- error=TRUE---------------------------------------------------------
## Fator com 3 níveis e 4 repetições
fator <- factor(rep(c("A", "B", "C"), each = 4))
## Cria a matriz do modelo sem nenhuma restrição
X <- matrix(0, nrow = 12, ncol = 3)
X[cbind(seq_along(fator), fator)] <- 1
(X <- cbind(1, X))
## X'
Xt <- t(X)
## X'X
(Xt %*% X)
## (X'X)^-1
solve(Xt %*% X)
## Impondo uma restrição: remove a coluna do primeiro nível -
## equivalente a zerar o primeiro nível do fator
(X <- X[, -2])
## X'
Xt <- t(X)
## X'X
Xt %*% X
## (X'X)^-1
solve(Xt %*% X)
MASS::fractions(solve(Xt %*% X))

