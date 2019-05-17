##======================================================================
## Tipos de contrastes em modelos lineares
##======================================================================

##----------------------------------------------------------------------
## Carrega base de dados
## Disponível em:
# url <- "http://leg.ufpr.br/~fernandomayer/data/pimentel_racoes.txt"
da <- read.table("dados/pimentel_racoes.txt",
                 header = TRUE, sep = "\t")
str(da)
xtabs(~ racoes, da)

##----------------------------------------------------------------------
## Visualização
library(lattice)
xyplot(ganhopeso ~ racoes, data = da)
xyplot(ganhopeso ~ racoes, data = da, type = c("p", "a"))
bwplot(ganhopeso ~ racoes, data = da) # pouco útil pelas poucas repetições

##----------------------------------------------------------------------
## Ajuste do modelo
m0 <- lm(ganhopeso ~ racoes, data = da)

## ANOVA e efeitos
anova(m0)   # quadro de análise de variância
summary(m0) # quadro de estimativas dos parâmetros/efeitos
with(da, tapply(ganhopeso, racoes, mean))

##----------------------------------------------------------------------
## E SE... tivessem apenas 2 racoes? O que voce faria? De que forma?

## Carrega a funcao subsetDropAll para fazer o subset
## Disponível em:
## http://leg.ufpr.br/~fernandomayer/aulas/ce074/scripts/subsetDropAll.R
source("scripts/subsetDropAll.R")
da2 <- subsetDropAll(da, racoes %in% c("A", "B"))
str(da2)

## Teste t para duas amostras (nao pareado), assumindo que as variancias
## entre os dois grupos sao iguais (suposicao de homcedasticidade da
## ANOVA)
teste <- t.test(ganhopeso ~ racoes, data = da2, var.equal = TRUE)
teste

## Mesmo assim, podemos fazer uma ANOVA so com 2 niveis?
## Por que nao?
m.teste <- lm(ganhopeso ~ racoes, data = da2)
anova(m.teste)
summary(m.teste)
## Qual a diferenca? Nenhuma!
## O teste t é um caso particular de uma ANOVA com um fator de 2
## niveis.
model.matrix(m.teste)

## Ajuste na mao
y <- da2$ganhopeso
X <- matrix(0, nrow = 10, ncol = 2)
X[cbind(seq_along(da2$racoes), da2$racoes)] <- 1
(X <- cbind(1, X))
## X'
Xt <- t(X)
## X'X
(Xt %*% X)
## (X'X)^-1
solve(Xt %*% X)
MASS::fractions(solve(Xt %*% X))
Xt %*% y
solve(Xt %*% X) %*% Xt %*% y

## Remove segunda coluna = assumir que o primeiro nivel do fator é
## igual a zero
(X <- X[, -2])
## X'
Xt <- t(X)
## X'X
(Xt %*% X)
## (X'X)^-1
solve(Xt %*% X)
solve(Xt %*% X) %*% Xt %*% y
tapply(da2$ganhopeso, da2$racoes, mean)

##----------------------------------------------------------------------
## Diferentes tipos de contrastes
contrasts(C(da2$racoes, treatment)) # (default) primeiro nível tem efeito 0
contrasts(C(da2$racoes, SAS))       # o último nível tem efeito 0
contrasts(C(da2$racoes, sum))       # soma dos efeitos igual zero
options()$contrasts # aqui você verifica o que tá sendo usado por default

## Contraste SAS
m.teste.sas <- lm(ganhopeso ~ racoes, data = da2,
                  contrasts = list(racoes = contr.SAS))
anova(m.teste.sas)
summary(m.teste.sas)
model.matrix(m.teste.sas)

## Contraste soma zero
m.teste.sum <- lm(ganhopeso ~ racoes, data = da2,
                  contrasts = list(racoes = contr.sum))
anova(m.teste.sum)
summary(m.teste.sum)
model.matrix(m.teste.sum)

mean(da2$ganhopeso)
mean(da2$ganhopeso) + coef(m.teste.sum)[2]
mean(da2$ganhopeso) - coef(m.teste.sum)[2]
tapply(da2$ganhopeso, da2$racoes, mean)
##----------------------------------------------------------------------
