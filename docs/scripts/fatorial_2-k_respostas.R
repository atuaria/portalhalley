##======================================================================
## Respostas dos exercícios da página fatorial_2-k.html
##======================================================================

##----------------------------------------------------------------------
## ----- ex1
url <- "http://leg.ufpr.br/~fernandomayer/data/BHH2/tab0510a.dat"
da <- read.table(url, header = TRUE)
str(da)
## Definições
k <- 4
r <- 2
## Calcula os contrastes e efeitos para poder comparar depois
tab <- model.matrix(~ x1 * x2 * x3 * x4, data = da)
contr <- t(tab[, -1]) %*% da$conversion
ef <- contr/(r * 2^(k-1))
## Soma a constante 5 as observações +1 de x1
da2 <- da
da2$conversion[da2$x1 == 1] <- da2$conversion[da2$x1 == 1] + 5
## Calcula contrastes e efeitos
tab2 <- model.matrix(~ x1 * x2 * x3 * x4, data = da2)
contr2 <- t(tab2[, -1]) %*% da2$conversion
ef2 <- contr2/(r * 2^(k-1))
## Compara os dois contrastes
cbind(contr, contr2)
cbind(ef, ef2)
## Adiciona uma constante (10) a todas as observações
da3 <- da
da3$conversion <- da3$conversion + 10
## Calcula contrastes e efeitos
tab3 <- model.matrix(~ x1 * x2 * x3 * x4, data = da3)
contr3 <- t(tab3[, -1]) %*% da3$conversion
ef3 <- contr3/(r * 2^(k-1))
## Compara todos os contrastes
cbind(contr, contr2, contr3)
cbind(ef, ef2, ef3)
## Compara as médias
c(mean(da$conversion), mean(da2$conversion), mean(da3$conversion))

##----------------------------------------------------------------------
## ----- ex2
url <- "http://leg.ufpr.br/~fernandomayer/data/BHH2/exe0503.dat"
db <- read.table(url, header = TRUE)
str(db)
## Calcula a média das 3 repetições
db$med <- apply(db[, 8:10], 1, mean)
## Definições
k <- 3
r <- 1 # usando a média, é como se tivessemos apenas uma repetição
## Calcula os contrastes e efeitos para poder comparar depois
tab <- model.matrix(~ depth * watering * type, data = db)
contr <- t(tab[, -1]) %*% db$med
ef <- contr/(r * 2^(k-1))
## Se "empilhar" o data frame
library(reshape)
db2 <- melt(db[, c("depth", "watering", "type", "rep1", "rep2", "rep3")],
            id = c("depth", "watering", "type"))
## é necessário fazer a soma das repetições para cada combinação
tot <- aggregate(value ~ depth + watering + type, data = db2, FUN = sum)
## e é necessário ajustar agora o número de repetições
r <- 3
tab2 <- model.matrix(~ depth * watering * type, data = tot)
contr2 <- t(tab2[, -1]) %*% tot$value
ef2 <- contr2/(r * 2^(k-1))
## Compara as duas estimativas
cbind(ef, ef2)
## Para obter o erro, fazemos uma ANOVA
m0 <- lm(value ~ depth * watering * type, data = db2)
anova(m0)
## O QM do resíduo é a estimativa de variância
sigma2 <- anova(m0)[8, "Mean Sq"]
sigma <- sqrt(sigma2)
## Portanto o erro é calculado como
ep.coef <- sigma * sqrt(1/(r*2^k))
## Note que esse é o erro associado aos coeficiemtes dos efeitos. Os
## erros dos efeitos são duas vezes essa quantidade
ep.ef <- 2 * ep.coef
## Confira com o summary:
summary(m0)
coef(m0) # coeficiente dos efeitos
2*coef(m0) # efeitos
ef2

##----------------------------------------------------------------------
## ----- ex3
## Basta alterar a chamada do modelo, onde o bloco aqui é o "variable"
m1 <- lm(value ~ variable + (depth * watering * type), data = db2)
## ANOVA
anova(m0)
anova(m1)
## O que muda na ANOVA é apenas a inclusão do bloco. As somas de
## quadrados dos efeitos continuam as mesmas. O que muda é a soma de
## quadrados dos resíduos, que por consequência vai mudar o QM do
## residuo e a estimativa da variância.
## Efeitos
coef(m1)
coef(m0)
## Os efeitos são os mesmos, com a inclusão dos efeitos de blocos.
## Teste t e erro-padrão
summary(m0)
summary(m1)
## O erro-padrão dos efeitos se altera pois o QM do resíduo foi alterado
## com a inclusão do bloco. OS EPs ficaram menores quando se
## consideraram blocos.
