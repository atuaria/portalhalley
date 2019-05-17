##======================================================================
## Resposta dos exercícios da página fatorial_pontos-centrais.html
##======================================================================

##======================================================================
## ----- ex1
url <- "http://leg.ufpr.br/~fernandomayer/data/montgomery_ex_14-23.txt"
dados <- read.table(url, header = TRUE)
str(dados)

## Definições
k <- 2
a <- b <- 2
r <- 1
n <- (r*2^k)
y <- dados$y

##----------------------------------------------------------------------
## Montando a tabela de sinais
tab <- model.matrix(~ A * B, data = dados)
tab

##----------------------------------------------------------------------
## Cálculo dos contrastes e efeitos
## Contrastes
(contr <- t(tab[, -1]) %*% y)
## Efeitos
(ef <- contr/(r*2^(k-1)))

##----------------------------------------------------------------------
## Gráfico de probabilidade normal dos efeitos estimados
qqaux <- qqnorm(ef, col = 2, pch = 19); qqline(ef)
text(qqaux$x, qqaux$y, rownames(qqaux$y), cex = 0.8, pos = 3)

##----------------------------------------------------------------------
## Ajuste e avaliação do modelo
m0 <- lm(y ~ (A + B)^2, data = dados)
anova(m0)
## Remove termos não importantes
m1 <- update(m0, . ~ . -A:B)
anova(m1)
## Avalia se existe falta de ajuste
dados$lof <- dados$A^2
m2 <- update(m1, . ~ . + lof)
anova(m2)
## Teste de razão de verossimilhança
anova(m1, m2)
## Não há falta de ajuste e a análise pode prosseguir

##======================================================================
## ----- ex2
url <- "http://leg.ufpr.br/~fernandomayer/data/montgomery_ICEQ_ex_13.9.csv"
dados <- read.table(url, header = TRUE, sep = ",")
str(dados)

## Definições
k <- 4
a <- b <- c <- d <- 2
r <- 1
n <- (r*2^k)
y <- dados$y

##----------------------------------------------------------------------
## Montando a tabela de sinais
tab <- model.matrix(~ A * B * C * D, data = dados)
tab

##----------------------------------------------------------------------
## Cálculo dos contrastes e efeitos
## Contrastes
(contr <- t(tab[, -1]) %*% y)
## Efeitos
(ef <- contr/(r*2^(k-1)))

##----------------------------------------------------------------------
## Gráfico de probabilidade normal dos efeitos estimados
qqaux <- qqnorm(ef, col = 2, pch = 19); qqline(ef)
text(qqaux$x, qqaux$y, rownames(qqaux$y), cex = 0.8, pos = 3)

##----------------------------------------------------------------------
## Ajuste e avaliação do modelo
m0 <- lm(y ~ (A + B + C + D)^2, data = dados)
anova(m0)
## Mantém termos importantes (ou aparentemente)
m1 <- update(m0, . ~ A*D + B*C)
anova(m1)
## Verifica se a interação B:C é realmente necessária
m2 <- update(m1, . ~ . - B*C)
anova(m2)
## Faz o TRV
anova(m1, m2)
## Não é importante, por isso fica com m2
## Avalia se existe falta de ajuste
dados$lof <- dados$A^2
m3 <- update(m2, . ~ . + lof)
anova(m3)
## Teste de razão de verossimilhança
anova(m2, m3)
## Não há falta de ajuste e a análise pode prosseguir
