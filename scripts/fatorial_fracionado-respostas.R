##======================================================================
## Resposta dos exercícios da página fatorial_fracionado.html
##======================================================================

##======================================================================
## ----- ex1
url <- "http://www.leg.ufpr.br/~fernandomayer/data/montgomery_14-41.txt"
dados <- read.table(url, header = TRUE)
dados
## a)
## Escolhendo a relação de definição
## I = ABCDE
## então usamos essa definição como o separador das meia frações. Cria a
## coluna ABCDE
dados$ABCDE <- with(dados, A*B*C*D*E)
## ordena pelos sinais dessa coluna
dados[order(dados$ABCDE, decreasing = TRUE), ]
## As combições com sinal positivo de ABCDE podem ser a fração
## principal, portanto, fazemos a seleção desses elementos
dados <- subset(dados, ABCDE == 1)
## Portanto, as combinaçoes seriam
row.names(dados)

## b)
## As relações associadas devem ser encontradas manualmente, ou usando
## a função abaixo
aliases <- function(ef, cd){
    ali <- function(ef, cd){
        ef <- gsub("\\W", "", ef)
        ef <- unlist(strsplit(ef, ""))
        cd <- gsub("\\W", "", cd)
        cd <- unlist(strsplit(cd, ""))
        x <- sort(c(ef, cd))
        tb <- table(x)
        paste(names(tb[tb==1]), collapse="")
    }
    sapply(ef, simplify=FALSE,
           function(e){
               x <- sapply(cd, function(i){
                   x <- ali(e, i)
               })
               x[order(nchar(x))]
           })
}
## Cria a tabela de sinais
(tab <- model.matrix(~ A*B*C*D*E, data = dados))
## Aplicando a função, encontrando os pares associados
aliases(colnames(tab)[-c(1, 32)], "ABCDE")

## c)
## Como vimos que existem colunas confundidas, vamos estimar os efeitos
## apenas daquelas únicas
(tab <- unique(tab, MARGIN = 2))
## Contrastes
contr <- t(tab[, -1]) %*% dados$Y
## Efeitos = contraste/(r2^{k-1})
## NOTE que agora o k é 4, pois o experimento básico é 2^{5-1} = 2^4
k <- 4
r <- 1
(ef <- contr/(r * 2^(k-1)))

## d)
## Gráfico de probabilidade normal dos efeitos estimados
qqaux <- qqnorm(ef, col = 2, pch = 19); qqline(ef)
text(qqaux$x, qqaux$y, rownames(qqaux$y), cex = 0.8, pos = 3)

## e)
## Os efeitos iportantes foram A, B, C, AB, e DE. Mas para sermos
## conservadores podemos ajustar um modelo com todos menos aqueles
## efeitos que estão exatamente na linha.
m0 <- lm(Y ~ (A + B + C + D + E)^2 - B:D - B:E, data = dados)
anova(m0)
## Mantendo os mais importantes
m1 <- update(m0, . ~ . - A:C - C:D - C:E)
anova(m1)
## Nova avaliação
m2 <- update(m1, . ~ . - A:D - B:C)
anova(m2)
## MAS note que
## DE = ABC
## AE = BCD
## Portanto podemos usar estas interações e eliminar E
m3 <- update(m2, . ~ A + B + C + D + A:B + A:B:C + B:C:D)
anova(m3)
## Agora podemos testar a exclusão de BCD, e por consequencia de D
m4 <- update(m3, . ~ . - D - B:C:D)
anova(m4)
## Fazendo o TRV
anova(m3, m4)
## Portanto, pode-se escolher o modelo com menos parâmetros (m4)

## f)
res <- residuals(m4)
qqnorm(res); qqline(res)

##======================================================================
## ----- ex2
url <- "http://www.leg.ufpr.br/~fernandomayer/data/montgomery_14-14.txt"
dados <- read.table(url, header = TRUE)
dados
## a)
## Escolhendo as relações de definição
## I = ABD
## I = ACE
## então usamos essas definições como o separador das meia-frações.
## Cria as colunas ABD e ACE
dados$ABD <- with(dados, A*B*D)
dados$ACE <- with(dados, A*C*E)
## ordena pelos sinais das duas colunas geradas
dados[order(dados$ABD, dados$ACE, decreasing = TRUE), ]
## As combinações com sinal positivo das duas colunas podem ser a fração
## principal, portanto, fazemos a seleção desses elementos
dados <- subset(dados, ABD == 1 & ACE == 1)
## Portanto, as combinaçoes seriam
row.names(dados)

## b)
## As relaçãoes associadas devem ser encontradas manualmente, ou usando
## a função abaixo
aliases <- function(ef, cd){
    ali <- function(ef, cd){
        ef <- gsub("\\W", "", ef)
        ef <- unlist(strsplit(ef, ""))
        cd <- gsub("\\W", "", cd)
        cd <- unlist(strsplit(cd, ""))
        x <- sort(c(ef, cd))
        tb <- table(x)
        paste(names(tb[tb==1]), collapse="")
    }
    sapply(ef, simplify=FALSE,
           function(e){
               x <- sapply(cd, function(i){
                   x <- ali(e, i)
               })
               x[order(nchar(x))]
           })
}
## Cria a tabela de sinais
(tab <- model.matrix(~ A*B*C*D*E, data = dados))
colnames(tab)
## Note que a interação generalizada de ABD com ACE é
## (ABD)(ACE) = BCDE
## Portanto, a relação completa de definição é
## I = ABD = ACE = BCDE
## Pode conferir com
tab[, which(apply(tab, 2, function(x) all(x == 1)))]
## Aplicando a função, encontrando os pares associados
aliases(colnames(tab)[-1], c("ABD", "ACE", "BCDE"))

## c)
## Como vimos que existem colunas confundidas, vamos estimar os efeitos
## apenas daquelas únicas
(tab <- unique(tab, MARGIN = 2))
## Contrastes
contr <- t(tab[, -1]) %*% dados$y
## Efeitos = contraste/(r2^{k-1})
## NOTE que agora o k é 3, pois o experimento básico é 2^{5-2} = 2^3
k <- 3
r <- 1
(ef <- contr/(r * 2^(k-1)))

## d)
## Gráfico de probabilidade normal dos efeitos estimados
qqaux <- qqnorm(ef, col = 2, pch = 19); qqline(ef)
text(qqaux$x, qqaux$y, rownames(qqaux$y), cex = 0.8, pos = 3)

## e)
## Como existe dúvida sobre quais fatores são importantes pelo gráfico,
## podemos ajustar um modelo com todos os efitos principais e verificar
## quais são significativos
m0 <- lm(y ~ A + B + C + D + E, data = dados)
anova(m0)
## Pelo resultado, podemos remover o fator C
m1 <- update(m0, . ~ . - C)
anova(m1)

## f)
res <- residuals(m1)
qqnorm(res); qqline(res)
