##======================================================================
## Análise completa de um experimento fatorial 2^2
##======================================================================

##----------------------------------------------------------------------
## Usando o exemplo do catalisador
url <- "http://leg.ufpr.br/~fernandomayer/data/temp_cat.csv"
dados <- read.csv(url)
str(dados)
dados$Temperatura <- as.factor(dados$Temperatura)
str(dados)

##----------------------------------------------------------------------
## Análise exploratória

## Gráficos de interação
with(dados,
     interaction.plot(Catalisador, Temperatura, Rendimento, mean))
wireframe(Rendimento ~ Catalisador * Temperatura, data = dados)
levelplot(Rendimento ~ Catalisador * Temperatura, data = dados)

##======================================================================
## Análise usando funções prontas

## Usando lm() com contraste soma zero
m0 <- lm(Rendimento ~ Catalisador * Temperatura, data = dados,
         contrasts = list(Temperatura = contr.sum,
                          Catalisador = contr.sum))
summary(m0)

## Usando aov() para poder calcular efeitos
m0aov <- aov(Rendimento ~ Catalisador * Temperatura, data = dados)
summary(m0aov)
## Exatamente igual a:
anova(m0)

## Cálculo da média geral e médias marginais
model.tables(m0aov, type = "means")

## Cálculo dos efeitos (variação de 0 a 1)
model.tables(m0aov, type = "effects")
## Efeitos de Yates (variação de -1 a 1)
#dae::yates.effects(m0aov, data = dados)
## A metade dos efeitos de Yates são os coeficientes do modelo
#dae::yates.effects(m0aov, data = dados)/2
## É o mesmo que
coef(m0)
## OBS.: Os sinais são trocados pois o "contr.sum" do R codifica o nível
## baixo com +1 e o nível alto com -1, que é o contrário da ordem de
## Yates
model.matrix(m0)

##======================================================================
## Análise fazendo os cálculos manualmente

##----------------------------------------------------------------------
## Componentes da ANOVA

## De maneira geral os efeitos podem ser calculados como
## ef = contraste/[r 2^(k-1)]
## onde o contraste depende do tratamento (veja a representação
## geométrica), r é o número de repetições e k o número de tratamentos.
## Pode-se mostrar que a soma de quadrados (SQ) para qualquer tratamento é
## SQ = contraste^2/[r 2^k]
## Sabemos que a SQ total pode ser decomposta em
## SQTOT = SQTrat + SQRES
## SQTOT = SQA + SQB + SQAB + SQRES
## onde SQTOT é a soma de quadrados total:
## SQTOT = \sum\sum\sum y_{ijk}^2 - [(\sum y_{ijk})^2/n]
## E SQRES é obtida por diferença:
## SQRES = SQTOT - SQA - SQB - SQAB
## Os graus de liberdade associados a soma de quadrados, podem ser
## decompostos em:
## SQTOT = SQA + SQB + SQAB + SQRES
## abr - 1 = (a - 1) + (b - 1) + (a - 1)(b - 1) + ab(r - 1)

## Assim, podemos calcular os componentes da ANOVA:

## Definições do experimento -------------------------------------------
k <- 2
a <- 2
b <- 2
r <- 2
n <- r * 2^k

## Totais --------------------------------------------------------------
## Para usar a representação geométrica e as equações derivadas dela,
## utilizam-se os totais das combinações dos tratamentos quando existe
## repetição
tot <- aggregate(Rendimento ~ Catalisador + Temperatura,
                 data = dados, FUN = sum)
tot <- tot$Rendimento
names(tot) <- c("(1)", "a", "b", "ab")
## Como fica a representacao geometrica?

## Contrastes ----------------------------------------------------------
(contrA <- (tot["a"] + tot["ab"] - tot["b"] - tot["(1)"]))
(contrB <- (tot["b"] + tot["ab"] - tot["a"] - tot["(1)"]))
(contrAB <- (tot["ab"] + tot["(1)"] - tot["a"] - tot["b"]))

## Somas de quadrados --------------------------------------------------
(sqA <- contrA^2/(r*2^k))
(sqB <- contrB^2/(r*2^k))
(sqAB <- contrAB^2/(r*2^k))
(sqTOT <- sum(dados$Rendimento^2) - (sum(dados$Rendimento)^2/n))
(sqRES <- sqTOT - sqA - sqB - sqAB)

## Graus de liberdade --------------------------------------------------
(glA <- a - 1)
(glB <- b - 1)
(glAB <- (a-1)*(b-1))
(glRES <- a*b*(r-1))

## Médias quadráticas --------------------------------------------------
(mqA <- sqA/glA)
(mqB <- sqB/glB)
(mqAB <- sqAB/glAB)
(mqRES <- sqRES/glRES)

## Valores F -----------------------------------------------------------
(fA <- mqA/mqRES)
(fB <- mqB/mqRES)
(fAB <- mqAB/mqRES)

## Valores p -----------------------------------------------------------
(pA <- pf(fA, df1 = glA, df2 = glRES, lower.tail = FALSE))
(pB <- pf(fB, df1 = glB, df2 = glRES, lower.tail = FALSE))
(pAB <- pf(fAB, df1 = glAB, df2 = glRES, lower.tail = FALSE))

## Tabela de ANOVA -----------------------------------------------------
tab.anova <- data.frame("GL" = c(glA, glB, glAB, glRES),
                        "SQ" = c(sqA, sqB, sqAB, sqRES),
                        "QM" = c(mqA, mqB, mqAB, mqRES),
                        "F" = c(fA, fB, fAB, NA),
                        "p-valor" = c(pA, pB, pAB, NA),
                        row.names = c("Catalisador", "Temperatura",
                                      "Catalisador:Temperatura",
                                      "Resíduo"))
tab.anova
## É o mesmo que
summary(m0aov)

##----------------------------------------------------------------------
## Cálculo dos efeitos, coeficientes dos efeitos e erros-padrões

## Cálculo dos efeitos -------------------------------------------------
## Já vimos que os efeitos de qualquer tratamento são calculados por
## ef = contraste/[r 2^(k-1)]
(efA <- contrA/(r*2^(k-1)))
(efB <- contrB/(r*2^(k-1)))
(efAB <- contrAB/(r*2^(k-1)))
## Portanto, os efeitos são
c(efA, efB, efAB)
## Que são os efeitos de Yates como calculados anteriormente
#dae::yates.effects(m0aov, data = dados)

## Coeficientes dos efeitos --------------------------------------------
## Ja vimos que a relacão entre os efeitos e os coefiecientes dos
## efeitos é dada por
## coef.ef = ef/2
## pois o efeito é medido em uma distância de -1 a 1, e os coeficientes
## medem a variação da resposta (Y) em uma unidade de variação em X.
(coefA <- efA/2)
(coefB <- efB/2)
(coefAB <- efAB/2)
## Portanto, os coefiecientes dos efeitos são
c(coefA, coefB, coefAB)
## Que são exatamente os efeitos de Yates divididos por 2
#dae::yates.effects(m0aov, data = dados)/2
## E os efeitos calculados com model.tables()
model.tables(m0aov, type = "effects")
## E os coeficientes do modelo ajustado pela lm() com "contr.sum" (com
## sinais trocados)
coef(m0)

## Erro Padrão dos coeficientes dos efeitos ----------------------------
## Sabemos que, de maneira geral, a variância das estimativas dos
## parâmetros de um modelo linear é dada por
## VAR[\hat{\beta}] = \hat{\sigma^2} (X'X)^-1
## onde \hat{\sigma^2} é obtido pelo QMRes, ou seja,
## \hat{\sigma^2} = QMRes
(sigma2 <- mqRES)
(sigma <- sqrt(sigma2))
## Pode-se mostrar que, para experimentos fatorias 2^k, todos os efeitos
## possuem a mesma precisão, que é dada por
## VAR[\hat{\beta}] = \hat{\sigma^2} (1/r2^k)
## Como o erro-padrão (EP) é a raíz quadrada da variância, então
## EP[\hat{\beta}] = \hat{\sigma} \sqrt{(1/r2^k)}
## Assim, podemos calcular os EPs como
(epA <- sigma * sqrt((1/(r * 2^k))))
(epB <- sigma * sqrt((1/(r * 2^k))))
(epAB <- sigma * sqrt((1/(r * 2^k))))
## Portanto
c(epA, epB, epAB)
## É identico a
summary(m0)$coefficients[, "Std. Error"]

## Note que, nesse caso, os EPs estão associados aos coeficientes dos
## efeitos, que medem a variação entre 0 e 1. Como os efeitos são duas
## vezes os coeficientes, então o EP dos efeitos é obtido pela
## multiplicação dos EPs dos coeficientes por 2:
c(epA, epB, epAB)*2
## Os EPs dos efeitos também podem ser calculados com a função
## se.contrast() aplicada a objetos da classe `aov`:
se.contrast(m0aov, list(Catalisador == "A", Catalisador == "B"),
            data = dados)
se.contrast(m0aov, list(Temperatura == "40", Temperatura == "60"),
            data = dados)
se.contrast(m0aov, list((Catalisador == "A" & Temperatura == "40") |
                        (Catalisador == "B" & Temperatura == "60"),
                        (Catalisador == "A" & Temperatura == "60") |
                        (Catalisador == "B" & Temperatura == "40")),
            data = dados)

## Teste t para os coeficientes ----------------------------------------
## Um coeficiente relaciona o fator à resposta, e o interesse está em
## saber se a estimatia do coeficiente é ou não diferente de zero. Um
## teste t pode ser usado para se avaliar a significância de um
## efeito.
## A estatística t para testar \beta = 0 em um experimento 2^k é então,
## t = \hat\beta / EP(coef)
## com graus de liberdade iguais aos da média quadrática dos erros
## (MQRES).
## Assim, os valores t e os p-valores associados podem ser calculados:
(tA <- coefA/epA)
(pA <- pt(abs(tA), df = glRES, lower.tail = FALSE) * 2)
(tB <- coefB/epB)
(pB <- pt(abs(tB), df = glRES, lower.tail = FALSE) * 2)
(tAB <- coefAB/epAB)
(pAB <- pt(abs(tAB), df = glRES, lower.tail = FALSE) * 2)

## Assim, podemos construir a tabela com o resumo do cálculo dos
## coeficientes e respectivos testes de hipótese:
tab.coef <- data.frame(Coeficientes = c(coefA, coefB, coefAB),
                       EP = c(epA, epB, epAB),
                       t = c(tA, tB, tAB),
                       "p-valor" = c(pA, pB, pAB),
                       row.names = c("Catalisador", "Temperatura",
                                     "Catalisador:Temperatura"))
tab.coef
## Que é o mesmo que (com sinais trocados)
summary(m0)$coefficients

##======================================================================
## Análise dos resíduos

## A análise dos resíduos permite verificar se as suposições:
## a) homegeneidade de variâncias
## b) normalidade da variável resposta
## foram atendidas.
## A análise padrão é através dos gráficos:
par(mfrow = c(2, 2))
plot(m0)
par(mfrow = c(1, 1))
## Nesse caso os resultados parecem indicar que nenhuma das suposições
## foram atendidas.
## NOTA: com apenas 8 observações será sempre difícil atender às
## suposições.

## Os resultados indicam que pode ser necessária alguma transformação da
## variável resposta para que as variâncias se tornem homogêneas e a
## distribuição dos dados mais próxima da normal. A transformação de
## Box-Cox pode indicar qual a melhor transformação para os dados
MASS::boxcox(m0)
## Nesse caso, com \lambda próximo de zero, a transformação adequada
## seria com o log. (Não será abordado aqui).

##======================================================================
## Predição

## A predição para experimentos fatoriais nada mais é do que o cálculo
## das médias para cada nível de cada tratamento, ou das médias das
## combinações dos tratamentos no caso das interações.

## Predição das médias para Catalisador
mean(dados$Rendimento) + coefA * -1
mean(dados$Rendimento) + coefA * 1
with(dados, tapply(Rendimento, Catalisador, mean))
## Predição das médias para Temperatura
mean(dados$Rendimento) + coefB * -1
mean(dados$Rendimento) + coefB * 1
with(dados, tapply(Rendimento, Temperatura, mean))
## Predição das médias para todas as combinações
mean(dados$Rendimento) + coefA * -1 + coefB * -1 + coefAB * 1
mean(dados$Rendimento) + coefA * -1 + coefB * 1 + coefAB * -1
mean(dados$Rendimento) + coefA * 1 + coefB * -1 + coefAB * -1
mean(dados$Rendimento) + coefA * 1 + coefB * 1 + coefAB * 1
with(dados, tapply(Rendimento, Catalisador:Temperatura, mean))

## Também é possível observar a PROJEÇÃO dos dados em termos de seus
## efeitos. Essa projeção é dada por uma tabela, com o mesmo número de
## linhas dos dados, onde cada linha é uma combinação linear da média
## geral com os efeitos estimados. No R usamos a função proj():
proj(m0)
## Para ficar mais claro, podemos combinar as informações dessa matriz
## com os dados observados:
proj.m0 <- cbind(proj(m0),
                 Rendimento = dados$Rendimento,
                 Média = apply(proj(m0)[, -5], 1, sum))
row.names(proj.m0) <- paste(dados$Catalisador, dados$Temperatura,
                             sep = ":")
proj.m0
## Removendo as linhas duplicadas
proj.m0[duplicated(row.names(proj.m0)),] # 1a repetição
proj.m0[!duplicated(row.names(proj.m0)),] # 2a repetição

## Como estamos codificando os níveis dos fatores como -1 e 1, então
## podemos teoricamente fazer a predição para qualquer valor nesse
## intervalo. podemos criar uma "malha" de valores entre -1 e 1, e fazer
## a predição para essa malha, da mesma forma como mostrado acima. No
## entanto, para tornar o cálculo mais eficiente podemos seguir a
## abordagem abaixo:
## Cria novo data frame com codificação -1 (baixo) e 1 (alto)
dados.new <- data.frame(
    Temperatura = ifelse(dados$Temperatura == "40", -1, 1),
    Catalisador = ifelse(dados$Catalisador == "A", -1, 1),
    Rendimento = dados$Rendimento)
## Ajusta o modelo com essa codificação, que é exatamente igual a
## declarar os contrastes soma zero (`contr.sum`)
m0.new <- lm(Rendimento ~ Catalisador * Temperatura, data = dados.new)
summary(m0.new) # Exatamente igual, mas com os sinais trocados
## Cria um grid regular com valores equiespaçados entre -1 e 1
pred <- expand.grid(Catalisador = seq(-1, 1, length = 20),
                    Temperatura = seq(-1, 1, length = 20))
## Faz o cálculo das predições para esse grid
pred$y <- predict(m0.new, newdata = pred)
## E existem várias formas de visualizar as predições
wireframe(y ~ Catalisador + Temperatura, data = pred)
wireframe(y ~ Catalisador + Temperatura, data = pred, drape = TRUE)
levelplot(y ~ Catalisador + Temperatura, data = pred)
levelplot(y ~ Catalisador + Temperatura, data = pred, cuts = 90)
levelplot(y ~ Catalisador + Temperatura, data = pred, cuts = 90,
          col.regions = heat.colors)

##======================================================================
## Testes de Tukey para os efeitos

## Assim como para experimentos com um único tratamento, o teste de
## Tukey para as diferenças entre médias pode ser aplicado também para
## experimentos fatorias.
TukeyHSD(m0aov)
## Note que as diferenças entre médias dos efeitos principais são os
## efeitos como já calculamos
#dae::yates.effects(m0aov, data = dados)
## E as diferenças individuais são as diferenças entre cada combinação
## de tratamentos
model.tables(m0aov, type = "means")

## Como a interação foi significativa (pela ANOVA), devemos olhar a
## comparação entre as médias das combinações de tratamentos. Para
## entender como funciona vamos analisar as comparaçoes dos efeitos
## principais primeiro. Lembre que a estatística de teste de Tukey é
## calculada como:
## W(\alpha %) = q_{t,v,alpha} * 2EP(diff)
## onde q_{t,v,alpha} é a amplitude estudentizada com t = número de
## médias sendo comparadas, v = graus de liberdade residual e alpha é o
## nível de significância. O EP(diff) é o Erro-padrão da diferença entre
## as médias, que no nosso caso é 2 vezes o EP calculado para os
## coeficientes dos efeitos. Portanto:

## Teste de Tukey para Catalisador:
## Diferenças entre médias
with(dados, tapply(Rendimento, Catalisador, mean))
dist(with(dados, tapply(Rendimento, Catalisador, mean)))
## Estatística de Tukey
qtukey(p = 0.95, nmeans = 2, df = glRES) * (epA * 2)

## Teste de Tukey para Temperatura:
## Diferenças entre médias
with(dados, tapply(Rendimento, Temperatura, mean))
dist(with(dados, tapply(Rendimento, Temperatura, mean)))
## Estatística de Tukey
qtukey(p = 0.95, nmeans = 2, df = glRES) * (epB * 2)

## Teste de Tukey para Catalisador:Temperatura:
## Diferenças entre médias
with(dados, tapply(Rendimento, Catalisador:Temperatura, mean))
dist(with(dados, tapply(Rendimento, Catalisador:Temperatura, mean)))
## Estatística de Tukey
qtukey(p = 0.95, nmeans = 4, df = glRES) * (epA * 2)

## O resultado também pode ser visto graficamente
par(mfrow = c(1, 3))
plot(TukeyHSD(m0aov))
par(mfrow = c(1, 1))

## Podemos então fazer um gráfico com as médias
layout(matrix(c(1, 2, 3, 3), 2, 2, byrow = TRUE))
with(dados, barplot(tapply(Rendimento, Catalisador, mean),
                    ylim = c(0, 90),
                    main = "Catalisador"))
with(dados, barplot(tapply(Rendimento, Temperatura, mean),
                    ylim = c(0, 90),
                    main = "Temperatura"))
with(dados, barplot(tapply(Rendimento, Catalisador:Temperatura, mean),
                    ylim = c(0, 90),
                    main = "Catalisador:Temperatura"))
par(mfrow = c(1, 1))
