##======================================================================
## Resposta dos exercícios da página confundimento-blocagem.html
##======================================================================

##======================================================================
## ----- ex6
url <- "http://www.leg.ufpr.br/~fernandomayer/data/montgomery_14-37.txt"
dados <- read.table(url, header = TRUE)
dados
##----------------------------------------------------------------------
## a. Quais efeitos estão confundidos com os blocos?
## O efeito ABC está confundido com blocos, onde A = Suco, B =
## Exercicio, C = Intervalo. Para ver isso, codifique cada coluna para
## (-1, 1), faça o produto e veja que o resultado será a codificação de
## Periodo (sendo pm = -1 e am = 1).
## Muda a ordem dos niveis de Periodo
levels(dados$Periodo)
dados$Periodo <- relevel(dados$Periodo, ref = "pm")
levels(dados$Periodo)
## Cria uma função para codificar fatores em (-1, 1)
cod.sum <- function(x){
    baixo <- min(as.numeric(x))
    alto <- max(as.numeric(x))
    cod <- ifelse(as.numeric(x) == baixo, -1, 1)
    return(cod)
}
## Aplica nos dados
dados2 <- data.frame(sapply(dados[, 1:4], cod.sum), dados[, 5])
names(dados2) <- c(LETTERS[1:3], "Bloco", "Glicose")
dados2
## Veja que
cbind(with(dados2, A*B*C), dados2$Bloco)
identical(with(dados2, A*B*C), dados2$Bloco)
##----------------------------------------------------------------------
## b. Analise os dados e tire conclusões.
## Usando dados2 que já está na codificação (-1, 1)
tab <- model.matrix(~ Bloco + (A + B + C)^2, data = dados2)
contr <- t(tab[, -(1:2)]) %*% dados2$Glicose
r <- 1
k <- 3
ef <- contr/(r * 2^(k-1))
aux <- qqnorm(ef, col = 2, pch = 19); qqline(ef)
text(aux$x, aux$y, rownames(aux$y), cex = 0.8, pos = 3)
## Nenhum parece ter efeito, mas podemos fazer a ANOVA excluindo AB que
## está mais próximo da linha
m0 <- lm(Glicose ~ Bloco + (A + B + C)^2 - A:B, data = dados2)
anova(m0)
## Todos os efeitos são pouco significativos.
summary(m0)

##======================================================================
## ----- ex7
url <- "http://leg.ufpr.br/~fernandomayer/data/montgomery_DAE_6-26.txt"
dados <- read.table(url, header = TRUE)
row.names(dados) <- apply(dados, 1,
                       function(i) paste(letters[1:5][i==1], collapse = ""))
row.names(dados)[1] <- "(1)"
dados
##----------------------------------------------------------------------
## a. Estime os efeitos e faça o gráfico normal de probabilidade.
tab <- model.matrix(~ A*B*C*D*E, data = dados)
contr <- t(tab[, -1]) %*% dados$y
r <- 1
k <- 5
ef <- contr/(r * 2^(k-1))
aux <- qqnorm(ef, col = 2, pch = 19); qqline(ef)
text(aux$x, aux$y, rownames(aux$y), cex = 0.8, pos = 3)
##----------------------------------------------------------------------
## b. Faça a ANOVA para confirmar os resultados da letra (a).
## Para ser conservador, considera todos os efeitos de segunda ordem
m0 <- lm(y ~ (A + B + C + D + E)^2, data = dados)
anova(m0)
## Efeitos importantes: A, B, C, AB, e DE
m1 <- update(m0, . ~ A + B + C + D + E + A:B + D:E)
anova(m1)
## Podemos testar a exclusão de DE, que inclui D e E não significativos
m2 <- update(m1, . ~ . - D - E - D:E)
anova(m2)
## TRV
anova(m0, m1, m2)
## Podemos ficar com o modelo mais simples (m2)
##----------------------------------------------------------------------
## c. Avalie os resíduos.
qqnorm(residuals(m2)); qqline(residuals(m2))
##----------------------------------------------------------------------
## d. Interprete os resultados e faça a projeção do experimento se
## necessário.
summary(m2)
## O experimento pode ser projetado para um fatorial 2^3, considerando
## que apenas os fatores A, B, e C foram importantes.

##======================================================================
## ----- ex8
url <- "http://leg.ufpr.br/~fernandomayer/data/montgomery_DAE_6-26.txt"
dados <- read.table(url, header = TRUE)
row.names(dados) <- apply(dados, 1,
                       function(i) paste(letters[1:5][i==1], collapse = ""))
row.names(dados)[1] <- "(1)"
dados
## Usando o contraste de definição
## Antes, converte para a codificação (0,1)
dados[, 1:5] <- ifelse(dados[, 1:5] == -1, 0, 1)
dados
## Como o fator a ser confundido é ABCDE, então
## L = x_1 + x_2 + x_3 + x_4 + x_5
alpha <- c(1, 1, 1, 1, 1)
## Cria a função para calcular os contrastes de definição
contr.def <- function(alpha, x){
    sum(alpha * x) %% 2
}
dados$bloco <- apply(dados[, 1:5], 1, contr.def, alpha = alpha)
dados <- dados[order(dados$bloco), ]
## define bloco como fator
dados$bloco <- as.factor(dados$bloco)
dados
## Croqui do experimento
matrix(row.names(dados), ncol = 2,
       dimnames = list(1:16, paste("Bloco", 1:2)))
## Analise dos dados
## Note que para calcular os contrastes, precisamos dos dados na
## codificação (-1,1)
dados[, 1:5] <- ifelse(dados[, 1:5] == 0, -1, 1)
## Monta a tebela de sinais. Repare que as interações vão até a quata
## ordem, pois a interação de quinta ordem, ABCDE, está confundida com
## bloco, que já está presente no modelo
tab <- model.matrix(~ bloco + (A + B + C + D + E)^4, data = dados)
contr <- t(tab[, -(1:2)]) %*% dados$y
r <- 1
k <- 5
ef <- contr/(r * 2^(k-1))
aux <- qqnorm(ef, col = 2, pch = 19); qqline(ef)
text(aux$x, aux$y, rownames(aux$y), cex = 0.8, pos = 3)

## Para ser conservador, considera todos os efeitos de segunda ordem
## NOte que da forma como bloco foi construido, deve ser especificado o
## contraste soma zero para bloco
m0 <- lm(y ~ bloco + (A + B + C + D + E)^2, data = dados,
         contrasts = list(bloco = contr.sum))
anova(m0)
## Efeitos importantes: A, B, C, AB, e DE
m1 <- update(m0, . ~ bloco + A + B + C + D + E + A:B + D:E)
anova(m1)
## Podemos testar a exclusão de DE, que inclui D e E não significativos
m2 <- update(m1, . ~ . - D - E - D:E)
anova(m2)
## TRV
anova(m0, m1, m2)
## Podemos ficar com o modelo mais simples (m2)
## Analise dos residuos
qqnorm(residuals(m2)); qqline(residuals(m2))
## Coeficientes
summary(m2)

##======================================================================
## ----- ex9
url <- "http://leg.ufpr.br/~fernandomayer/data/montgomery_DAE_6-26.txt"
dados <- read.table(url, header = TRUE)
row.names(dados) <- apply(dados, 1,
                       function(i) paste(letters[1:5][i==1], collapse = ""))
row.names(dados)[1] <- "(1)"
dados
## Usando o contraste de definição, e utilizando ABC e CDE como
## confundimento. Consequentemente temos tambem
## (ABC)(CDE) = ABC²DE = ABDE confundido.
## Antes, converte para a codificação (0,1)
dados[, 1:5] <- ifelse(dados[, 1:5] == -1, 0, 1)
dados
## A interacção ABC tem o contraste de definição
## L_1 = x_1 + x_2 + x_3
alpha1 <- c(1, 1, 1, 0, 0)
L1 <- apply(dados[, 1:5], 1, contr.def, alpha = alpha1)
## E a interação CDE possui o contraste de definção
## L_2 = x_3 + x_4 + x_5
alpha2 <- c(0, 0, 1, 1, 1)
L2 <- apply(dados[, 1:5], 1, contr.def, alpha = alpha2)
## Juntando os pares (L1,L2) e criando os blocos
dados$bloco <- interaction(L1, L2, sep = "") # bloco já é fator
dados <- dados[order(dados$bloco), ]
dados
## Croqui do exprimento
matrix(row.names(dados), ncol = 4,
       dimnames = list(1:8, paste("Bloco", 1:4)))
## Analise dos dados
## Note que para calcular os contrastes, precisamos dos dados na
## codificação (-1,1)
dados[, 1:5] <- ifelse(dados[, 1:5] == 0, -1, 1)
## Monta a tebela de sinais. Note que é feita a tabela com todas as
## interações, menos ABD, CDE, e ABDE, que estão confundidas com bloco,
## e bloco já está no modelo
tab <- model.matrix(~ bloco + (A+B+C+D+E)^5 - A:B:C - C:D:E - A:B:D:E,
                    data = dados)
contr <- t(tab[, -(1:4)]) %*% dados$y
r <- 1
k <- 5
ef <- contr/(r * 2^(k-1))
aux <- qqnorm(ef, col = 2, pch = 19); qqline(ef)
text(aux$x, aux$y, rownames(aux$y), cex = 0.8, pos = 3)
## Para ser conservador, considera todos os efeitos de segunda ordem
## Note que da forma como bloco foi construido, deve ser especificado o
## contraste soma zero para bloco
m0 <- lm(y ~ bloco + (A + B + C + D + E)^2, data = dados,
         contrasts = list(bloco = contr.sum))
anova(m0)
## Efeitos importantes: A, B, C, AB, AE e DE
m1 <- update(m0, . ~ bloco + A + B + C + D + E + A:B + A:E + D:E)
anova(m1)
## Podemos testar a exclusão de AE
m2 <- update(m1, . ~ . - A:E)
anova(m2)
## Podemos testar a exclusão de DE, e consequentemente D e E
m3 <- update(m2, . ~ . - D - E - D:E)
anova(m3)
## TRV
anova(m0, m1, m2, m3)
## Nesse caso podemos ficar com o modelo que considera AE e DE (m1),
## pois existe diferença significativa entre ele e os demais modelos
## (tanto com menos como com mais parâmetros)
## Analise dos residuos
qqnorm(residuals(m1)); qqline(residuals(m1))
## Coeficientes
summary(m1)
## Esse modelo fica com termos diferentes dos anteriores devido à
## inclusão dos blocos, e pelo fato de que os blocos são a composição de
## 3 termos (os efeitos confundidos). Isso alterou a soma de quadrado
## dos resíduos e por consequência o teste F na ANOVA. Isso pode levar à
## diferentes conclusões.

##======================================================================
## ----- ex10
url <- "http://leg.ufpr.br/~fernandomayer/data/montgomery_DAE_6-26.txt"
dados <- read.table(url, header = TRUE)
row.names(dados) <- apply(dados, 1,
                       function(i) paste(letters[1:5][i==1], collapse = ""))
row.names(dados)[1] <- "(1)"
dados
## Usando o contraste de definição, e utilizando ACDE e BCD como
## confundimento. Consequentemente temos tambem
## (ACDE)(BCD) = ABC²D²E = ABE confundido.
## Antes, converte para a codificação (0,1)
dados[, 1:5] <- ifelse(dados[, 1:5] == -1, 0, 1)
dados
## A interacção ACDE tem o contraste de definição
## L_1 = x_1 + x_3 + x_4 + x_5
alpha1 <- c(1, 0, 1, 1, 1)
L1 <- apply(dados[, 1:5], 1, contr.def, alpha = alpha1)
## E a interação BCD possui o contraste de definção
## L_2 = x_2 + x_3 + x_4
alpha2 <- c(0, 1, 1, 1, 0)
L2 <- apply(dados[, 1:5], 1, contr.def, alpha = alpha2)
## Juntando os pares (L1,L2) e criando os blocos
dados$bloco <- interaction(L1, L2, sep = "") # bloco já é fator
dados <- dados[order(dados$bloco), ]
dados
## Croqui do exprimento
matrix(row.names(dados), ncol = 4,
       dimnames = list(1:8, paste("Bloco", 1:4)))
## Analise dos dados
## Note que para calcular os contrastes, precisamos dos dados na
## codificação (-1,1)
dados[, 1:5] <- ifelse(dados[, 1:5] == 0, -1, 1)
## Monta a tebela de sinais. Note que é feita a tabela com todas as
## interações, menos ACDE, BCD, e ABE, que estão confundidas com bloco,
## e bloco já está no modelo
tab <- model.matrix(~ bloco + (A+B+C+D+E)^5 - A:C:D:E - B:C:D - A:B:E,
                    data = dados)
contr <- t(tab[, -(1:4)]) %*% dados$y
r <- 1
k <- 5
ef <- contr/(r * 2^(k-1))
aux <- qqnorm(ef, col = 2, pch = 19); qqline(ef)
text(aux$x, aux$y, rownames(aux$y), cex = 0.8, pos = 3)
## Para ser conservador, considera todos os efeitos de segunda ordem
## Note que da forma como bloco foi construido, deve ser especificado o
## contraste soma zero para bloco
m0 <- lm(y ~ bloco + (A + B + C + D + E)^2, data = dados,
         contrasts = list(bloco = contr.sum))
anova(m0)
## Efeitos importantes: A, B, C, AB, e DE
m1 <- update(m0, . ~ bloco + A + B + C + D + E + A:B + D:E)
anova(m1)
## Podemos testar a exclusão de DE, e consequentemente D e E
m2 <- update(m1, . ~ . - D - E - D:E)
anova(m2)
## TRV
anova(m0, m1, m2)
## Aqui podemos ficar com o modelo que possui menos parâmetros (m2), já
##que ele não difere daquele qie considera mais parâmetros.
## Analise dos residuos
qqnorm(residuals(m2)); qqline(residuals(m2))
## Coeficientes
summary(m2)
## Note que o resultado aqui é diferente (em termos de fatores
## importantes). Essa diferença se dá pelos diferentes contrastes de
## definição escolhidos. Dependendo dos fatores a serem confundidos,
## eles podem ter mais ou menos importância, e isso afeta diretamente a
## soma de quadrados de bloco (que é a combinação destes efeitos), e
## consequentemente na soma de quadrado do resíduo. Como os contrastes de
## definição (efeitos que serão confundidos com blocos) devem ser
## escolhidos ANTES de se realizar o experimento (para que possam se
## determinar quais tratamentos serão corridos em quais blocos), não há
## como saber qual combinação será aquela que produzirá o menor
## confundimento (i.e. menor soma de quadrado de bloco). Se houver algum
## experimento prévio ou piloto, as informações desse experimento podem
## ser utilizadas para se determinar os contrastes de definição de um
## próximo experimento.
