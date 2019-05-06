# Lista de Exercícios Parte 1

# Configurando o diretório de trabalho
# Coloque entre aspas o diretório de trabalho que você está usando no seu computador
# Não use diretórios com espaço no nome
setwd("/Users/ls_rafael/Documents/GitHub/prevendo_ocorrencia_de_cancer/Exercicios")
getwd()


## Exercício 1 - Massa de dados aleatória

# Criando a massa de dados (apesar de aleatória, y possui uma relação com os dados de x)
x <- seq(0, 100)
y <- 2 * x + 35

# Imprimindo as variáveis
x
y

# Gerando uma distribuição normal
?rnorm
y1 <- y + rnorm(n = 101, mean = 0, sd = 50)
y1

summary(y1)
hist(y1)

# Crie um plot do relacionamento de x e y1
?plot
plot(x, y1, pch = 19, main = 'Correlação entre X e Y1', xlab = 'x', ylab = 'y1')

cor(x,y1)

# Unindo as variáveis em um data frame
df_xy1 <- data.frame(x,y1)
cor(df_xy1) # Relação forte de 77.29%

pairs(df_xy1)

library(corrplot)
corrplot(cor(df_xy1),method = 'number')
corrplot(cor(df_xy1),method = 'pie')

# Crie um modelo de regressão para as duas variáveis x e y1
?lm
modelo_xy1 <- lm(y1 ~., data = df_xy1)

# Capture os coeficentes
a <- modelo_xy1$coefficients[1]
b <- modelo_xy1$coefficients[2]

# Fórmula de Regressão
y2 <- a + b*x

# Visualize a linha de regressão
?lines

plot(df_xy1)
lines(y2, x, lwd=3)

# Simulando outras possíveis linhas de regressão
y3 <- (y2[51]-50*(b-1))+(b-1)*x
y4 <- (y2[51]-50*(b+1))+(b+1)*x
y5 <- (y2[51]-50*(b+2))+(b+2)*x
lines(x,y3,lty=3)
lines(x,y4,lty=3)
lines(x,y5,lty=3)

## Exercício 2 - Pesquisa sobre idade e tempo de reação

# Criando os dados
Idade <- c(9,13,14,21,15,18,20,8,14,23,
           16,21,10,12,20,9,13,5,15,21)

Tempo <- c(17.87,13.75,12.72,6.98,11.01,10.48,10.19,
           19.11,12.72,0.45,10.67,1.59,14.91,14.14,
           9.40,16.23,12.74,20.64,12.34,6.44)

# Unindo os vetores em um data frame
df_ex2 <- data.frame(Idade,Tempo)

# Crie um Gráfico de Dispersão (ScatterPlot)
plot(df_ex2,main = 'Pesquisa Sobre Idade e Tempo de Reação', ylab = 'Tempo de Reação', xlab = 'Idade')

# Checar correlação
cor(df_ex2) # Forte correlação negativa de -93.95%

# Crie um modelo de regressão
modelo_ex2 <- lm(Tempo ~ Idade, data = df_ex2)

summary(modelo_ex2)

# Calcule a reta de regressão
# y <- a + b*x

# Extração dos coeficientes
a <- modelo_ex2$coefficients[1]
b <- modelo_ex2$coefficients[2]

rt <- a + b * df_ex2$Idade

# Crie o gráfico da reta
lines(rt,Idade)


# Exercício 3 - Relação entre altura e peso

# Criando os dados
alturas = c(176, 154, 138, 196, 132, 176, 181, 169, 150, 175)
pesos = c(82, 49, 53, 112, 47, 69, 77, 71, 62, 78)

plot(alturas, pesos, pch = 16, cex = 1.3, col = "blue", 
     main = "Altura x Peso", 
     ylab = "Peso Corporal (kg)", 
     xlab = "Altura (cm)")

# Crie o modelo de regressão
cor(alturas, pesos)

df_ex3 <- data.frame(alturas,pesos)

modelo_ex3 <- lm(df_ex3$pesos ~ df_ex3$alturas, data = df_ex3)

# Visualizando o modelo
summary(modelo_ex3)

# Gere a linha de regressão
a <- modelo_ex3$coefficients[1]
b <- modelo_ex3$coefficients[2]

rt <- a + b * df_ex3$pesos

abline(a,b)

# Faça as previsões de pesos com base na nova lista de alturas
alturas2 = data.frame(c(179, 152, 134, 197, 131, 178, 185, 162, 155, 172))

predicao_pesos <- predict(modelo_ex3, alturas2)

# Plot
plot(alturas, pesos, pch = 16, cex = 1.3, 
     col = "blue", 
     main = "Altura x Peso", 
     ylab = "Peso (kg)", 
     xlab = "Altura (cm)")

# Construindo a linha de regressão
abline(lm(pesos ~ alturas)) 

# Obtendo o tamanho de uma das amostras de dados
num <- length(alturas)
num

# Gerando um gráfico com os valores residuais
for (k in 1: num)  
  lines(c(alturas[k], alturas[k]), 
        c(pesos[k], pesos[k]))

# Gerando gráficos com a distribuição dos resíduos
par(mfrow = c(2,2))
plot(modelo_ex3)
