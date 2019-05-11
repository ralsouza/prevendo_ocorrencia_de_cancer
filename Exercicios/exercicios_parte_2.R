# Lista de Exercícios Parte 2 - Capítulo 11

# Diretório de trabalho
setwd("/Users/ls_rafael/Documents/GitHub/prevendo_ocorrencia_de_cancer/Exercicios")

# Regressão Linear
# Definição do Problema: Prever as notas dos alunos com base em diversas métricas
# https://archive.ics.uci.edu/ml/datasets/Student+Performance
# Dataset com dados de estudantes
# Vamos prever a nota final (grade) dos alunos

# install.packages("ggplot2")
# install.packages("ggthemes")
# install.packages("dplyr")
library(ggplot2)
library(ggthemes)
library(dplyr)
library(psych)

# Carregando o dataset
df <- read.csv2('estudantes.csv')

#### 1. Análise Exploratória ####
head(df)
summary(df)
str(df)
any(is.na(df))

# Checagem da correlação entre as variáveis numéricas
cor.plot(df[c('age','Medu','Fedu','traveltime','studytime','failures','famrel',
              'freetime','goout','Dalc','Walc','health','absences','G1','G2','G3')],numbers = TRUE)

# Análise: A correlação entre as variáveis G1, G2 e G3 possuem correlação positiva forte, em torno de 80~90%
#          Também existe correlação negativa média/moderada entre as variáveis failure e G1, G2 e G3, em torno de 35~36%
#          Correlação positiva média/moderada entre as variáveis Medu e Fedu, de 62%
#          Correlação positiva média/moderada entre as variáveis Dalc e Walc, 65%
#          E uma correlação positiva média/moderada entre as variáveis Walc e goout, 42%

# Verificação por Histograma
# Análise das avaliações G1 e G2
ggplot(df,aes(G1, fill=..count..)) + geom_histogram(bins = 20)
ggplot(df,aes(G2, fill=..count..)) + geom_histogram(bins = 20)

# Análise da G3, variável alvo
ggplot(df,aes(G3, fill=..count..)) + geom_histogram(bins = 20)

# Análise da das faltas (absences), pois no modelo_v1 identificamos que o modelo avaliou esta variável
# como significativa
ggplot(df,aes(absences, fill=..count..)) + geom_histogram(bins = 20)

# Análise da G3: Houveram quase 40 notas 0 na G3, o que será que houve? 
#                Inclusive a quantidade de notas 9 está bem mais alta que nas outras avaliações

#### 2. Criação e Treinamento do Modelo ####
# Divisão dos dados entre treino e teste
?dplyr::sample_frac
?dplyr::anti_join

# Inclusão do indice para divisão
df$index <- 1:nrow(df)

# Divisão do data frame
df_treino <- df %>% dplyr::sample_frac(.75)
df_teste <- dplyr::anti_join(df, df_treino, by = 'index')

# Remoção do indice nos novos data frames
df_treino$index <- NULL
df_teste$index <- NULL

# Treino do modelo_v1
modelo_v1 <- lm(G3 ~., data = df_treino)
summary(modelo_v1) # Precisão: 83.57%

# Conforme o resumo do modelo_v1, a versão abaixo segue apenas com as variáveis significantes ao modelo
modelo_v2 <- lm(G3 ~ age + romantic + famrel + Walc + absences + G1 + G2, data = df_treino)
summary(modelo_v2) # Precisão: 84.08%

# Removendo a variável menos significante Walc
modelo_v4 <- lm(G3 ~ age + romantic + famrel + absences + G1 + G2, data = df_treino)
summary(modelo_v4) # Precisão: 84.08%

# Removendo as variáveis menos significante age, romantic e famrel
modelo_v5 <- lm(G3 ~ absences + G1 + G2, data = df_treino)
summary(modelo_v5) # Precisão: 83.32%

# Removendo a variávei menos significante G1
modelo_v6 <- lm(G3 ~ absences + G2, data = df_treino)
summary(modelo_v6) # Precisão: 83.00%

# Remoção de romantic
modelo_v7 <- lm(G3 ~ age + famrel + absences + G1 + G2, data = df_treino)
summary(modelo_v7) # Precisão: 83.89%

# O modelo_v4 apresentou o melhor desempenho, pois aprensentou a mesma precisão que modelo_v2 
# possuindo uma menos

# Análise dos resíduos do modelo_v4
res_modelo_v4 <- as.data.frame(resid(modelo_v4))

colnames(res_modelo_v4) <- 'res'

plot(df_treino$G3, fitted(res_modelo_v4),
     ylab = 'Resíduos', xlab = 'G3',
     main = 'Análise de Resíduos no modelo_v4')
abline(0,0) # Linha de horizonte
# A média dos resíduos está próxima de zero, o que é bom

ggplot(res_modelo_v4,aes(res, fill = ..count..)) + geom_histogram(bins = 30)
# Aqui mostram notas previstas abaixo de zero, o que não é possível. É necessário investigar mais

par(mfrow = c(2,2))
plot(modelo_v4)





