# Lista de Exercícios Parte 2 - Capítulo 11

# Diretório de trabalho
setwd("/Users/ls_rafael/Documents/GitHub/prevendo_ocorrencia_de_cancer/Exercicios")

# Regressão Linear
# Definição do Problema: Prever as notas dos alunos com base em diversas métricas
# https://archive.ics.uci.edu/ml/datasets/Student+Performance
# Dataset com dados de estudantes
# Vamos prever a nota final (grade) dos alunos

# Setup Libraries 
# install.packages("ggplot2")
# install.packages("ggthemes")
# install.packages("dplyr")
library(ggplot2)
library(ggthemes)
library(dplyr)
library(psych)
library(gmodels)

# Carregando o dataset
df <- read.csv2('estudantes.csv')
df2 <- read.csv2('estudantes.csv')
df3 <- read.csv2('estudantes.csv')
df4 <- read.csv2('estudantes.csv')

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
df2$index <- 1:nrow(df2)
df3$index <- 1:nrow(df3)
df4$index <- 1:nrow(df4)

# Divisão do data frame
df_treino <- df %>% dplyr::sample_frac(.75)
df_treino2 <- df %>% dplyr::sample_frac(.70)
df_treino3 <- df %>% dplyr::sample_frac(.60)
df_treino4 <- df %>% dplyr::sample_frac(.65)

df_teste <- dplyr::anti_join(df, df_treino, by = 'index')
df_teste2 <- dplyr::anti_join(df, df_treino2, by = 'index')
df_teste3 <- dplyr::anti_join(df, df_treino3, by = 'index')
df_teste4 <- dplyr::anti_join(df, df_treino4, by = 'index')

# Remoção do indice nos novos data frames
df_treino$index <- NULL
df_treino2$index <- NULL
df_treino3$index <- NULL
df_treino4$index <- NULL

df_teste$index <- NULL
df_teste2$index <- NULL
df_teste3$index <- NULL
df_teste4$index <- NULL

# Treino do modelo_v1
modelo_v1 <- lm(G3 ~., data = df_treino)
modelo_v1.2 <- lm(G3 ~., data = df_treino2)
modelo_v1.3 <- lm(G3 ~., data = df_treino3)
modelo_v1.4 <- lm(G3 ~., data = df_treino4)

summary(modelo_v1) # Precisão: 83.57%
summary(modelo_v1.2) # Precisão: 84.26%
summary(modelo_v1.3) # Precisão: 86.68%
summary(modelo_v1.4) # Precisão: 83.21%

# Conforme o resumo do modelo_v1, a versão abaixo segue apenas com as variáveis significantes ao modelo
modelo_v2 <- lm(G3 ~ age + romantic + famrel + Walc + absences + G1 + G2, data = df_treino)
modelo_v2.2 <- lm(G3 ~ age + romantic + famrel + Walc + absences + G1 + G2, data = df_treino2)
modelo_v2.3 <- lm(G3 ~ age + romantic + famrel + Walc + absences + G1 + G2, data = df_treino3)
modelo_v2.4 <- lm(G3 ~ age + romantic + famrel + Walc + absences + G1 + G2, data = df_treino4)

summary(modelo_v2) # Precisão: 84.08%
summary(modelo_v2.2) # Precisão: 82.55%
summary(modelo_v2.3) # Precisão: 83.32%
summary(modelo_v2.4) # Precisão: 80.68%

# Removendo a variável menos significante Walc
modelo_v4 <- lm(G3 ~ age + romantic + famrel + absences + G1 + G2, data = df_treino)
modelo_v4.2 <- lm(G3 ~ age + romantic + famrel + absences + G1 + G2, data = df_treino2)
modelo_v4.3 <- lm(G3 ~ age + romantic + famrel + absences + G1 + G2, data = df_treino3)

summary(modelo_v4) # Precisão: 84.08%
summary(modelo_v4.2) # Precisão: 82.47%
summary(modelo_v4.3) # Precisão: 83.22%

# Removendo as variáveis menos significante age, romantic e famrel
modelo_v5 <- lm(G3 ~ absences + G1 + G2, data = df_treino)
modelo_v5.2 <- lm(G3 ~ absences + G1 + G2, data = df_treino2)

summary(modelo_v5) # Precisão: 83.32%
summary(modelo_v5.2) # Precisão: 81.69%

# Removendo a variávei menos significante G1
modelo_v6 <- lm(G3 ~ absences + G2, data = df_treino)
modelo_v6.2 <- lm(G3 ~ absences + G2, data = df_treino2)

summary(modelo_v6) # Precisão: 83.00%
summary(modelo_v6.2) # Precisão: 81.27%

# Remoção de romantic
modelo_v7 <- lm(G3 ~ age + famrel + absences + G1 + G2, data = df_treino)
modelo_v7.1 <- lm(G3 ~ age + famrel + absences + G1 + G2, data = df_treino2)

summary(modelo_v7) # Precisão: 83.89%
summary(modelo_v7.1) # Precisão: 82.37%

# O modelo_v1.3 apresentou o melhor desempenho

# Análise dos resíduos do modelo_v4
res_modelo_v4 <- as.data.frame(resid(modelo_v4))
res_modelo_v1.2 <- as.data.frame(resid(modelo_v1.2))
res_modelo_v1.3 <- as.data.frame(resid(modelo_v1.3))

colnames(res_modelo_v4) <- 'res'
colnames(res_modelo_v1.2) <- 'res'
colnames(res_modelo_v1.3) <- 'res'

dev.off()
ggplot(res_modelo_v4,aes(res, fill = ..count..)) + geom_histogram(bins = 30)
ggplot(res_modelo_v1.2,aes(res, fill = ..count..)) + geom_histogram(bins = 30)
ggplot(res_modelo_v1.3,aes(res, fill = ..count..)) + geom_histogram(bins = 30)
# Aqui mostram notas previstas abaixo de zero, o que não é possível. É necessário investigar mais

# Checar os resíduos com plot de diagnóstico
# Plot Diagnostics for an lm Object
?plot.lm

par(mfrow = c(2,2))
plot(modelo_v4)
plot(modelo_v1.2)

# Como o modelo_v1.3 apresentou o melhor desempenho, segue abaixo a análise do diagnóstico dos resíduos
# https://data.library.virginia.edu/diagnostic-plots/
# http://analyticspro.org/2016/03/07/r-tutorial-how-to-use-diagnostic-plots-for-regression-models/ 
#
# Residuals vs Fitted: Como os resíduos não estão igualmente espalhados em torno da linha horizontal e 
#                      ao que parece existe um padrão distinto. É uma indicação que existem relacionamentos não lineares.
#                      As observações numerdas como 55, 143 e 18 parecem se destacar no plot. 
# Normal Q-Q: Há um desvio severo da linha normal
# Scale-Location: Os pontos parecem mais ou menos espalhados randomicamente, nas não de forma perfeita
# Residuals vs Leverage: Não há casos influentes, mal é possível visualizar a linha de distância de Cook

plot(modelo_v1.3)
# Precisão do modelo linear: 86.68%

#### 3. Execução das Predições ####

predicao_v1 <- predict(modelo_v1.3)
View(predicao_v1)

predicao_v2 <- predict(modelo_v1.3,df_teste3)
View(predicao_v2)




