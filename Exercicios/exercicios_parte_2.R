# Lista de Exercícios Parte 2 - Capítulo 11

# Diretório de trabalho
# OSx
setwd("/Users/ls_rafael/Documents/GitHub/prevendo_ocorrencia_de_cancer/Exercicios")

# Linux
setwd('/home/ralsouza/Documents/r_projects/prevendo_ocorrencia_de_cancer/Exercicios')

# Regressão Linear
# Definição do Problema: Prever as notas dos alunos com base em diversas métricas
# https://archive.ics.uci.edu/ml/datasets/Student+Performance
# Dataset com dados de estudantes
# Vamos prever a nota final (grade) dos alunos

# Setup Libraries 
# install.packages("ggplot2")
# install.packages("ggthemes")
# install.packages("dplyr")
# install.packages('psych')

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
# Modelo V1
df_treino1 <- df %>% dplyr::sample_frac(.75)
df_teste1 <- dplyr::anti_join(df, df_treino1, by = 'index')
# Remoção do indice
df_treino1$index <- NULL
df_teste1$index <- NULL

# Modelo V2
df_treino2 <- df %>% dplyr::sample_frac(.80)
df_teste2 <- dplyr::anti_join(df, df_treino2, by = 'index')
# Remoção do indice
df_treino2$index <- NULL
df_teste2$index <- NULL


# Treinamento dos modelos
# Modelo V1
modelo_v1 <- lm(G3 ~., data = df_treino1)
sumario_modelo_v1 <- summary(modelo_v1)
r_squared_v1 <- sumario_modelo_v1$r.squared
fstatistics_v1 <- sumario_modelo_v1$fstatistic
sumario_v1

# Modelo V2
modelo_v2 <- lm(G3 ~., data = df_treino2)
sumario_modelo_v2 <- summary(modelo_v2)
r_squared_v2 <- sumario_modelo_v2$r.squared
fstatistics_v2 <- sumario_modelo_v2$fstatistic
sumario_modelo_v2



# Análise dos resíduos de treino
# Modelo V1
res_modelo_v1 <- as.data.frame(resid(modelo_v1))
colnames(res_modelo_v1) <- 'res'

# Modelo V2
res_modelo_v2 <- as.data.frame(resid(modelo_v2))
colnames(res_modelo_v2) <- 'res'

dev.off()
ggplot(res_modelo_v1, aes(res, fill = ..count..)) + geom_histogram(bins = 30)
# O modelo previu notas abaixo de zero, o que não é possível

# Medidas de Diagnóstico
# Plot Diagnostics for an lm Object
?plot.lm
par(mfrow = c(2,2))
plot(modelo_v1)
plot(modelo_v2)

# https://data.library.virginia.edu/diagnostic-plots/
# http://analyticspro.org/2016/03/07/r-tutorial-how-to-use-diagnostic-plots-for-regression-models/ 
#
# Residuals vs Fitted: Como os resíduos não estão igualmente espalhados em torno da linha horizontal e 
#                      ao que parece existe um padrão distinto. É uma indicação que existem relacionamentos não lineares.
#                      As observações numerdas como 55, 143 e 18 parecem se destacar no plot. 
# Normal Q-Q: Há um desvio severo da linha normal
# Scale-Location: Os pontos parecem mais ou menos espalhados randomicamente, nas não de forma perfeita
# Residuals vs Leverage: Não há casos influentes, mal é possível visualizar a linha de distância de Cook

#### 3. Execução das Predições ####

# Predicao_v1
predicao_v1 <- predict(modelo_v1, df_teste1)
sumario_predicao_v1 <- summary(predicao_v1)
resultados_v1 <- cbind(predicao_v1, df_teste1$G3)
colnames(resultados_v1) <- c('Predito','Observado')
resultados_v1 <- as.data.frame(resultados_v1)

# Desempenho do Modelo
sse_v1 <- sum((resultados_v1$Predito - resultados_v1$Observado)^2)
sst_v1 <- sum((mean(df$G3) - resultados_v1$Observado)^2)
R2_v1 <- 1-sse_v1/sst_v1
R2_v1
# Desempenho modelo_v1: 0.7646619

# Predicao_v2
predicao_v2 <- predict(modelo_v2, df_teste2)
sumario_predicao_v2 <- summary(predicao_v2)
resultados_v2 <- cbind(predicao_v2, df_teste2$G3)
colnames(resultados_v2) <- c('Predito','Observado')
resultados_v2 <- as.data.frame(resultados_v2)

# Desempenho do Modelo
sse_v2 <- sum((resultados_v2$Predito - resultados_v2$Observado)^2)
sst_v2 <- sum((mean(df$G3) - resultados_v2$Observado)^2)
R2_v2 <- 1-sse_v2/sst_v2
R2_v2
# Desempenho modelo_v2: 0.7767996



