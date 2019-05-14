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
df_treino2 <- df %>% dplyr::sample_frac(.60)
df_teste2 <- dplyr::anti_join(df, df_treino2, by = 'index')
# Remoção do indice
df_treino2$index <- NULL
df_teste2$index <- NULL

# Modelo V3
df_treino3 <- df %>% dplyr::sample_frac(.65)
df_teste3 <- dplyr::anti_join(df, df_treino3, by = 'index')
# Remoção do indice
df_treino3$index <- NULL
df_teste3$index <- NULL

# Modelo V4
df_treino4 <- df %>% dplyr::sample_frac(.60)
df_teste4 <- dplyr::anti_join(df, df_treino4, by = 'index')
# Remoção do indice
df_treino4$index <- NULL
df_teste4$index <- NULL

# Modelo V5
df_treino5 <- df %>% dplyr::sample_frac(.70)
df_teste5 <- dplyr::anti_join(df, df_treino5, by = 'index')
# Remoção do indice
df_treino5$index <- NULL
df_teste5$index <- NULL

# Treinamento dos modelos
# Modelo V1
modelo_v1 <- lm(G3 ~., data = df_treino1)
sumario_modelo_v1 <- summary(modelo_v1)
r_squared_v1 <- sumario_modelo_v1$r.squared
fstatistics_v1 <- sumario_modelo_v1$fstatistic
sumario_modelo_v1

# Modelo V2
modelo_v2 <- lm(G3 ~., data = df_treino2)
sumario_modelo_v2 <- summary(modelo_v2)
r_squared_v2 <- sumario_modelo_v2$r.squared
fstatistics_v2 <- sumario_modelo_v2$fstatistic
sumario_modelo_v2

# Modelo V3
modelo_v3 <- lm(G3 ~., data = df_treino3)
sumario_modelo_v3 <- summary(modelo_v3)
r_squared_v3 <- sumario_modelo_v3$r.squared
fstatistics_v3 <- sumario_modelo_v3$fstatistic
sumario_modelo_v3

# Modelo V4
modelo_v4 <- lm(G3 ~., data = df_treino4)
sumario_modelo_v4 <- summary(modelo_v4)
r_squared_v4 <- sumario_modelo_v4
fstatistics_v4 <- sumario_modelo_v4$fstatistic
sumario_modelo_v4

# Para tentar melhorar o desempenho deste modelo, 
# serão selecionados as variáveis com nível de significancia menor que 1%
nvl_sig_lt1 <- sumario_modelo_v4$coefficients[-1,4] < 0.001
nvl_sig_lt1 <- names(nvl_sig_lt1)[nvl_sig_lt1 == TRUE]
nvl_sig_lt1 <- c('G3', nvl_sig_lt1)
df_treino4_nvl_sig_lt1 <- df_treino4[nvl_sig_lt1]

# Rodar novamente o modelo_v4 com o novo data set reduzido
modelo_v4.1 <- lm(G3 ~., data = df_treino4_nvl_sig_lt1)
sumario_modelo_v4.1 <- summary(modelo_v4.1)
r_squared_v4.1 <- sumario_modelo_v4.1$r.squared
fstatistics_v4.1 <- sumario_modelo_v4.1$fstatistic
sumario_modelo_v4.1

# Modelo V5
modelo_v5 <- lm(G3 ~., data = df_treino5)
sumario_modelo_v5 <- summary(modelo_v5)
r_squared_v5 <- sumario_modelo_v5
fstatistics_v5 <- sumario_modelo_v5$fstatistic
sumario_modelo_v5

# Análise dos resíduos de treino
# Modelo V1
res_modelo_v1 <- as.data.frame(resid(modelo_v1))
colnames(res_modelo_v1) <- 'res'

dev.off()
ggplot(res_modelo_v1, aes(res, fill = ..count..)) + geom_histogram(bins = 30)
# O modelo previu notas abaixo de zero, o que não é possível

# Modelo V2
res_modelo_v2 <- as.data.frame(resid(modelo_v2))
colnames(res_modelo_v2) <- 'res'

dev.off()
ggplot(res_modelo_v3, aes(res, fill = ..count..)) + geom_histogram(bins = 30)
# O modelo previu notas abaixo de zero, o que não é possível

# Modelo V3
res_modelo_v3 <- as.data.frame(resid(modelo_v3))
colnames(res_modelo_v3) <- 'res'

dev.off()
ggplot(res_modelo_v2, aes(res, fill = ..count..)) + geom_histogram(bins = 30)
# O modelo previu notas abaixo de zero, o que não é possível

# Modelo V4
res_modelo_v4.1 <- as.data.frame(resid(modelo_v4.1))
colnames(res_modelo_v4.1) <- 'res'

dev.off()
ggplot(res_modelo_v4.1, aes(res, fill = ..count..)) + geom_histogram(bins = 30)
# O modelo previu notas abaixo de zero, o que não é possível

# Modelo V5
res_modelo_v5 <- as.data.frame(resid(modelo_v5))
colnames(res_modelo_v5) <- 'res'

dev.off()
ggplot(res_modelo_v5, aes(res, fill = ..count..)) + geom_histogram(bins = 30)
# O modelo previu notas abaixo de zero, o que não é possível



# Medidas de Diagnóstico
# Plot Diagnostics for an lm Object
?plot.lm
par(mfrow = c(2,2))
plot(modelo_v1)
plot(modelo_v2)
plot(modelo_v3)
plot(modelo_v4.1)
plot(modelo_v5)


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

# Função para tratar os valores negativos, as novas que forem negativas, serão alteradas para zero
tratar_zeros <- function(x){
  if(x < 0 ){
    return(0)
  }else{
    return(x)
  }
}

# Predicao_v1 - Este modelo trouxe o melhor resultado, então o trabalho se focará neste modelo
predicao_v1 <- predict(modelo_v1, df_teste1)
sumario_predicao_v1 <- summary(predicao_v1)
resultados_v1 <- cbind(predicao_v1, df_teste1$G3)
colnames(resultados_v1) <- c('Predito','Observado')
resultados_v1 <- as.data.frame(resultados_v1)
min(resultados_v1$Predito) # Ainda existem os valores negativos

# Desempenho do modelo_v1 antes do tratamento dos números negativos
sse_v1 <- sum((resultados_v1$Predito - resultados_v1$Observado)^2)
sst_v1 <- sum((mean(df$G3) - resultados_v1$Observado)^2)
R2_v1 <- 1-sse_v1/sst_v1
R2_v1
# Desempenho modelo_v1 antes do tratamento dos números negativos: 0.8306656

# Tratamento das notas menores que zero no modelo_v1
resultados_v1$Predito <- sapply(resultados_v1$Predito, tratar_zeros)
min(resultados_v1$Predito)

# Desempenho do modelo_v1 após o tratamento dos números negativos
sse_v1 <- sum((resultados_v1$Predito - resultados_v1$Observado)^2)
sst_v1 <- sum((mean(df$G3) - resultados_v1$Observado)^2)
R2_v1 <- 1-(sse_v1/sst_v1)
R2_v1
# Desempenho modelo_v1 antes do tratamento dos números negativos: 0.8346144

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
# Desempenho modelo_v2: 0.734633

# Predicao_v3
predicao_v3 <- predict(modelo_v3, df_teste3)
sumario_predicao_v3 <- summary(predicao_v3)
resultados_v3 <- cbind(predicao_v3, df_teste3$G3)
colnames(resultados_v3) <- c('Predito','Observado')
resultados_v3 <- as.data.frame(resultados_v3)

# Desempenho do Modelo
sse_v3 <- sum((resultados_v3$Predito - resultados_v3$Observado)^2)
sst_v3 <- sum((mean(df$G3) - resultados_v3$Observado)^2)
R2_v3 <- 1-sse_v3/sst_v3
R2_v3
# Desempenho modelo_v2: 0.7981727

# Predicao_v4
predicao_v4 <- predict(modelo_v4, df_teste4)
sumario_predicao_v4 <- summary(predicao_v4)
resultados_v4 <- cbind(predicao_v4, df_teste4$G3)
colnames(resultados_v4) <- c('Predito','Observado')
resultados_v4 <- as.data.frame(resultados_v4)

# Desempenho do Modelo
sse_v4 <- sum((resultados_v3$Predito - resultados_v3$Observado)^2)
sst_v4 <- sum((mean(df$G3) - resultados_v4$Observado)^2)
R2_v4 <- 1-sse_v4/sst_v4
R2_v4
# Desempenho modelo_v2: 0.7821376

# Predicao_v5
predicao_v5 <- predict(modelo_v5, df_teste5)
sumario_predicao_v5 <- summary(predicao_v5)
resultados_v5 <- cbind(predicao_v5, df_teste5$G3)
colnames(resultados_v5) <- c('Predito','Observado')
resultados_v5 <- as.data.frame(resultados_v5)

# Desempenho do Modelo
sse_v5 <- sum((resultados_v5$Predito - resultados_v5$Observado)^2)
sst_v5 <- sum((mean(df$G3) - resultados_v5$Observado)^2)
R2_v5 <- 1-sse_v5/sst_v5
R2_v5
# Desempenho modelo_v2: 0.7671282

