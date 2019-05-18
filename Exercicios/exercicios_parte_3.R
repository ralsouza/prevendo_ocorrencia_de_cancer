# Lista de Exercícios Parte 3 - Capítulo 11

# Diretório de trabalho OS X
setwd("/Users/ls_rafael/Documents/GitHub/prevendo_ocorrencia_de_cancer/Exercicios")
getwd()

# Definindo o Problema: Analisando dados das casas de Boston, nos EUA e fazendo previsoes.

# The Boston Housing Dataset
# http://www.cs.toronto.edu/~delve/data/boston/bostonDetail.html

# O modelo deve prever a MEDV (Valor da Mediana de ocupação das casas). 
# Utilize um modelo de rede neural!

#### Library Setup ####
# install.packages('caTools')
# install.packages('MASS')
# install.packages("dplyr")
# install.packages('psych')

library(neuralnet)
library(MASS)
library(dplyr)
library(psych)

#### Carga dos Dados ####

# Descrição variáveis do dataset:
# https://cran.r-project.org/web/packages/MASS/MASS.pdf, página 20 
#   crim: per capita crime rate by town.
#   zn: proportion of residential land zoned for lots over 25,000 sq.ft.
#   indus: proportion of non-retail business acres per town.
#   chas: Charles River dummy variable (= 1 if tract bounds river; 0 otherwise). 
#   nox: nitrogen oxides concentration (parts per 10 million).
#   rm: average number of rooms per dwelling.
#   age: proportion of owner-occupied units built prior to 1940.
#   dis: weighted mean of distances to five Boston employment centres. 
#   rad: index of accessibility to radial highways.
#   tax: full-value property-tax rate per $10,000.
#   ptratio: pupil-teacher ratio by town.
#   black: 1000(Bk − 0.63)2 where Bk is the proportion of blacks by town. 
#   lstat: lower status of the population (percent).
#   medv: median value of owner-occupied homes in $1,000.

df <- Boston

#### Análise Exploratória ####

head(df)
str(df)
summary(df)
any(is.na(df))

plot(df$medv)
hist(df$medv)

cor.plot(df, numbers = TRUE)

#### Seleção de Amostras para Treino e Teste ####

# Inclusão do indice para divisão
df$index <- 1:nrow(df)

df_treino_v2 <- df %>% dplyr::sample_frac(.70)
df_teste_v2 <- dplyr::anti_join(df, amostra_treino_v2, by = 'index')

# Seleção dos dados de Treino
df_treino_v1 = subset(df, amostra_treino_v1 == TRUE)
df_teste_v1 = subset(df,amostra_treino_v1 == FALSE)
