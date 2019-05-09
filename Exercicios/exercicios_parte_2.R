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

# Explorando os dados
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

ggplot(df,aes(G1)) + geom_histogram(binwidth = 1)
ggplot(df,aes(G2)) + geom_histogram(binwidth = 1)
ggplot(df,aes(G3)) + geom_histogram(binwidth = 1)

# Análise: Houveram quase 40 notas 0 na G3, o que será que houve?


ggplot(df,aes(G3, fill = romantic)) + geom_histogram(binwidth = 1)





