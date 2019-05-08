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
  install.packages('sjPlot')
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

# Conversão das variáveis categórias para tipo numérico
df_num <- as.data.frame(lapply(df,as.integer))

cor.plot(df_num[c('studytime','G1','G2','G3')], numbers = TRUE)

ggplot(df,aes(sex,G1,G2)) + geom_point()

