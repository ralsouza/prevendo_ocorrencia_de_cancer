# Lista de Exercícios Parte 3 - Capítulo 11

# Diretório de trabalho OS X
setwd("/Users/ls_rafael/Documents/GitHub/prevendo_ocorrencia_de_cancer/Exercicios")
getwd()

# Definindo o Problema: Analisando dados das casas de Boston, nos EUA e fazendo previsoes.

# The Boston Housing Dataset
# http://www.cs.toronto.edu/~delve/data/boston/bostonDetail.html

# O modelo deve prever a MEDV (Valor da Mediana de ocupação das casas). Utilize um modelo de rede neural!

# Carregando o pacote MASS
library(MASS)
?MASS

# Importando os dados do dataset Boston
set.seed(101)
dados <- Boston
head(dados)

# Resumo dos dados
str(dados)
summary(dados)
any(is.na(dados))

# Carregando o pacote para Redes Neurais
install.packages("neuralnet")
library(neuralnet)
?neuralnet
