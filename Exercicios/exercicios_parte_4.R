# Lista de Exercícios Parte 4 - Capítulo 11

# Diretório de trabalho OS X
setwd("/Users/ls_rafael/Documents/GitHub/prevendo_ocorrencia_de_cancer/Exercicios")
getwd()

# Definindo o Problema: OCR - Optical Character Recognition
# Seu modelo deve prever o caracter a partir do dataset fornecido. 
# Ao digitar um caracter, um número ou uma letra, este desenho é na verdade 
# uma matriz de pixels, que são pequenos pontos desenhados na imagem (caracter). 
# A ideia de prever o caracter é que o modelo calcule a relacão entre os pixels e 
# ser capaz de predizer se tal letra é realmente a letra com questão com parando 
# os dados de treino e testes. Esta é uma atividade de visão computacional.
# Use um modelo SVM que é uma subcategoria de rede neural.

# Dataset: 
#   https://archive.ics.uci.edu/ml/datasets/Letter+Recognition
# Referências: 
#   https://rstudio-pubs-static.s3.amazonaws.com/228914_7d93c6a33e9d4b61aa0015d801eee23e.html
#   http://www.rpubs.com/jasonchanhku/ocr
#   Melhor: 
#           https://charleshsliao.wordpress.com/2017/03/06/kernels-svm-and-a-letter-recognition-example/ 

## Explorando e preparando os dados
letters <- read.csv("letterdata.csv")
dim(letters)
str(letters)
summary(letters)
head(letters)

# Criando dados de treino e dados de teste
treino_idx_v1 <- sample(nrow(letters), 2/3 * nrow(letters))
treino_v1 <- letters[treino_idx_v1, ]
teste_v1 <- letters[-treino_idx_v1, ]

dim(treino_v1)
dim(teste_v1)

## Treinando o Modelo
# Kernlab: https://cran.r-project.org/web/packages/kernlab/index.html
install.packages("kernlab")
library(kernlab)
?kernlab

# Criação do modelo
# https://www.rdocumentation.org/packages/kernlab/versions/0.9-27/topics/ksvm
?ksvm

# Modelo com Vanilladot - Kernel Linear
modelo_v1 <- ksvm(letter ~., data = treino_v1, kernel = 'vanilladot')
summary(modelo_v1)

# Modelo com Polydot - Kernel Polinomial
modelo_v2 <- ksvm(letter ~., data = treino_v1, kernel = 'polydot')
summary(modelo_v2)

# Modelo com RBFdot - Kernel Radial (Gaussiano)
modelo_v3 <- ksvm(letter ~., data = treino_v1, kernel = 'rbfdot')
summary(modelo_v3)

# Predizendo o caracter

# Modelo Versão 1
predict_v1 <- predict(modelo_v1, teste_v1)
summary(predict_v1)

# Modelo Versão 2
predict_v2 <- predict(modelo_v2, teste_v1)
summary(predict_v2)

# Modelo Versão 3
predict_v3 <- predict(modelo_v3, teste_v1)
summary(predict_v3)

# Visualização dos Resultados
# Letras nas linhas representam as predições feitas pelo modelo
# E as letras nas colunas representam as letras no dataset de treino
table(predict_v1, teste_v1$letter)
table(predict_v2, teste_v1$letter)
table(predict_v3, teste_v1$letter)

# Mensuração dos Erros
erro_v1 <- (sum(predict_v1 != teste_v1$letter) / nrow(teste_v1))
print(paste0('Precisão da versão 1 com kernel linear (vanilladot): ', 1 - erro_v1))

erro_v2 <- (sum(predict_v2 != teste_v1$letter) / nrow(teste_v1))
print(paste0('Precisão da versão 2 com kernel polinomial (polydot): ', 1 - erro_v2))

erro_v3 <- (sum(predict_v3 != teste_v1$letter) / nrow(teste_v1))
print(paste0('Precisão da versão 3 com kernel RBF (rbfdot): ', 1 - erro_v3))








