# Lista de Exercícios Parte 4 - Capítulo 11

# Diretório de trabalho OS X
setwd("/Users/ls_rafael/Documents/GitHub/prevendo_ocorrencia_de_cancer/Exercicios")
getwd()

# Definindo o Problema: OCR - Optical Character Recognition
# Seu modelo deve prever o caracter a partir do dataset fornecido. Use um modelo SVM
# Dataset: 
#   https://archive.ics.uci.edu/ml/datasets/Letter+Recognition
# Referências: 
#   https://rstudio-pubs-static.s3.amazonaws.com/228914_7d93c6a33e9d4b61aa0015d801eee23e.html
#   http://www.rpubs.com/jasonchanhku/ocr

## Explorando e preparando os dados
letters <- read.csv("letterdata.csv")
dim(letters)
str(letters)
summary(letters)
head(letters)

# Criando dados de treino e dados de teste
letters_treino <- letters[1:16000, ]
letters_teste  <- letters[16001:20000, ]

dim(letters_treino)
dim(letters_teste)

## Treinando o Modelo
# Kernlab: https://cran.r-project.org/web/packages/kernlab/index.html
install.packages("kernlab")
library(kernlab)
?kernlab

# Criando o modelo com o kernel vanilladot
# https://www.rdocumentation.org/packages/kernlab/versions/0.9-27/topics/ksvm
letter_classifier <- ksvm(letter ~., data = letters_treino, kernel = 'vanilladot')
letter_classifier

# Predizendo o caracter
pred_caract <- predict(letter_classifier, letters_teste[ ,2:17])

# Visualizar Resultados
head(pred_caract)
table(pred_caract, letters_teste[ ,17])


