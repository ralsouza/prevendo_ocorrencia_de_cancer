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

# Criando o modelo com o kernel vanilladot
# https://www.rdocumentation.org/packages/kernlab/versions/0.9-27/topics/ksvm
?ksvm

modelo_v1 <- ksvm(letter ~., data = treino_v1, kernel = 'vanilladot')
summary(modelo_v1)

# Predizendo o caracter
predict_v1 <- predict(modelo_v1, teste_v1)
summary(predict_v1)

# Visualização dos Resultados
table(predict_v1, teste_v1$letter)

# Mensuração dos Erros
erro_v1 <- (sum(predict_v1 != teste_v1$letter) / nrow(teste_v1))
print(paste0('Precisão da versão 1 com kernel Vanilladot: ', 1 - erro_v1))



