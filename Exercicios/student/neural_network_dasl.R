# Redes Neurais

# Tutorial:
# https://www.r-bloggers.com/fitting-a-neural-network-in-r-neuralnet-package/

setwd('/Users/ls_rafael/Documents/GitHub/prevendo_ocorrencia_de_cancer/Exercicios')

#### Library Setup ####
# install.packages('caTools')
# install.packages('MASS')
# install.packages('caTools')

library(neuralnet)
library(MASS)
library(caTools)

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














