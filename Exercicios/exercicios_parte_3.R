# Lista de Exercícios Parte 3 - Capítulo 11

# Diretório de trabalho OS X
setwd("/Users/ls_rafael/Documents/GitHub/prevendo_ocorrencia_de_cancer/Exercicios")
getwd()

# Definindo o Problema: Analisando dados das casas de Boston, nos EUA e fazendo previsoes.

# The Boston Housing Dataset
# http://www.cs.toronto.edu/~delve/data/boston/bostonDetail.html

# Tutorial
# https://www.r-bloggers.com/fitting-a-neural-network-in-r-neuralnet-package/

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
library(ggplot2)

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

hist(df$medv, 
     xlab = 'MEDV', 
     ylab = 'Frequencia', 
     main = 'Histograma MEDV', 
     col  = 'lightblue')

cor.plot(df, numbers = TRUE)

#### Seleção de Amostras para Treino e Teste ####

# Inclusão do indice para divisão
df$index <- 1:nrow(df)

# Divisão dos Datasets
?sample_frac

# Versão 1 - Divisão 70/30
df_treino_v1 <- df %>% dplyr::sample_frac(.70)
df_teste_v1 <- dplyr::anti_join(df, df_treino_v1, by = 'index')

# Versão 2 - Outra técnica de divisão (Mais interessante)
df_treino_v2_idx <- sample(nrow(df), 2/3 * nrow(df))
df_treino_v2 <- df[ df_treino_v2_idx, ]
df_teste_v2  <- df[-df_treino_v2_idx, ]

# Remoção dos Indices
df_treino_v1$index <- NULL
df_teste_v1$index <- NULL
df$index <- NULL

# Criação do Modelo Linear para Comparação
lm_fit_v1 <- glm(medv ~., data = df_treino_v1)
summary(lm_fit_v1)

lm_fit_v2 <- glm(medv ~., data = df_treino_v2)
summary(lm_fit_v2)


# Predição do Modelo Linear
pr_fit_v1 <- predict(lm_fit_v1, df_teste_v1)
pr_fit_v2 <- predict(lm_fit_v2, df_teste_v2)

# Medida de quão longe as previsões estão longe dos dados reais usando MSE - valores próximos de zero são melhores
mse_lm_v1 <- sum((pr_fit_v1 - df_teste_v1$medv)^2)/nrow(df_teste_v1) # 15.18%
mse_lm_v2 <- sum((pr_fit_v2 - df_teste_v2$medv)^2)/nrow(df_teste_v2) # 27.02%

#### Normalização dos Dados ####
# Pode ser feito como pre-processamento
# Redes neurais não são tão fáceis de treinar e ajustar, então alguma preparação é necessária
# Será usada a técnica de normalização min-max (min-max scale)
# Normalmente escalando os dados em intervalos [0,1] ou [-1,1] tende a dar resultados melhores

# Normalizar os Datasets de Treino e Teste
# Treino
maxs <- apply(df_treino_v1, 2, max)
mins <- apply(df_treino_v1, 2, min)

maxs_v2 <- apply(df_treino_v2, 2, max)
mins_v2 <- apply(df_treino_v2, 2, min)

df_treino_norm_v2 <- as.data.frame(scale(df_treino_v2, center = mins_v2, scale = maxs_v2 - mins_v2))
summary(df_treino_norm_v2)

# Teste
maxs_v1 <- apply(df_teste_v1, 2, max)
mins_v1 <- apply(df_teste_v1, 2, min)

maxs_v2 <- apply(df_teste_v2, 2, max)
mins_v2 <- apply(df_teste_v2, 2, min)

df_teste_norm_v2 <- as.data.frame(scale(df_teste_v2, center = mins_v2, scale = maxs_v2 - mins_v2))
summary(df_teste_norm_v2)

#### Treinamento do Modelo #### 
nn_v2 <- neuralnet(medv ~., data = df_treino_norm_v2, hidden = c(5,3), linear.output = TRUE)

plot(nn_v1)
plot(nn_v2)

#### Predição MEDV (Valor da Mediana de ocupação das casas) ####
dim(df_teste_norm_v1)
dim(df_teste_norm_v2)

# Computar os dados apenas das variáveis preditoras, desconsiderando
# a variável alvo dos testes
?predict.nn

pred_nn_v1 <- predict(nn_v1, df_teste_norm_v1[ ,1:13])
summary(pred_nn_v1)

pred_nn_v2 <- predict(nn_v2, df_teste_norm_v2[ ,1:13])
summary(pred_nn_v2)

# A predição será com os dados normalizados, é necessário redimensionar
# para o estado natural
# Versão 1
pred_nn_v1 <- pred_nn_v1*(max(df$medv) - min(df$medv)) + min(df$medv)+min(df$medv)
test_r_v1 <- (df_teste_norm_v1$medv)*(max(df$medv)-min(df$medv))+min(df$medv)

# Mean Squared Error
mse_nn_v1 <- sum((test_r_v1 - pred_nn_v1)^2)/nrow(df_teste_norm_v1)

# Erros de previsao
error_df_v1 <- data.frame(test_r_v1, pred_nn_v1)
head(error_df_v1)

# Plot dos erros
ggplot(error_df_v1, aes(x = test_r_v1,y = pred_nn_v1)) + 
  geom_point() + stat_smooth()

# Versão 2
pred_nn_v2 <- pred_nn_v2*(max(df$medv) - min(df$medv)) + min(df$medv)+min(df$medv)
test_r_v2 <- (df_teste_norm_v2$medv)*(max(df$medv)-min(df$medv))+min(df$medv)

# Mean Squared Error
mse_nn_v2 <- sum((test_r_v2 - pred_nn_v2)^2)/nrow(df_teste_norm_v2)

# Erros de previsao
error_df_v2 <- data.frame(test_r_v2, pred_nn_v2)
head(error_df_v2)

#### Comparação dos dois MSEs do modelo linear e neural ####
print(paste(mse_lm_v1, mse_nn_v1))
print(paste(mse_lm_v2, mse_nn_v2))

# Plots de Comparação entre os Modelos
par(mfrow=c(1,2))

# Plot versão 1
plot(df_teste_v1$medv,pred_nn_v1,col='red',main='Real vs Predito - Rede Neural',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')

plot(df_teste_v1$medv,pr_fit_v1,col='blue',main='Real vs Predito - Modelo Linear',pch=18, cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)

# Comparação em um plot
plot(df_teste_v1$medv,pred_nn_v1,col='red',main='Real vs Predito - NN e LM',pch=18,cex=0.7)
points(df_teste_v1$medv,pr_fit_v1,col='blue',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend=c('NN','LM'),pch=18,col=c('red','blue'))

# Plot versão 2
plot(df_teste_v2$medv,pred_nn_v2,col='red',main='Real vs Predito - Rede Neural',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')

plot(df_teste_v2$medv,pr_fit_v2,col='blue',main='Real vs Predito - Modelo Linear',pch=18, cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)

# Comparação em um plot
plot(df_teste_v2$medv,pred_nn_v2,col='red',main='Real vs Predito - NN e LM',pch=18,cex=0.7)
points(df_teste_v2$medv,pr_fit_v2,col='blue',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend=c('NN','LM'),pch=18,col=c('red','blue'))

