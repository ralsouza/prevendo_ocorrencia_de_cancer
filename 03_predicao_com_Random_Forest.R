# Prevendo a ocorrência de câncer mama
# Algoritimo Random Forest - É um dos algoritimos de ML mais precisos, ele funciona melhor quando
# primeiros as variáveis mais relevantes para o modelo são calculadas e apenas estas usadas pelo 
# algoritimo

setwd('/Users/ls_rafael/Documents/GitHub/prevendo_ocorrencia_de_cancer')

#### Etapa 1 - Criação do Modelo ####
install.packages('rpart')
library(rpart)

# Criação do Modelo
modelo_rf_v1 <- rpart(diagnosis ~ .,data = dados_treino, control = rpart.control(cp = .0005))

#### Etapa 2 - Predições ####
pred_rf <- predict(modelo_rf_v1, dados_teste, type = 'class')

#### Etapa 3 - Analisando o Desempenho ####

# Previsões Corretas dos Dados de Teste
mean(pred_rf == dados_teste$diagnosis)
# Parece que o percentual de acertos parece que foi inferior ao KNN e SVM, ver enunciado para prováveis causas

# Confusion Matrix
table(pred_rf, dados_teste$diagnosis)







