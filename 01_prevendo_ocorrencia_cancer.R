# Prevendo a ocorrência de câncer mama

setwd('/Users/ls_rafael/Documents/GitHub/prevendo_ocorrencia_de_cancer')

# Definição do Problema de Negócio: Previsão de Ocorrência de Câncer de Mama
# http://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Wisconsin+%28Diagnostic%29

#### Etapa 1 - Coleta de Dados ####

# Os dados do câncer da mama incluem 569 observações de biópsias de câncer, 
# cada um com 32 características (variáveis). Uma característica é um número de 
# identificação (ID), outro é o diagnóstico de câncer e 30 são medidas laboratoriais 
# numéricas. O diagnóstico é codificado como "M" para indicar maligno ou "B" para 
# indicar benigno
dados <- read.csv('dataset.csv', stringsAsFactors = FALSE)
str(dados)
View(dados)

#### Etapa 2 - Pré-Processamento ####

# Excluindo a coluna ID
# Independentemente do método de aprendizagem de máquina, deve sempre ser excluídas 
# variáveis de ID. Caso contrário, isso pode levar a resultados errados porque o ID 
# pode ser usado para unicamente "prever" cada exemplo. Por conseguinte, um modelo 
# que inclui um identificador pode sofrer de superajuste (overfitting), 
# e será muito difícil usá-lo para generalizar outros dados
dados$id = NULL

# Ajuste da sigla de diagnóstico
?sapply
dados$diagnosis = sapply(dados$diagnosis, function(x){ifelse(x=='M','Maligno','Benigno')})

# Muitos classificadores requerem que as variáveis sejam do tipo Fator, mas nem sempre obrigatório
table(dados$diagnosis)
dados$diagnosis <- factor(dados$diagnosis, levels = c('Benigno','Maligno'), labels = c('Benigno','Maligno'))
str(dados$diagnosis)

# Verificação das proporções dos dados, em %
round(prop.table(table(dados$diagnosis)) * 100, digits = 1)

# Normalização
# Detectamos um problema de escala entre os dados, que então precisam ser normalizados
# O cálculo de distância feito pelo kNN é dependente das medidas de escala nos dados de entrada.

# Como as variáveis numéricas estão em escalas diferentes, isso é um problema pois muitos
# algoritimos de ML esperam receber os dados na mesma escala e em uma distribuição normal. Ou seja, com
# média igual a 0 e desvio padrão igual a 1

summary(dados[c('radius_mean','area_mean','smoothness_mean')])
# A média e mediana de smoothness_mean são parecidas, o que indica uma distribuição normal, mas não
# nas outras variáveis. Então é necessário aplicar uma normalização

# Função de normalização
normalizar <- function(x){
  return((x - min(x)) / (max(x) - min(x)))
}

# Testando a função de normalização
normalizar(c(1,2,3,4,5))
normalizar(c(10,20,30,40,50))

# Aplicação da Normalização 
dados_norm <- as.data.frame(lapply(dados[2:31], normalizar))
View(dados_norm)

#### Etapa 3 - Treinando o Modelo com KNN ####
# K-nearest neighbors
install.packages('class')
library(class)
?knn

# Divisão dos dados de treino e teste
dados_treino <- dados_norm[1:469, ]
dados_teste  <- dados_norm[470:569, ] 

# Criação das labels para os dados de treino e teste 
dados_treino_labels <- dados[1:469, 1]
dados_teste_labels <- dados[470:569,1]
length(dados_treino_labels)
length(dados_teste_labels)

# Criação do modelo preditivo
modelo_knn_v1 <- knn(train = dados_treino,
                     test = dados_teste,
                     cl = dados_treino_labels,
                     k = 21) # k é a distância euclidiana, que verificará os 21 pontos de dados mais 
                             # próximos de cada ponto de dado

# A função knn() retorna um objeto do tipo fator com as previsões para cada exemplo no dataset de teste
summary(modelo_knn_v1)

#### Etapa 4 - Avaliação e Interpretação do Modelo ####

library(gmodels)

# Criando uma tabela cruzada (Confusion Matrix) dos dados previstos x dados atuais
# Usaremos amostra com 100 observações: length(dados_teste_labels)
CrossTable(x = dados_teste_labels, y = modelo_knn_v1, prop.chisq = FALSE)

# Interpretando os Resultados
# A tabela cruzada mostra 4 possíveis valores, que representam os falso/verdadeiro positivo e negativo
# Temos duas colunas listando os labels originais nos dados observados
# Temos duas linhas listando os labels dos dados de teste

# Temos:
# Cenário 1: Célula Benigno (Observado) x Benigno (Previsto) - 61 casos - true positive 
# Cenário 2: Célula Maligno (Observado) x Benigno (Previsto) - 00 casos - false positive
# Cenário 3: Célula Benigno (Observado) x Maligno (Previsto) - 02 casos - false negative (o modelo errou)
# Cenário 4: Célula Maligno (Observado) x Maligno (Previsto) - 37 casos - true negative 

# Lendo a Confusion Matrix (Perspectiva de ter ou não a doença):

# True Negative  = nosso modelo previu que a pessoa NÃO tinha a doença e os dados mostraram que realmente a pessoa NÃO tinha a doença
# False Positive = nosso modelo previu que a pessoa tinha a doença e os dados mostraram que NÃO, a pessoa tinha a doença
# False Negative = nosso modelo previu que a pessoa NÃO tinha a doença e os dados mostraram que SIM, a pessoa tinha a doença
# True Positive = nosso modelo previu que a pessoa tinha a doença e os dados mostraram que SIM, a pessoa tinha a doença

# Falso Positivo - Erro Tipo I
# Falso Negativo - Erro Tipo II

# Taxa de acerto do Modelo: 98% (acertou 98 em 100)














