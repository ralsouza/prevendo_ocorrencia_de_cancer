# Prevendo a ocorrência de câncer mama
# Algoritimo SVM

setwd('/Users/ls_rafael/Documents/GitHub/prevendo_ocorrencia_de_cancer')

# Definição da semente para os resultados reproduzíveis
set.seed(40)

#### Etapa 1 - Preparação dos Dados ####
dados <- read.csv('dataset.csv', stringsAsFactors = FALSE)
dados$id = NULL  

# Criação de um índice randomico para separação dos dados de treino de teste
dados[ ,'index'] <- ifelse(runif(nrow(dados)) < 0.8,1,0)
View(dados)

# Criação dos Dados de Treino e Teste
dados_treino <- dados[dados$index == 1, ]
dados_teste <- dados[dados$index == 0, ]

# Armazenar o índice do dataset de treino
indice_treino <- grep('index', names(dados_treino))

# Remoção dos índices dos datasets, não serão mais necessários
dados_treino <- dados_treino[ ,-indice_treino]
dados_teste <- dados_teste[ ,-indice_treino]

# Obter o índice da coluna da variável target no dataset 
target_index <- grep('diag',names(dados)) # grep usado para pesquisar a variável que inicia com 'diag'

#### Etapa 2 - Criação do Modelo ####
# Ajusta-se o kernel para radial, já que o conjunto de dados não tem um 
# plano linear que pode ser desenhado
install.packages('e1071')
library(e1071)
