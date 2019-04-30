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
