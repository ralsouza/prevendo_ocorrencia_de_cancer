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
# e será muito difícil usá-lo para generalizar outros dados.