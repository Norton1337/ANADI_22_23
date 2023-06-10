library(caret)
library(lubridate)
library(rpart)
library(rpart.plot)
library(rattle)
library(class)

set.seed(123)
setwd("C:/Users/manu0/Desktop/RESTO/ANADI/TP2/data")
data <- read.csv("ciclismo.csv")

#Transformando as datas em idades
ages <- interval(as.Date(data$dob), today()) %/% years(1)
data$age <- ages

#Omitindo NA's
data <- na.omit(data)

#Retirando a coluna dos Id's e das datas de nascimento
data <- data[,-c(1, 10)]

#Função de normalização
min_max <- function(data_aux) {
  min_value <- min(data_aux)
  max_value <- max(data_aux)
  result <- (data_aux - min_value)/(max_value - min_value)
  return(result)
}

#Normalização dos atributos
data$age <- min_max(data$age)
data$altitude_results <- min_max(data$altitude)
data$vo2_results <- min_max(data$vo2_results)
data$hr_results <- min_max(data$hr_results)

#Aplicando o One-Label enconder nos atributos categoricos binarios
data$gender<- ifelse(data$gender == "female", 0, 1)
data$Winter.Training.Camp<- ifelse(data$Winter.Training.Camp == "none", 0, 1)
data$Pro.level <- ifelse(data$Pro.level == "Continental", 0, 1)

#Aplicando o One-Hot Enconder nos atrbutos categoricos não binarios
encoded_df <- dummyVars("~.", data = data)
data <- data.frame(predict(encoded_df, newdata = data))


#1 - Estude a capacidade preditiva relativamente ao atributo “Pro_level” 
#usando os seguintes métodos:

#divisão do dataset em treino e validação 

sample = sample(c(TRUE,FALSE), nrow(data), replace = TRUE, prob = c(0.7,0.3))

prolevel.train = data[sample,]
prolevel.test = data[!sample,]

#árvore de decisão;
#A arvore de decisão tem melhores resultados sem essas colunas
#data <- data[,-c(9,2,5,8,10)]

tree.model <- rpart(Pro.level ~ . , data = prolevel.train, method = "class")
fancyRpartPlot(tree.model)

rpart.plot(tree.model, digits = 3, fallen.leaves = TRUE)

tree.pred = predict(tree.model, prolevel.test, type="class")

parse_results <- function(m.conf) {
  
  accuracy <- 100*round((m.conf[1,1]+m.conf[2,2])/sum(m.conf),4)
  recall= m.conf[1,1]/(m.conf[1,1]+m.conf[1,2])
  precision = m.conf[1,1]/(m.conf[1,1]+m.conf[2,1])
  f1=(2*precision * recall)/(precision+recall)
  
  message("accuracy: ", accuracy, "%")
  message("Recall: ", recall)
  message("precision: ", precision)
  message("F1: ", f1)
  
  my_list <- list("F1" = f1, "precision" = precision, "recall" = recall, "accuracy"=accuracy)
  return(my_list)
}

m.conf<-table(prolevel.test$Pro.level,tree.pred)
parse_results(m.conf)

#KNN
#Atinge 100% de accuracy sem esses dois atrbutos
#data <- data[,-c(9,2)]

prolevel.train$Pro.level <- factor(prolevel.train$Pro.level)
cv <- trainControl(method = "cv", 
                   number = 10, 
                   search = "grid")

tuned_model <- train(Pro.level ~ ., 
                     data = prolevel.train, 
                     method = "knn", 
                     trControl = cv, 
                     tuneGrid = expand.grid(k = 1:10))

best_k <- tuned_model$bestTune$k

knn.model <- knn(train = prolevel.train, 
                   test = prolevel.test,
                   cl = prolevel.train$Pro.level, 
                   k = best_k)

confusion_matrix <- table(knn.model, prolevel.test$Pro.level)
parse_results(confusion_matrix)

#Rede Neuronal

#a) Usando o método k-fold cross validation obtenha a média e o desvio padrão da 
#taxa de acerto da previsão do atributo “Pro_level” com os dois melhores 
#modelos obtidos na alínea anterior. 

#b) Dos três modelos, um é conhecido por ter uma forma de aprendizagem 
#conhecida como “Lazy Learning”, identifique o modelo e as implicações deste 
#tipo de modelos. 

#O K-NN é um algoritmo de classificação e regressão que funciona encontrando os "vizinhos mais próximos" de uma instância de entrada não rotulada dentro do conjunto de dados de treino. Ele não requer uma fase de treino explícita, mas armazena todo o conjunto de dados de treino em memória. Durante a fase de teste, ele calcula a distância entre a instância de entrada não rotulada e todas as outras instâncias no conjunto de treino. Em seguida, classifica a instância de entrada com base nas classes dos k vizinhos mais próximos (onde k é um número pré-definido).
#Implicações do "Lazy Learning" (aprendizagem preguiçosa):
#Baixo custo de treino: Como o K-NN não requer uma fase de treino explícita, o custo computacional para treinar o modelo é mínimo. Isso torna o treino muito rápido em comparação com outros algoritmos de aprendizado de máquina.
#Sensível a dados de treino: O desempenho do K-NN depende fortemente dos dados de treino. Se o conjunto de treino contiver ruído ou outliers, isso pode levar a classificações incorretas ou imprecisas.
#Alta carga computacional durante o teste: durante a fase de teste, o K-NN precisa calcular as distâncias entre a instância de entrada não rotulada e todas as outras instâncias de treino. Isso pode ser computacionalmente caro, especialmente para grandes conjuntos de dados.
#Não aprende representações abstratas: O K-NN não aprende representações abstratas dos dados, como outros algoritmos de aprendizado de máquina, como redes neurais. Em vez disso, ele baseia as suas classificações diretamente nas instâncias de treino. Isso pode limitar a sua capacidade de generalizar para novos dados que possuem características diferentes dos dados de treino.

#c) Verifique se existe diferença significativa no desempenho dos dois melhores 
#modelos obtidos anteriormente (use um nível de significância de 5%). 
#Identifique o modelo que apresenta o melhor desempenho. 

#d) Compare os resultados dos modelos. Discuta em detalhe qual o modelo que 
#apresentou melhor e pior desempenho de acordo com os critérios: Accuracy; 
#Sensitivity; Specificity e F1.


