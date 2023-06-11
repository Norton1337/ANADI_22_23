library(caret)
library(lubridate)
library(rpart)
library(rpart.plot)
library(rattle)
library(class)
library(neuralnet)

set.seed(7)
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

#data <- data[,-c(9,2)]
#data <- data[,c(3,4,6,7,8)]
#data <- data[,-c(9,2,5,10)]
data <- data[,c(3,4,6,7,8,10)]
encoded_df <- dummyVars("~.", data = data)
data <- data.frame(predict(encoded_df, newdata = data))


#1 - Estude a capacidade preditiva relativamente ao atributo “Pro_level” 
#usando os seguintes métodos:

#divisão do dataset em treino e validação 


sample = sample(c(TRUE,FALSE), nrow(data), replace = TRUE, prob = c(0.7,0.3))

prolevel.train = data[sample,]
labels_train <- prolevel.train[,which(names(prolevel.train) == "Pro.level")]
prolevel.train <- prolevel.train[,-which(names(prolevel.train) == "Pro.level")]

prolevel.test = data[!sample,]
labels_test <- prolevel.test[,which(names(prolevel.test) == "Pro.level")]
prolevel.test <- prolevel.test[,-which(names(prolevel.test) == "Pro.level")]



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

labels_train <- factor(labels_train)
param_grid <- expand.grid(k = 1:10)
# Train and tune the KNN model
knn_model <- train(
  x = prolevel.train,
  y = labels_train,
  method = "knn",
  tuneGrid = param_grid,
  trControl = trainControl(method = "cv", number = 5)  # Specify the number of cross-validation folds
)

# Print the best tuned parameter
print(knn_model$bestTune)

# Make predictions using the tuned KNN model
knn_predictions <- predict(knn_model, prolevel.test)

labels_test <- factor(labels_test)

m.conf<-table(labels_test,knn_predictions)
parse_results(m.conf)

#Rede Neuronal
#data <- data[,-c(9,2,5,8,10)]
#Atinge em média 67/68% de accuracy sem esses dados e com nummodes = 4
numnodes <- 2

nn.model <- 
  neuralnet(
    Pro.level ~ .,
    data = prolevel.train,
    hidden = numnodes,
    act.fct = "logistic", 
    linear.output = FALSE
  )
plot(nn.model)

nn.model$result.matrix

predictions <- compute(nn.model, prolevel.test[, -8])
predicted_classes <- ifelse(predictions$net.result > 0.5, 1, 0)

confusion_matrix <- table(predicted_classes, prolevel.test$Pro.level)
parse_results(confusion_matrix)


#a) Usando o método k-fold cross validation obtenha a média e o desvio padrão da 
#taxa de acerto da previsão do atributo “Pro_level” com os dois melhores 
#modelos obtidos na alínea anterior. 

#d) Compare os resultados dos modelos. Discuta em detalhe qual o modelo que 
#apresentou melhor e pior desempenho de acordo com os critérios: Accuracy; 
#Sensitivity; Specificity e F1.

RMSE <- function(test, predicted) {
  sqrt(mean((test - predicted) ^ 2))
}

#Arvore de decisão
experiments = 20
accuracy_tree = numeric()
recall_tree = numeric()
precision_tree = numeric()
rmse_tree = numeric()
specificity_tree = numeric()

for (i in 1:experiments) {
  
  sample=sample(1:nrow(data), 0.7*nrow(data))
  data.train = data[sample, ]
  data.test = data[-sample, ]
  
  tree.model = rpart(Pro.level ~ . ,data = data.train, method="class")
  tree.pred = predict(tree.model, data.test, type='class')
  
  m.conf <- confusionMatrix(factor(tree.pred), factor(data.test$Pro.level))
  
  #Accuracy for each experiment
  accuracy_tree[i] = m.conf$overall['Accuracy']
  
  #Recall for each experiment
  recall_tree[i] = m.conf$byClass['Sensitivity']
  
  #Precision for each experiment
  precision_tree[i] = m.conf$byClass['Precision']

  rmse_tree[i] = RMSE(as.numeric(data.test$Pro.level),as.numeric(tree.pred))
  
  specificity_tree[i] <- m.conf$byClass['Specificity']
    
  
}

mean_accuracy_tree = 100*round(mean(accuracy_tree),4)
cat("average acc:", mean_accuracy_tree, "%, std deviation:", round(sd(accuracy_tree),4))
cat("recall:", 100*round(mean(recall_tree),3), "%, std deviation:", round(sd(recall_tree),3))
cat("precision:", 100*round(mean(precision_tree),3), "%, std deviation:", round(sd(precision_tree),3))
f1=(2*round(mean(precision_tree),3)*round(sd(recall_tree),3))/(round(mean(precision_tree),3)+round(sd(recall_tree),3))
cat("\n F1: ",f1)
cat("average specificity:", 100*round(mean(specificity_tree),3), "%, std deviation:", round(sd(specificity_tree),4))


#Rede Neuronal
experiments = 20
accuracy_nn = numeric()
recall_nn = numeric()
precision_nn = numeric()
rmse_nn = numeric()
specificity_nn = numeric()

for (i in 1:experiments) {

  sample = sample(c(TRUE,FALSE), nrow(data), replace = TRUE, prob = c(0.7,0.3))
  prolevel.train = data[sample,]
  prolevel.test = data[!sample,]
  
  numnodes <- 2

  nn.model <- 
    neuralnet(
      Pro.level ~ .,
      data = prolevel.train,
      hidden = numnodes,
      act.fct = "logistic", 
      linear.output = FALSE
    )
  plot(nn.model)
  
  nn.model$result.matrix
  
  predictions <- compute(nn.model, prolevel.test[, -8])
  predicted_classes <- ifelse(predictions$net.result > 0.5, 1, 0)
  
  m.conf <- confusionMatrix(factor(predicted_classes), factor(prolevel.test$Pro.level))
  
  #Accuracy for each experiment
  accuracy_nn[i] = m.conf$overall['Accuracy']
  
  #Recall for each experiment
  recall_nn[i] = m.conf$byClass['Sensitivity']
  
  #Precision for each experiment
  precision_nn[i] = m.conf$byClass['Precision']
  
  rmse_nn[i] = RMSE(as.numeric(data.test$Pro.level),as.numeric(tree.pred))
  
  specificity_nn[i] <- m.conf$byClass['Specificity']
  
}

mean_accuracy_nn = 100*round(mean(accuracy_nn),4)
cat("average acc:", mean_accuracy_nn, "%, std deviation:", round(sd(accuracy_nn),4))
cat("recall:", 100*round(mean(recall_nn),3), "%, std deviation:", round(sd(recall_nn),3))
cat("precision:", 100*round(mean(precision_nn),3), "%, std deviation:", round(sd(precision_knn),3))
f1=(2*round(mean(precision_nn),3)*round(sd(recall_nn),3))/(round(mean(precision_nn),3)+round(sd(recall_nn),3))
cat("\n F1: ",f1)
cat("average specificity:", 100*round(mean(specificity_nn),3), "%, std deviation:", round(sd(specificity_nn),4))



#KNN
experiments = 20
accuracy_knn = numeric()
recall_knn = numeric()
precision_knn = numeric()
rmse_knn = numeric()
specificity_knn = numeric()


for (i in 1:experiments) {
  
  sample = sample(c(TRUE,FALSE), nrow(data), replace = TRUE, prob = c(0.7,0.3))
  
  prolevel.train = data[sample,]
  labels_train <- prolevel.train[,which(names(prolevel.train) == "Pro.level")]
  prolevel.train <- prolevel.train[,-which(names(prolevel.train) == "Pro.level")]
  
  prolevel.test = data[!sample,]
  labels_test <- prolevel.test[,which(names(prolevel.test) == "Pro.level")]
  prolevel.test <- prolevel.test[,-which(names(prolevel.test) == "Pro.level")]
  
  labels_train <- factor(labels_train)
  param_grid <- expand.grid(k = 1:15)
  # Train and tune the KNN model
  knn_model <- train(
    x = prolevel.train,
    y = labels_train,
    method = "knn",
    tuneGrid = param_grid,
    trControl = trainControl(method = "cv", number = 10)  # Specify the number of cross-validation folds
  )
  
  # Print the best tuned parameter
  print(knn_model$bestTune)
  
  # Make predictions using the tuned KNN model
  knn_predictions <- predict(knn_model, prolevel.test)
  
  labels_test <- factor(labels_test)
  
  m.conf <- confusionMatrix(factor(knn_predictions), factor(labels_test))
  
  #Accuracy for each experiment
  accuracy_knn[i] = m.conf$overall['Accuracy']
  
  #Recall for each experiment
  recall_knn[i] = m.conf$byClass['Sensitivity']
  
  #Precision for each experiment
  precision_knn[i] = m.conf$byClass['Precision']
  
  rmse_knn[i] = RMSE(as.numeric(data.test$Pro.level),as.numeric(tree.pred))
  
  specificity_knn[i] <- m.conf$byClass['Specificity']
  
}

mean_accuracy_knn = 100*round(mean(accuracy_knn),4)
cat("average acc:", mean_accuracy_knn, "%, std deviation:", round(sd(accuracy_knn),4))
cat("recall:", 100*round(mean(recall_knn),3), "%, std deviation:", round(sd(recall_knn),3))
cat("precision:", 100*round(mean(precision_knn),3), "%, std deviation:", round(sd(precision_knn),3))
f1=(2*round(mean(precision_knn),3)*round(sd(recall_knn),3))/(round(mean(precision_knn),3)+round(sd(recall_knn),3))
cat("\n F1: ",f1)
cat("average specificity:", 100*round(mean(specificity_knn),3), "%, std deviation:", round(sd(specificity_knn),4))

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

#Hipótese nula (H0): Não há diferença significativa no desempenho dos dois melhores modelos.
#Hipótese alternativa (H1): Há diferença significativa no desempenho dos dois melhores modelos.

result <- wilcox.test(rmse_nn, rmse_tree)

if (result$p.value < 0.05) {
  print("Rejeita-se H0")
} else {
  print("Rejeita-se H1")
}








