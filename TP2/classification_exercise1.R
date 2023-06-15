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

data <- data[,c(4,6,7,8)]
encoded_df <- dummyVars("~.", data = data)
data <- data.frame(predict(encoded_df, newdata = data))


#1 - Estude a capacidade preditiva relativamente ao atributo “Pro_level” 
#usando os seguintes métodos:

#divisão do dataset em treino e validação (70% e 30%)
sample = sample(c(TRUE,FALSE), nrow(data), replace = TRUE, prob = c(0.7,0.3))

prolevel.train = data[sample,]
prolevel.test = data[!sample,]

#árvore de decisão;
#67.12% de Accuracy
tree.model <- rpart(Pro.level ~ hr_results + vo2_results + altitude_results, data = prolevel.train, method = "class")
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

#Rede Neuronal
#67.46% de Accuracy
accuracy = numeric()

for (k in 1:10){
  nn.model <- 
    neuralnet(
      Pro.level ~ hr_results + vo2_results + altitude_results,
      data = prolevel.train,
      hidden = k,
      act.fct = "logistic", 
      linear.output = FALSE
    )
  
  predictions <- compute(nn.model, prolevel.test[, -1])
  predicted_classes <- ifelse(predictions$net.result > 0.5, 1, 0)
  
  confusion_nn <- confusionMatrix(factor(predicted_classes), factor(prolevel.test$Pro.level))

  accuracy[k] = confusion_nn$overall["Accuracy"]
  
}

numnodes <- which.max(accuracy)
cores <- rep("lightblue", 10)  # Vetor de cores com "red" repetido 10 vezes
cores[numnodes] <- "blue"       # Definindo a cor azul para o ponto de número 5

plot(c(1:10), accuracy, type = "p", pch = 16, col = cores, xlab = "Nodes Number in a Neural Network", ylab = "Accuracy")


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

predictions <- compute(nn.model, prolevel.test[, -1])
predicted_classes <- ifelse(predictions$net.result > 0.5, 1, 0)

confusion_matrix <- table(predicted_classes, prolevel.test$Pro.level)
parse_results(confusion_matrix)

#KNN
#66.44% de Accuracy

labels_train <- prolevel.train[,which(names(prolevel.train) == "Pro.level")]
train <- prolevel.train[,-which(names(prolevel.train) == "Pro.level")]

labels_test <- prolevel.test[,which(names(prolevel.test) == "Pro.level")]
test <- prolevel.test[,-which(names(prolevel.test) == "Pro.level")]

labels_train <- factor(labels_train)

for (k in 1:15){
  predictions <- knn(train, test, labels_train, k, prob=TRUE)
  
  labels_test <- factor(labels_test)
  
  confusion_knn <- confusionMatrix(labels_test,predictions)
  
  accuracy[k] = confusion_knn$overall["Accuracy"]
  #print(accuracy[k])
  
}

best_k <- which.max(accuracy)

cores <- rep("lightblue", 10)  # Vetor de cores com "red" repetido 10 vezes
cores[best_k] <- "blue"       # Definindo a cor azul para o ponto de número 5

plot(c(1:15), accuracy, type = "p", pch = 16, col = cores, xlab = "k-nearest neighbors", ylab = "Accuracy")


predictions <- knn(train, test, labels_train, best_k, prob=TRUE)
labels_test <- factor(labels_test)

m.conf<-table(labels_test,predictions)
parse_results(m.conf)

# Confusion matrix for kNN
confusion_knn <- confusionMatrix(labels_test,predictions)
accuracy_knn <- confusion_knn$overall["Accuracy"]
sensitivity_knn <- confusion_knn$byClass["Sensitivity"]
specificity_knn <- confusion_knn$byClass["Specificity"]
f1_knn <- confusion_knn$byClass["F1"]
precision_knn <- confusion_knn$byClass["Pos Pred Value"]

#a) Usando o método k-fold cross validation obtenha a média e o desvio padrão da 
#taxa de acerto da previsão do atributo “Pro_level” com os dois melhores 
#modelos obtidos na alínea anterior. 

RMSE <- function(test, predicted) {
  sqrt(mean((test - predicted) ^ 2))
}

k <- 10  # Number of folds
#Arvore de decisão
# Melhores resultados cp=0.05  Accuracy=0.6506394

nrFolds <- 10
accuracy_tree<-numeric()
recall_tree <- numeric()
precision_tree <- numeric()
rsme_tree <- numeric()

folds <- rep_len(1:nrFolds, nrow(prolevel.train))
folds <- sample(folds, length(folds))

for(k in 1:nrFolds) {
  # actual split of the data
  fold <- which(folds == k)
  #k-1 to train and 1 to test
  data.train <- prolevel.train[-fold,]
  data.test <- prolevel.train[fold,]
  tree.model = rpart(Pro.level ~ hr_results + vo2_results + altitude_results ,data = prolevel.train, method="class")
  tree.pred = predict(tree.model, data.test, type='class')
  #head(tree.pred)
  #printcp(tree.model)
  confusion_tree <- confusionMatrix(as.factor(tree.pred), as.factor(data.test$Pro.level))
  #or each fold:
  accuracy_tree[k] = confusion_tree$overall["Accuracy"]
  recall_tree[k] = confusion_tree$byClass["Sensitivity"]
  precision_tree[k] = confusion_tree$byClass["Pos Pred Value"]
  rsme_tree[k] = RMSE(as.numeric(data.test$Pro.level),as.numeric(tree.pred))

}

cat("taxa de acerto media:", 100*round(mean(accuracy_tree),4), "%, desvio:", round(sd(accuracy_tree),4))
cat("recall:", 100*round(mean(recall_tree),3), "%, desvio:", round(sd(recall_tree),3))
cat("precision:", 100*round(mean(precision_tree),3), "%, desvio:", round(sd(precision_tree),3))
f1=(2*round(mean(precision_tree),3)*round(sd(recall_tree),3))/(round(mean(precision_tree),3)+round(sd(recall_tree),3))
cat("\n F1: ",f1)
cat("Média RMSE:", round(mean(rsme_tree),4))
print(rsme_tree)


#Rede Neuronal
#Melhores resultados size = 6 and decay = 0.1, Accuracy = 0.6608056
nrFolds <- 10
accuracy_nn<-numeric()
recall_nn <- numeric()
precision_nn <- numeric()
rsme_nn <- numeric()

folds <- rep_len(1:nrFolds, nrow(prolevel.train))
folds <- sample(folds, length(folds))

for(k in 1:nrFolds) {
  # actual split of the data
  fold <- which(folds == k)
  
  #k-1 to train and 1 to test
  data.train <- prolevel.train[-fold,]
  data.test <- prolevel.train[fold,]
  
  nn.model <- 
    neuralnet(
      Pro.level ~ hr_results + vo2_results + altitude_results,
      data = data.train,
      hidden = 1,
      act.fct = "logistic", 
      linear.output = FALSE
    )
  
  predictions <- compute(nn.model, data.test[, -1])
  predicted_classes <- ifelse(predictions$net.result > 0.5, 1, 0)
  
  confusion_nn <- confusionMatrix(as.factor(predicted_classes), as.factor(data.test$Pro.level))

  accuracy_nn[k] = confusion_nn$overall["Accuracy"]
  recall_nn[k] = confusion_nn$byClass["Sensitivity"]
  precision_nn[k] = confusion_nn$byClass["Pos Pred Value"]
  rsme_nn[k] = RMSE(as.numeric(data.test$Pro.level), as.numeric(predicted_classes))
  
}

cat("taxa de acerto media:", 100*round(mean(accuracy_nn),4), "%, desvio:", round(sd(accuracy_nn),4))
cat("recall:", 100*round(mean(recall_nn),3), "%, desvio:", round(sd(recall_nn),3))
cat("precision:", 100*round(mean(precision_nn),3), "%, desvio:", round(sd(precision_nn),3))
f1=(2*round(mean(precision_nn),3)*round(sd(recall_nn),3))/(round(mean(precision_nn),3)+round(sd(recall_nn),3))
cat("\n F1: ",f1)
cat("Média RMSE:", round(mean(rsme_nn),4))

print(rsme_nn)

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

#Hipótese nula (H0): Há diferença significativa no desempenho dos dois melhores modelos.
#Hipótese alternativa (H1): Não Há diferença significativa no desempenho dos dois melhores modelos.

result <- wilcox.test(rsme_nn,rsme_tree)
result$p.value

if (result$p.value < 0.05) {
  print("Rejeita-se H0")
} else {
  print("Rejeita-se H1")
}


#d) Compare os resultados dos modelos. Discuta em detalhe qual o modelo que 
#apresentou melhor e pior desempenho de acordo com os critérios: Accuracy; 
#Sensitivity; Specificity e F1.


predictions_tree <-predict(tree.model, prolevel.test, type='class')
predictions <- compute(nn.model, prolevel.test[, -1])
predictions_nn <- ifelse(predictions$net.result > 0.5, 1, 0)



# Confusion matrix for neural network
confusion_nn <- confusionMatrix(as.factor(predictions_nn), as.factor(prolevel.test$Pro.level))
accuracy_nn <- confusion_nn$overall["Accuracy"]
precision_nn <- confusion_nn$byClass["Pos Pred Value"]
sensitivity_nn <- confusion_nn$byClass["Sensitivity"]
specificity_nn <- confusion_nn$byClass["Specificity"]
f1_nn <- confusion_nn$byClass["F1"]

# Confusion matrix for decision tree
confusion_tree <- confusionMatrix(predictions_tree, as.factor(prolevel.test$Pro.level))
accuracy_tree <- confusion_tree$overall["Accuracy"]
sensitivity_tree <- confusion_tree$byClass["Sensitivity"]
precision_tree <- confusion_tree$byClass["Pos Pred Value"]
specificity_tree <- confusion_tree$byClass["Specificity"]
f1_tree <- confusion_tree$byClass["F1"]





