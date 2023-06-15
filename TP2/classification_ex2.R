library(caret)
library(lubridate)
library(rpart)
library(rpart.plot)
library(rattle)
library(class)
library(neuralnet)

set.seed(123)
setwd("./TP2/data")
getwd()
data <- read.csv("ciclismo.csv")


ages <- interval(as.Date(data$dob), today()) %/% years(1)
data$age <- ages

data <- na.omit(data)
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
library(caret)
# install.packages("hardhat")
data <- data[,c(5,6,7,8)]
encoded_df <- dummyVars("~.", data = data)
data <- data.frame(predict(encoded_df, newdata = data))


# 1) Estude a capacidade preditiva relativamente ao atributo “Winter_training_camp”
# usando os seguintes métodos:
# - árvore de decisão;
# - rede neuronal; 

sample = sample(c(TRUE,FALSE), nrow(data), replace = TRUE, prob = c(0.7,0.3))

winterTrainingCamp.train = data[sample,]
winterTrainingCamp.test = data[!sample,]


# - árvore de decisão;

tree.model <- rpart(Winter.Training.Camp ~ hr_results + vo2_results + altitude_results, data = winterTrainingCamp.train, method = "class")
fancyRpartPlot(tree.model)

rpart.plot(tree.model, digits = 3, fallen.leaves = TRUE)

tree.pred = predict(tree.model, winterTrainingCamp.test, type = "class")

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

m.conf <- table(winterTrainingCamp.test$Winter.Training.Camp, tree.pred)
parse_results(m.conf)


#66.1% accuracy



#rede neuronal

numnodes <- 2

nn.model <- 
  neuralnet(
    Winter.Training.Camp ~ .,
    data = winterTrainingCamp.train,
    hidden = numnodes,
    act.fct = "logistic", 
    linear.output = FALSE
  )
plot(nn.model)

nn.model$result.matrix

predictions <- compute(nn.model, winterTrainingCamp.test[, -8])
predicted_classes <- ifelse(predictions$net.result > 0.5, 1, 0)
length(predicted_classes)
length(winterTrainingCamp.test$Winter.Training.Camp)
confusion_matrix <- table(predicted_classes, winterTrainingCamp.test$Winter.Training.Camp)
parse_results(confusion_matrix)

#71.25% accuracy

# a) Usando o método k-fold cross validation obtenha a média e o desvio padrão da 
# taxa de acerto da previsão do atributo “Winter_training_camp” com os dois 
# melhores modelos obtidos na alínea anterior

RMSE <- function(test, predicted) {
  sqrt(mean((test - predicted) ^ 2))
}

k <- 10  # Number of folds
ctrl = trainControl(
  method = "cv",
  number = k,
  verboseIter = FALSE
)

# - árvore de decisão;


nrFolds <- 10
accuracy_tree<-numeric()
recall_tree <- numeric()
precision_tree <- numeric()
rsme_tree <- numeric()

folds <- rep_len(1:nrFolds, nrow(winterTrainingCamp.train))
folds <- sample(folds, length(folds))

for(k in 1:nrFolds) {
  # actual split of the data
  fold <- which(folds == k)
  #k-1 to train and 1 to test
  data.train <- winterTrainingCamp.train[-fold,]
  data.test <- winterTrainingCamp.train[fold,]
  tree.model = rpart(Winter.Training.Camp ~ hr_results + vo2_results + altitude_results ,data = winterTrainingCamp.train, method="class")
  tree.pred = predict(tree.model, data.test, type='class')
  #head(tree.pred)
  #printcp(tree.model)
  confusion_tree <- confusionMatrix(as.factor(tree.pred), as.factor(data.test$Winter.Training.Camp))
  #or each fold:
  accuracy_tree[k] = confusion_tree$overall["Accuracy"]
  recall_tree[k] = confusion_tree$byClass["Sensitivity"]
  precision_tree[k] = confusion_tree$byClass["Pos Pred Value"]
  rsme_tree[k] = RMSE(as.numeric(data.test$Winter.Training.Camp),as.numeric(tree.pred))

}

cat("taxa de acerto media:", 100*round(mean(accuracy_tree),4), "%, desvio:", round(sd(accuracy_tree),4))
cat("recall:", 100*round(mean(recall_tree),3), "%, desvio:", round(sd(recall_tree),3))
cat("precision:", 100*round(mean(precision_tree),3), "%, desvio:", round(sd(precision_tree),3))
f1=(2*round(mean(precision_tree),3)*round(sd(recall_tree),3))/(round(mean(precision_tree),3)+round(sd(recall_tree),3))
cat("\n F1: ",f1)
cat("Média RMSE:", round(mean(rsme_tree),4))

#taxa de acerto media : 73.19%, desvio : 0.0643

# - rede neuronal;

nrFolds <- 10
accuracy_nn<-numeric()
recall_nn <- numeric()
precision_nn <- numeric()
rsme_nn <- numeric()

folds <- rep_len(1:nrFolds, nrow(winterTrainingCamp.train))
folds <- sample(folds, length(folds))

for(k in 1:nrFolds) {
  # actual split of the data
  fold <- which(folds == k)
  
  #k-1 to train and 1 to test
  data.train <- winterTrainingCamp.train[-fold,]
  data.test <- winterTrainingCamp.train[fold,]
  
  numnodes <- 2
  
  nn.model <- 
    neuralnet(
      Winter.Training.Camp ~ hr_results + vo2_results + altitude_results,
      data = data.train,
      hidden = numnodes,
      act.fct = "logistic", 
      linear.output = FALSE
    )
  
  predictions <- compute(nn.model, data.test[, -1])
  predicted_classes <- ifelse(predictions$net.result > 0.5, 1, 0)
  
  confusion_nn <- confusionMatrix(as.factor(predicted_classes), as.factor(data.test$Winter.Training.Camp))

  accuracy_nn[k] = confusion_nn$overall["Accuracy"]
  recall_nn[k] = confusion_nn$byClass["Sensitivity"]
  precision_nn[k] = confusion_nn$byClass["Pos Pred Value"]
  rsme_nn[k] = RMSE(as.numeric(data.test$Winter.Training.Camp), as.numeric(predicted_classes))
  
}

cat("taxa de acerto media:", 100*round(mean(accuracy_nn),4), "%, desvio:", round(sd(accuracy_nn),4))
cat("recall:", 100*round(mean(recall_nn),3), "%, desvio:", round(sd(recall_nn),3))
cat("precision:", 100*round(mean(precision_nn),3), "%, desvio:", round(sd(precision_nn),3))
f1=(2*round(mean(precision_nn),3)*round(sd(recall_nn),3))/(round(mean(precision_nn),3)+round(sd(recall_nn),3))
cat("\n F1: ",f1)
cat("Média RMSE:", round(mean(rsme_nn),4))

#Taxa de acerto media: 58.58%, desvio: 0.0554


#b) Verifique se existe diferença significativa no desempenho dos dois melhores 
# modelos obtidos anteriormente (use um nível de significância de 5%).


#Hipótese nula (H0): There is no significant difference in the performance of the decision tree and neural network models.
#Hipótese alternativa (H1): There is a significant difference in the performance of the decision tree and neural network models.

result <- wilcox.test(rsme_nn,rsme_tree)
result$p.value

if (result$p.value < 0.05) {
  print("Rejeita-se H0")
} else {
  print("Rejeita-se H1")
}


# c) Compare os resultados dos modelos. Identifique o modelo que apresenta o 
# melhor desempenho, de acordo com os critérios: Accuracy; Sensitivity; 
# Specificity e F1.


predictions_tree <- predict(model_tree, newdata = winterTrainingCamp.test[-1])
predictions_nn <- predict(model_nn, newdata = winterTrainingCamp.test[-1])

# Confusion matrix for neural network
confusion_nn <- confusionMatrix(predictions_nn, as.factor(winterTrainingCamp.test$Winter.Training.Camp))
accuracy_nn <- confusion_nn$overall["Accuracy"] # 0.5
sensitivity_nn <- confusion_nn$byClass["Sensitivity"] # 0.7560976
specificity_nn <- confusion_nn$byClass["Specificity"] # 0.137931
f1_nn <- confusion_nn$byClass["F1"] #0.6391753

# Confusion matrix for decision tree
confusion_tree <- confusionMatrix(predictions_tree, as.factor(winterTrainingCamp.test$Winter.Training.Camp))
accuracy_tree <- confusion_tree$overall["Accuracy"] # 0.6305085
sensitivity_tree <- confusion_tree$byClass["Sensitivity"] #0.7424242
specificity_tree <- confusion_tree$byClass["Specificity"] #0.4020619
f1_tree <- confusion_tree$byClass["F1"] #0.7295285


#Accuracy -> Decision Tree
#Sensitivity -> Neural Network
#Specificity -> Decision Tree
#F1 -> Decision Tree



