library(rpart)
library(caTools)
install.packages("tidyverse")
library(tidyverse)
# cross - validation methods
install.packages("caret", dependencies = c("Depends", "Suggests"))
install.packages("gower")
install.packages("hardhat")
library(caret)

#setwd("C:/Users/manu0/Desktop/RESTO/ANADI/TP2")
#data <- read.csv("ciclismo.csv")
setwd("C:/Users/asus/Desktop/ANADI/iteracao_2/anadi_isep_23/TP2/data")
normalised_data <- read.csv("normalised_data.csv")
#normalised_data
#1 - Estude a capacidade preditiva relativamente ao atributo “Pro_level” 
#usando os seguintes métodos:

#árvore de decisão;




#3 - Estude a capacidade preditiva relativamente ao atributo “Gender” usando os seguintes métodos:
# Convert 'gender' to a factor
normalised_data$gender <- as.factor(normalised_data$gender)

# Set control for cross-validation
ctrl <- trainControl(method = "cv", number = 10)

# Define the formula
formula <- gender ~ altitude_results + hr_results + vo2_results

# Neural Network model
nn <- train(formula, data = normalised_data, method = 'nnet', linout = FALSE, trControl = ctrl)

# K-Nearest Neighbors model
knn <- train(formula, data = normalised_data, method = "knn", trControl = ctrl)

# Extract accuracies from the resampling results
accuracy_nn <- nn$resample$Accuracy
accuracy_knn <- knn$resample$Accuracy

# Calculate mean and standard deviation of the accuracies
mean_acc_nn <- mean(accuracy_nn, na.rm = TRUE)
sd_acc_nn <- sd(accuracy_nn, na.rm = TRUE)

mean_acc_knn <- mean(accuracy_knn, na.rm = TRUE)
sd_acc_knn <- sd(accuracy_knn, na.rm = TRUE)

# Print the average and standard deviation of the accuracy for both models
print(paste("Neural Network - Mean accuracy: ", mean_acc_nn, " SD: ", sd_acc_nn))

print(paste("K-Nearest Neighbors - Mean accuracy: ", mean_acc_knn, " SD: ", sd_acc_knn))


#alinea b)
# Paired t-test
t_test_result <- t.test(accuracy_nn, accuracy_knn, paired = TRUE)

# Print the result
print(t_test_result)


#alinea c
# Make predictions using the models
nn_pred <- predict(nn, newdata = normalised_data)
knn_pred <- predict(knn, newdata = normalised_data)

# Compute confusion matrix for the neural network model
cm_nn <- confusionMatrix(nn_pred, normalised_data$gender)

# Compute confusion matrix for the KNN model
cm_knn <- confusionMatrix(knn_pred, normalised_data$gender)

# Print the overall statistics including Accuracy, Sensitivity, Specificity, and F1 score for neural network
print(paste("Neural Network - Accuracy: ", cm_nn$overall["Accuracy"]))
print(paste("Neural Network - Sensitivity: ", cm_nn$byClass["Sensitivity"]))
print(paste("Neural Network - Specificity: ", cm_nn$byClass["Specificity"]))
print(paste("Neural Network - F1 Score: ", cm_nn$byClass["F1"]))

# Print the overall statistics including Accuracy, Sensitivity, Specificity, and F1 score for KNN
print(paste("K-Nearest Neighbors - Accuracy: ", cm_knn$overall["Accuracy"]))
print(paste("K-Nearest Neighbors - Sensitivity: ", cm_knn$byClass["Sensitivity"]))
print(paste("K-Nearest Neighbors - Specificity: ", cm_knn$byClass["Specificity"]))
print(paste("K-Nearest Neighbors - F1 Score: ", cm_knn$byClass["F1"]))
