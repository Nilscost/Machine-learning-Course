library(caret)
library(dslabs)
library(MLmetrics)
set.seed(1, sample.kind="Rounding")
index1 <- createDataPartition(heights$sex, times = 1, p = 0.5, list = FALSE)
train_set <- heights %>% slice(-index1)
test_set  <-  heights %>% slice(index1)
F1 <- function(k){
  knn_fit <- knn3(train_set$sex ~ ., data = train_set, k = k)
  y_hat_knn <- predict(knn_fit, test_set, type = "class")
  CM <- confusionMatrix(data = y_hat_knn, reference = test_set$sex)
  CM$byClass[7]
}
k <- seq(1, 101, 3)
Scores <- sapply(k,F1)
max(Scores)
k[which.max(Scores)]




set.seed(1, sample.kind="Rounding")
knn_fit <- knn3(train_set$sex ~ train_set$height, data = train_set, k= 49)
y_hat_knn <- predict(knn_fit, test_set, type = "class")
CM <- confusionMatrix(data = y_hat_knn, reference = test_set$sex)
sens <- CM$byClass["Sensitivity"]
spec <- CM$byClass["Specificity"]
CM$byClass[7]
F_1_H <- 2*sens*spec/(sens+spec)
F_1_H
F1_Score(test_set$sex,y_hat_knn)



set.seed(1, sample.kind="Rounding")
F1 <- function(k){
knn_fit <- knn3(train_set$sex ~ train_set$height, data = train_set, k= k)
y_hat_knn <- predict(knn_fit, test_set, type = "class")
CM <- confusionMatrix(data = y_hat_knn, reference = test_set$sex)
sens <- CM$byClass["Sensitivity"]
spec <- CM$byClass["Specificity"]
F_1_H <- 2*(sens*spec)/(sens+spec)
F_1_H
}
k <- seq(1, 101, 3)
Scores <- sapply(k,F1)
max(Scores)
k[which.max(Scores)]


