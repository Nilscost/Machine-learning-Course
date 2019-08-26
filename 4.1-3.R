library(dslabs)
library(caret)
data("tissue_gene_expression")


set.seed(1, sample.kind="Rounding")

index1 <- createDataPartition(tissue_gene_expression$y, times = 1, p = 0.5, list = FALSE)
train_set <- as.data.frame(tissue_gene_expression) %>% slice(-index1)
test_set  <-  as.data.frame(tissue_gene_expression) %>% slice(index1)
k <- seq(1, 11, 2)
sapply(k, function(k){knn_fit <- knn3(train_set$y ~ ., data = train_set, k = k)
y_hat_knn <- predict(knn_fit, test_set, type = "class")
confusionMatrix(data = y_hat_knn, reference = test_set$y)$overall[["Accuracy"]]})
