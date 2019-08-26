set.seed(1, sample.kind="Rounding") 
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
set.seed(1, sample.kind="Rounding") 
predictions <- replicate(100, {
  index1 <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-index1)
  test_set  <-  dat %>% slice(index1)
  fit <- lm(y ~ x, data = train_set)
  y_hat <- fit$coef[1] + fit$coef[2]*test_set$x
  sqrt(mean((y_hat - test_set$y)^2))})
mean(predictions)
sd(predictions)