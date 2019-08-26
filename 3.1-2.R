
function1 <- function(n){Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
predictions <- replicate(100, {
  index1 <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-index1)
  test_set  <-  dat %>% slice(index1)
  fit <- lm(y ~ x, data = train_set)
  y_hat <- fit$coef[1] + fit$coef[2]*test_set$x
  sqrt(mean((y_hat - test_set$y)^2))})
print(n)
print(mean(predictions))
print(sd(predictions))}
set.seed(1, sample.kind="Rounding")
n <- c(100, 500, 1000, 5000, 10000)
sapply(n, function1)