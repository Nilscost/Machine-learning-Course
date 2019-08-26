set.seed(1, sample.kind="Rounding") 
n <- 100
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))
set.seed(1, sample.kind="Rounding") 
index1 <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-index1)
test_set  <-  dat %>% slice(index1)
fit <- lm(y ~ x_2, data = train_set)
y_hat <- fit$coef[1] + fit$coef[2]*test_set$x_2
sqrt(mean((y_hat - test_set$y)^2))