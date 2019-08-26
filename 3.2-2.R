library(broom)
mnist_27$train %>% glm(y ~ x_2, family = "binomial", data = .) %>% tidy()
qplot(x_2, y, data = mnist_27$train)
mnist_27$train %>% ggplot(aes(x_2, y)) +     geom_point() + 
  geom_smooth(color="red", span = 0.5, method = "loess", method.args = list(degree=1))