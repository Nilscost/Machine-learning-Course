library(dslabs)
library(dplyr)
library(lubridate)


data("reported_heights")

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

y_hat <- factor(ifelse(x == "inclass", "Female", "Male"))
confusionMatrix(data=y_hat, reference = y)
table(y_hat, y)
