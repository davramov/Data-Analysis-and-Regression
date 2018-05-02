library("plyr")
library("tidyverse")
library("gmodels")



m1 <- read_csv("m1.csv")
plot(m1)

m1lm <-lm(y~x,data=m1)
summary(m1lm)
plot(m1lm)

x <- store$x
y <- store$y
x_avg = ave(store$x)
y_avg = ave(store$y)
SSxy = sum((x-x_avg)*(y-y_avg))
SSxx = sum((x-x_avg)^2)
b1 = SSxy/SSxx
b0 = y_avg[1] - b1*x_avg[1]


