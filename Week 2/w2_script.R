library("plyr")
library("tidyverse")
library("gmodels")

store <- read_csv("store.csv")
tp <- read_csv("tp.csv")
plot(store)

storelm <-lm(y~x,date=store)
summary(storelm)
plot(storelm)

x <- store$x
y <- store$y
x_avg = ave(store$x)
y_avg = ave(store$y)
SSxy = sum((x-x_avg)*(y-y_avg))
SSxx = sum((x-x_avg)^2)
b1 = SSxy/SSxx
b0 = y_avg[1] - b1*x_avg[1]


