library("plyr")
library("tidyverse")
library("gmodels")

ojuice <- read_csv("OJUICE.csv")
x <- ojuice$Pectin
y <- ojuice$SweetIndex
df <- data.frame(x,y)


pl <- ggplot(ojuice, aes(x,y)) + geom_point()
pl + xlab("Pectin") + ylab("Sweet Index")

attach(ojuice)
plot(Pectin,SweetIndex,main="Scatterplot of Pectin vs SweetIndex")

ojuice_lm <-lm(SweetIndex~Pectin,data=ojuice)
summary(ojuice_lm)
anova(ojuice_lm)

Residuals <- resid(ojuice_lm)
sum_residuals=sum(Residuals)
sum_residuals

plot(Pectin, Residuals, main="Scatterplot of Pectin vs Residuals")
abline(0,0)

StandardResiduals = rstandard(ojuice_lm)
qqnorm(ojuice$Pectin,ylab="Pectin", xlab="Normal Scores",main="OJUICE data")

qqnorm(ojuice$SweetIndex,ylab="Sweet Index", xlab="Normal Scores",main="OJUICE data")

qqnorm(StandardResiduals,ylab="Standard Residuals", xlab="Normal Scores",main="OJUICE data")
qqline(StandardResiduals)

x_avg = ave(ojuice$Pectin)
y_avg = ave(ojuice$SweetIndex)
SSxy = sum((x-x_avg)*(y-y_avg))
SSxx = sum((x-x_avg)^2)
b1 = SSxy/SSxx
b0 = y_avg[1] - b1*x_avg[1]


