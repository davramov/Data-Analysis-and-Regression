library("plyr")
library("tidyverse")
library("gmodels")
library("alluvial")

epa <- read_csv("auto.csv")

m1=mean(epa$`Vehicle Age`)
m2=mean(epa$Mileage)
m3=mean(epa$MPG)
sd1=sd(epa$`Vehicle Age`)
sd2=sd(epa$Mileage)
sd3=sd(epa$MPG)
r1min=m1-2*sd1
r1max=m1+2*sd1
r2min=m2-2*sd2
r2max=m2+2*sd2
r3min=m3-2*sd3
r3max=m3+2*sd3

epa %>%
  select(Gender, Purchased, Type) %>%
  subset(Gender=="Female") %>%
  subset(Type == "Large SUV")

epa %>%
  select(Gender, Purchased) %>%
  subset(Gender=="Female") %>%
  subset(Purchased=="Used")

epa %>%
  select(Type, Purchased) %>%
  subset(Type == "Small SUV") %>%
  subset(Purchased =="Used")

epa %>%
  select(Type, Gender, Purchased) %>%
  subset(Type == "Small SUV") %>%
  subset(Purchased =="Used")

hist(epa$MPG)

mean(epa$MPG)
sd(epa$MPG)

epa %>%
  select(Gender) %>%
  subset(Gender == "Male")
