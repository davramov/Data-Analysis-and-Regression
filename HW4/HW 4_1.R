credit <- read.csv("GermanCredit.csv",header=T)
summary(credit)

fullmod = glm(Class ~ .,data=credit,family=binomial)
summary(fullmod)

set.seed(10)
train=sample(1000,600)

#all data
fullmod = glm(Class ~ .,data=credit,family=binomial)
summary(fullmod)

#training subset
fullmod = glm(Class ~ .,data=credit,family=binomial,subset=train)
summary(fullmod)

#testing subset
fullmod = glm(Class ~ .,data=credit,family=binomial,subset=-train)
summary(fullmod)

nothing <- glm(Class ~ 1,data=credit,family=binomial,subset=train)
summary(nothing)

backwards = step(fullmod)
#  backwards = step(fullmod,trace=0) would suppress step by step output.

backwards = step(fullmod, direction="backward")
forwards = step(fullmod, direction="forward")
both = step(fullmod, direction="both")

full_backstep = backwards
train_backstep = backwards
test_backstep = backwards



formula(backwards)
summary(backwards)
backwards$anova

B = matrix(
  c(993.8,608.1,608.14,601.33,598.26,578.01),
 ncol=1))

1-pchisq(746.09-572.26,599 - 572.26)

1-pchisq(746.09-510.14,599 - 551)
#1-pchisq(null deviance - residual deviance, null df - resid df)

fullmod_f=fullmod$fitted.values
fullmod_f[fullmod_f>0.5]=1
fullmod_f[fullmod_f<=0.5]=0
table(fullmod_f,credit[-train,1])
prop.table(table(fullmod_f,credit[-train,1]),2)

y = predict(glm(Class ~ .,data=credit,family=binomial))
y=predict(full_backstep)
y=predict(train_backstep)
y=predict(test_backstep)
y[y>0.5]=1
y[y<=0.5]=0
table(y,credit[,1])
prop.table(table(y,credit[,1]),2)


require(pROC)
g <- roc()
plot(g) 
