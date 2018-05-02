lung_data <- read.csv('LungCancer.csv')
library(rpart)
library(rpart.plot)
library(caret)

lung_data <- na.omit(data)

survival = glm(Survival.Dead ~ .,
               family = binomial(link = logit),
               data = lung_data)
summary(survival)
formula(survival)

#full data set confusion matrix
fullmod_f = survival$fitted.values
fullmod_f[fullmod_f > 0.5] = 1
fullmod_f[fullmod_f <= 0.5] = 0
table(fullmod_f, lung_data[, 1])
prop.table(table(fullmod_f, lung_data[, 1]), 2)

ggplot(lung_data, aes(Months.Last.Contact.or.Death)) + geom_density()
ggplot(lung_data, aes(T.Stage)) + geom_density()
ggplot(lung_data, aes(N.Stage)) + geom_density()

ggplot(lung_data,
       aes(x = Diagnosis.Age, y = Months.Last.Contact.or.Death)) +
  geom_point(size = 3, aes(shape = Survival.Status)) + geom_density2d()

ggplot(lung_data,
       aes(x = Diagnosis.Age, y = Months.Last.Contact.or.Death)) +
  geom_point(size = 3, aes(shape = Survival.Status)) + geom_density2d()


#when this equals 0, the null hypothesis is rejected
#1-pchisq(null deviance - residual deviance, null df - resid df)
1 - pchisq(659.93 - 357.92, 477 - 455)
#^ that = 0 :arthurdance:

tree = rpart(Survival.Dead ~ ., data = lung_data, method = "class")
summary(tree)
print(tree)
printcp(tree)
rpart.plot(tree)
text(tree)

lung_data <- na.omit(lung_data)
set.seed(5)
train = sample(477, 287)

plot(lung_data$N.Stage, lung_data$T.Stage)
ggplot(lung_data) + geom_bar(aes(First.Progression.or.Relapse))

trainmod = glm(
  Survival.Dead ~ .,
  data = lung_data,
  family = binomial(link = logit),
  subset = train
)
summary(trainmod)

backwards = step(trainmod)
summary(backwards)
formula(backwards)

survivalmod_full = glm(
  Survival.Dead ~
    Gender.Male +
    Diagnosis.Age +
    Race.Asian +
    Race.Black +
    Race.Unknown +
    Race.White +
    Adjuvant.Chemo +
    Adjuvant.RT +
    Chemo.RT.Interaction +
    First.Progression.or.Relapse +
    Months.to.Last.Clinical.Assessment +
    Progression.Assessment.Death.Interaction +
    N.Stage +
    T.Stage +
    N.T.Interaction,
  family = binomial(link = logit),
  data = lung_data
)
summary(survival)
formula(survival)

#full data set confusion matrix
fullmod_f = survivalmod_full$fitted.values
fullmod_f[fullmod_f > 0.5] = 1
fullmod_f[fullmod_f <= 0.5] = 0
table(fullmod_f, lung_data[, 1])
prop.table(table(fullmod_f, lung_data[, 1]), 2)

survivalmod_train = glm(
  Survival.Dead ~
    Gender.Male +
    Diagnosis.Age +
    Race.Asian +
    Race.Black +
    Race.Unknown +
    Race.White +
    Adjuvant.Chemo +
    Adjuvant.RT +
    Chemo.RT.Interaction +
    First.Progression.or.Relapse +
    Months.to.Last.Clinical.Assessment +
    Progression.Assessment.Death.Interaction +
    N.Stage +
    T.Stage +
    N.T.Interaction,
  family = binomial(link = logit),
  data = lung_data,
  subset = train
)
summary(survivalmod_train)

#train data set confusion matrix
trainmod_f = survivalmod_train$fitted.values
trainmod_f[trainmod_f > 0.5] = 1
trainmod_f[trainmod_f <= 0.5] = 0
table(trainmod_f, lung_data[train, 1])
prop.table(table(trainmod_f, lung_data[train, 1]), 2)

survivalmod_validation = glm(
  Survival.Dead ~
    Gender.Male +
    Diagnosis.Age +
    Race.Asian +
    Race.Black +
    Race.Unknown +
    Race.White +
    Adjuvant.Chemo +
    Adjuvant.RT +
    Chemo.RT.Interaction +
    First.Progression.or.Relapse +
    Months.to.Last.Clinical.Assessment +
    Progression.Assessment.Death.Interaction +
    N.Stage +
    T.Stage +
    N.T.Interaction,
  family = binomial(link = logit),
  data = lung_data,
  subset = -train
)

#predicted confusion matrix based on validation dataset
y = predict(survivalmod_validation)
y[y > 0.5] = 1
y[y <= 0.5] = 0
table(y, lung_data[-train, 1])
prop.table(table(y, lung_data[-train, 1]), 2)

#create ROC curve of the full dataset
y = predict(survivalmod_full)
require(pROC)
g <- roc(Survival.Dead ~ y, data = lung_data, subset = train)
plot(g)
g
