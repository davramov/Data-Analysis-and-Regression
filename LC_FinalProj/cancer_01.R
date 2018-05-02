#https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE68465

data <- read.csv('LungCancer.csv')
library(rpart)
library(rpart.plot)
library(survminer)
library(survival)

lung_data=data
lung_data

# exclude variable Survival.Status
myvars <- names(data) %in% c("Survival.Status", "Gender","time","status","Smoking.Status","MargNeg.CurrentSmoker","MargNeg.NeverSmoker","MargNeg.ExSmoker") 
lung_data <- data[!myvars]

summary(lung_data)

survival_time = lm(Months.Last.Contact.or.Death~.,data=lung_data)
summary(survival_time)
backwards = step(survival_time)
summary(backwards)

ggplot(data=lung_data, aes(x = N.Stage,y = Months.Last.Contact.or.Death,shape = Survival.Status)) + geom_jitter()

ggplot(data=lung_data, aes(x = T.Stage,y = Months.Last.Contact.or.Death,shape = Survival.Status)) + geom_jitter()

survival = glm(Months.to.Last.Followup ~ .,
               family = binomial(link = logit),
               data = lung_data)
summary(survival)
formula(survival)

your.data <- data.frame(lung_data)
time <- your.data$time
status <- your.data$status

lfit <- aareg(Surv(time, status) ~ Diagnosis.Age + Gender.Male, data=lung_data,nmin=1)
plot(lfit)
fit <- survfit(Surv(time,status)~ Gender, data=lung_data)
ggsurvplot(fit,data=lung_data,risk.table=FALSE)

fit <- survfit(Surv(time,status)~ Smoking.Status, data=lung_data)
ggsurvplot(fit,data=lung_data,risk.table=FALSE)

fit <- survfit(Surv(time,status)~ Margins.Negative, data=lung_data)
ggsurvplot(fit,data=lung_data,risk.table=FALSE)

gsurvplot(
  fit,                     # survfit object with calculated statistics.
  data = BRCAOV.survInfo,  # data used to fit survival curves. 
  risk.table = TRUE,       # show risk table.
  pval = TRUE,             # show p-value of log-rank test.
  conf.int = TRUE,         # show confidence intervals for 
  # point estimaes of survival curves.
  xlim = c(0,2000),        # present narrower X axis, but not affect
  # survival estimates.
  break.time.by = 500,     # break X axis in time intervals by 500.
  ggtheme = theme_minimal(), # customize plot and risk table with a theme.
  risk.table.y.text.col = T, # colour risk table text annotations.
  risk.table.y.text = FALSE # show bars instead of names in text annotations
  # in legend of risk table
)

summary(survival)
#full data set confusion matrix
fullmod_f = survival$fitted.values
fullmod_f[fullmod_f > 0.5] = 1
fullmod_f[fullmod_f <= 0.5] = 0
table(fullmod_f, lung_data[, 1])
prop.table(table(fullmod_f, lung_data[, 1]), 2)

alive <- subset(lung_data, Survival.Dead<0.5)
dead <- subset(lung_data, Survival.Dead>0.5)
ggplot() + geom_density(data=alive, aes(Months.Last.Contact.or.Death), color="red") + geom_density(data=dead, aes(Months.Last.Contact.or.Death),color="blue")

ggplot() + geom_point(data=dead, aes(Months.Last.Contact.or.Death ))
library(survival)

ggplot() + geom_density(data=lung_data, aes(T.Stage)) + geom_density(lung_data, aes(N.Stage))

ggplot(lung_data,
       aes(x = Diagnosis.Age, y = Months.Last.Contact.or.Death)) +
  geom_point(size = 2, aes(shape = Survival.Status)) + geom_density2d()

ggplot(lung_data,
       aes(x = Diagnosis.Age, y = Months.Last.Contact.or.Death)) +
  geom_point(size = 2, aes(shape = Survival.Status)) + geom_density2d()


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

set.seed(5)
train = sample(477, 287)

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
    First.Progression.or.Relapse +
    Months.to.Last.Clinical.Assessment +
    Progression.Assessment.Death.Interaction +
    N.Stage +
    T.Stage +
    N.T.Interaction +
    Current.Smoker	+
    Never.Smoker	+
    Ex.Smoker	+
    Margins.Negative,
  family = binomial(link = logit),
  data = lung_data
)
summary(survivalmod_full)
formula(survivalmod_full)

#1-pchisq(null deviance - residual deviance, null df - resid df)
1 - pchisq(658.69 - 355.70, 476 - 458)

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
    First.Progression.or.Relapse +
    Months.to.Last.Clinical.Assessment +
    Progression.Assessment.Death.Interaction +
    N.Stage +
    T.Stage +
    N.T.Interaction +
    Current.Smoker	+
    Never.Smoker	+
    Ex.Smoker	+
    Margins.Negative,
  family = binomial(link = logit),
  data = lung_data,
  subset = train
)
summary(survivalmod_train)

#1-pchisq(null deviance - residual deviance, null df - resid df)
1 - pchisq(396.61 - 217.09, 286 - 268)

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
    First.Progression.or.Relapse +
    Months.to.Last.Clinical.Assessment +
    Progression.Assessment.Death.Interaction +
    N.Stage +
    T.Stage +
    N.T.Interaction +
    Current.Smoker	+
    Never.Smoker	+
    Ex.Smoker	+
    Margins.Negative,
  family = binomial(link = logit),
  data = lung_data,
  subset = -train
)
summary(survivalmod_validation)

#1-pchisq(null deviance - residual deviance, null df - resid df)
1 - pchisq(262.05 - 108.84, 189 - 171)

#predicted confusion matrix based on validation dataset
y = predict(survivalmod_validation)
y[y > 0.5] = 1
y[y <= 0.5] = 0
table(y, lung_data[-train, 1])
prop.table(table(y, lung_data[-train, 1]), 2)

#create ROC curve of the full dataset
y = predict(survivalmod_full)
require(pROC)
g <- roc(Survival.Dead ~ y, data = lung_data)
plot(g)
g
