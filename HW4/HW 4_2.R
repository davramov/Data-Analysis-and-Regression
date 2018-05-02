credit <- read.csv("GermanCredit.csv",header=T)
summary(credit)

#logistic regression, all variables, all data points
fullmod = glm(Class ~ .,data=credit,family=binomial)
summary(fullmod)

#set the seed and training subset
set.seed(10)
train=sample(1000,600)

#training subset
trainmod = glm(Class ~ .,data=credit,family=binomial,subset=train)
summary(trainmod)

#null training subset
nothing <- glm(Class ~ 1,data=credit,family=binomial,subset=train)
summary(nothing)

#backwards stepwise logistic regression to narrow the number of variables
backwards = step(trainmod)
#  backwards = step(fullmod,trace=0) would suppress step by step output.
summary(backwards)
formula(backwards)

## the following equations are used to compare the efficacy of the step() function between training and testing subsets with the full data set

#train data set
trainmod1 = glm(Class ~ Duration + Amount + InstallmentRatePercentage + Age + NumberExistingCredits + NumberPeopleMaintenance + ForeignWorker + CheckingAccountStatus.lt.0 + CheckingAccountStatus.0.to.200 + CheckingAccountStatus.gt.200 + CreditHistory.NoCredit.AllPaid + CreditHistory.ThisBank.AllPaid + CreditHistory.PaidDuly +  CreditHistory.Delay + Purpose.NewCar + Purpose.Furniture.Equipment + Purpose.Repairs + Purpose.Education + SavingsAccountBonds.lt.100 + SavingsAccountBonds.100.to.500 + EmploymentDuration.lt.1 + EmploymentDuration.1.to.4 + Personal.Male.Single + OtherDebtorsGuarantors.None + OtherDebtorsGuarantors.CoApplicant + OtherInstallmentPlans.Bank,data=credit,family=binomial,subset=train)
summary(trainmod1)

#test data set
testmod = glm(Class ~ Duration + Amount + InstallmentRatePercentage + Age + NumberExistingCredits + NumberPeopleMaintenance + ForeignWorker + CheckingAccountStatus.lt.0 + CheckingAccountStatus.0.to.200 + CheckingAccountStatus.gt.200 + CreditHistory.NoCredit.AllPaid + CreditHistory.ThisBank.AllPaid + CreditHistory.PaidDuly +  CreditHistory.Delay + Purpose.NewCar + Purpose.Furniture.Equipment + Purpose.Repairs + Purpose.Education + SavingsAccountBonds.lt.100 + SavingsAccountBonds.100.to.500 + EmploymentDuration.lt.1 + EmploymentDuration.1.to.4 + Personal.Male.Single + OtherDebtorsGuarantors.None + OtherDebtorsGuarantors.CoApplicant + OtherInstallmentPlans.Bank,data=credit,family=binomial,subset=-train)
summary(testmod)

#full data set
fullmod = glm(Class ~ Duration + Amount + InstallmentRatePercentage + Age + NumberExistingCredits + NumberPeopleMaintenance + ForeignWorker + CheckingAccountStatus.lt.0 + CheckingAccountStatus.0.to.200 + CheckingAccountStatus.gt.200 + CreditHistory.NoCredit.AllPaid + CreditHistory.ThisBank.AllPaid + CreditHistory.PaidDuly +  CreditHistory.Delay + Purpose.NewCar + Purpose.Furniture.Equipment + Purpose.Repairs + Purpose.Education + SavingsAccountBonds.lt.100 + SavingsAccountBonds.100.to.500 + EmploymentDuration.lt.1 + EmploymentDuration.1.to.4 + Personal.Male.Single + OtherDebtorsGuarantors.None + OtherDebtorsGuarantors.CoApplicant + OtherInstallmentPlans.Bank,data=credit,family=binomial)
summary(fullmod)

#when this equals 0, the null hypothesis is rejected
#1-pchisq(null deviance - residual deviance, null df - resid df)

#train data set confusion matrix
trainmod_f=trainmod1$fitted.values
trainmod_f[trainmod_f>0.5]=1
trainmod_f[trainmod_f<=0.5]=0
table(trainmod_f,credit[train,1])
prop.table(table(trainmod_f,credit[train,1]),2)

#full data set confusion matrix
fullmod_f=fullmod$fitted.values
fullmod_f[fullmod_f>0.5]=1
fullmod_f[fullmod_f<=0.5]=0
table(fullmod_f,credit[,1])
prop.table(table(fullmod_f,credit[,1]),2)

#predicted confusion matrix based on validation dataset
y = predict(testmod)
y[y>0.5]=1
y[y<=0.5]=0
table(y,credit[-train,1])
prop.table(table(y,credit[-train,1]),2)

#create ROC curve of the full dataset
y=predict(fullmod)
require(pROC)
g <- roc(Class ~ y, data = credit)
plot(g) 
g
