credit <- read.csv("GermanCredit.csv",header=T)
summary(credit)

glm(Class ~ .,data=credit,family=binomial(link=logit))

set.seed(10)
train=sample(1000,600)

glm(Class ~ .,data=credit,family=binomial(link=logit),subset=train)

c_glm=glm(Class ~ .,data=credit,family=binomial(link=logit),subset=train)

c_glm_fitted=c_glm$fitted.values

c_glm_fitted[c_glm_fitted>0.5]=1
c_glm_fitted[c_glm_fitted<=0.5]=0
table(c_glm_fitted,credit[train,1])
prop.table(table(c_glm_fitted,credit[train,1]),2)

summary(c_glm)
summary(c_glm_fitted)
c_glm_fitted



c_glm_subset=glm(Class ~ CheckingAccountStatus.lt.0 +
                 CheckingAccountStatus.0.to.200 + 
                 CreditHistory.ThisBank.AllPaid + 
                 InstallmentRatePercentage +
                 ForeignWorker +
                 CreditHistory.NoCredit.AllPaid +
                 SavingsAccountBonds.lt.100 +
                 OtherDebtorsGuarantors.CoApplicant +
                 OtherInstallmentPlans.Bank +
                 Duration +
                 Amount +
                 Age +
                 OtherDebtorsGuarantors.None +
                 NumberExistingCredits +
                 NumberPeopleMaintenance +
                 CheckingAccountStatus.gt.200 +
                 CreditHistory.PaidDuly +
                 Purpose.Education +
                 Property.RealEstate +
                 Property.Insurance,data=credit,family=binomial(link=logit),subset=train)



summary(c_glm_subset)

c_glm_subset1=glm(Class ~ CreditHistory.ThisBank.AllPaid+
                  OtherDebtorsGuarantors.CoApplicant+
                  Age+
                  CheckingAccountStatus.lt.0+
                  CheckingAccountStatus.0.to.200+
                  CreditHistory.NoCredit.AllPaid+
                  SavingsAccountBonds.lt.100+
                  Duration+
                  OtherDebtorsGuarantors.None+
                  InstallmentRatePercentage+
                  ForeignWorker+
                  Purpose.Education,data=credit,family=binomial(link=logit),subset=train)
summary(c_glm_subset1)

c_glm_subset1=predict(glm(Class ~ CreditHistory.ThisBank.AllPaid+
                    OtherDebtorsGuarantors.CoApplicant+
                    Age+
                    CheckingAccountStatus.lt.0+
                    CheckingAccountStatus.0.to.200+
                    CreditHistory.NoCredit.AllPaid+
                    SavingsAccountBonds.lt.100+
                    Duration+
                    OtherDebtorsGuarantors.None+
                    InstallmentRatePercentage+
                    ForeignWorker+
                    Purpose.Education,data=credit,family=binomial(link=logit),subset=train))

summary(c_glm_subset1)

c_glmsub1_fitted=c_glm_subset1$fitted.values

c_glmsub1_fitted[c_glmsub1_fitted>0.5]=1
c_glmsub1_fitted[c_glmsub1_fitted<=0.5]=0
table(c_glmsub1_fitted,credit[train,1])
prop.table(table(c_glmsub1_fitted,credit[train,1]),2)

1-pchisq(746.09-572.26,599-587)
#output is p-value