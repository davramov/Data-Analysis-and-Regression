library("plyr")
library("tidyverse")
library("gmodels")

gas <- read.csv("GASTURBINE.csv")
summary(gas)
gas_lm = lm(HEATRATE~RPM+INLET.TEMP+EXH.TEMP+CPRATIO+AIRFLOW+INLET.EXH+dummy.traditional+dummy.advanced,data=gas)
summary(gas_lm)
anova(gas_lm)

#removed INLET.EXH
gas_lm = lm(HEATRATE~RPM+INLET.TEMP+EXH.TEMP+CPRATIO+AIRFLOW+dummy.traditional+dummy.advanced,data=gas)
summary(gas_lm)

#removed INLET.EXH + CPRATIO
gas_lm = lm(HEATRATE~RPM+INLET.TEMP+EXH.TEMP+AIRFLOW+dummy.traditional+dummy.advanced,data=gas)
summary(gas_lm)

#removed INLET.EXH + CPRATIO + dummy
gas_lm = lm(HEATRATE~RPM+INLET.TEMP+EXH.TEMP+AIRFLOW,data=gas)
summary(gas_lm)

#removed dummy
gas_lm = lm(HEATRATE~RPM+INLET.TEMP+EXH.TEMP+CPRATIO+AIRFLOW+INLET.EXH,data=gas)
summary(gas_lm)

attach(gas)
cpi = (gas$CPRATIO)^(-1)
IEi = sqrt((gas$INLET.EXH)^(-1.5))
plot(IEi,HEATRATE)
summary(lm(HEATRATE~IEi,data=gas))

#removed INLET.EXH, inversed CPRATIO
gas_lm = lm(HEATRATE~RPM+INLET.TEMP+EXH.TEMP+cpi+AIRFLOW+dummy.traditional+dummy.advanced,data=gas)
summary(gas_lm)

#removed INLET.EXH, and dummies; inversed CPRATIO
gas_lm = lm(HEATRATE~RPM+INLET.TEMP+EXH.TEMP+cpi,data=gas)
summary(gas_lm)
