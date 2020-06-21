library(ISLR)

reg.fit.fwd = regsubsets(Salary~.,data=Hitters,nvmax=19,method="forward")
summary(reg.fit.fwd)

reg.fit.bwd = regsubsets(Salary~.,data=Hitters,nvmax=19,method="backward")
summary(reg.fit.bwd)

# As 7 melhores variaveis pelo processo do best subset
coef(regfit.full, 7)

# As 7 melhores variaveis pelo processo do forward
coef(reg.fit.fwd, 7)

# As 7 melhores variaveis pelo processo do backward
coef(reg.fit.bwd, 7)

# Para cada processo é apresentado variaveis diferentes