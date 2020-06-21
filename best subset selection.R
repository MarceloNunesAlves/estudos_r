library(ISLR)
fix(Hitters) #Mostra os dados em um grid
names(Hitters)

dim(Hitters) #Dataset de salario de jogadores de baseball
sum(is.na(Hitters$Salary)) #Qtde de registros sem o salario preenchido

Hitters=na.omit(Hitters) #remove os registros com NA

dim(Hitters)

sum(is.na(Hitters))

library(leaps)
regfit.full=regsubsets(Salary~., Hitters)
summary(regfit.full)

#Por padrão regsubset apresenta apenas os 8 melhores variaveis do modelo

regfit.full=regsubsets(Salary~., Hitters, nvmax=19) #Com nvmax é possivel definir um numero maior
reg.summary = summary(regfit.full)

names(reg.summary)

reg.summary$rsq #O R2 das 19 variaveis

par(mfrow=c(2,2))
plot(reg.summary$rss,xlab = "Number of Variables",ylab="RSS",type="l")
plot(reg.summary$adjr2,xlab="Number of Variables",ylab = "Adjusted RSq", type = "l")


which.max(reg.summary$adjr2) #Apresenta a posição do maior valor do vetor
points(11, reg.summary$adjr2[11], col="red", cex=2, pch=20)

plot(reg.summary$cp,xlab = "Number of Variables", ylab="Cp", type="l")
which.min(reg.summary$cp) #Apresenta a posição do menor valor do vetor
points(10, reg.summary$cp[10], col="red", cex=2, pch=20)

which.min(reg.summary$bic)
plot(reg.summary$bic,xlab = "Number of Variables", ylab="BIC", type="l")
points(6, reg.summary$bic[6], col="red", cex=2, pch=20)

#No top do gráfico mostra as melhores variaveis
plot(regfit.full,scale = "r2")
plot(regfit.full,scale = "adjr2")
plot(regfit.full,scale = "Cp")
plot(regfit.full,scale = "bic")

#Pegando o BIC que possui a menor quantidade de variaveis é possivel extrair os coeficientes com o comando abaixo
coef(regfit.full, 6)
