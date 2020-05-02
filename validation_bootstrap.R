library(ISLR)

#Bootstramp é um forma de validação que constitui em criar novas amostras baseado nos dados originais

#Função que calcula o alpha
alpha.fn = function(data, index){
  X=data$X[index]
  Y=data$Y[index]
  return ((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}

alpha.fn(Portfolio, 1:100)

set.seed(1)
alpha.fn(Portfolio, sample(100,100,replace=T))

boot(Portfolio, alpha.fn, R=1000)


#Utilizando para estimar o desempenho de feature na previsão
boot.fn=function(data, index){
  return(coef(lm(mpg~horsepower, data=data, subset = index)))
}
boot.fn(Auto, 1:392)

#
set.seed(1)
boot.fn(Auto, sample(392,392,replace=T))

boot(Auto, boot.fn, 1000)

summary(lm(mpg~horsepower, data=Auto))$coef

