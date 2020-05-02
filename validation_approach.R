library(ISLR)
set.seed(1)
train=sample(392,196)

#Treinamento de regressão linear apenas nas amostras
lm.fit = lm(mpg~horsepower, data=Auto, subset=train)

#Faz o teste nos dados que não foram usados no trainamento
attach(Auto)
mean((mpg-predict(lm.fit,Auto))[-train]^2)

#Uso de função polinomial
lm.fit2 = lm(mpg~poly(horsepower,2), data=Auto, subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)

lm.fit2 = lm(mpg~poly(horsepower,3), data=Auto, subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)

#Alterando a semente
set.seed(2)
train=sample(392,196)

#Treinamento de regressão linear apenas nas amostras
lm.fit = lm(mpg~horsepower, subset=train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)

lm.fit2 = lm(mpg~poly(horsepower,2), data=Auto, subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)

lm.fit2 = lm(mpg~poly(horsepower,3), data=Auto, subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)
