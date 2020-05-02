library(ISLR)
set.seed(2)
train=sample(392,196)

# Na função de regressão logistica, quando não é passado a "family" funciona igual a um regressão linear
glm.fit = glm(mpg~horsepower, data=Auto)
coef(glm.fit)

# Função de regressão linear
lm.fit = lm(mpg~horsepower, data=Auto)
coef(lm.fit)

#Será utilizado a função pq pode ser usada junta com a função cv.glm()
library(boot)
glm.fit = glm(mpg~horsepower, data=Auto)
cv.err=cv.glm(Auto, glm.fit)
cv.err$delta

cv.error=rep(0,5)
for(i in 1:5){
  glm.fit = glm(mpg~poly(horsepower,i), data=Auto)
  cv.error[i]=cv.glm(Auto, glm.fit)$delta[1]
}

cv.error

#Implementando o k-Fold
set.seed(17)
cv.error.10=rep(0,10)
for(i in 1:10){
  glm.fit = glm(mpg~poly(horsepower,i), data=Auto)
  cv.error.10[i]=cv.glm(Auto, glm.fit, K=10)$delta[1]
}

cv.error.10
