#7 - moving beyond linearity

library(ISLR)
#Dados de salarios - 3000 Trabalhados homens no Mid-Atlantic region
attach(Wage)

#Regressão polinomial e Step Functions

#Regressão com 4 graus de liberdade
fit=lm(wage~poly(age,4),data=Wage)
#Os coeficientes gerados
coef(summary(fit))

#Regressão com 4 graus de liberdade sem polinômios ortogonais
fit2=lm(wage~poly(age,4,raw=T),data=Wage)
#Os coeficientes gerados
coef(summary(fit2))

#Regressão com 4 graus de liberdade sem polinômios ortogonais
fit2a=lm(wage~age+I(age^2)+I(age^3)+I(age^4),data=Wage)
#Os coeficientes gerados
coef(summary(fit2a))

#Regressão com 4 graus de liberdade sem polinômios ortogonais
fit2b=lm(wage~age+cbind(age^2)+cbind(age^3)+cbind(age^4),data=Wage)
#Os coeficientes gerados
coef(summary(fit2b))

#Criar um grid de idades onde será utilizado para a predição (Utilizando o modelo polinômios ortogonais)
agelims=range(age)
age.grid=seq(from=agelims[1],to=agelims[2])
preds=predict(fit,newdata = list(age=age.grid),se=TRUE)
se.bands=cbind(preds$fit+2*preds$se.fit, preds$fit-2*preds$se.fit)

#Criação do gráfico
par(mfrow=c(1,2), mar=c(4.5,4.5,1,1),oma=c(0,0,4,0))
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
title("Polinomio de 4 grau", outer= "blue")
matlines(age.grid, se.bands, lwd=1,col="blue", lty=3)

preds2=predict(fit2,newdata = list(age=age.grid),se=TRUE)
max(abs(preds$fit-preds2$fit))

#Agora será testado com 5 graus, mas para isso será utilizado um função anova() que usa o F-test para verificar a performance do Modelo
fit.1=lm(wage~age, data=Wage)
fit.2=lm(wage~poly(age,2), data=Wage)
fit.3=lm(wage~poly(age,3), data=Wage)
fit.4=lm(wage~poly(age,4), data=Wage)
fit.5=lm(wage~poly(age,5), data=Wage)

# Analisando, vemos que o fit.4 é o que mais se aproxima de 5% e por isso seria uma otima escolha
anova(fit.1, fit.2, fit.3, fit.4, fit.5)

# Mas poderia obter o mesmo resultado analisando apenas o polinomio ortogonal do 5 grau
coef(summary(fit.5))
#Os valores de p-values são semelhantes da função anova

#A diferença que usar a função anova possibilita utilizar outros modelos na comparação, exemplo:
fit.1=lm(wage~education+age, data=Wage)
fit.2=lm(wage~education+poly(age,2), data=Wage)
fit.3=lm(wage~education+poly(age,3), data=Wage)

anova(fit.1, fit.2, fit.3)

#Agora será criado um regressão logistica quando o individuo ganhar acima de 250k
fit=glm(I(wage>250)~poly(age, 4), data=Wage, family=binomial)

preds=predict(fit, newdata = list(age=age.grid), se=T)
