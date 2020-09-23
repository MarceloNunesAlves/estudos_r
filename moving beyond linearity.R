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

pfit=exp(preds$fit)/(1+exp(preds$fit))
se.bands.logit = cbind(preds$fit+2*preds$se.fit, preds$fit-2*preds$se.fit)
se.bands = exp(se.bands.logit) / (1+exp(se.bands.logit))

preds = predict(fit, newdata = list(age=age.grid), type="response",se=T)

#Gráfico com as predições
plot(age, I(wage>250),xlim=agelims,type="n",ylim=c(0,.2))
points(jitter(age), I((wage>250)/5), cex=.5, pch="|",col="darkgrey")
lines(age.grid, pfit, lwd=2, col="blue")
matlines(age.grid,se.bands, lwd=1, col="blue", lty=3)

# Utilização da função cut para a mesma abordagem
table(cut(age,4))
fit=lm(wage~cut(age,4),data=Wage)
coef(summary(fit))
#Separa os ranges de idade de 33,5, 49 e 64,5 anos, e cria uma regrassão para cada faixa de idade

# Splines

library(splines)
#Neste ponto é espeficicado as quebras, que geram seis funções diferentes
fit=lm(wage~bs(age,knots=c(25,40,60)), data=Wage)
pred=predict(fit,newdata = list(age=age.grid),se=T)
plot(age,wage,col="gray")
lines(age.grid, pred$fit,lwd=2)
lines(age.grid, pred$fit+2*pred$se,lty="dashed")
lines(age.grid, pred$fit-2*pred$se,lty="dashed")

#Criando spline uniforme atraves das quebra dos quartis 25%, 50% e 75%
dim(bs(age,knots=c(25,40,60)))
dim(bs(age,df=6))
attr(bs(age,df=6), "knots")

#Função ns permiti utilizar 4 grau de liberdade
fit2=lm(wage~ns(age,df=4),data=Wage)
pred2=predict(fit2,newdata=list(age=age.grid),se=T)
lines(age.grid, pred2$fit, col="red", lwd=2)

#Função smoothing spline
plot(age, wage, xlim=agelims, cex=.5, col="darkgrey")
title("Smoothing Spline")
# Selecionando 16 graus de liberdade
fit=smooth.spline(age,wage,df=16)
# Selecionando graus de liberdade atraves do cross-validation (Validação cruzada)
fit2=smooth.spline(age,wage,cv=TRUE)
fit2$df
lines(fit, col="red", lwd=2)
lines(fit2, col="blue", lwd=2)
legend('topright', legend=c("16 DF", "6.8 DF"), col=c("red", "blue"), lty=1, lwd=2, cex=.8)

# Função local regression
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
title("Local Regression")
fit=loess(wage~age,span=.2,data=Wage)
fit2=loess(wage~age,span=.5,data=Wage)
lines(age.grid,predict(fit,data.frame(age=age.grid)),col="red",lwd=2)
lines(age.grid,predict(fit2,data.frame(age=age.grid)),col="blue",lwd=2)
legend('topright', legend=c("Span=0.2", "Span=0.5"), col=c("red", "blue"), lty=1, lwd=2, cex=.8)
# O resultado consiste em 20% ou 50% das observações. Quanto maior for a extensão, mais suave será o ajuste
