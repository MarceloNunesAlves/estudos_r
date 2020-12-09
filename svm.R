library(e1071)

# Criando dados fictício para a analise
set.seed(1)
x=matrix(rnorm(20*2), ncol=2)
y=c(rep(-1,10), rep(1,10))
x[y==1,]=x[y==1,]+1
plot(x, col=(3-y))

dat = data.frame(x=x, y=as.factor(y))
#kernel linear é usado para dados lineares. cost qto maior mas overfitting e menor mais underfitting
#scale=FALSE faz com que a função não escale variaveis com media zero ou desvio padrão 1
svmfit=svm(y~.,data=dat,kernel="linear",cost=10,scale=FALSE)

#Gera um gráfico com a separação das classes
plot(svmfit, dat)

#Este dados é as observações que fazem parte 7 vetores de suporte (Numero de observações dentro da margem)
svmfit$index

#Para mais detalhes do modelo
summary(svmfit)

svmfit=svm(y~.,data=dat,kernel="linear",cost=0.1,scale=FALSE)
plot(svmfit, dat)
# Com a nova parametrização com o custo menorm a quantidade de vetores aumentou pois a margem esta maior
svmfit$index

#tune é uma forma de fazer o CV do SVM
set.seed(1)
tune.out=tune(svm, y~.,data=dat,kernel="linear",ranges = list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)))

#Aqui é apresentado o resumo com os melhores parametros
summary(tune.out)

#Pegando o melhor modelo
bestmod=tune.out$best.model
summary(bestmod)

#Gerando os dados de teste
xtest=matrix(rnorm(20*2),ncol=2)
ytest=sample(c(-1,1), 20, rep=TRUE)
xtest[ytest==1,]=xtest[ytest==1,]+1
testdat=data.frame(x=xtest, y=as.factor(ytest))

#Prevendo com o melhor modelo CV
ypred = predict(bestmod, testdat)
table(predict = ypred, truth=testdat$y)

#Testando com um custo diferente
svmfit=svm(y~.,data=dat,kernel="linear",cost=0.01,scale=FALSE)
ypred = predict(svmfit, testdat)
table(predict = ypred, truth=testdat$y)
#Resulta mostra uma curaciadade inferior

#Agora com dados com uma separação claramente linear
x[y==1,]=x[y==1,]+0.5
plot(x, col=(y+5)/2, pch=19)

dat=data.frame(x=x, y=as.factor(y))
svmfit=svm(y~.,data=dat,kernel="linear",cost=1e5)
#Gerou uma margem com apenas 3 vetores de suporte
summary(svmfit)

#Alterando o custo para 1
svmfit=svm(y~.,data=dat,kernel="linear",cost=1)
# Este modelo uso 7 vetores e provavelmente terá um desempenho melhor do que o custo de 1e5
summary(svmfit)

#Trabalhando com outros kernel
set.seed(1)
x=matrix(rnorm(200*2), ncol=2)
x[1:100,]=x[1:100,]+2
x[101:150,]=x[101:150,]-2
y=c(rep(1,150),rep(2,50))
dat=data.frame(x=x, y=as.factor(y))

#Analisando o gráfico vemos que o um determinada classe sem encontra no centro
plot(x, col=y)

#Usando o kernel radial
train=sample(200,100)
svmfit=svm(y~.,data=dat[train,],kernel="radial",gamma=1,cost=1)
plot(svmfit, dat[train,])

summary(svmfit)

#Por conta do erro elevado talvez seja necessario aumentar o custo mesmo correndo o risco de overfitting
svmfit=svm(y~.,data=dat[train,],kernel="radial",gamma=1,cost=1e5)
plot(svmfit, dat[train,])

#Utilizando o CV para validar também o valor de gamma
set.seed(1)
tune.out=tune(svm, y~.,data=dat[train,],kernel="radial",ranges=list(cost=c(0.1, 1, 10, 100, 1000),gamma=c(0.5,1,2,3,4)))
summary(tune.out)

#Previsão com os dados de teste
table(true=dat[-train,"y"], pred=predict(tune.out$best.model,newdata=dat[-train,]))

#ROC Curves
library(ROCR)
#Criar um função para gerar o grafico ROC comparando o valor predito com o valor real
rocplot=function(pred, truth, ...){
  predob=prediction(pred, truth)
  perf=performance(predob, "tpr", "fpr")
  plot(perf,...)
}

svmfit.opt=svm(y~.,data=dat[train,],kernel="radial",gamma=2,cost=1,decision.values=T)
# a relação entre o valor ajustado e a previsão de classe para uma dada observação é simples:
# se o valor ajustado exceder zero, a observação é atribuída a uma classe e se for menor que zero do que é atribuída à outra.
# Para obter os valores ajustados para um determinado ajuste do modelo SVM, usamos decision.values=TRUE
fitted=attributes(predict(svmfit.opt,dat[train,],decision.values=TRUE))$decision.values
par(mfrow=c(1,2))
rocplot(fitted,dat[train,"y"],main="Training Data")

# Agora configurando um modelo mais flexivel, com um gamma maior
svmfit.flex=svm(y~.,data=dat[train,],kernel="radial",gamma=50,cost=1,decision.values=T)
fitted=attributes(predict(svmfit.flex,dat[train,],decision.values=TRUE))$decision.values
rocplot(fitted,dat[train,"y"],main="Training Data",add=T,col="red")

# Criando o gráfico com os dados de teste
fitted=attributes(predict(svmfit.opt,dat[-train,],decision.values=TRUE))$decision.values
rocplot(fitted,dat[train,"y"],main="Test Data")
fitted=attributes(predict(svmfit.flex,dat[-train,],decision.values=TRUE))$decision.values
rocplot(fitted,dat[train,"y"],main="Test Data",add=T,col="red")

#SVM com multiplas classes
set.seed(1)
# Utilizando a abordagem 1 vs 1.
x=rbind(x, matrix(rnorm(50*2), ncol=2))
y=c(y, rep(0,50))
x[y==0,2]=x[y==0,2]+2
dat=data.frame(x=x, y=as.factor(y))
par(mfrow=c(1,1))
#Apresentando os 3 diferentes grupos
plot(x, col=(y+1))

svmfit=svm(y~.,data=dat,kernel="radial",gamma=1,cost=10)
plot(svmfit, dat)

#Utilizando o dataset khan
# O conjunto de dados de Khan, que consiste em várias amostras de tecido correspondentes a quatro tipos distintos de pequenos tumores de células azuis redondas.
# Para cada amostra de tecido, medições de expressão gênica estão disponíveis.
library(ISLR)
names(Khan)
dim(Khan$xtrain)
dim(Khan$xtest)
length(Khan$ytrain)
length(Khan$ytest)

#Treinando o modelo para prever qual é o tipo de cancer
dat=data.frame(x=Khan$xtrain, y=as.factor(Khan$ytrain))

#Será utilizando um modelo linear pois o dataset possui muitos atributos e um kernel mais flexível seria desnecessário.
out=svm(y~.,data=dat,kernel="linear",cost=10)
summary(out)

#Com os dados de treino vemos que foi possivel a separação dos dados
table(out$fitted, dat$y)

dat.te=data.frame(x=Khan$xtest, y=as.factor(Khan$ytest))
pred.te=predict(out, newdata=dat.te)

#A perfomance do modelo continua alta mas vemos dois erros na previsão
table(pred.te, dat.te$y)