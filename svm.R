library(e1071)

# Criando dados fictício para a analise
set.seed(1)
x=matrix(rnorm(20*2), ncol=2)
y=c(rep(-1,10), rep(1,10))
x[y==1,]=x[y==1,]+1
plot(x, col=(3-y))

dat = data.frame(x=x, y=as.factor(y))
#kernel linear é usado para dados lineares. cost qto maior mas overfit e menor mais underfit
#scale=FALSE faz com que a função não escale variaveis com media zero ou desvio padrão 1
svmfit=svm(y~.,data=dat,kernel="linear",cost=10,scale=FALSE)

#Gera um gráfico com a separação das classes
plot(svmfit, dat)

#Este dados é as observações que fazem parte 7 vetores de suporte (Numero de observações dentro da margem???)
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
