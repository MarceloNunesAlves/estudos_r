#Chossing among models using the validation set Approach and Cross-Validation
library(ISLR)
#Biblioteca da função regsubsets
library(leaps)

#Separando modelo de teste e treino
set.seed(1)

# Elimina os registro com NA
Hitters=na.omit(Hitters)

train = sample(c(TRUE, FALSE), nrow(Hitters), rep=TRUE)
test = (!train)

# Pesquisa o melhor modelo de varios tamanhos
regfit.best = regsubsets(Salary~., data=Hitters[train,], nvmax=19)
regfit.best

# Gera um teste das 19 possibilidades de campos
test.mat = model.matrix(Salary~.,data=Hitters[test,])
test.mat

# Teste as 19 (erro) pesquisar no github do livro
val.errors=rep(NA,19)
for(i in 1:19){
  coefi = coef(regfit.best, id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((Hitters$Salary[test]-pred)^2)
}

length(Hitters$Salary[test])
length(Hitters[test,])

#################

val.errors
which.min(val.errors)
coef(regfit.best,10)
predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}
regfit.best=regsubsets(Salary~.,data=Hitters,nvmax=19)
coef(regfit.best,10)

#Ajuda a identificar as melhores variaveis com o cross-validation
#Inicializando a analise
k=10
set.seed(1)
folds=sample(1:k,nrow(Hitters),replace=TRUE)
cv.errors=matrix(NA,k,19, dimnames=list(NULL, paste(1:19)))

#Implementando a validação
for(j in 1:k){
  #Dados de treino para cada K
  best.fit=regsubsets(Salary~.,data=Hitters[folds!=j,],nvmax=19)
  for(i in 1:19){
    #Dados de teste para cada K
    pred=predict(best.fit,Hitters[folds==j,],id=i)
    cv.errors[j,i]=mean( (Hitters$Salary[folds==j]-pred)^2)
  }
}

mean.cv.errors=apply(cv.errors,2,mean)
mean.cv.errors
par(mfrow=c(1,1))
plot(mean.cv.errors,type='b')
reg.best=regsubsets(Salary~.,data=Hitters, nvmax=19)
coef(reg.best,11)

#Lab 2 Rigde Regression and the Lasso

# Este processo efetua a dummização do dataset pois o metodo glmnet só trabalha com numeros
x=model.matrix(Salary~.,Hitters)[,-1]
y=Hitters$Salary

#Carregar biblioteca
library(glmnet)
#Sequencia de lambda que será utilizada
grid=10^seq(10,-2,length=100)
#Por padrão os valores serão normalizados, caso não queria (standardize=FALSE)
ridge.mod=glmnet(x,y,apha=0,lambda=grid)

dim(coef(ridge.mod))

# Quanto menor for o coeficiente maior será o lambda
ridge.mod$lambda[50]

coef(ridge.mod)[,50]

sqrt(sum(coef(ridge.mod)[-1,50]^2))

# Quanto menor for o coeficiente maior será o lambda
ridge.mod$lambda[60]

coef(ridge.mod)[,60]

sqrt(sum(coef(ridge.mod)[-1,60]^2))

predict(ridge.mod,s=50,type = "coefficients")[1:20,]

# Separação de dados de treino e teste 
set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

# Efetuado o treinamento com lambda=4 e utilizando os dados de teste para a validação
rigde.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid,thresh=1e-12)
rigde.pred=predict(ridge.mod,s=4,newx=x[test,])
mean((rigde.pred-y.test)^2)


rigde.pred=predict(ridge.mod,s=1e10,newx=x[test,])
mean((rigde.pred-y.test)^2)

rigde.pred=predict(rigde.mod,s=0,newx=x[test,],exact=T,x=x[train,],y=y[train])
mean((ridge.pred-y.test)^2)
lm(y~x,subset=train)
predict(rigde.mod,s=0,exact=T,type="coefficients",x=x[train,],y=y[train])[1:20,]

# Validação kfold para selecionar o melhor lambda
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam

ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
mean((rigde.pred-y.test)^2)

#Após selecionar o melhor lambda é só aplicar no dataset completo
out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)[1:20,]

# The lasso
lasso.mod = glmnet(x[train,],y[train],alpha=1,lambda=grid)
# Alguns coeficientes ficaram proximo de zero
plot(lasso.mod)

# Lasso com cross-validation
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)

# O melhor valor de alpha
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod, s=bestlam, newx=x[test,])
mean((lasso.pred-y.test)^2)

out = glmnet(x,y,alpha=1,lambda = grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:20,]
# Dos 19 campos 12 estão com o zerado
lasso.coef
# Os campos que serão utilizado na predição
lasso.coef[lasso.coef!=0]

# Principal components regression
library(pls)
set.seed(2)
# scale=TRUE => Para normalizar os dados preditor / validation="CV" kfolds
pcr.fit=pcr(Salary~., data=Hitters, scale=TRUE, validation="CV")

# as composições de campos criados
summary(pcr.fit)

# Gráfico com o cross-validation
validationplot(pcr.fit,val.type="MSEP")

# Selecionando um quantidade especifica de components.
# Por exemplo M=1 => 38,31% de toda a variacia - M=6 => 88,63 - M=19 => 100%
set.seed(1)
pcr.fit=pcr(Salary~., data=Hitters, scale=TRUE, validation="CV")
validationplot(pcr.fit,val.type="MSEP")

# Test com apenas 7 components
pcr.pred=predict(pcr.fit,x[test,],ncomp=7)
mean((pcr.pred-y.test)^2)

# Treino com apenas 7 componentes
pcr.fit=pcr(y~x,scale=TRUE,ncomp=7)
summary(pcr.fit)

# Partial least squares
set.seed(1)
pls.fit=plsr(Salary~., data=Hitters,subset=train,scale=TRUE,validation="CV")
summary(pls.fit)
validationplot(pls.fit,val.type="MSEP")

# Neste dataset foi retornado a quantidade de 2 com a maior performance
pls.pred=predict(pls.fit,x[test,],ncomp=2)
mean((pls.pred-y.test)^2)

# Treinando apenas com 2 componentes
pls.fit=plsr(Salary~., data=Hitters,subset=train,scale=TRUE,ncomp=2)
summary(pls.fit)
