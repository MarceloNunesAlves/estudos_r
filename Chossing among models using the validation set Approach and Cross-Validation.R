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
