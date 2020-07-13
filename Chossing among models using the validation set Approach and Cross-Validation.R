#Chossing among models using the validation set Approach and Cross-Validation
library(ISLR)
library(leaps) #Biblioteca da função regsubsets

#Separando modelo de teste e treino
set.seed(1)

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
