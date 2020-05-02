library(class)
library(MASS)

# Necessito attach para ter acesso aos Direction
attach(Smarket)
contrasts(Direction)

# Dados de treinamento
train=(Year<2005)
Smarket.2005=Smarket[!train, ]
dim(Smarket.2005)
Direction.2005=Direction[!train]

#Para o KNN é necessario separar em matriz os dados
train.X = cbind(Lag1, Lag2)[train ,]
test.X = cbind(Lag1, Lag2)[!train, ]
train.Direction=Direction[train]

set.seed(1)
knn.pred=knn(train.X, test.X, train.Direction, k=1)
table(knn.pred, Direction.2005)

# Acurácia deu 50%, muito ruim pois foi utilizado k=1
(83+43)/252

# Com k = 3
knn.pred=knn(train.X, test.X, train.Direction, k=3)
table(knn.pred, Direction.2005)

# Acurácia deu 53%, muito ruim, para esse cenario o melhor modelo foi QDA
mean(knn.pred == Direction.2005)

# Outra abordagem

dim(Caravan)

attach(Caravan)
summary(Caravan)

# Processo de normalização dos dados
standardized.X = scale(Caravan[,-86])
var(Caravan[,1])
var(Caravan[,2])

var(standardized.X[,1])
var(standardized.X[,2])

test=1:1000
train.X = standardized.X[-test,]
test.X = standardized.X[test,]
train.Y = Purchase[-test]
test.Y = Purchase[test]

set.seed(1)
knn.pred = knn(train.X, test.X, train.Y, k=1)
mean(test.Y != knn.pred)
mean(test.Y != "No")

table(knn.pred,test.Y)

#Negative predictive value (NPV)
9/(68+9)

# k = 3
knn.pred = knn(train.X, test.X, train.Y, k=3)
table(knn.pred,test.Y)

#Negative predictive value (NPV)
5/(21+5)

# k = 5
knn.pred = knn(train.X, test.X, train.Y, k=5)
table(knn.pred,test.Y)

#Negative predictive value (NPV)
4/(11+4)

## Mesmo teste, agora utilizando a regressão logistica

glm.fits = glm(Purchase~., data=Caravan, family = binomial, subset = -test)

glm.probs = predict(glm.fits, Caravan[test,], type="response")
glm.pred=rep("No", 1000)
glm.pred[glm.probs>.5]="Yes"
table(glm.pred,test.Y)

# Zero (NPV)

glm.pred=rep("No", 1000)
glm.pred[glm.probs>.25]="Yes"
table(glm.pred,test.Y)

#Negative predictive value (NPV)
11/(22+11)
