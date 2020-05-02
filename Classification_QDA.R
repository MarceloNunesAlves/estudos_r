library(ISLR)
library(MASS)

# Necessito attach para ter acesso aos Direction
attach(Smarket)
contrasts(Direction)

# Dados de treinamento
train=(Year<2005)
Smarket.2005=Smarket[!train, ]
dim(Smarket.2005)
Direction.2005=Direction[!train]

qda.fit = qda(Direction ~ Lag1+Lag2, data=Smarket, subset = train)
qda.fit

qda.pred = predict(qda.fit, Smarket.2005)
names(qda.pred)

qda.class = qda.pred$class
table(qda.class, Direction.2005)

mean(qda.class==Direction.2005)