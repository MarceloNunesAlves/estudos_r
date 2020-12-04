library(tree)

#Será utilizado o dataset Carseats

library(ISLR)
attach(Carseats)

#Cria uma nova variaves, se o valor de Sales for superior a 8 será YES caso contrario NO
High=factor(ifelse(Sales<=8,"No","Yes"))

#Merger dos DOIS dataset em apenas UM
Carseats = data.frame(Carseats, High)

#Para treinamento para prever a variavel High, com todos os campos exceto o Sales
tree.carseats=tree(High~.-Sales,Carseats)

#Apresenta a lista de variaveis usada na analise, numero de nos
summary(tree.carseats)

#Apresentar graficamente a arvore
plot(tree.carseats)
#Adiciona os label e rotulos de cada quebra
text(tree.carseats,pretty=0)

# Detalhe da arvore em texto
tree.carseats

# Efetuando uma analise atraves dos dados de teste
set.seed(2)
train=sample(1:nrow(Carseats), 200)
Carseats.test=Carseats[-train,]
High.test=High[-train]
tree.carseats=tree(High~.-Sales,Carseats,subset=train)
tree.pred=predict(tree.carseats,Carseats.test,type="class")
table(tree.pred,High.test)

# % Curaciadade
(104+50)/200

# Executando a analise com cross-validation
set.seed(3)
cv.carseats=cv.tree(tree.carseats,FUN=prune.misclass)
names(cv.carseats)

cv.carseats

#Analisando os erros do cross-validation
par(mfrow=c(1,2))
plot(cv.carseats$size,cv.carseats$dev,type="b")
plot(cv.carseats$k,cv.carseats$dev,type="b")

#Aplicando a função prune para os 9 melhores nós
prune.carseats=prune.misclass(tree.carseats,best = 9)
plot(prune.carseats)
text(prune.carseats, pretty=0)

#Testando novamento agora com o CV de 9 nós
tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
(97+58)/200

#No livro apresenta que se caso aumentar o numero de nós pode prejudicar o modelo, mas não foi o que ocorreu
prune.carseats=prune.misclass(tree.carseats,best=15)
plot(prune.carseats)
text(prune.carseats,pretty=0)
tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
(102+53)/200

# Fitting Regression Trees (Regressão linear com dataset Boston)
library(MASS)
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston = tree(medv~.,Boston,subset=train)
summary(tree.boston)

# Visualizar a arvore criada
plot(tree.boston)
text(tree.boston, pretty=0)

# Agora utilizando cross-validation
cv.boston=cv.tree(tree.boston)
plot(cv.boston$size,cv.boston$dev,type='b')

# Testar com apenas os 5 melhores nós
prune.boston=prune.tree(tree.boston,best=5)
plot(prune.boston)
text(prune.boston, pretty=0)

# Testando com os dados de teste - 35.28 (Melhor)
yhat = predict(tree.boston,newdata = Boston[-train,])
boston.test = Boston[-train,"medv"]
plot(yhat, boston.test)
abline(0,1)
mean((yhat-boston.test)^2)

# Com prune a taxa é maior 35.90
yhat = predict(prune.boston,newdata = Boston[-train,])
boston.test = Boston[-train,"medv"]
plot(yhat, boston.test)
abline(0,1)
mean((yhat-boston.test)^2)

# Bagging and Ramdom Forests
library(randomForest)
set.seed(1)
# O argumento mtry indica que deverá ser utilizado todos os 13 campos preditores
bag.boston=randomForest(medv~.,data = Boston,subset = train, mtry=13, importance = TRUE)
bag.boston

# Testando os dados com o processo de Bagging (23.59)
yhat.bag = predict(bag.boston,newdata=Boston[-train,])
plot(yhat.bag, boston.test)
abline(0,1)
mean((yhat.bag-boston.test)^2)

# Limitando o numero de arvores ntree (22.53)
bag.boston=randomForest(medv~.,data = Boston,subset = train, mtry=13, ntree=25, importance = TRUE)
yhat.bag = predict(bag.boston,newdata=Boston[-train,])
mean((yhat.bag-boston.test)^2)

# Utilizando menos mtry (19.54)
rf.boston=randomForest(medv~.,data = Boston,subset = train, mtry=6, importance = TRUE)
yhat.bag = predict(rf.boston,newdata=Boston[-train,])
mean((yhat.bag-boston.test)^2)

# Analisar a importancia de cada variavel
importance(rf.boston)

# Vendo gráficamente
varImpPlot(rf.boston)

# Nesta analise podemos notar que as variaves mais importante na predição são as rm e lstat


# Boosting
# Para problemas de regressão linear deve se usar a distribution="gaussian", ou para classificação distribution="bernoulli"
library(gbm)
set.seed(1)
boost.boston = gbm(medv~.,data=Boston[train,],distribution="gaussian", n.trees = 5000,interaction.depth = 4)
#5000 arvores e com a profundidade maxima de 4

# Neste sumario é possivel visualizar que as variaveis lstats e rm são mais importantes
summary(boost.boston)

#Esta analise mostra que o preço medio da casa cresce com o rm e diminui com lstat
par(mfrow=c(1,2))
plot(boost.boston, i="rm")
plot(boost.boston, i="lstat")

yhat.boost = predict(boost.boston, newdata = Boston[-train,], n.trees=5000)
mean((yhat.boost-boston.test)^2)

#Talvez alterando o paramtro de lambda posso obter uma taxa de erro menor (default=0.0001) => 0.2
boost.boston = gbm(medv~.,data=Boston[train,],distribution="gaussian", n.trees = 5000,interaction.depth = 4, shrinkage = 0.2, verbose=F)
yhat.boost = predict(boost.boston, newdata = Boston[-train,], n.trees=5000)
mean((yhat.boost-boston.test)^2)
