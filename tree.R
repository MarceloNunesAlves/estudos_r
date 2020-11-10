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

# Fitting Regression Trees
