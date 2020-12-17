# O primeiro exemplo será o de PCA com dados de crimes no USA
states=row.names(USArrests)
#Nome dos 50 estados dos EUA
states

#As colunas são os tipo de crimes cometidos
names(USArrests)

#Verificando as diferentes escalas
apply(USArrests, 2, mean)

#Agora a variancia
apply(USArrests, 2, var)

# Quando os dados possui uma diferença significante na escala é importante normalizar, deixando o valor da media proxima de zero e o desvio padrão de 1
pr.out = prcomp(USArrests, scale=TRUE)

# Nome dos atributos
names(pr.out)

# Apresenta o valor da media para normalizar
pr.out$center

# Apresenta o valor do desvio padrão para normalizar
pr.out$scale

# Contem todos os valores de PCA calculados por Colunas
pr.out$rotation

# Contem todos os valores de PCA calculados por Linhas
pr.out$x

# Grafico com as proporções
biplot(pr.out, scale=0)

# Invertendo o gráfico
pr.out$rotation=-pr.out$rotation
pr.out$x=-pr.out$x
biplot(pr.out, scale=0)

# Desvio padrão dos componentes principais
pr.out$sdev

# Variancia dos componentes principais
pr.var=pr.out$sdev^2
pr.var

# Calculando a representatividade de cada componentes principais
pve=pr.var/sum(pr.var)
pve
#PC1 => 62% e PC2 24% => esta duas colunas explicam 86% dos dados

# Mostrando o resultado no gráfico
plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,1), type='b')

# Gráfico os dados acumulados
plot(cumsum(pve), xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,1), type='b')

#A função cumsum é utilizada para acumulo de valores, segue abaixo um exemplo
a=c(1,2,8,-3)
cumsum(a)


## Clustering
#Kmeans
set.seed(2)

# Gerando dados fake
x=matrix(rnorm(50*2), ncol=2)
x[1:25, 1] = x[1:25,1] + 3
x[1:25, 2] = x[1:25,2] - 4

# Processando a clusterização
km.out=kmeans(x, 2, nstart=20)

km.out$cluster

# Gráfico com o resultado
plot(x, col=(km.out$cluster+1), main="K-Means Clustering Results with K=2", ylab="", pch=20, cex=2)

# Sabemos que são 2 dois cluster pois são dados fakes mas no mundo real deveria haver algumas tentativas com valores diferentes de K.
set.seed(4)
km.out=kmeans(x, 3, nstart=20)
km.out

# Comparando a performance com diferentes nstart
set.seed(3)
km.out=kmeans(x,3,nstart=1)
km.out$tot.withinss
km.out=kmeans(x,3,nstart=20)
km.out$tot.withinss

# O objetivo é minimizar esse valor.
# Importante, utilize sempre um valor de 20 a 50 para não encontrar um otimo local indesejavel

#Hierarchical Clustering

#função dist é usada para calcular a distancia euclideana entre as observação.
dist(x)
# Metodo: complete
# É a dissimilaridade máxima intercluster. Calcule todas as dissimilaridades de pares entre as observações no cluster A e as observações no cluster B e registre a maior dessas dissimilaridades.
hc.complete=hclust(dist(x), method = "complete")

# Metodo: average
#É a dissimilaridade intercluster média. Calcule todas as dissimilaridades de pares entre as observações no cluster A e as observações no cluster B e registre a média dessas dissimilaridades.
hc.average=hclust(dist(x), method = "average")

# Metodo: single
#É a dissimilaridade intercluster mínima. Calcule todas as dissimilaridades de pares entre as observações no cluster A e as observações no cluster B e registre a menor dessas dissimilaridades. A ligação única pode resultar em clusters estendidos e posteriores, nos quais as observações únicas são fundidas uma de cada vez.
hc.single=hclust(dist(x), method = "single")

par(mfrow=c(1,3))
plot(hc.complete, main="Complete Linkage", xlab="", sub="", cex=.9)
plot(hc.average, main="Average Linkage", xlab="", sub="", cex=.9)
plot(hc.single, main="Single Linkage", xlab="", sub="", cex=.9)

# Obtendo os labels de cada grupo
cutree(hc.complete, 2)
cutree(hc.average, 2)
cutree(hc.single, 2)

# O unico que não separou corretamente para esse caso foi o single, mas se aumentar o grupos ele faz uma melhor separação
cutree(hc.single, 4)

# Aplicando normalização nos dados
xsc=scale(x)
plot(hclust(dist(xsc), method="complete"), main="Hierarchical Clustering with Scaled Features")

# A distância baseada em correlação pode ser calculada usando a função as.dist (), que converte uma matriz quadrada simétrica arbitrária em uma forma que a função hclust () reconhece como uma matriz de distância.
# No entanto, isso só faz sentido para dados com pelo menos três features, uma vez que a correlação absoluta entre quaisquer duas observações com medições em dois features é sempre 1.
x=matrix(rnorm(30*3), ncol=3)
dd=as.dist(1-cor(t(x)))
plot(hclust(dd, method="complete"), main="Complete Linkage with Correlation - Based Distance", xlab="", sub="")

# Dados NCI60
library(ISLR)
nci.labs=NCI60$labs
nci.data=NCI60$data

# nci.labs é os tipos de cancer se só será utilizado para comparar a performance das tecnicas de apredizado não supervisionado
dim(nci.data)

# Visualizando os dados dos tipos de cancer
nci.labs[1:4]

table(nci.labs)

# PCA
pr.out=prcomp(nci.data, scale=TRUE)

# Criando 64 cores para ser utilizado na diferenciação de cada grupo
Cols=function(vec){
  cols=rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}

# Os gráficos resultantes são mostra as linhas celulares correspondentes a um único tipo de câncer tendem a ter valores semelhantes nos primeiros vetores de pontuação de componente principal.
# Isso indica que as linhas de células do mesmo tipo de câncer tendem a ter níveis de expressão gênica semelhantes.
par(mfrow=c(1,2))
plot(pr.out$x[,1:2], col=Cols(nci.labs), pch=19, xlab="Z1", yylab = "Z2")
plot(pr.out$x[,c(1,3)], col=Cols(nci.labs), pch=19, xlab="Z1", yylab = "Z3")

# Analisando os indicadores do PCA
summary(pr.out)

# Analisando graficamente
plot(pr.out)

# Calculando o PVE (variância dos dados)
pve=100*pr.out$sdev^2/sum(pr.out$sdev^2)
par(mfrow=c(1,2))
plot(pve, type="o", ylab="PVE", xlab="Principal Component", col="blue")
plot(cumsum(pve), type="o", ylab="Cumulative PVE", xlab="Principal Component", col="brown3")

# Os gráficos mostram que, juntos, os primeiros 7 componentes principais explicam cerca de 40% da variância dos dados.
# Aparentemente é uma pessima opção para analisar os dados, ficando muito abaixo dos 80% e com muitos componentes para explicar apenas 40%.

# Clustering - NCI60

# Normalização dos dados
sd.data=scale(nci.data)

# Processar os dados com os diferentes tipos
par(mfrow=c(1,3))
data.dist=dist(sd.data)
plot(hclust(data.dist), labels=nci.labs, main="Complete Linkage", xlab="", sub="", ylab="")
plot(hclust(data.dist, method="average"), labels=nci.labs, main="Average Linkage", xlab="", sub="", ylab="")
plot(hclust(data.dist, method="single"), labels=nci.labs, main="Average Linkage", xlab="", sub="", ylab="")

# Fazendo um corte por agrupamento
hc.out=hclust(dist(sd.data))
hc.clusters=cutree(hc.out, 4)
table(hc.clusters,nci.labs)

# Apresentando gráficamente o corte
par(mfrow=c(1,1))
plot(hc.out, labels=nci.labs)
abline(h=139, col="red")

# Resumo do objeto
hc.out

# Comparando o resultado do Hierarchical clustering and K-means
set.seed(2)
km.out=kmeans(sd.data, 4, nstart=20)
km.clusters=km.out$cluster
table(km.clusters, hc.clusters)

# Em vez de realizar o clustering hierárquico em toda a matriz de dados, podemos simplesmente realizar o clustering hierárquico nos primeiros vetores de pontuação do PCA
hc.out=hclust(dist(pr.out$x[,1:5]))
plot(hc.out, labels=nci.labs,main="Hier. Clustt on First Five Score Vectores")
table(cutree(hc.out,4), nci.labs)

# Não surpreendentemente, esses resultados são diferentes daqueles que obtivemos quando executamos o clustering hierárquico no conjunto de dados completo.
# Às vezes, realizar o clustering nos primeiros vetores de pontuação do PCA pode fornecer melhores resultados do que o clustering nos dados completos.
# Nessa situação, podemos ver a etapa do PCA como a remoção de ruído dos dados.
# Também poderíamos realizar o agrupamento K-means nos primeiros vetores de pontuação do PCA, em vez do conjunto de dados completo.