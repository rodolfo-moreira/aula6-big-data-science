# Aula 5
m <- mtcars

plot(m$mpg~m$wt)
set.seed(33)
va<-sample(32)

# Separa a base do treino pela base de teste
treino<-m[va[1:24],]
teste<-m[va[25:32],]

# Transforma��o Lienar
# Hip�tese: wt tem comportamento exponencial
# Para linealizar wt utilizaremos log, inverso
cor(m$mpg, m$wt)
cor(m$mpg, log(m$wt))

plot(m$mpg~m$wt,col=m$cyl)

#Modelo de regress�o Linear
#mod<-lm(mpg~wt,data=treino)

#Logaritmos
#mod<-lm(mpg~log(wt),data=treino)
#Polinonimos
#mod<-lm(mpg~poly(wt,2),data=treino)
#mod<-lm(mpg~poly(wt,3),data=treino)
#Experimentos para ajuste de curva
#aos pontos de treino
#mod<-lm(mpg~poly(wt,4),data=treino)
#mod<-lm(mpg~poly(wt,16),data=treino)
# An�li. par. Regress�o


#Vari. Cyl como categ�rica
  mod<-lm(mpg~wt+as.factor(cyl), data=treino)

summary(mod)

#Previs�o em teste
p<-predict(mod, newdata=teste)

#compara��o previsto vs real vs Error
cbind(p, teste$mpg, p-teste$mpg)

# SSE
sse <- sum((p-teste$mpg)^2)


## CLASSIFICADORES

d <- read.csv("train.csv")

install.packages("party")
library(party)

# �rvore com uma variavel
#mod<-ctree(Survived~Sex, data=d)
#plot(mod, type="simple")

# �rvore com duas variaveis
#mod<-ctree(Survived~Sex+Pclass, data=d)
#plot(mod, type="simple")

# �rvore com tr�s variaveis
#mod<-ctree(Survived~Sex+Pclass+Age, data=d)
#plot(mod, type="simple")

mod<-ctree(Survived~Sex+Pclass+Embarked, data=d)
plot(mod, type="simple")

# ML Classificador AD

# Separa��o da base de treino e teste
# Base atemporais se faz an�lise aleat�rias
set.seed(33)
va<-sample(nrow(d))
treino <- d[va[1:691],]
teste <- d[va[692:891],]


#Modelagem
mod<-ctree(Survived~Sex+Pclass+Age+Embarked+Parch+SibSp, data=d)

#Previs�o em teste
p <- predict(mod, newdata=teste)

#Analise das predi��es
prev<-ifelse(p<.5,0,1)
cbind(prev, teste$Survived)

#matrix de confus�o
cm<-table(teste$Survived,prev )
prec<-cm[2,2]/(cm[2,2]+cm[2,1])
prec<-cm[2,2]/(cm[2,2]+cm[1,2])

table(teste$Survived,prev )



































