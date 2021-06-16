remove(list=ls())
setwd("D:/Videos/_Analise multivariada/2.2 Distancia de mahaanobis")
D=read.table("Dados.txt",head=TRUE)
D$Trat=as.factor(D$Trat)
D$Bloco=as.factor(D$Bloco)

m=aov(cbind(X1,X2,X3,X4)~Trat+Bloco,data=D)
anova(m)

SQP=crossprod(m$residuals)
CRE=SQP/m$df.residual

Med=aggregate(D[,3:6],by=list(D$Trat),mean)[,-1]
Med2=as.matrix(Med)

V=Med2[1,]-Med2[2,]
t(V)%*%solve(CRE)%*%V

#install.packages("biotool")
library(biotools)
?biotools
Dist=D2.dist(Med2,CRE)
dendo=hclust(Dist)
plot(dendo)
