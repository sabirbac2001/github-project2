##TP3
getwd()
rm(list=ls())

##Q1
setwd("D:/s3/P2/ADD/tp/TP3")


##Q2
data =read.csv("percVec.csv",header = TRUE,sep=";",quote="\"",dec=".")
dim(data)
View(data)
str(data)


##Q3
#3 to 7 char
class(data[,3])
for (i in 3:7) {
  data[,i]<-as.factor(data[,i])  
}

class(data[,3])

#8 to 39 numc
class(data[,8])
for (i in 8:39) {
  data[,i]<-as.numeric(data[,i])
  
}
class(data[,8])


##Q4
min(data$MontBoutique)
max(data$MontBoutique)

min(data$MontMeuble)
max(data$MontMeuble)

##on remarque qu'il ya une erreur ici, car le porcentage varie de 0% ==> 100%
summary(data$MontBoutique)
summary(data$MontMeuble)
##on ne peut pas travaller sur ses deux il faux les filtrer

##Q5
#cette fois on va utiliser la fonction plot
plot(data$MontMeuble~data$MontBoutique,data=data,main='%_Meuble ~ %_Boutique',xlab='%_Boutique',ylab ='%_Meuble')
##on remarque que il ya des valeurs qui depasse les 100%


##Q6
data1<- data[data$MontBoutique<=100 & data$MontMeuble<=100,]
data2<- data[data$MontBoutique + data$MontMeuble<=100,]
str(data)
dim(data)
dim(data)
dim(data1)
dim(data2)
View(data1)

data=data2

##Q7

plot(data$MontMeuble~data$MontBoutique,data=data,main='%_Meuble ~ %_Boutique',xlab='%_Boutique',ylab ='%_Meuble')

##Q8

cor(data$MontBoutique,data$MontMeuble,method = "pearson")
#l'association entre les deux variables est forte


##Q9
reg<-lm(data$MontMeuble~data$MontBoutique)
reg
summary(reg)
##des proba sont proche de 0 donc on peut faire des alternative , 
##on fix une seuil d'erreur pour ne le pas depassee


#Q10
reg
summary(reg)

res=resid(reg)
mean(res)
#ecart type
sd(res)
sqrt(var(res))

#equation reduite
plot(df$MontBoutique,df$MontMeuble)
reg 
abline (reg,col="red", lw=2)


##Q11
##verifier lautocorelation et l homodisa
#Moyenne
res<-resid(reg)
mean(res)
#ecart type
sd(res)
sqrt(var(res))
#equation etabli
plot(data$MontBoutique,data$MontMeuble)
reg
abline(reg,col="red",lw=2)

##Q11
#affichage de 4 partie
layout(matrix(1:4,2,2))
plot(reg)

#verifier la normalite

plot(reg,1)
#verifier la normalite  des residus via qq plot
plot(reg,2)
plot(reg,3)
plot(reg,4)
#il ne suit pas la loi normal , pas de normalite des residus

install.packages("car")
install.packages("lmtest")
library(car)
library(lmtest)
#q11#a
plot(reg,2)
#graphique
plot(reg,1)
plot(reg,3)

#homosecite
ncvTest
ncvTest(reg)

durbinWatsonTest(reg)
#le resultat  



#14
reg2<-lm(data$NbPassages~data$NbTotal+data$NbRetours)
reg2

#verifier la normalite

plot(reg2,1)
#verifier la normalite  des residus via qq plot
plot(reg2,2)
plot(reg2,3)
plot(reg2,4)
#il ne suit pas la loi normal , pas de normalite des residus

#q11#a
plot(reg2,2)
#graphique
plot(reg2,1)
plot(reg2,3)

#homosecite
ncvTest
ncvTest(reg2)

durbinWatsonTest(reg3)
#le resultat on a l'autocorelation 






#15
reg3<-lm(data$NbPassages~data$NbTotal+data$NbRetours+data$HorsWknd)
reg3



#verifier la normalite

plot(reg3,1)
#verifier la normalite  des residus via qq plot
plot(reg3,2)
plot(reg3,3)
plot(reg3,4)
#il ne suit pas la loi normal , pas de normalite des residus

#q11#a
plot(reg3,2)
#graphique
plot(reg3,1)
plot(reg3,3)

#homosecite
ncvTest
ncvTest(reg3)

durbinWatsonTest(reg3)






