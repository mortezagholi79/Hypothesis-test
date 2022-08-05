library("car")
#step.1
#all
data.all<-data.frame(read.table("C:/Users/12345/Desktop/Stock.market.txt",header = T))


#group1:khesapa
#group2:khodro
#group3:khegostar
#group4:khebahman
#group5:khetogha
#group6:khmohareke

y1<-data.all$Volume
y2<-data.all$Number.of.transactions
y3<-data.all$Final.price.percentage
Group<-as.factor(data.all$group)


#step.2
(mean<-aggregate(data.all [,2:4] , list(Group) , mean))
mean$Volume/mean$Number.of.transactions


#step.3
#test
result<-manova(cbind(y1,y2,y3)~Group,data=data.all)

summary(result,test = "Pillai")
summary(result,test = "Hotelling-Lawley")
summary(result,test = "Wilks")
summary(result,test = "Roy")

#step.4
summary(aov(y1~Group))
summary(aov(y2~Group))
summary(aov(y3~Group))


#step.5 
summary(result)$SS

E<-summary(result)$SS$Group
H<-summary(result)$SS$Residuals
A<-t(E)%*%H
e <- eigen(A)
e$values


#step.6
data.all$y3<-(y3)*10
Modell<-lm(cbind(y1,y2,y3)~Group,data = data.all)
h01<- matrix(c(2 ,  1 , -1 , 2 , 1 , -2),1 ,6)
linearHypothesis(Modell, h01)

h02<- matrix(c(-2,1,-1,0,1,-2),1 ,6)
linearHypothesis(Modell, h02)




