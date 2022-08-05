install.packages("heplots")
library(heplots)
data(RootStock)
attach(RootStock)

Root.Model<-lm(cbind(girth4,ext4,girth15,weight15)~rootstock,data = RootStock)
Anova(Root.Model)
h<- matrix(c(2,-1,-1,-1,-1,2, 1, 0,0,0,0,-1), 2, 6, byrow=TRUE)
linearHypothesis(Root.Model, h)







