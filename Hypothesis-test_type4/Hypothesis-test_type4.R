#16.2.b
#step1:Data definition
  fish.data<- read.table("C:/Users/12345/Desktop/data_Hypothesis-test_type4.txt")
  
#step3:Get the matrix E and H
  method<-as.factor(fish.data$V1)   
  result <- manova(cbind(fish.data$V2,fish.data$V3,fish.data$V4,fish.data$V5) ~method, data = fish.data)
  fish.summary <- summary(result)
  mat<-fish.summary$SS
  
  E<-mat$Residuals
  H<-mat$method
  
#step4:Get special values
  A<-t(E)%*%H
  e <- eigen(A)
  e$values
  
#step6:test
 
  summary(result,test = "Pillai")
  summary(result,test = "Hotelling-Lawley")
  summary(result,test = "Wilks")
  summary(result,test = "Roy")
  
  
  
#16.2.c

  
summary(aov(fish.data$V2 ~method))
summary(aov(fish.data$V3 ~method))
summary(aov(fish.data$V4 ~method))
summary(aov(fish.data$V5 ~method))
