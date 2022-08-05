#DATA:SBD= Snap Bean Data
SBD<-read.table("C:/Users/12345/Desktop/data_Hypothesis-test_type6.txt",header = T)

response<-cbind(SBD$y1,SBD$y2,SBD$y3,SBD$y4)
result<-manova(response~SBD$s+SBD$v+SBD$s:SBD$v)
summary(result,test = "Wilks")


A<-as.factor(SBD$s)
B<-as.factor(SBD$v)
result2<-manova(response~A+B+A:B)
summary(result2,test="Wilks")

result3<-manova(response~A+B)
summary(result3,test="Wilks")

result2$coefficients
