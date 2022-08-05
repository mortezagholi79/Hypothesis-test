#Read Data
Patient.number<-c(1:16)
As.patients1<-c(81,461,20,450,246,166,63,64,155,151,166,37,223,138,72,245)
As.patients2<-c(74,423,16,450,87,115,50,50,113,38,156,27,218,138,39,231)
Matched.Contorols1<-c(72,134,84,98,48,142,113,90,30,260,116,87,69,100,315,188)
Matched.Contorols2<-c(33,18,20,58,13,49,38,24,18,34,20,27,32,27,39,65)
As.Ma<-matrix(c(Patient.number,As.patients1,As.patients2,Matched.Contorols1,Matched.Contorols2),byrow = F,ncol = 5,
   dimnames =list(c(1:16),c("Patient.number","Ascorbate.patients1","Ascorbate.patients2","Matched.Contorols1","Matched.Contorols2")))

#A
Y<-As.Ma[,2:3]
X<-As.Ma[,4:5]
result<-Mpaired(T1=X,T2=Y)
summary(result)
#B
Y1<-As.Ma[,2]
X1<-As.Ma[,4]
t.test(X1,Y1,paired = T)

Y2<-As.Ma[,3]
X2<-As.Ma[,5]
t.test(X2,Y2,paired = T)