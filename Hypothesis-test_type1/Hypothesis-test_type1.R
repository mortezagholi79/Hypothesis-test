# Required libraries
library(ICSNP)
library(foreign)
#Read Data
jawbone<-read.table("C://Users/12345/Desktop/data.E12.txt",header = T)

# Multivariate test
HotellingsT2(jawbone,mu=c(48,49,50,51),test = "chi")

# Univariate test
t.test(y1,mu=48)
t.test(y2,mu=49)
t.test(y3,mu=50)
t.test(y4,mu=51)