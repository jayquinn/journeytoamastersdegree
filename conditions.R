setwd("C:/git/journeytoamastersdegree")
source("names.r")
get.IV.names()

opt1 = c(2,3,4,5) #n.cat
opt2 = c(200, 500, 1000, 1500, 2000) #s.size
opt3 = c("시이발") #i.diff
opt4 = c("시이팔") #i.disc 
opt5 = c(5, 10, 15, 20) #n.item
opt6 = c("시이벌") #reli
opts = expand.grid(opt1,opt2,opt3,opt4,opt5,opt6)
write.table(opts,"conditions.csv",row.names=F,sep=",")
