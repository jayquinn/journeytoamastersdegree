

#범주 수 2, 5, 혼합(2,3,5)

nsize = c(250, 500, 750)
nitem = c(10, 20, 30)
diffmn = c(0, 0.75, 1.5)
diffsd = c(0.5, 1)

discmi = c(1, 0.8, 0.5)
discma = c(1, 1.2, 1.5)

cond = expand.grid(nsize,nitem,diffmn,diffsd,discmi,discma)
colnames(cond) = c("nsize","nitem","diffmn","diffsd","discmi","discma")


write.table(cond,"conditions.csv",row.names=F,sep=",")
