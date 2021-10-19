

#범주 수 2, 5, 혼합(2,3,5)

nsize = c(250, 750)
nitem = c(10,30)
diffmn = c(0, -1.5) # 이거 -로 고쳐야함..
diffsd = c(1)

discmi = c(1, 0.5)
discma = c(1, 1.5)

cond = expand.grid(nsize,nitem,diffmn,diffsd,discmi,discma)
colnames(cond) = c("nsize","nitem","diffmn","diffsd","discmi","discma")


write.table(cond,"conditions.csv",row.names=F,sep=",")
