setwd("c:\\git\\journeytoamastersdegree")
cellloop = 1:10
condloop = 1:nrow(cond)

k = 29

colnames(paddle)<-c("pers","mark1","mark2","mark3",
                    "CTT","tmark1","tmark2","tmark3",
                    "CFA","fmark1","fmark2","fmark3",
                    "PCM","pmark1","pmark2","pmark3",
                    "GPCM","gmark1","gmark2","gmark3")
paddle$pers = rnorm(cond$nsize[k], mean = 0, sd = 1)
paddle$pers %>% quantile(c(0.05,0.10,0.15)) -> personcrit
paddle$mark1[which(paddle$pers<=personcrit[1])] <- 1
paddle$mark2[which(paddle$pers<=personcrit[2])] <- 1
paddle$mark3[which(paddle$pers<=personcrit[3])] <- 1
a = runif(cond$nitem[k], min = cond$discmi[k] , max = cond$discma[k] )
b = matrix(data = rnorm(cond$nitem[k],mean = cond$diffmn[k], sd = cond$diffsd[k]), nrow = cond$nitem[k])
d = cbind(a,b)
si <- t(apply(d, 1,
              function(x) traditional2mirt(x = c(a=x,g=0,u=1), '2PL', ncat=2)))
N = paddle$pers
model.cfa = 'F1 =~ Item_1 + Item_2 + Item_3 + Item_4 + Item_5 + 
                   Item_6 + Item_7 + Item_8 + Item_9 + Item_10'
response = simdata(a = si[,1], d = si[,2], N = length(N),Theta = as.matrix(paddle$pers), itemtype = '2PL')
results.cfa<-cfa(model=model.cfa,data = response)
res = lavPredict(results.cfa, method = "regression", se = T)
print(res)
