matrix(data = 0, nrow = cond$nsize[k], ncol = 20) %>% as.data.frame -> paddle
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
#hist(paddle$pers)

a = runif(cond$nitem[k], min = cond$discmi[k] , max = cond$discma[k] )
b = matrix(data = rnorm(cond$nitem[k],mean = cond$diffmn[k], sd = cond$diffsd[k]), nrow = cond$nitem[k])
d = cbind(a,b)
si <- t(apply(d, 1,
              function(x) traditional2mirt(x = c(a=x,g=0,u=1), '2PL', ncat=2)))
N = paddle$pers
response = simdata(a = si[,1], d = si[,2], N = length(N),Theta = as.matrix(paddle$pers), itemtype = '2PL')







paddle$CTT = apply(response,1,sum)
paddle$CTT %>% quantile(c(0.05,0.10,0.15)) -> CTTcrit
paddle$tmark1[which(paddle$CTT<=CTTcrit[1])] <- 1
paddle$tmark2[which(paddle$CTT<=CTTcrit[2])] <- 1
paddle$tmark3[which(paddle$CTT<=CTTcrit[3])] <- 1

#hist(paddle$CTT)


if (cond$nitem[k] == 10) {
  model.cfa = 'F1 =~ Item_1 + Item_2 + Item_3 + Item_4 + Item_5 + 
                   Item_6 + Item_7 + Item_8 + Item_9 + Item_10'
}  else if(cond$nitem[k] == 20) {
  model.cfa = 'F1 =~ Item_1 + Item_2 + Item_3+ Item_4+ Item_5 + 
                   Item_6 + Item_7 + Item_8 + Item_9 + Item_10 +
                   Item_11 + Item_12 + Item_13 + Item_14 + Item_15 +
                   Item_16 + Item_17 + Item_18 + Item_19 + Item_20'
}  else {
  model.cfa = 'F1 =~ Item_1 + Item_2 + Item_3+ Item_4+ Item_5 + 
                   Item_6 + Item_7 + Item_8 + Item_9 + Item_10 +
                   Item_11 + Item_12 + Item_13 + Item_14 + Item_15 +
                   Item_16 + Item_17 + Item_18 + Item_19 + Item_20 +
                   Item_21 + Item_22 + Item_23 + Item_24 + Item_25 +
                   Item_26 + Item_27 + Item_28 + Item_29 + Item_30'
}
results.cfa<-try(cfa(model=model.cfa,data = response),silent = T)
#summary(results.cfa)
tryCatch(expr = lavPredict(results.cfa, method = "regression", se = T),
         error = function(e) rep(0,times = cond$nsize[k]),
         warning = function(e) cat(k ,"조건", i,"번째 반복에서 CFA warning")) %>% as.vector() -> paddle$CFA
#hist(paddle$CFA)

paddle$CFA %>% quantile(c(0.05,0.10,0.15)) -> CFAcrit
paddle$fmark1[which(paddle$CFA<=CFAcrit[1])] <- 1
paddle$fmark2[which(paddle$CFA<=CFAcrit[2])] <- 1
paddle$fmark3[which(paddle$CFA<=CFAcrit[3])] <- 1



paddle %>% select(contains("mark")) %>% lapply(as.factor) -> tmp
assign(paste0("paddle","c",k,"t",i),tmp)
assign(paste0("paddle","c",k,"t",i),response)