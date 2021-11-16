matrix(data = 0, nrow = cond$nsize[k], ncol = 10) %>% as.data.frame -> paddle
colnames(paddle)<-c("pers","mark",
                    "CTT","tmark",
                    "CFA","fmark",
                    "PCM","pmark",
                    "GPCM","gmark")
paddle$pers = rnorm(cond$nsize[k], mean = 0, sd = 1)
paddle$pers %>% quantile(c(0.05,0.10,0.15)) -> personcrit
paddle$mark[which(paddle$pers<=personcrit[3])] <- 3
paddle$mark[which(paddle$pers<=personcrit[2])] <- 2
paddle$mark[which(paddle$pers<=personcrit[1])] <- 1
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
paddle$tmark[which(paddle$CTT<=CTTcrit[3])] <- 3
paddle$tmark[which(paddle$CTT<=CTTcrit[2])] <- 2
paddle$tmark[which(paddle$CTT<=CTTcrit[1])] <- 1

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
paddle$fmark[which(paddle$CFA<=CFAcrit[3])] <- 3
paddle$fmark[which(paddle$CFA<=CFAcrit[2])] <- 2
paddle$fmark[which(paddle$CFA<=CFAcrit[1])] <- 1



if (cond$nitem[k] == 10) {
  model.rasch <- 'F1 = 1-10' 
}  else if(cond$nitem[k] == 20) {
  model.rasch <- 'F1 = 1-20' 
}  else {
  model.rasch <- 'F1 = 1-30' 
}
results.rasch <- try(mirt(data=response, model=model.rasch, itemtype="Rasch", SE=TRUE, verbose=T),silent = T)
#summary(results.rasch)
coef.rasch <- try(coef(results.rasch, IRTpars=TRUE, simplify=TRUE),silent = T)
#print(coef.rasch)
tryCatch(expr = fscores(results.rasch,method = 'EAP'),
         error = function(e) rep(0,times = cond$nsize[k]),
         warning = function(e) cat(k ,"조건", i,"번째 반복에서 PCM warning")) %>% as.vector() -> paddle$PCM


#hist(paddle$PCM)# EAP(default) MAP ML WLE EAPsum

paddle$PCM %>% quantile(c(0.05,0.10,0.15)) -> PCMcrit
paddle$pmark[which(paddle$PCM<=PCMcrit[3])] <- 3
paddle$pmark[which(paddle$PCM<=PCMcrit[2])] <- 2
paddle$pmark[which(paddle$PCM<=PCMcrit[1])] <- 1


if (cond$nitem[k] == 10) {
  model.2pl <- 'F1 = 1-10' 
}  else if(cond$nitem[k] == 20) {
  model.2pl <- 'F1 = 1-20' 
}  else {
  model.2pl <- 'F1 = 1-30' 
}
results.2pl <- try(mirt(data=response, model=model.2pl, itemtype="2PL", SE=TRUE, verbose=T),silent = T)
#summary(results.2pl)
coef.2pl <- try(coef(results.2pl, IRTpars=TRUE, simplify=TRUE),silent = T)
#print(coef.2pl)
tryCatch(expr = fscores(results.2pl,method = 'EAP'),
         error = function(e) rep(0,times = cond$nsize[k]),
         warning = function(e) cat(k ,"조건", i,"번째 반복에서 GPCM warning")) %>% as.vector() -> paddle$GPCM
#hist(paddle$GPCM)# EAP(default) MAP ML WLE EAPsum
paddle$GPCM %>% quantile(c(0.05,0.10,0.15)) -> GPCMcrit
paddle$gmark[which(paddle$GPCM<=GPCMcrit[3])] <- 3
paddle$gmark[which(paddle$GPCM<=GPCMcrit[2])] <- 2
paddle$gmark[which(paddle$GPCM<=GPCMcrit[1])] <- 1


paddle %>% select(contains("mark")) %>% lapply(as.factor) -> tmp

tmp$mark %>% confusionMatrix(tmp$tmark) -> cfst
tmp$mark %>% confusionMatrix(tmp$fmark) -> cfsf
tmp$mark %>% confusionMatrix(tmp$pmark) -> cfsp
tmp$mark %>% confusionMatrix(tmp$gmark) -> cfsg



tmp$mark1 %>% confusionMatrix(tmp$tmark1,positive = "1") -> cfst1
tmp$mark2 %>% confusionMatrix(tmp$tmark2,positive = "1") -> cfst2
tmp$mark3 %>% confusionMatrix(tmp$tmark3,positive = "1") -> cfst3
tmp$mark1 %>% confusionMatrix(tmp$fmark1,positive = "1") -> cfsf1
tmp$mark2 %>% confusionMatrix(tmp$fmark2,positive = "1") -> cfsf2
tmp$mark3 %>% confusionMatrix(tmp$fmark3,positive = "1") -> cfsf3
tmp$mark1 %>% confusionMatrix(tmp$pmark1,positive = "1") -> cfsp1
tmp$mark2 %>% confusionMatrix(tmp$pmark2,positive = "1") -> cfsp2
tmp$mark3 %>% confusionMatrix(tmp$pmark3,positive = "1") -> cfsp3
tmp$mark1 %>% confusionMatrix(tmp$gmark1,positive = "1") -> cfsg1
tmp$mark2 %>% confusionMatrix(tmp$gmark2,positive = "1") -> cfsg2
tmp$mark3 %>% confusionMatrix(tmp$gmark3,positive = "1") -> cfsg3



cfsg3
########assign 친구들 #######
assign(paste0("paddle","c",k,"r",i),tmp)
assign(paste0("response","c",k,"r",i),response)
assign(paste0("cfst1","c",k,"r",i),cfst1)
assign(paste0("cfst2","c",k,"r",i),cfst2)
assign(paste0("cfst3","c",k,"r",i),cfst3)

assign(paste0("cfsf1","c",k,"r",i),cfsf1)
assign(paste0("cfsf2","c",k,"r",i),cfsf2)
assign(paste0("cfsf3","c",k,"r",i),cfsf3)

assign(paste0("cfsp1","c",k,"r",i),cfsp1)
assign(paste0("cfsp2","c",k,"r",i),cfsp2)
assign(paste0("cfsp3","c",k,"r",i),cfsp3)

assign(paste0("cfsg1","c",k,"r",i),cfsg1)
assign(paste0("cfsg2","c",k,"r",i),cfsg2)
assign(paste0("cfsg3","c",k,"r",i),cfsg3)
