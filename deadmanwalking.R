setwd("c:\\git\\journeytoamastersdegree")
#코딩 순서
# 조건변경 -> 각 셀당 10번 반복  -> 값 산출(리스트 1-1:3 phi, 1-4:6 kappa)

#최종 원하는 결과 -> 데이터 프레임: 각 파이, 카파의 평균값을 싱글남바로 볼 수 있게

# 데이터 프레임 행 : 컨디션 열: 파이(모델별(4)x규준별(3))=12, 카파(모델4x규준3)= 12 = 열갯수 24개
# 먼저 할 일 : 24개따리 열 산출하기
#그보다 먼저 할 일: 각 24개따리 열 산출할 값들 10개씩 돌려서 평균내기



#문항 범주 수 2
# 난이도 0
# 변별도 0.5~1.5
# 문항 수 15
# 표본크기 250




setwd("c:\\git\\journeytoamastersdegree")
cellloop = 1:10
condloop = 1:nrow(cond)
for(k in condloop){
  for(i in cellloop){
    
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
    paddle$pmark1[which(paddle$PCM<=PCMcrit[1])] <- 1
    paddle$pmark2[which(paddle$PCM<=PCMcrit[2])] <- 1
    paddle$pmark3[which(paddle$PCM<=PCMcrit[3])] <- 1
    
    
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
    paddle$gmark1[which(paddle$GPCM<=GPCMcrit[1])] <- 1
    paddle$gmark2[which(paddle$GPCM<=GPCMcrit[2])] <- 1
    paddle$gmark3[which(paddle$GPCM<=GPCMcrit[3])] <- 1
    
    
    #mark에서 1이고 tfpg에서 1이면 TP 
    #mark에서 0이고 tfpg에서 1이면 FP
    #mark에서 1이고 tfpg에서 0이면 FN
    #mark에서 0이고 tfpg에서 0이면 TN
    
    
    
    
    paddle %>% filter(mark1==1 & tmark1 ==1) %>% nrow() -> TP
    paddle %>% filter(mark1==0 & tmark1 ==1) %>% nrow() -> FP
    paddle %>% filter(mark1==1 & tmark1 ==0) %>% nrow() -> FN
    paddle %>% filter(mark1==0 & tmark1 ==0) %>% nrow() -> TN
    tmptype1tmark1 = FP/(TP+FP+FN+TN)
    tmptype2tmark1 = FN/(TP+FP+FN+TN)
    type1tmark1 = append(type1tmark1, values = tmptype1tmark1)
    type2tmark1 = append(type2tmark1, values = tmptype2tmark1)
    cfs = matrix(c(TP,FP,FN,TN),ncol=2, dimnames=list(c("Actual Positive","Actual Negative"),c("Predicted Positive","Predicted Negative")))
    phi(cfs)
    tmphitmark1 = append(tmphitmark1,values=phi(cfs))
    paddle %>% filter(mark2==1 & tmark2 ==1) %>% nrow() -> TP
    paddle %>% filter(mark2==0 & tmark2 ==1) %>% nrow() -> FP
    paddle %>% filter(mark2==1 & tmark2 ==0) %>% nrow() -> FN
    paddle %>% filter(mark2==0 & tmark2 ==0) %>% nrow() -> TN
    tmptype1tmark2 = FP/(TP+FP+FN+TN)
    tmptype2tmark2 = FN/(TP+FP+FN+TN)
    type1tmark2 = append(type1tmark2, values = tmptype1tmark2)
    type2tmark2 = append(type2tmark2, values = tmptype2tmark2)
    cfs = matrix(c(TP,FP,FN,TN),ncol=2, dimnames=list(c("Actual Positive","Actual Negative"),c("Predicted Positive","Predicted Negative")))
    phi(cfs)
    tmphitmark2 = append(tmphitmark2,values=phi(cfs))
    paddle %>% filter(mark3==1 & tmark3 ==1) %>% nrow() -> TP
    paddle %>% filter(mark3==0 & tmark3 ==1) %>% nrow() -> FP
    paddle %>% filter(mark3==1 & tmark3 ==0) %>% nrow() -> FN
    paddle %>% filter(mark3==0 & tmark3 ==0) %>% nrow() -> TN
    tmptype1tmark3 = FP/(TP+FP+FN+TN)
    tmptype2tmark3 = FN/(TP+FP+FN+TN)
    type1tmark3 = append(type1tmark3, values = tmptype1tmark3)
    type2tmark3 = append(type2tmark3, values = tmptype2tmark3)
    cfs = matrix(c(TP,FP,FN,TN),ncol=2, dimnames=list(c("Actual Positive","Actual Negative"),c("Predicted Positive","Predicted Negative")))
    phi(cfs)
    tmphitmark3 = append(tmphitmark3,values=phi(cfs))
    
    
    
    
    
    
    paddle %>% filter(mark1==1 & fmark1 ==1) %>% nrow() -> TP
    paddle %>% filter(mark1==0 & fmark1 ==1) %>% nrow() -> FP
    paddle %>% filter(mark1==1 & fmark1 ==0) %>% nrow() -> FN
    paddle %>% filter(mark1==0 & fmark1 ==0) %>% nrow() -> TN
    tmptype1fmark1 = FP/(TP+FP+FN+TN)
    tmptype2fmark1 = FN/(TP+FP+FN+TN)
    type1fmark1 = append(type1fmark1, values = tmptype1fmark1)
    type2fmark1 = append(type2fmark1, values = tmptype2fmark1)
    cfs = matrix(c(TP,FP,FN,TN),ncol=2, dimnames=list(c("Actual Positive","Actual Negative"),c("Predicted Positive","Predicted Negative")))
    phi(cfs)
    tmphifmark1 = append(tmphifmark1,values=phi(cfs))
    paddle %>% filter(mark2==1 & fmark2 ==1) %>% nrow() -> TP
    paddle %>% filter(mark2==0 & fmark2 ==1) %>% nrow() -> FP
    paddle %>% filter(mark2==1 & fmark2 ==0) %>% nrow() -> FN
    paddle %>% filter(mark2==0 & fmark2 ==0) %>% nrow() -> TN
    tmptype1fmark2 = FP/(TP+FP+FN+TN)
    tmptype2fmark2 = FN/(TP+FP+FN+TN)
    type1fmark2 = append(type1fmark2, values = tmptype1fmark2)
    type2fmark2 = append(type2fmark2, values = tmptype2fmark2)
    cfs = matrix(c(TP,FP,FN,TN),ncol=2, dimnames=list(c("Actual Positive","Actual Negative"),c("Predicted Positive","Predicted Negative")))
    phi(cfs)
    tmphifmark2 = append(tmphifmark2,values=phi(cfs))
    paddle %>% filter(mark3==1 & fmark3 ==1) %>% nrow() -> TP
    paddle %>% filter(mark3==0 & fmark3 ==1) %>% nrow() -> FP
    paddle %>% filter(mark3==1 & fmark3 ==0) %>% nrow() -> FN
    paddle %>% filter(mark3==0 & fmark3 ==0) %>% nrow() -> TN
    tmptype1fmark3 = FP/(TP+FP+FN+TN)
    tmptype2fmark3 = FN/(TP+FP+FN+TN)
    type1fmark3 = append(type1fmark3, values = tmptype1fmark3)
    type2fmark3 = append(type2fmark3, values = tmptype2fmark3)
    cfs = matrix(c(TP,FP,FN,TN),ncol=2, dimnames=list(c("Actual Positive","Actual Negative"),c("Predicted Positive","Predicted Negative")))
    phi(cfs)
    tmphifmark3 = append(tmphifmark3,values=phi(cfs))
    
    
    
    
    
    
    paddle %>% filter(mark1==1 & pmark1 ==1) %>% nrow() -> TP
    paddle %>% filter(mark1==0 & pmark1 ==1) %>% nrow() -> FP
    paddle %>% filter(mark1==1 & pmark1 ==0) %>% nrow() -> FN
    paddle %>% filter(mark1==0 & pmark1 ==0) %>% nrow() -> TN
    tmptype1pmark1 = FP/(TP+FP+FN+TN)
    tmptype2pmark1 = FN/(TP+FP+FN+TN)
    type1pmark1 = append(type1pmark1, values = tmptype1pmark1)
    type2pmark1 = append(type2pmark1, values = tmptype2pmark1)
    cfs = matrix(c(TP,FP,FN,TN),ncol=2, dimnames=list(c("Actual Positive","Actual Negative"),c("Predicted Positive","Predicted Negative")))
    phi(cfs)
    tmphipmark1 = append(tmphipmark1,values=phi(cfs))
    paddle %>% filter(mark2==1 & pmark2 ==1) %>% nrow() -> TP
    paddle %>% filter(mark2==0 & pmark2 ==1) %>% nrow() -> FP
    paddle %>% filter(mark2==1 & pmark2 ==0) %>% nrow() -> FN
    paddle %>% filter(mark2==0 & pmark2 ==0) %>% nrow() -> TN
    tmptype1pmark2 = FP/(TP+FP+FN+TN)
    tmptype2pmark2 = FN/(TP+FP+FN+TN)
    type1pmark2 = append(type1pmark2, values = tmptype1pmark2)
    type2pmark2 = append(type2pmark2, values = tmptype2pmark2)
    cfs = matrix(c(TP,FP,FN,TN),ncol=2, dimnames=list(c("Actual Positive","Actual Negative"),c("Predicted Positive","Predicted Negative")))
    phi(cfs)
    tmphipmark2 = append(tmphipmark2,values=phi(cfs))
    paddle %>% filter(mark3==1 & pmark3 ==1) %>% nrow() -> TP
    paddle %>% filter(mark3==0 & pmark3 ==1) %>% nrow() -> FP
    paddle %>% filter(mark3==1 & pmark3 ==0) %>% nrow() -> FN
    paddle %>% filter(mark3==0 & pmark3 ==0) %>% nrow() -> TN
    tmptype1pmark3 = FP/(TP+FP+FN+TN)
    tmptype2pmark3 = FN/(TP+FP+FN+TN)
    type1pmark3 = append(type1pmark3, values = tmptype1pmark3)
    type2pmark3 = append(type2pmark3, values = tmptype2pmark3)
    cfs = matrix(c(TP,FP,FN,TN),ncol=2, dimnames=list(c("Actual Positive","Actual Negative"),c("Predicted Positive","Predicted Negative")))
    phi(cfs)
    tmphipmark3 = append(tmphipmark3,values=phi(cfs))
    
    
    
    
    
    
    
    paddle %>% filter(mark1==1 & gmark1 ==1) %>% nrow() -> TP
    paddle %>% filter(mark1==0 & gmark1 ==1) %>% nrow() -> FP
    paddle %>% filter(mark1==1 & gmark1 ==0) %>% nrow() -> FN
    paddle %>% filter(mark1==0 & gmark1 ==0) %>% nrow() -> TN
    tmptype1gmark1 = FP/(TP+FP+FN+TN)
    tmptype2gmark1 = FN/(TP+FP+FN+TN)
    type1gmark1 = append(type1gmark1, values = tmptype1gmark1)
    type2gmark1 = append(type2gmark1, values = tmptype2gmark1)
    cfs = matrix(c(TP,FP,FN,TN),ncol=2, dimnames=list(c("Actual Positive","Actual Negative"),c("Predicted Positive","Predicted Negative")))
    phi(cfs)
    tmphigmark1 = append(tmphigmark1,values=phi(cfs))
    paddle %>% filter(mark2==1 & gmark2 ==1) %>% nrow() -> TP
    paddle %>% filter(mark2==0 & gmark2 ==1) %>% nrow() -> FP
    paddle %>% filter(mark2==1 & gmark2 ==0) %>% nrow() -> FN
    paddle %>% filter(mark2==0 & gmark2 ==0) %>% nrow() -> TN
    tmptype1gmark2 = FP/(TP+FP+FN+TN)
    tmptype2gmark2 = FN/(TP+FP+FN+TN)
    type1gmark2 = append(type1gmark2, values = tmptype1gmark2)
    type2gmark2 = append(type2gmark2, values = tmptype2gmark2)
    cfs = matrix(c(TP,FP,FN,TN),ncol=2, dimnames=list(c("Actual Positive","Actual Negative"),c("Predicted Positive","Predicted Negative")))
    phi(cfs)
    tmphigmark2 = append(tmphigmark2,values=phi(cfs))
    paddle %>% filter(mark3==1 & gmark3 ==1) %>% nrow() -> TP
    paddle %>% filter(mark3==0 & gmark3 ==1) %>% nrow() -> FP
    paddle %>% filter(mark3==1 & gmark3 ==0) %>% nrow() -> FN
    paddle %>% filter(mark3==0 & gmark3 ==0) %>% nrow() -> TN
    tmptype1gmark3 = FP/(TP+FP+FN+TN)
    tmptype2gmark3 = FN/(TP+FP+FN+TN)
    type1gmark3 = append(type1gmark3, values = tmptype1gmark3)
    type2gmark3 = append(type2gmark3, values = tmptype2gmark3)
    cfs = matrix(c(TP,FP,FN,TN),ncol=2, dimnames=list(c("Actual Positive","Actual Negative"),c("Predicted Positive","Predicted Negative")))
    phi(cfs)
    tmphigmark3 = append(tmphigmark3,values=phi(cfs))
    
    
    
    
    
    
    
    
    cohen.kappa(select(paddle,mark1,tmark1)) -> tmpkapt1
    tmpkaptmark1 = append(tmpkaptmark1, values = tmpkapt1$kappa)
    cohen.kappa(select(paddle,mark2,tmark2)) -> tmpkapt2
    tmpkaptmark2 = append(tmpkaptmark2, values = tmpkapt2$kappa)
    cohen.kappa(select(paddle,mark3,tmark3)) -> tmpkapt3
    tmpkaptmark3 = append(tmpkaptmark3, values = tmpkapt3$kappa)
    
    cohen.kappa(select(paddle,mark1,fmark1)) -> tmpkapf1
    tmpkapfmark1 = append(tmpkapfmark1, values = tmpkapf1$kappa)
    cohen.kappa(select(paddle,mark2,fmark2)) -> tmpkapf2
    tmpkapfmark2 = append(tmpkapfmark2, values = tmpkapf2$kappa)
    cohen.kappa(select(paddle,mark3,fmark3)) -> tmpkapf3
    tmpkapfmark3 = append(tmpkapfmark3, values = tmpkapf3$kappa)
    
    cohen.kappa(select(paddle,mark1,pmark1)) -> tmpkapp1
    tmpkappmark1 = append(tmpkappmark1, values = tmpkapp1$kappa)
    cohen.kappa(select(paddle,mark2,pmark2)) -> tmpkapp2
    tmpkappmark2 = append(tmpkappmark2, values = tmpkapp2$kappa)
    cohen.kappa(select(paddle,mark3,pmark3)) -> tmpkapp3
    tmpkappmark3 = append(tmpkappmark3, values = tmpkapp3$kappa)
    
    cohen.kappa(select(paddle,mark1,gmark1)) -> tmpkapg1
    tmpkapgmark1 = append(tmpkapgmark1, values = tmpkapg1$kappa)
    cohen.kappa(select(paddle,mark2,gmark2)) -> tmpkapg2
    tmpkapgmark2 = append(tmpkapgmark2, values = tmpkapg2$kappa)
    cohen.kappa(select(paddle,mark3,gmark3)) -> tmpkapg3
    tmpkapgmark3 = append(tmpkapgmark3, values = tmpkapg3$kappa)
    cat(k,"번째 조건",i,"번째 셀 반복중")
  }
  agg = cbind(tmphitmark1,tmphitmark2,tmphitmark3,tmphifmark1,tmphifmark2,tmphifmark3,tmphipmark1,tmphipmark2,tmphipmark3,tmphigmark1,tmphigmark2,tmphigmark3,
              tmpkaptmark1,tmpkaptmark2,tmpkaptmark3,tmpkapfmark1,tmpkapfmark2,tmpkapfmark3,tmpkappmark1,tmpkappmark2,tmpkappmark3,tmpkapgmark1,tmpkapgmark2,tmpkapgmark3,
              type1tmark1,type1tmark2,type1tmark3,type1fmark1,type1fmark2,type1fmark3,type1pmark1,type1pmark2,type1pmark3,type1gmark1,type1gmark2,type1gmark3,
              type2tmark1,type2tmark2,type2tmark3,type2fmark1,type2fmark2,type2fmark3,type2pmark1,type2pmark2,type2pmark3,type2gmark1,type2gmark2,type2gmark3)
  agg[which(agg==0)]<-NA
  apply(agg,2, mean) %>% t() %>% as.data.frame %>% rbind(boat) -> boat
  cat(k,"번째 조건반복중")
}

colnames(boat) = c("CTTphi1","CTTphi2","CTTphi3","CFAphi1","CFAphi2","CFAphi3","PCMphi1","PCMphi2","PCMphi3","GPCMphi1","GPCMphi2","GPCMphi3",
                   "CTTkappa1","CTTkappa2","CTTkappa3","CFAkappa1","CFAkappa2","CFAkappa3","PCMkappa1","PCMkappa2","PCMkappa3","GPCMkappa1","GPCMkappa2","GPCMkappa3",
                   "CTTtypeone1","CTTtypeone2","CTTtypeone3","CFAtypeone1","CFAtypeone2","CFAtypeone3","PCMtypeone1","PCMtypeone2","PCMtypeone3","GPCMtypeone1","GPCMtypeone2","GPCMtypeone3",
                   "CTTtypetwo1","CTTtypetwo2","CTTtypetwo3","CFAtypetwo1","CFAtypetwo2","CFAtypetwo3","PCMtypetwo1","PCMtypetwo2","PCMtypetwo3","GPCMtypetwo1","GPCMtypetwo2","GPCMtypetwo3")
write.table(boat,"boat2-211019.csv",row.names=F,sep=",")
geterrmessage()
