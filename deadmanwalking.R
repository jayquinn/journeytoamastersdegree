setwd("c:\\git\\journeytoamastersdegree")


#문항 범주 수 2
# 난이도 0
# 변별도 0.5~1.5
# 문항 수 15
# 표본크기 250
matrix(data = 0, nrow = 250, ncol = 20) %>% as.data.frame -> paddle
colnames(paddle)<-c("pers","mark1","mark2","mark3",
                         "CTT","tmark1","tmark2","tmark3",
                         "CFA","fmark1","fmark2","fmark3",
                         "PCM","pmark1","pmark2","pmark3",
                         "GPCM","gmark1","gmark2","gmark3")
paddle$pers = rnorm(250, mean = 0, sd = 1)
paddle$pers %>% quantile(c(0.05,0.10,0.15)) -> personcrit
paddle$mark1[which(person$pers<=personcrit[1])] <- 1
paddle$mark2[which(person$pers<=personcrit[2])] <- 1
paddle$mark3[which(person$pers<=personcrit[3])] <- 1
hist(paddle$pers)
a = runif(15, min = 0, max = 1.5)
d = rnorm(15, mean = 0, sd = 1)
N = 250
response = simdata(a = a, d = d, N = N,Theta = as.matrix(person$pers), itemtype = '2PL')
#CTT
paddle$CTT = apply(response,1,sum)
paddle$CTT %>% quantile(c(0.05,0.10,0.15)) -> CTTcrit
paddle$tmark1[which(paddle$CTT<=CTTcrit[1])] <- 1
paddle$tmark2[which(paddle$CTT<=CTTcrit[2])] <- 1
paddle$tmark3[which(paddle$CTT<=CTTcrit[3])] <- 1

hist(paddle$CTT)

#CFA
model.cfa = 'F1 =~ Item_1 + Item_2 + Item_3+ Item_4+ Item_5 + 
                   Item_6+ Item_7+ Item_8+ Item_9+ Item_10 +
                   Item_11+ Item_12+ Item_13+ Item_14+ Item_15'
results.cfa<-cfa(model=model.cfa,data = response)
summary(results.cfa)
lavPredict(results.cfa, method = "regression", se = T) %>% as.vector() -> paddle$CFA
hist(paddle$CFA)

paddle$CFA %>% quantile(c(0.05,0.10,0.15)) -> CFAcrit
paddle$fmark1[which(paddle$CFA<=CFAcrit[1])] <- 1
paddle$fmark2[which(paddle$CFA<=CFAcrit[2])] <- 1
paddle$fmark3[which(paddle$CFA<=CFAcrit[3])] <- 1

#RASCH
model.rasch <- 'F1 = 1-15' 
results.rasch <- mirt(data=response, model=model.rasch, itemtype="Rasch", SE=TRUE, verbose=T)
summary(results.rasch)
coef.rasch <- coef(results.rasch, IRTpars=TRUE, simplify=TRUE)
print(coef.rasch)
fscores(results.rasch,method = 'EAP') %>% as.vector() -> paddle$PCM


hist(paddle$PCM)# EAP(default) MAP ML WLE EAPsum

paddle$PCM %>% quantile(c(0.05,0.10,0.15)) -> PCMcrit
paddle$pmark1[which(paddle$PCM<=PCMcrit[1])] <- 1
paddle$pmark2[which(paddle$PCM<=PCMcrit[2])] <- 1
paddle$pmark3[which(paddle$PCM<=PCMcrit[3])] <- 1

#2PL
model.2pl <- 'F1 = 1-15' 
results.2pl <- mirt(data=response, model=model.2pl, itemtype="2PL", SE=TRUE, verbose=T)
summary(results.2pl)
coef.2pl <- coef(results.2pl, IRTpars=TRUE, simplify=TRUE)
print(coef.2pl)
fscores(results.2pl,method = 'EAP') %>% as.vector() -> paddle$GPCM

hist(score.2pl)# EAP(default) MAP ML WLE EAPsum
paddle$GPCM %>% quantile(c(0.05,0.10,0.15)) -> GPCMcrit
paddle$gmark1[which(paddle$GPCM<=GPCMcrit[1])] <- 1
paddle$gmark2[which(paddle$GPCM<=GPCMcrit[2])] <- 1
paddle$gmark3[which(paddle$GPCM<=GPCMcrit[3])] <- 1
head(paddle)

?cohen.kappa()
