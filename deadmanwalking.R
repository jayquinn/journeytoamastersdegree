setwd("c:\\git\\journeytoamastersdegree")

source("conditions.r")

#문항 범주 수 2
# 난이도 0
# 변별도 0.5~1.5
# 문항 수 15
# 표본크기 250

person = rnorm(250, mean = 0, sd = 1)
a = runif(15, min = 0, max = 1.5)
d = rnorm(15, mean = 0, sd = 1)
N = 250
response = simdata(a = a, d = d, N = N, itemtype = '2PL')
#CTT
sumscore = apply(response,1,sum)
hist(sumscore)
#CFA
model.cfa = 'F1 =~ Item_1 + Item_2 + Item_3+ Item_4+ Item_5+ Item_6+ Item_7+ Item_8+ Item_9+ Item_10 +
                   Item_11+ Item_12+ Item_13+ Item_14+ Item_15'
results.cfa<-cfa(model=model.cfa,data = response)
summary(results.cfa)
score.CFA<-lavPredict(results.cfa, method = "regression", se = T, acov = T)
hist(score.CFA)
#RASCH
model.rasch <- 'F1 = 1-15' 
results.rasch <- mirt(data=response, model=model.2pl, itemtype="Rasch", SE=TRUE, verbose=T)
summary(results.rasch)
coef.rasch <- coef(results.rasch, IRTpars=TRUE, simplify=TRUE)
print(coef.rasch)
score.rasch<-fscores(results.rasch,method = 'EAP')
hist(score.rasch)# EAP(default) MAP ML WLE EAPsum
#2PL
model.2pl <- 'F1 = 1-15' 
results.2pl <- mirt(data=response, model=model.2pl, itemtype="2PL", SE=TRUE, verbose=T)
summary(results.2pl)
coef.2pl <- coef(results.2pl, IRTpars=TRUE, simplify=TRUE)
print(coef.2pl)
score.2pl<-fscores(results.2pl,method = 'EAP')
hist(score.2pl)# EAP(default) MAP ML WLE EAPsum



