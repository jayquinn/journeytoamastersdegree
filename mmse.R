install.packages("dplyr")
install.packages("mirt")
install.packages("lavaan")
install.packages("Epi")
library(dplyr)
library(mirt)
library(lavaan)
library(Epi)
dat<-read.csv("C:/git/journeytoamastersdegree/mmse.csv",header=T,sep=",")
head(dat)
######################################################################################
######################################################################################
###############################전처리 시작############################################
######################################################################################
######################################################################################
attach(dat)
#401 시간지남력 - 연월일 0123 
a401<-ifelse(C401==3,3,
             ifelse(C401==2,2,
                    ifelse(C401==1,1,
                           ifelse(C401==-8,NA,
                                  ifelse(C401==-9,0,
                                         ifelse(C401==5,0,NA))))))
head(a401)
table(C401,a401,useNA='always')
length(C401);length(a401)
table(C401)
table(a401)
#402 시간지남력 - 요일 01
a402<-ifelse(C402==1,1,
             ifelse(C402==-8,NA,
                    ifelse(C402==-9,0,
                           ifelse(C402==5,0,NA))))
head(a402)
table(C402,a402,useNA='always')
length(C402);length(a402)
table(a402)
#403 시간지남력- 계절 01
a403<-ifelse(C403==1,1,
             ifelse(C403==-8,NA,
                    ifelse(C403==-9,0,
                           ifelse(C403==5,0,NA))))
table(C403,a403,useNA='always')
table(C403)
table(a403)
length(C403);length(a403)

#404 장소지남력 - 현위치 01
a404<-ifelse(C404==1,1,
             ifelse(C404==-8,NA,
                    ifelse(C404==-9,0,
                           ifelse(C404==5,0,NA))))
table(C404,a404,useNA='always')
table(C404)
table(a404)
length(C404);length(a404)
#405 장소지남력 - 시/구/동/번지 01234
a405<-ifelse(C405==4,4,
             ifelse(C405==3,3,
                    ifelse(C405==2,2,
                           ifelse(C405==1,1,
                                  ifelse(C405==-8,NA,
                                         ifelse(C405==-9,0,
                                                ifelse(C405==5,0,NA)))))))
head(a405)
table(C405,a405,useNA='always')
table(C405)
table(a405)
length(C405);length(a405)
#406 기억력 테스트(3단어) 0123
a406<-ifelse(C406==3,3,
             ifelse(C406==2,2,
                    ifelse(C406==1,1,
                           ifelse(C406==-8,NA,
                                  ifelse(C406==-9,0,
                                         ifelse(C406==5,0,NA))))))
table(C406,a406,useNA='always')
table(C406)
table(a406)
length(C406);length(a406)
#407 주의 집중 및 계산 (뺄셈 1) 01
a407<-ifelse(C407==1,1,
             ifelse(C407==-8,NA,
                    ifelse(C407==-9,0,
                           ifelse(C407==5,0,NA))))
table(C407,a407,useNA='always')
table(C407)
table(a407)
length(C407);length(a407)
#408 주의 집중 및 계산 (뺄셈 2) 01
a408<-ifelse(C408==1,1,
             ifelse(C408==-8,NA,
                    ifelse(C408==-9,0,
                           ifelse(C408==5,0,NA))))
table(C408,a408,useNA='always')
table(C408)
table(a408)
length(C408);length(a408)
#409 주의 집중 및 계산 (뺄셈 3) 01
a409<-ifelse(C409==1,1,
             ifelse(C409==-8,NA,
                    ifelse(C409==-9,0,
                           ifelse(C409==5,0,NA))))
table(C409,a409,useNA='always')
table(C409)
table(a409)
length(C409);length(a409)
#410 주의 집중 및 계산 (뺄셈 4) 01
a410<-ifelse(C410==1,1,
             ifelse(C410==-8,NA,
                    ifelse(C410==-9,0,
                           ifelse(C410==5,0,NA))))
table(C410,a410,useNA='always')
table(C410)
table(a410)
length(C410);length(a410)
#411 주의 집중 및 계산 (뺄셈 5) 01
a411<-ifelse(C411==1,1,
             ifelse(C411==-8,NA,
                    ifelse(C411==-9,0,
                           ifelse(C411==5,0,NA))))
table(C411,a411,useNA='always')
table(C411)
table(a411)
length(C411);length(a411)
#412 기억력 테스트(3단어 재확인) 0123
a412<-ifelse(C412==3,3,
             ifelse(C412==2,2,
                    ifelse(C412==1,1,
                           ifelse(C412==-8,NA,
                                  ifelse(C412==-9,0,
                                         ifelse(C412==5,0,NA))))))
table(C412,a412,useNA='always')
table(C412)
table(a412)
length(C412);length(a412)
#413 소지품의 용도 (소지품 1) 01
a413<-ifelse(C413==1,1,
             ifelse(C413==-8,NA,
                    ifelse(C413==-9,0,
                           ifelse(C413==5,0,NA))))
table(C413,a413,useNA='always')
table(C413)
table(a413)
length(C413);length(a413)
#414 소지품의 용도 (소지품 2) 01
a414<-ifelse(C414==1,1,
             ifelse(C414==-8,NA,
                    ifelse(C414==-9,0,
                           ifelse(C414==5,0,NA))))
table(C414,a414,useNA='always')
table(C414)
table(a414)
length(C414);length(a414)
#415 따라서 말하기 01
a415<-ifelse(C415==1,1,
             ifelse(C415==-8,NA,
                    ifelse(C415==-9,0,
                           ifelse(C415==5,0,NA))))
table(C415,a415,useNA='always')
table(C415)
table(a415)
length(C415);length(a415)
#416 명령시행_종이 뒤집기, 접기, 건네주기 0123
a416<-ifelse(C416==3,3,
             ifelse(C416==2,2,
                    ifelse(C416==1,1,
                           ifelse(C416==-8,NA,
                                  ifelse(C416==-9,0,
                                         ifelse(C416==5,0,NA))))))
table(C416,a416,useNA='always')
table(C416)
table(a416)
length(C416);length(a416)
#417 명령시행_읽고 눈감기 012
a417<-ifelse(C417==3,1,
             ifelse(C417==1,1,
                    ifelse(C417==-8,NA,
                           ifelse(C417==-9,0,
                                  ifelse(C417==5,0,NA)))))
table(C417,a417,useNA='always')
table(C417)
table(a417)
length(C417);length(a417)
#418 명령시행_기분 또는 날씨에 대해 쓰기 01
a418<-ifelse(C418==1,1,
             ifelse(C418==-8,NA,
                    ifelse(C418==-9,0,
                           ifelse(C418==5,0,NA))))
table(C418,a418,useNA='always')
table(C418)
table(a418)
length(C418);length(a418)
#419 명령시행_제시된 그림 똑같이 그리기01
a419<-ifelse(C419==1,1,
             ifelse(C419==-8,NA,
                    ifelse(C419==-9,0,
                           ifelse(C419==5,0,NA))))
table(C419,a419,useNA='always')
table(C419)
table(a419)
length(C419);length(a419)
detach(dat)
#진단정보(diag) 추가
attach(dat)
response.raw<-data.frame(a401,a402,a403,a404,a405,a406,a407,a408,a409,a410,a411,a412,a413,a414,a415,a416,a417,a418,a419,dat$diag,dat$year,dat$edu,dat$gender)
detach(dat)
#### 진단정보(diag)  1 치매, 3경도인지장애, 5 정상
#24~30 정상
#18~23 경도인지장애
#0~17 분명한 인지기능장애
#### 진단정보 치매 + 경도인지장애 합치기
response.raw$diag<-ifelse(dat$diag==5,1,
                          ifelse(dat$diag==3,0,
                                 ifelse(dat$diag==1,0,dat$diag))) #정상1, 경도인지 + 치매 0
table(response.raw$diag)
head(response.raw$diag)
print(diag)
#### 나이변환
response.raw$dat.year<-2021-response.raw$dat.year
#검사 응답이 모두 NA인 행제거
response.clean <- response.raw %>% filter(!is.na(a401) & !is.na(a402) &!is.na(a403)&!is.na(a404)&!is.na(a405)&!is.na(a406)&!is.na(a407)&!is.na(a408)&!is.na(a409)&!is.na(a410)&!is.na(a411)&!is.na(a412)&!is.na(a413)&!is.na(a414)&!is.na(a415)&!is.na(a416)&!is.na(a417)&!is.na(a418)&!is.na(a419))
response<-response.clean
#검사응답이 모두 NA인 행을 제거한연령 학력, 성별 진단정보(diag), 
clean.diag<-response[,21:24] #이거 진단 넘버 잘봐야함 !!!!!!!
# 성별: 남1 여5
# 학력: 초졸이하1 중졸2 고졸3 대졸이상4 
#진단정보(diag)제외한 검사세트 찐클린
response<-response[,1:19]
######################################################################################
######################################################################################
###############################여기까지 전처리########################################
######################################################################################
######################################################################################
#### CTT 점수산출 ####
score.CTT<-vector("double",nrow(response))
for ( i in 1:nrow(response) ){
  score.CTT[[i]]<-sum(response[i,1:19],na.rm=T)}
table(score.CTT)
hist(score.CTT)
#### PCM 점수산출 ####
#library(mirt)
model.pcm <- 'F1 = 1-19' 
results.pcm <- mirt(data=response, model=model.pcm, itemtype="Rasch", SE=TRUE, verbose=FALSE)
coef.pcm <- coef(results.pcm, IRTpars=TRUE, simplify=TRUE)
items.pcm <- as.data.frame(coef.pcm$items)
print(items.pcm)
summary(results.pcm)
plot(results.pcm, type = 'trace', which.items = c(1:19))
plot(results.pcm, type = 'infotrace', which.items = c(1:19))
plot(results.pcm, type = 'info', theta_lim = c(-4,4), lwd=2)
plot(results.pcm, type = 'SE', theta_lim = c(-4,4), lwd=2)
plot(results.pcm, type = 'score', theta_lim = c(-4,4), lwd=2)
plot(results.pcm, type = 'itemscore', theta_lim = c(-4,4), lwd=2)
plot(results.pcm, type = 'rxx', theta_lim = c(-4,4), lwd=2)

score.PCM<-fscores(results.pcm,method = 'EAP')
hist(score.PCM)# EAP(default) MAP ML WLE EAPsum

#### GPCM 점수 산출####
#library(mirt)
model.gpcm <- 'F1 = 1-19' 
results.gpcm <- mirt(data=response, model=model.gpcm, itemtype="gpcm", SE=TRUE, verbose=FALSE)
coef.gpcm <- coef(results.gpcm, IRTpars=TRUE, simplify=TRUE)
items.gpcm <- as.data.frame(coef.gpcm$items)
print(items.gpcm)
summary(results.gpcm)
plot(results.gpcm, type = 'trace', which.items = c(1:19))
plot(results.gpcm, type = 'infotrace', which.items = c(1:19))
plot(results.gpcm, type = 'info', theta_lim = c(-4,4), lwd=2)
plot(results.gpcm, type = 'SE', theta_lim = c(-4,4), lwd=2)
plot(results.gpcm, type = 'score', theta_lim = c(-4,4), lwd=2)
plot(results.gpcm, type = 'itemscore', theta_lim = c(-4,4), lwd=2)
plot(results.gpcm, type = 'rxx', theta_lim = c(-4,4), lwd=2)
score.GPCM<-fscores(results.gpcm,method = 'EAP')
hist(score.GPCM)# EAP(default) MAP ML WLE EAPsum
#### CFA 점수산출####
#library(lavaan)
model.cfa<-'F1=~a401+a402+a403+a404+a405+a406+a407+a408+a409+a410+a411+a412+a413+a414+a415+a416+a417+a418+a419'
results.cfa<-cfa(model=model.cfa,data = response)
summary(results.cfa)
score.CFA<-lavPredict(results.cfa)
hist(score.CFA)

######################################################################################
#### 각 점수 데이터프레임화 ####
######################################################################################
score.frame<-cbind(score.CTT,score.CFA,score.PCM,score.GPCM)
colnames(score.frame)<-c("CTT","CFA","PCM","GPCM")
head(score.frame)
score.frame.t<-as_tibble(score.frame)
#스코어 프레임
scoreframe<-cbind(score.frame.t,clean.diag)
colnames(scoreframe)  <- c('CTT','CFA','PCM','GPCM','age','edu','gender','diag') ###########사실상 데이터 완성본 #####

quantile(sf$PCM,0.25)
##### 23점 이하 사람에게 마커 붙이기
score.frame.t %>% mutate(markerCTT = case_when(CTT <= 23 ~ '1',
                                            CTT > 23 ~ '0'),
                         markerCFA = case_when(CFA <= quantile(sf$CFA,0.25) ~ '1',
                                               CFA > quantile(sf$CFA,0.25) ~ '0'),
                         markerPCM = case_when(PCM <= quantile(sf$PCM,0.25) ~ '1',
                                               PCM > quantile(sf$PCM,0.25) ~ '0'),
                         markerGPCM = case_when(GPCM <= quantile(sf$GPCM,0.25) ~ '1',
                                               GPCM > quantile(sf$GPCM,0.25) ~ '0')
                         ) -> sf


# 이거 012 기준으로 qunatile 짜서 각 quantile 안에 누가 몇명 속해있는지 알아보고
sf %>% filter(markerCTT == 1 & markerCFA == 1) %>% nrow() -> TP
sf %>% filter(markerCTT == 0 & markerCFA == 1) %>% nrow() -> FP
sf %>% filter(markerCTT == 1 & markerCFA == 0) %>% nrow() -> FN
sf %>% filter(markerCTT == 0 & markerCFA == 0) %>% nrow() -> TN
cfs = matrix(c(TP,FP,FN,TN),ncol=2, dimnames=list(c("Actual Positive","Actual Negative"),c("Predicted Positive","Predicted Negative")))
phi(cfs)
Kappa(cfs)
sf %>% filter(markerCTT == 1 & markerPCM == 1) %>% nrow() -> TP
sf %>% filter(markerCTT == 0 & markerPCM == 1) %>% nrow() -> FP
sf %>% filter(markerCTT == 1 & markerPCM == 0) %>% nrow() -> FN
sf %>% filter(markerCTT == 0 & markerPCM == 0) %>% nrow() -> TN
cfs = matrix(c(TP,FP,FN,TN),ncol=2, dimnames=list(c("Actual Positive","Actual Negative"),c("Predicted Positive","Predicted Negative")))
phi(cfs)
Kappa(cfs)
sf %>% filter(markerCTT == 1 & markerGPCM == 1) %>% nrow() -> TP
sf %>% filter(markerCTT == 0 & markerGPCM == 1) %>% nrow() -> FP
sf %>% filter(markerCTT == 1 & markerGPCM == 0) %>% nrow() -> FN
sf %>% filter(markerCTT == 0 & markerGPCM == 0) %>% nrow() -> TN
cfs = matrix(c(TP,FP,FN,TN),ncol=2, dimnames=list(c("Actual Positive","Actual Negative"),c("Predicted Positive","Predicted Negative")))
phi(cfs)
Kappa(cfs)
sf %>% filter(markerCFA == 1 & markerPCM == 1) %>% nrow() -> TP
sf %>% filter(markerCFA == 0 & markerPCM == 1) %>% nrow() -> FP
sf %>% filter(markerCFA == 1 & markerPCM == 0) %>% nrow() -> FN
sf %>% filter(markerCFA == 0 & markerPCM == 0) %>% nrow() -> TN
cfs = matrix(c(TP,FP,FN,TN),ncol=2, dimnames=list(c("Actual Positive","Actual Negative"),c("Predicted Positive","Predicted Negative")))
phi(cfs)
Kappa(cfs)
sf %>% filter(markerCFA == 1 & markerGPCM == 1) %>% nrow() -> TP
sf %>% filter(markerCFA == 0 & markerGPCM == 1) %>% nrow() -> FP
sf %>% filter(markerCFA == 1 & markerGPCM == 0) %>% nrow() -> FN
sf %>% filter(markerCFA == 0 & markerGPCM == 0) %>% nrow() -> TN
cfs = matrix(c(TP,FP,FN,TN),ncol=2, dimnames=list(c("Actual Positive","Actual Negative"),c("Predicted Positive","Predicted Negative")))
phi(cfs)
Kappa(cfs)
sf %>% filter(markerPCM == 1 & markerGPCM == 1) %>% nrow() -> TP
sf %>% filter(markerPCM == 0 & markerGPCM == 1) %>% nrow() -> FP
sf %>% filter(markerPCM == 1 & markerGPCM == 0) %>% nrow() -> FN
sf %>% filter(markerPCM == 0 & markerGPCM == 0) %>% nrow() -> TN
cfs = matrix(c(TP,FP,FN,TN),ncol=2, dimnames=list(c("Actual Positive","Actual Negative"),c("Predicted Positive","Predicted Negative")))
phi(cfs)
Kappa(cfs)
##상관그림
plot(score.frame.t)
# sort 상관 그림
attach(score.frame.t)
par(mfrow=c(2,3))
plot(x =sort(CTT), y = sort(CFA))
plot(x =sort(CTT), y = sort(PCM))
plot(x =sort(CTT), y = sort(GPCM))
plot(x =sort(CFA), y = sort(PCM))
plot(x =sort(CFA), y = sort(GPCM))
plot(x =sort(PCM), y = sort(GPCM))
par(mfrow=c(1,1))
detach(score.frame.t)
# 점수별 상관비교
attach(score.frame.t)
cor(CTT,CFA,method="spearman")
cor(CTT,PCM,method="spearman")
cor(CTT,GPCM,method="spearman")
cor(CFA,PCM,method="spearman")
cor(CFA,GPCM,method="spearman")
cor(PCM,GPCM,method="spearman")
cor(CTT,CFA)
cor(CTT,PCM)
cor(CTT,GPCM)
cor(CFA,PCM)
cor(CFA,GPCM)
cor(PCM,GPCM)
detach(score.frame.t)




#### 특정점수 이하 분석 ####
#CTT 특정점수이하만 남기기
undercut<-filter(score.frame.t,CTT<35)
head(undercut)
nrow(undercut)
#상관비교
attach(undercut)
cor(CTT,CFA,method="spearman")
cor(CTT,PCM,method="spearman")
cor(CTT,GPCM,method="spearman")
cor(CFA,PCM,method="spearman")
cor(CFA,GPCM,method="spearman")
cor(PCM,GPCM,method="spearman")
cor(CTT,CFA)
cor(CTT,PCM)
cor(CTT,GPCM)
cor(CFA,PCM)
cor(CFA,GPCM)
cor(PCM,GPCM)
detach(undercut)
plot(undercut)


#### Qunatile 10% ####
attach(score.frame.t)
quantile(CTT,probs=seq(0,1,0.0025))
qtcut_CTT <- filter(score.frame.t,CTT<18)
quantile(PCM,probs=seq(0,1,0.0025))
qtcut_PCM <- filter(score.frame.t,PCM<(-2.28))
quantile(GPCM,probs=seq(0,1,0.0025))
qtcut_GPCM <- filter(score.frame.t,GPCM<(-1.15))
quantile(CFA,probs=seq(0,1,0.0025))
qtcut_CFA <- filter(score.frame.t,CFA<(-0.61))
nrow(qtcut_CTT)
nrow(qtcut_CFA)
nrow(qtcut_PCM)
nrow(qtcut_GPCM)
#이 사람들이 같은 사람들이냐?
filter(qtcut_PCM,CTT>17)
filter(qtcut_GPCM,CTT>17)
filter(qtcut_CFA,CTT>17)
detach(score.frame.t)
#### Standard deviation 1.5sd ####
attach(score.frame.t)
mean(CTT) - (sd(CTT)*1.5)  # 6 * 1.5 = 9 -> 26-9 = 17부터 치매
mean(PCM) - sd(PCM)*1.5 # 1.87 * 1.5 = 2.8 -> 0.05 - 2.8 = -2.75 부터 치매 
mean(GPCM) - sd(GPCM)*1.5 # 0.91 * 1.5 = 1.37 -> 0 - 1.37 = -1.37 부터 치매 
mean(CFA) - sd(CFA)*1.5 # 0.46 * 1.5 = 0.69 -> 0 - 0.69 = -0.69 부터 치매


sdcut_CTT<-filter(score.frame.t,CTT<=17)
sdcut_CFA<-filter(score.frame.t,CFA<=(-0.69))
sdcut_PCM<-filter(score.frame.t,PCM<=(-2.75))
sdcut_GPCM<-filter(score.frame.t,GPCM<(-1.37))
nrow(sdcut_CTT)
nrow(sdcut_CFA)
nrow(sdcut_PCM)
nrow(sdcut_GPCM)
filter(sdcut_PCM,CTT>=15)
filter(sdcut_GPCM,CTT>=16)
filter(sdcut_CFA,CTT>=18)
detach(score.frame.t)

#히스토그램 - PCM GPCM CFA 겹쳐서 그리기 -> 레인지가 다르다 다르다 다르다
attach(score.frame.t)
x_range=seq(-6,3,by=0.05)
y_max=2500
#hist_CTT<-hist(CTT, breaks=x_range, plot = FALSE)
hist_CFA<-hist(CFA, breaks=x_range, plot = FALSE)
hist_PCM<-hist(PCM, breaks=x_range, plot = FALSE)
hist_GPCM<-hist(GPCM, breaks=x_range, plot = FALSE)
#plot(hist_CTT, col=adjustcolor("red",alpha=0.5),ann=FALSE,axes=FALSE,ylim=c(0,y_max))
plot(hist_CFA,col=adjustcolor("green",alpha=0.5),ann=FALSE,axes=FALSE,ylim=c(0,y_max))
plot(hist_PCM,col=adjustcolor("blue",alpha=0.5), add = TRUE)
plot(hist_GPCM,col=adjustcolor("red",alpha=0.5), add = TRUE)
detach(score.frame.t)

# 히스토그램 핀포인트 각각 그리기
attach(score.frame.t)
#x_range=seq(-6,3,by=0.05)
#y_max=2500
#hist_CFA<-hist(CFA, breaks=x_range, plot = TRUE)
#hist_PCM<-hist(PCM, breaks=x_range, plot = TRUE)
#hist_GPCM<-hist(GPCM, breaks=x_range, plot = TRUE)
par(mfrow=c(2,2))
hist(CTT);abline(v=quantile(CTT,probs = 0.25),col="blue",lwd=2)
hist(CFA);abline(v=quantile(CFA,probs = 0.25),col="blue",lwd=2)
hist(PCM);abline(v=quantile(PCM,probs = 0.25),col="blue",lwd=2)
hist(GPCM);abline(v=quantile(GPCM,probs = 0.25),col="blue",lwd=2)
par(mfrow=c(1,1))
detach(score.frame.t)
#### 힛트다 힛트 CTT ####
# 17이하 치매의심, 18이상 23이하 인지기능 저하, 24이상 정상
attach(scoreframe) #도합 6548
CTT_a<-filter(scoreframe,CTT>=24&diag==5) #4843
CTT_b<-filter(scoreframe,CTT>=24&diag<=3) #9
CTT_c<-filter(scoreframe,CTT<=23&diag==5) #1620
CTT_d<-filter(scoreframe,CTT<=23&diag<=3) #76 도합 6548
CTT_FPR<-(nrow(CTT_c)/(nrow(CTT_a)+nrow(CTT_c)))
CTT_FNR<-(nrow(CTT_b)/(nrow(CTT_b)+nrow(CTT_d))) 
CTT_sens<-(nrow(CTT_d)/(nrow(CTT_b)+nrow(CTT_d)))
CTT_spec<-(nrow(CTT_a)/(nrow(CTT_a)+nrow(CTT_c)))
CTT_PPP<-(nrow(CTT_d)/(nrow(CTT_d)+nrow(CTT_c)))
CTT_NPP<-(nrow(CTT_a)/(nrow(CTT_a)+nrow(CTT_b)))
CTT_PCO<-((nrow(CTT_a)+nrow(CTT_b))/(nrow(scoreframe)))
CTT_Md<-sqrt((1-CTT_sens)^2+(1-CTT_spec)^2)

