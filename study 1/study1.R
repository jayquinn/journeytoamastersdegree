dat<-read.csv("C:/git/journeytoamastersdegree/mmse.csv",header=T,sep=",")
# C420은 도움받은여부
# str, w는 69~~이고 lt는 61인데 str, w가 다 포함함. 아마 lt는 추가 안된 오리지날 같고... 어쨋든 데이터 동일함.
attach(dat) #코드 전처리 시작
#401 시간지남력 - 연월일 0123 
a401<-ifelse(C401==3,3,
             ifelse(C401==2,2,
                    ifelse(C401==1,1,
                           ifelse(C401==-8,NA,
                                  ifelse(C401==-9,0,
                                         ifelse(C401==5,0,NA))))))
#402 시간지남력 - 요일 01
a402<-ifelse(C402==1,1,
             ifelse(C402==-8,NA,
                    ifelse(C402==-9,0,
                           ifelse(C402==5,0,NA))))
#403 시간지남력- 계절 01
a403<-ifelse(C403==1,1,
             ifelse(C403==-8,NA,
                    ifelse(C403==-9,0,
                           ifelse(C403==5,0,NA))))
#404 장소지남력 - 현위치 01
a404<-ifelse(C404==1,1,
             ifelse(C404==-8,NA,
                    ifelse(C404==-9,0,
                           ifelse(C404==5,0,NA))))
#405 장소지남력 - 시/구/동/번지 01234
a405<-ifelse(C405==4,4,
             ifelse(C405==3,3,
                    ifelse(C405==2,2,
                           ifelse(C405==1,1,
                                  ifelse(C405==-8,NA,
                                         ifelse(C405==-9,0,
                                                ifelse(C405==5,0,NA)))))))
#406 기억력 테스트(3단어) 0123
a406<-ifelse(C406==3,3,
             ifelse(C406==2,2,
                    ifelse(C406==1,1,
                           ifelse(C406==-8,NA,
                                  ifelse(C406==-9,0,
                                         ifelse(C406==5,0,NA))))))
#407 주의 집중 및 계산 (뺄셈 1) 01
a407<-ifelse(C407==1,1,
             ifelse(C407==-8,NA,
                    ifelse(C407==-9,0,
                           ifelse(C407==5,0,NA))))
#408 주의 집중 및 계산 (뺄셈 2) 01
a408<-ifelse(C408==1,1,
             ifelse(C408==-8,NA,
                    ifelse(C408==-9,0,
                           ifelse(C408==5,0,NA))))
#409 주의 집중 및 계산 (뺄셈 3) 01
a409<-ifelse(C409==1,1,
             ifelse(C409==-8,NA,
                    ifelse(C409==-9,0,
                           ifelse(C409==5,0,NA))))
#410 주의 집중 및 계산 (뺄셈 4) 01
a410<-ifelse(C410==1,1,
             ifelse(C410==-8,NA,
                    ifelse(C410==-9,0,
                           ifelse(C410==5,0,NA))))
#411 주의 집중 및 계산 (뺄셈 5) 01
a411<-ifelse(C411==1,1,
             ifelse(C411==-8,NA,
                    ifelse(C411==-9,0,
                           ifelse(C411==5,0,NA))))
#412 기억력 테스트(3단어 재확인) 0123
a412<-ifelse(C412==3,3,
             ifelse(C412==2,2,
                    ifelse(C412==1,1,
                           ifelse(C412==-8,NA,
                                  ifelse(C412==-9,0,
                                         ifelse(C412==5,0,NA))))))
#413 소지품의 용도 (소지품 1) 01
a413<-ifelse(C413==1,1,
             ifelse(C413==-8,NA,
                    ifelse(C413==-9,0,
                           ifelse(C413==5,0,NA))))
#414 소지품의 용도 (소지품 2) 01
a414<-ifelse(C414==1,1,
             ifelse(C414==-8,NA,
                    ifelse(C414==-9,0,
                           ifelse(C414==5,0,NA))))
#415 따라서 말하기 01
a415<-ifelse(C415==1,1,
             ifelse(C415==-8,NA,
                    ifelse(C415==-9,0,
                           ifelse(C415==5,0,NA))))
#416 명령시행_종이 뒤집기, 접기, 건네주기 0123
a416<-ifelse(C416==3,3,
             ifelse(C416==2,2,
                    ifelse(C416==1,1,
                           ifelse(C416==-8,NA,
                                  ifelse(C416==-9,0,
                                         ifelse(C416==5,0,NA))))))
#417 명령시행_읽고 눈감기 01
a417<-ifelse(C417==3,1,
             ifelse(C417==1,1,
                    ifelse(C417==-8,NA,
                           ifelse(C417==-9,0,
                                  ifelse(C417==5,0,NA)))))
#418 명령시행_기분 또는 날씨에 대해 쓰기 01
a418<-ifelse(C418==1,1,
             ifelse(C418==-8,NA,
                    ifelse(C418==-9,0,
                           ifelse(C418==5,0,NA))))
#419 명령시행_제시된 그림 똑같이 그리기01
a419<-ifelse(C419==1,1,
             ifelse(C419==-8,NA,
                    ifelse(C419==-9,0,
                           ifelse(C419==5,0,NA))))
#420 도움받은정도 -9 모르겟음 -8 응답거부 1도움전혀받지않음 2가끔 3대부분
#취합
response<-data.frame(a401,a402,a403,a404,a405,a406,a407,a408,a409,a410,a411,a412,a413,a414,a415,a416,a417,a418,a419)
#인지기능 저하자 마킹
response$diag<-ifelse(dat$diag==5,0,
                      ifelse(dat$diag==3,1,
                             ifelse(dat$diag==1,1,dat$diag))) #5 치매아님 -9 모르겟음 -8 응답거부 1 치매 3 경도인지장애
#나이 입력
response$age = 2018-dat$year
response$gender = dat$gender # 1 = 남자, 5 = 여자
#검사 응답이 모두 NA인 행제거
response<- response %>% filter(!is.na(a401) & !is.na(a402) &!is.na(a403)&!is.na(a404)&!is.na(a405)&!is.na(a406)&!is.na(a407)&!is.na(a408)&!is.na(a409)&!is.na(a410)&!is.na(a411)&!is.na(a412)&!is.na(a413)&!is.na(a414)&!is.na(a415)&!is.na(a416)&!is.na(a417)&!is.na(a418)&!is.na(a419))
detach(dat)
#CTT 점수 산출
score.CTT<-vector("double",nrow(response))
for ( i in 1:nrow(response) ){
  score.CTT[[i]]<-sum(response[i,1:19],na.rm=T)}
#PCM 점수 산출
model.pcm <- 'F1 = 1-19' 
results.pcm <- mirt(data=response[,1:19], model=model.pcm, itemtype="Rasch", SE=TRUE, verbose=FALSE)
score.PCM<-fscores(results.pcm,method = 'EAP')
#plot(results.pcm, type = 'score', theta_lim = c(-12, 3), main = "")
#GPCM 점수 산출
model.gpcm <- 'F1 = 1-19' 
results.gpcm <- mirt(data=response[,1:19], model=model.gpcm, itemtype="gpcm", SE=TRUE, verbose=FALSE)
score.GPCM<-fscores(results.gpcm,method = 'EAP')
#CFA 점수 산출
model.cfa<-'F1=~a401+a402+a403+a404+a405+a406+a407+a408+a409+a410+a411+a412+a413+a414+a415+a416+a417+a418+a419'
results.cfa<-cfa(model=model.cfa,data = response[,1:19])
score.CFA<-lavPredict(results.cfa,method = "regression")
summary(results.cfa, fit.measures = T)
#PCA 점수 산출
#cormat = cor(response[,1:19])
scree(response[,1:19])
VSS.scree(cormat)
results.pca = principal(response[,1:19],nfactors=1,residuals = T,scores=T,cor = "cor",method="regression",rotate = "none")
score.PCA <-results.pca$scores
#점수 취합
score.frame<-cbind(score.CTT,score.PCA,score.PCM,score.GPCM,response$diag,response$age,response$gender); colnames(score.frame)<-c("CTT","PCA","PCM","GPCM","diag","age","gender")
as.data.frame(score.frame) -> score.frame
#23/24기준에 마커
score.frame %>% mutate(markerCTT = case_when(CTT <= quantile(score.frame$CTT,0.2792118519932) ~ '1',
                                             CTT > quantile(score.frame$CTT,0.2792118519932) ~ '0'),
                       markerPCA = case_when(PCA <= quantile(score.frame$PCA,0.2792118519932) ~ '1',
                                             PCA > quantile(score.frame$PCA,0.2792118519932) ~ '0'),
                       markerPCM = case_when(PCM <= quantile(score.frame$PCM,0.2792118519932) ~ '1',
                                             PCM > quantile(score.frame$PCM,0.2792118519932) ~ '0'),
                       markerGPCM = case_when(GPCM <= quantile(score.frame$GPCM,0.2792118519932) ~ '1',
                                              GPCM > quantile(score.frame$GPCM,0.2792118519932) ~ '0')) -> sf
sf %>% mutate(agegroup = case_when(age >= 91 ~ '4',
                                   age >= 81  & age < 91 ~ '3',
                                   age >= 71  & age < 81 ~ '2',
                                   age < 71 ~ '1')) -> sf

sf <- mutate_at(sf, vars(starts_with("marker")), as.factor)
sf$diag <- as.factor(sf$diag)
sf$gender <- as.factor(sf$gender)

#이상만(70세)
sf %>% filter(age < 71) -> sf
sf %>% filter(age >= 71  & age < 81) -> sf
sf %>% filter(age >= 81  & age < 91) -> sf
sf %>% filter(age >= 91) -> sf



#종속변수 - 파이 계수
phi(confusionMatrix(sf$markerCTT,sf$markerPCA)[[2]],3)
phi(confusionMatrix(sf$markerCTT,sf$markerPCM)[[2]],3)
phi(confusionMatrix(sf$markerCTT,sf$markerGPCM)[[2]],3)
phi(confusionMatrix(sf$markerPCA,sf$markerPCM)[[2]],3)
phi(confusionMatrix(sf$markerPCA,sf$markerGPCM)[[2]],3)
phi(confusionMatrix(sf$markerPCM,sf$markerGPCM)[[2]],3)
#카파
round(confusionMatrix(sf$markerCTT,sf$markerPCA)[[3]][[2]],3)
round(confusionMatrix(sf$markerCTT,sf$markerPCM)[[3]][[2]],3) # sf %>% filter(markerCTT == 1 & markerPCM == 0)
round(confusionMatrix(sf$markerCTT,sf$markerGPCM)[[3]][[2]],3)
round(confusionMatrix(sf$markerPCA,sf$markerPCM)[[3]][[2]],3)
round(confusionMatrix(sf$markerPCA,sf$markerGPCM)[[3]][[2]],3)
round(confusionMatrix(sf$markerPCM,sf$markerGPCM)[[3]][[2]],3)
#f1 score
confusionMatrix(sf$markerCTT ,sf$diag, mode = "everything", positive="1")
confusionMatrix(sf$markerPCA ,sf$diag, mode = "everything", positive="1")
confusionMatrix(sf$markerPCM ,sf$diag, mode = "everything", positive="1")
confusionMatrix(sf$markerGPCM ,sf$diag, mode = "everything", positive="1")
#종속변수 - 피어슨 상관계수
round(cor(sf$CTT,sf$PCA),3)
round(cor(sf$CTT,sf$PCM),3)
round(cor(sf$CTT,sf$GPCM),3)
round(cor(sf$PCA,sf$PCM),3)
round(cor(sf$PCA,sf$GPCM),3)
round(cor(sf$PCM,sf$GPCM),3)
#종속변수 - 스피어만 상관계수
round(cor(sf$CTT,sf$PCA,method="spearman"),3)
round(cor(sf$CTT,sf$PCM,method="spearman"),3)
round(cor(sf$CTT,sf$GPCM,method="spearman"),3)
round(cor(sf$PCA,sf$PCM,method="spearman"),3)
round(cor(sf$PCA,sf$GPCM,method="spearman"),3)
round(cor(sf$PCM,sf$GPCM,method="spearman"),3)
# 전체 상관그림
plot(sf[,1:4])
# 개별 상관그림
plot(x =sf$CTT, y = sf$PCA,cex=0.5); fit<-loess.smooth(x=sf$CTT,y=sf$PCA); lines(fit$x,fit$y,lwd = 2)
plot(x =sf$CTT, y = sf$PCM,cex=0.5); fit<-loess.smooth(x=sf$CTT,y=sf$PCM); lines(fit$x,fit$y,lwd = 2)
plot(x =sf$CTT, y = sf$GPCM,cex=0.5); fit<-loess.smooth(x=sf$CTT,y=sf$GPCM); lines(fit$x,fit$y,lwd = 2)
plot(x =sf$PCA, y = sf$PCM,cex=0.5); fit<-loess.smooth(x=sf$PCA,y=sf$PCM); lines(fit$x,fit$y,lwd = 2)
plot(x =sf$PCA, y = sf$GPCM,cex=0.5); fit<-loess.smooth(x=sf$PCA,y=sf$GPCM); lines(fit$x,fit$y,lwd = 2)
plot(x =sf$PCM, y = sf$GPCM,cex=0.5); fit<-loess.smooth(x=sf$PCM,y=sf$GPCM); lines(fit$x,fit$y,lwd = 2)

png(filename="CTT-PCA.png",width=600,height=600,unit="px",bg="transparent")
plot(x =sf$CTT, y = sf$PCA,cex=1.5,axes=F,ann=F); fit<-loess.smooth(x=sf$CTT,y=sf$PCA); lines(fit$x,fit$y,lwd = 2)
dev.off()
png(filename="CTT-PCM.png",width=600,height=600,unit="px",bg="transparent")
plot(x =sf$CTT, y = sf$PCM,cex=1.5,axes=F,ann=F); fit<-loess.smooth(x=sf$CTT,y=sf$PCM); lines(fit$x,fit$y,lwd = 2)
dev.off()
png(filename="CTT-GPCM.png",width=600,height=600,unit="px",bg="transparent")
plot(x =sf$CTT, y = sf$GPCM,cex=1,axes=F,ann=F); fit<-loess.smooth(x=sf$CTT,y=sf$GPCM); lines(fit$x,fit$y,lwd = 2)
dev.off()
png(filename="PCA-PCM.png",width=600,height=600,unit="px",bg="transparent")
plot(x =sf$PCA, y = sf$PCM,cex=1,axes=F,ann=F); fit<-loess.smooth(x=sf$PCA,y=sf$PCM); lines(fit$x,fit$y,lwd = 2)
dev.off()
png(filename="PCA-GPCM.png",width=600,height=600,unit="px",bg="transparent")
plot(x =sf$PCA, y = sf$GPCM,cex=1,axes=F,ann=F); fit<-loess.smooth(x=sf$PCA,y=sf$GPCM); lines(fit$x,fit$y,lwd = 2)
dev.off()
png(filename="PCM-GPCM.png",width=600,height=600,unit="px",bg="transparent")
plot(x =sf$PCM, y = sf$GPCM,cex=1,axes=F,ann=F); fit<-loess.smooth(x=sf$PCM,y=sf$GPCM); lines(fit$x,fit$y,lwd = 2)
dev.off()

# 종속변수 - ROC curve
plot.roc(diag ~ CTT,print.auc=T,print.thres="best",print.thres.best.method="youden", data = sf) # subset 확인해서 잘 써먹기 ?roc에 예제 있음
plot.roc(diag ~ PCA, data = sf,print.auc=T,print.thres="best",print.thres.best.method="youden")
plot.roc(diag ~ PCM, data = sf,print.auc=T,print.thres="best",print.thres.best.method="youden")
plot.roc(diag ~ GPCM, data = sf,print.auc=T,print.thres="best",print.thres.best.method="youden")
# 종속변수 -PR curve
# Create a ROC curve:
data(aSAH)
roc.s100b <- roc(aSAH$outcome, aSAH$s100b, percent = TRUE)
# Get the coordinates of S100B threshold 0.55
coords(roc.s100b, 0.55, transpose = FALSE)
# Get the coordinates at 50% sensitivity
coords(roc=roc.s100b, x=50, input="sensitivity", transpose = FALSE)
# Can be abbreviated:
coords(roc.s100b, 50, "se", transpose = FALSE)
# Works with smoothed ROC curves
coords(smooth(roc.s100b), 90, "specificity", transpose = FALSE)
# Get the sensitivities for all thresholds
cc <- coords(roc.s100b, "all", ret="sensitivity", transpose = FALSE)
print(cc$sensitivity)
# Get the best threshold
coords(roc.s100b, "best", ret="threshold", transpose = FALSE)


# Get the best threshold according to different methods
roc.ndka <- roc(aSAH$outcome, aSAH$ndka, percent=TRUE)
coords(roc.ndka, "best", ret="threshold", transpose = FALSE,
       best.method="youden") # default
coords(roc.ndka, "best", ret="threshold", transpose = FALSE,
       best.method="closest.topleft")
# and with different weights
coords(roc.ndka, "best", ret="threshold", transpose = FALSE,
       best.method="youden", best.weights=c(50, 0.2))
coords(roc.ndka, "best", ret="threshold", transpose = FALSE,
       best.method="closest.topleft", best.weights=c(5, 0.2))
# This is available with the plot.roc function too:
plot(roc.ndka, print.thres="best", print.thres.best.method="youden",
     print.thres.best.weights=c(50, 0.2))
# Return more values:
coords(roc.s100b, "best", ret=c("threshold", "specificity", "sensitivity", "accuracy",
                                "precision", "recall"), transpose = FALSE)
# Return all values
coords(roc.s100b, "best", ret = "all", transpose = FALSE)
# You can use coords to plot for instance a sensitivity + specificity vs. cut-off diagram
plot(specificity + sensitivity ~ threshold,
     coords(roc.ndka, "all", transpose = FALSE),
     type = "l", log="x",
     subset = is.finite(threshold))
# Plot the Precision-Recall curve
plot(precision ~ recall,
     coords(roc.ndka, "all", ret = c("recall", "precision"), transpose = FALSE),
     type="l", ylim = c(0, 100))











# Get the best threshold according to different methods
roc.ndka <- roc(sf$diag, sf$GPCM, percent=TRUE)
# Plot the Precision-Recall curve
plot(precision ~ recall,
     coords(roc.ndka, "all", ret = c("recall", "precision"), transpose = FALSE),
     type="l", ylim = c(0, 100))

# This is available with the plot.roc function too:
plot(roc.ndka, print.thres="best", print.thres.best.method="youden",
     print.thres.best.weights=c(50, 0.2))




confusionMatrix(sf$markerCTT,as.factor(sf$diag),positive = "1")
#recall = 0.8941176
confusionMatrix(sf$markerPCA,as.factor(sf$diag),positive = "1")
#recall = 0.8588235
confusionMatrix(as.factor(sf$markerPCM),as.factor(sf$diag),positive = "1")
#recall = 0.8823529
confusionMatrix(sf$markerGPCM,as.factor(sf$diag),positive = "1")
#recall = 0.8588235


pr = pr.curve(scores.class0=sf[sf$diag=="1",]$CTT,scores.class1 = sf[sf$diag=="0",]$CTT, curve=T)
plot(pr)
y <- as.data.frame(pr$curve)
ggplot(y, aes(y$V1, y$V2))+geom_path()+ylim(0,1)
