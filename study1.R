dat<-read.csv("C:/git/journeytoamastersdegree/mmse.csv",header=T,sep=",")
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
#취합
response<-data.frame(a401,a402,a403,a404,a405,a406,a407,a408,a409,a410,a411,a412,a413,a414,a415,a416,a417,a418,a419)
#검사 응답이 모두 NA인 행제거
response<- response %>% filter(!is.na(a401) & !is.na(a402) &!is.na(a403)&!is.na(a404)&!is.na(a405)&!is.na(a406)&!is.na(a407)&!is.na(a408)&!is.na(a409)&!is.na(a410)&!is.na(a411)&!is.na(a412)&!is.na(a413)&!is.na(a414)&!is.na(a415)&!is.na(a416)&!is.na(a417)&!is.na(a418)&!is.na(a419))
detach(dat)
#CTT 점수 산출
score.CTT<-vector("double",nrow(response))
for ( i in 1:nrow(response) ){
  score.CTT[[i]]<-sum(response[i,1:19],na.rm=T)}
#PCM 점수 산출
model.pcm <- 'F1 = 1-19' 
results.pcm <- mirt(data=response, model=model.pcm, itemtype="Rasch", SE=TRUE, verbose=FALSE)
score.PCM<-fscores(results.pcm,method = 'EAP')
#GPCM 점수 산출
model.gpcm <- 'F1 = 1-19' 
results.gpcm <- mirt(data=response, model=model.gpcm, itemtype="gpcm", SE=TRUE, verbose=FALSE)
score.GPCM<-fscores(results.gpcm,method = 'EAP')
#CFA 점수 산출
model.cfa<-'F1=~a401+a402+a403+a404+a405+a406+a407+a408+a409+a410+a411+a412+a413+a414+a415+a416+a417+a418+a419'
results.cfa<-cfa(model=model.cfa,data = response)
score.CFA<-lavPredict(results.cfa,method = "regression")
summary(results.cfa, fit.measures = T)
#점수 취합
score.frame<-cbind(score.CTT,score.CFA,score.PCM,score.GPCM); colnames(score.frame)<-c("CTT","CFA","PCM","GPCM")
as.data.frame(score.frame) -> score.frame
#제 5 백분위수에 마커
score.frame %>% mutate(markerCTT = case_when(CTT <= quantile(score.frame$CTT,0.5) ~ '1',
                                               CTT > quantile(score.frame$CTT,0.5) ~ '0'),
                         markerCFA = case_when(CFA <= quantile(score.frame$CFA,0.5) ~ '1',
                                               CFA > quantile(score.frame$CFA,0.5) ~ '0'),
                         markerPCM = case_when(PCM <= quantile(score.frame$PCM,0.5) ~ '1',
                                               PCM > quantile(score.frame$PCM,0.5) ~ '0'),
                         markerGPCM = case_when(GPCM <= quantile(score.frame$GPCM,0.5) ~ '1',
                                                GPCM > quantile(score.frame$GPCM,0.5) ~ '0')) -> sf
#종속변수 - 파이 계수
sf <- mutate_at(sf, vars(starts_with("marker")), as.factor)
phi(confusionMatrix(sf$markerCTT,sf$markerCFA)[[2]],3)
phi(confusionMatrix(sf$markerCTT,sf$markerPCM)[[2]],3)
phi(confusionMatrix(sf$markerCTT,sf$markerGPCM)[[2]],3)
phi(confusionMatrix(sf$markerCFA,sf$markerPCM)[[2]],3)
phi(confusionMatrix(sf$markerCFA,sf$markerGPCM)[[2]],3)
phi(confusionMatrix(sf$markerPCM,sf$markerGPCM)[[2]],3)
round(confusionMatrix(sf$markerCTT,sf$markerCFA)[[3]][[2]],3)
round(confusionMatrix(sf$markerCTT,sf$markerPCM)[[3]][[2]],3)
round(confusionMatrix(sf$markerCTT,sf$markerGPCM)[[3]][[2]],3)
round(confusionMatrix(sf$markerCFA,sf$markerPCM)[[3]][[2]],3)
round(confusionMatrix(sf$markerCFA,sf$markerGPCM)[[3]][[2]],3)
round(confusionMatrix(sf$markerPCM,sf$markerGPCM)[[3]][[2]],3)
#종속변수 - 피어슨 상관계수
round(cor(sf$CTT,sf$CFA),3)
round(cor(sf$CTT,sf$PCM),3)
round(cor(sf$CTT,sf$GPCM),3)
round(cor(sf$CFA,sf$PCM),3)
round(cor(sf$CFA,sf$GPCM),3)
round(cor(sf$PCM,sf$GPCM),3)
#종속변수 - 스피어만 상관계수
round(cor(sf$CTT,sf$CFA,method="spearman"),3)
round(cor(sf$CTT,sf$PCM,method="spearman"),3)
round(cor(sf$CTT,sf$GPCM,method="spearman"),3)
round(cor(sf$CFA,sf$PCM,method="spearman"),3)
round(cor(sf$CFA,sf$GPCM,method="spearman"),3)
round(cor(sf$PCM,sf$GPCM,method="spearman"),3)
# 전체 상관그림
plot(score.frame)
# 개별 상관그림
plot(x =sf$CTT, y = sf$CFA,cex=0.5); fit<-loess.smooth(x=sf$CTT,y=sf$CFA); lines(fit$x,fit$y,lwd = 2)
plot(x =sf$CTT, y = sf$PCM,cex=0.5); fit<-loess.smooth(x=sf$CTT,y=sf$PCM); lines(fit$x,fit$y,lwd = 2)
plot(x =sf$CTT, y = sf$GPCM,cex=0.5); fit<-loess.smooth(x=sf$CTT,y=sf$GPCM); lines(fit$x,fit$y,lwd = 2)
plot(x =sf$CFA, y = sf$PCM,cex=0.5); fit<-loess.smooth(x=sf$CFA,y=sf$PCM); lines(fit$x,fit$y,lwd = 2)
plot(x =sf$CFA, y = sf$GPCM,cex=0.5); fit<-loess.smooth(x=sf$CFA,y=sf$GPCM); lines(fit$x,fit$y,lwd = 2)
plot(x =sf$PCM, y = sf$GPCM,cex=0.5); fit<-loess.smooth(x=sf$PCM,y=sf$GPCM); lines(fit$x,fit$y,lwd = 2)

png(filename="CTT-CFA.png",width=600,height=600,unit="px",bg="transparent")
plot(x =sf$CTT, y = sf$CFA,cex=1.5,axes=F,ann=F); fit<-loess.smooth(x=sf$CTT,y=sf$CFA); lines(fit$x,fit$y,lwd = 2)
dev.off()
png(filename="CTT-PCM.png",width=600,height=600,unit="px",bg="transparent")
plot(x =sf$CTT, y = sf$PCM,cex=1.5,axes=F,ann=F); fit<-loess.smooth(x=sf$CTT,y=sf$PCM); lines(fit$x,fit$y,lwd = 2)
dev.off()
png(filename="CTT-GPCM.png",width=600,height=600,unit="px",bg="transparent")
plot(x =sf$CTT, y = sf$GPCM,cex=1,axes=F,ann=F); fit<-loess.smooth(x=sf$CTT,y=sf$GPCM); lines(fit$x,fit$y,lwd = 2)
dev.off()
png(filename="CFA-PCM.png",width=600,height=600,unit="px",bg="transparent")
plot(x =sf$CFA, y = sf$PCM,cex=1,axes=F,ann=F); fit<-loess.smooth(x=sf$CFA,y=sf$PCM); lines(fit$x,fit$y,lwd = 2)
dev.off()
png(filename="CFA-GPCM.png",width=600,height=600,unit="px",bg="transparent")
plot(x =sf$CFA, y = sf$GPCM,cex=1,axes=F,ann=F); fit<-loess.smooth(x=sf$CFA,y=sf$GPCM); lines(fit$x,fit$y,lwd = 2)
dev.off()
png(filename="PCM-GPCM.png",width=600,height=600,unit="px",bg="transparent")
plot(x =sf$PCM, y = sf$GPCM,cex=1,axes=F,ann=F); fit<-loess.smooth(x=sf$PCM,y=sf$GPCM); lines(fit$x,fit$y,lwd = 2)
dev.off()

