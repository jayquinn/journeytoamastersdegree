# 히스토그램 + density curve + qq plot (가운데 정규분포로 넣은...) + 뒤집기도 들어있슴
# C#T#T binwdith = 1
# P#C#A binwidth = 0.212
# P#C#M binwidth = 0.7
# G#P#C#M binwidth = 0.32
A = ggplot(data = sf, aes(x = PCA)) + 
  theme_bw() + 
  geom_histogram(aes(y = ..count..), colour = 1, fill = "white", binwidth = 0.212) + 
  stat_function(fun = function(x) dnorm(x, mean = median(range(sf$PCA)), sd = sd(sf$PCA)) * nrow(sf) * 0.212) +
  scale_y_continuous("빈도 수") + 
  scale_x_continuous("가중 합산점수") #n 뒤에 ,color = "black", size =  //  걍 이거 해

B = ggplot(data = sf, aes(sample = PCA)) + stat_qq() + coord_flip()  + theme_bw() + stat_qq_line() +
  scale_y_continuous("sample quantiles") + 
  scale_x_continuous("theoretical quatiles")

grid.arrange(A, B, nrow = 2 ,widths = c(2.5,1))



#boxplot + qq
A = ggplot() + 
  geom_boxplot(data = filter(sf, sf$gender == 1), mapping = aes(x = agegroup, y = PCA, group = agegroup))+
  geom_point(data = meandotmale, mapping = aes(x = agegroup, y = Avg)) + theme_bw()+ 
  geom_line(data = meandotmale, mapping = aes(x = agegroup, y = Avg, group = 1))+
  scale_y_continuous(name = "가중 특질점수") +
  scale_x_discrete(labels = abbreviate, name = "연령")  + ggtitle("남성")

B = ggplot(data = filter(sf, sf$gender == 1), aes(sample = PCA)) + stat_qq() + coord_flip()  + theme_bw() + stat_qq_line() +
  scale_y_continuous("sample quantiles") + 
  scale_x_continuous("theoretical quatiles")


C = ggplot() + 
  geom_boxplot(data = filter(sf, sf$gender == 5), mapping = aes(x = agegroup, y = PCA, group = agegroup))+
  geom_point(data = meandotfemale, mapping = aes(x = agegroup, y = Avg)) + theme_bw()+ 
  geom_line(data = meandotfemale, mapping = aes(x = agegroup, y = Avg, group = 1))+
  scale_y_continuous(name = "가중 특절점수") +
  scale_x_discrete(labels = abbreviate, name = "연령")  + ggtitle("여성")

D = ggplot(data = filter(sf, sf$gender == 5), aes(sample = PCA)) + stat_qq() + coord_flip()  + theme_bw() + stat_qq_line() +
  scale_y_continuous("sample quantiles") + 
  scale_x_continuous("theoretical quatiles")

grid.arrange(arrangeGrob(A,B,widths=c(2.5,1),ncol=2) , arrangeGrob(C,D,widths=c(2.5,1),ncol=2), nrow = 2)

#연령별 qq
A = ggplot(data = filter(sf, sf$gender == 1, sf$agegroup == 1), aes(sample = PCA)) + stat_qq() + coord_flip()  + theme_bw() + stat_qq_line() +
  scale_y_continuous("sample quantiles male 1") + 
  scale_x_continuous("theoretical quatiles")
B = ggplot(data = filter(sf, sf$gender == 1, sf$agegroup == 2), aes(sample = PCA)) + stat_qq() + coord_flip()  + theme_bw() + stat_qq_line() +
  scale_y_continuous("sample quantiles male 2") + 
  scale_x_continuous("theoretical quatiles")
C = ggplot(data = filter(sf, sf$gender == 1, sf$agegroup == 3), aes(sample = PCA)) + stat_qq() + coord_flip()  + theme_bw() + stat_qq_line() +
  scale_y_continuous("sample quantiles male 3") + 
  scale_x_continuous("theoretical quatiles")
D = ggplot(data = filter(sf, sf$gender == 1, sf$agegroup == 4), aes(sample = PCA)) + stat_qq() + coord_flip()  + theme_bw() + stat_qq_line() +
  scale_y_continuous("sample quantiles male 4") + 
  scale_x_continuous("theoretical quatiles")

grid.arrange(A,B,C,D,E,ncol=5)
#상단 박스 하단 QQ
sf %>% filter(gender == 1)%>% group_by(agegroup) %>% summarise(Avg = mean(GPCM)) -> meandotmale
sf %>% filter(gender == 5)%>% group_by(agegroup) %>% summarise(Avg = mean(GPCM)) -> meandotfemale
A = ggplot() + 
  geom_boxplot(data = filter(sf, sf$gender == 1), mapping = aes(x = agegroup, y = GPCM, group = agegroup))+
  geom_point(data = meandotmale, mapping = aes(x = agegroup, y = Avg)) + theme_bw()+ 
  geom_line(data = meandotmale, mapping = aes(x = agegroup, y = Avg, group = 1))+
  scale_y_continuous(name = "비가중 특질 점수") +
  scale_x_discrete(labels = abbreviate, name = "연령")  + ggtitle("남성")
B = ggplot(data = filter(sf, sf$gender == 1, sf$agegroup == 1), aes(sample = GPCM)) + stat_qq() + coord_flip()  + theme_bw() + stat_qq_line() +
  scale_y_continuous("sample quantiles") + 
  scale_x_continuous("theoretical quatiles")
C = ggplot(data = filter(sf, sf$gender == 1, sf$agegroup == 2), aes(sample = GPCM)) + stat_qq() + coord_flip()  + theme_bw() + stat_qq_line() +
  scale_y_continuous("sample quantiles") + 
  scale_x_continuous("theoretical quatiles")
D = ggplot(data = filter(sf, sf$gender == 1, sf$agegroup == 3), aes(sample = GPCM)) + stat_qq() + coord_flip()  + theme_bw() + stat_qq_line() +
  scale_y_continuous("sample quantiles") + 
  scale_x_continuous("theoretical quatiles")
E = ggplot(data = filter(sf, sf$gender == 1, sf$agegroup == 4), aes(sample = GPCM)) + stat_qq() + coord_flip()  + theme_bw() + stat_qq_line() +
  scale_y_continuous("sample quantiles") + 
  scale_x_continuous("theoretical quatiles")
FF = ggplot() + 
  geom_boxplot(data = filter(sf, sf$gender == 5), mapping = aes(x = agegroup, y = GPCM, group = agegroup))+
  geom_point(data = meandotmale, mapping = aes(x = agegroup, y = Avg)) + theme_bw()+ 
  geom_line(data = meandotmale, mapping = aes(x = agegroup, y = Avg, group = 1))+
  scale_y_continuous(name = "비가중 특질점수") +
  scale_x_discrete(labels = abbreviate, name = "연령")  + ggtitle("여성")
G = ggplot(data = filter(sf, sf$gender == 5, sf$agegroup == 1), aes(sample = GPCM)) + stat_qq() + coord_flip()  + theme_bw() + stat_qq_line() +
  scale_y_continuous("sample quantiles") + 
  scale_x_continuous("theoretical quatiles")
H = ggplot(data = filter(sf, sf$gender == 5, sf$agegroup == 2), aes(sample = GPCM)) + stat_qq() + coord_flip()  + theme_bw() + stat_qq_line() +
  scale_y_continuous("sample quantiles") + 
  scale_x_continuous("theoretical quatiles")
I = ggplot(data = filter(sf, sf$gender == 5, sf$agegroup == 3), aes(sample = GPCM)) + stat_qq() + coord_flip()  + theme_bw() + stat_qq_line() +
  scale_y_continuous("sample quantiles") + 
  scale_x_continuous("theoretical quatiles")
J = ggplot(data = filter(sf, sf$gender == 5, sf$agegroup == 4), aes(sample = GPCM)) + stat_qq() + coord_flip()  + theme_bw() + stat_qq_line() +
  scale_y_continuous("sample quantiles") + 
  scale_x_continuous("theoretical quatiles")
grid.arrange(A, arrangeGrob(B,C,D,E,ncol=4),FF, arrangeGrob(G,H,I,J,ncol=4), nrow = 4,heights=c(1,0.5,1,0.5))

#PCM
for(i in 1:19) {
  assign(paste0("plot_",i),plot(results.gpcm, type = 'trace', which.items = c(i))) 
}
plot_20 = plot(results.pcm, type = 'score', theta_lim = c(-4,4), lwd=2)
grid.arrange(plot_1,plot_2,plot_3,plot_4,plot_5,plot_6,plot_7,plot_8,plot_9,plot_10,plot_11,plot_12,
             plot_13,plot_14,plot_15,plot_16,plot_17,plot_18,plot_19,plot_20,ncol=4)

#수동 주성분 점수 내기
cormat = cor(response[1:19])
e = eigen(cormat)
sponse = scale(response[1:19])
dap = sponse %*% e$vectors
colnames(dap) <- paste0('pc', 1:19)
head(dap)

#PCA 가중합 반응값(비표준화)
weightedsum = data.frame()
for (i in 1:nrow(response)) {
  newone = response[i,1:19] *results.pca$Structure
  weightedsum = rbind(weightedsum,newone)
}
weightedsum$tot = apply(weightedsum, 1, sum) # 총점


#각각그림
plot(x =sf$CTT,xlab = "비가중 합산점수", y = sf$PCA,ylab = "가중 합산점수",cex=0.5); fit<-loess.smooth(x=sf$CTT,y=sf$PCA); lines(fit$x,fit$y,lwd = 1);abline(v = quantile(sf$CTT,cutoff)); abline(h = quantile(sf$PCA,cutoff))
plot(x =sf$CTT,xlab = "비가중 합산점수", y = sf$PCM,ylab = "비가중 특질점수",cex=0.5); fit<-loess.smooth(x=sf$CTT,y=sf$PCM); lines(fit$x,fit$y,lwd = 1);abline(v = quantile(sf$CTT,cutoff)); abline(h = quantile(sf$PCM,cutoff))
plot(x =sf$CTT,xlab = "비가중 합산점수", y = sf$GPCM,ylab = "가중 특질점수",cex=0.5); fit<-loess.smooth(x=sf$CTT,y=sf$GPCM); lines(fit$x,fit$y,lwd = 1);abline(v = quantile(sf$CTT,cutoff)); abline(h = quantile(sf$GPCM,cutoff))
plot(x =sf$PCA,xlab = "가중 합산점수", y = sf$PCM,ylab = "비가중 특질점수",cex=0.5); fit<-loess.smooth(x=sf$PCA,y=sf$PCM); lines(fit$x,fit$y,lwd = 1);abline(v = quantile(sf$PCA,cutoff)); abline(h = quantile(sf$PCA,cutoff))
plot(x =sf$PCA,xlab = "가중 합산점수", y = sf$GPCM,ylab = "가중 특질점수",cex=0.5); fit<-loess.smooth(x=sf$PCA,y=sf$GPCM); lines(fit$x,fit$y,lwd = 1);abline(v = quantile(sf$PCA,cutoff)); abline(h = quantile(sf$GPCM,cutoff))
plot(x =sf$PCM,xlab = "비가중 특질점수",y = sf$GPCM,ylab = "가중 특질점수",cex=0.5); fit<-loess.smooth(x=sf$PCM,y=sf$GPCM); lines(fit$x,fit$y,lwd = 1);abline(v = quantile(sf$PCM,cutoff)); abline(h = quantile(sf$GPCM,cutoff))
#jittered
jit = sf[,1:4]
jit$SUM <- jitter(jit$SUM, factor = 1)
jit$PCM <- jitter(jit$PCM, factor = 1)
plot(jit)
#변별도 산점도 (CFA-GPCM)
ttt = summary(results.cfa)

coef.gpcm <- coef(results.gpcm, IRTpars=TRUE, simplify=TRUE)
lb = 1:19
scattdisc = cbind(coef.gpcm$items[,1],ttt$PE$est[1:19],lb); colnames(scattdisc) = c("GPCM","CFA","item")
scattdisc <- as.data.frame(scattdisc)
plot(scattdisc[,1:2])
text(scattdisc, labels = scattdisc$item,cex= 1, pos=4)
cor(scattdisc[,1:2])

# 문항특성곡선
plot(results.pcm, type = 'trace', which.items = c(1:19),par.settings=bwtheme)
plot(results.pcm, type = 'score', theta_lim = c(-4,4), lwd=2,par.settings=bwtheme)
plot(results.gpcm, type = 'trace', which.items = c(1:19),par.settings=bwtheme)
plot(results.gpcm, type = 'score', theta_lim = c(-4,4), lwd=2,par.settings=bwtheme)

#PCA 공분산 스크리도표
scponse <- scale(response[,1:19], center=TRUE, scale=FALSE)
ev <- eigen(cov(scponse))
PC = c(1:19)
Eigen_Values <-ev$values
Scree <- data.frame(PC, Eigen_Values)
plot(Scree, main = "스크리 도표",ylim=c(0,8), xlab = "성분 수", ylab = "성분의 고윳값",pch = 16); lines(Scree);abline(h=1)



#이상만(70세)
sf %>% filter(age < 71) -> sf
sf %>% filter(age >= 71  & age < 81) -> sf
sf %>% filter(age >= 81  & age < 91) -> sf
sf %>% filter(age >= 91) -> sf



#종속변수 - 파이 계수
phi(confusionMatrix(sf$markerSUM,sf$markerPCA)[[2]],3)
phi(confusionMatrix(sf$markerSUM,sf$markerPCM)[[2]],3)
phi(confusionMatrix(sf$markerSUM,sf$markerGPCM)[[2]],3)
phi(confusionMatrix(sf$markerPCA,sf$markerPCM)[[2]],3)
phi(confusionMatrix(sf$markerPCA,sf$markerGPCM)[[2]],3)
phi(confusionMatrix(sf$markerPCM,sf$markerGPCM)[[2]],3)
#카파
round(confusionMatrix(sf$markerSUM,sf$markerPCA)[[3]][[2]],3)
round(confusionMatrix(sf$markerSUM,sf$markerPCM)[[3]][[2]],3) # sf %>% filter(markerSUM == 1 & markerPCM == 0)
round(confusionMatrix(sf$markerSUM,sf$markerGPCM)[[3]][[2]],3)
round(confusionMatrix(sf$markerPCA,sf$markerPCM)[[3]][[2]],3)
round(confusionMatrix(sf$markerPCA,sf$markerGPCM)[[3]][[2]],3)
round(confusionMatrix(sf$markerPCM,sf$markerGPCM)[[3]][[2]],3)
#f1 score
confusionMatrix(sf$markerSUM ,sf$diag, mode = "everything", positive="1")
confusionMatrix(sf$markerPCA ,sf$diag, mode = "everything", positive="1")
confusionMatrix(sf$markerPCM ,sf$diag, mode = "everything", positive="1")
confusionMatrix(sf$markerGPCM ,sf$diag, mode = "everything", positive="1")
#종속변수 - 피어슨 상관계수
round(cor(sf$SUM,sf$PCA),3)
round(cor(sf$SUM,sf$PCM),3)
round(cor(sf$SUM,sf$GPCM),3)
round(cor(sf$PCA,sf$PCM),3)
round(cor(sf$PCA,sf$GPCM),3)
round(cor(sf$PCM,sf$GPCM),3)
#종속변수 - 스피어만 상관계수
round(cor(sf$SUM,sf$PCA,method="spearman"),3)
round(cor(sf$SUM,sf$PCM,method="spearman"),3)
round(cor(sf$SUM,sf$GPCM,method="spearman"),3)
round(cor(sf$PCA,sf$PCM,method="spearman"),3)
round(cor(sf$PCA,sf$GPCM,method="spearman"),3)
round(cor(sf$PCM,sf$GPCM,method="spearman"),3)

png(filename="SUM-PCA.png",width=600,height=600,unit="px",bg="transparent")
plot(x =sf$SUM, y = sf$PCA,cex=1.5,axes=F,ann=F); fit<-loess.smooth(x=sf$SUM,y=sf$PCA); lines(fit$x,fit$y,lwd = 2)
dev.off()
png(filename="SUM-PCM.png",width=600,height=600,unit="px",bg="transparent")
plot(x =sf$SUM, y = sf$PCM,cex=1.5,axes=F,ann=F); fit<-loess.smooth(x=sf$SUM,y=sf$PCM); lines(fit$x,fit$y,lwd = 2)
dev.off()
png(filename="SUM-GPCM.png",width=600,height=600,unit="px",bg="transparent")
plot(x =sf$SUM, y = sf$GPCM,cex=1,axes=F,ann=F); fit<-loess.smooth(x=sf$SUM,y=sf$GPCM); lines(fit$x,fit$y,lwd = 2)
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

