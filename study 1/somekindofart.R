sf %>% write.csv("C:/git/journeytoamastersdegree/ssff.csv")
#png(filename="SVLT-REC.png",width=1200,height=600,unit="px",bg="transparent")
#grid.arrange(a,b,c,d,e,f, nrow=3, ncol=2)
#dev.off()
#
# 히스토그램 + density curve + qq plot
# S#U#M binwdith = 1
A = ggplot(data = sf, aes(x = SUM)) + 
  theme_bw() + theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) + 
  geom_histogram(aes(y = ..count..), colour = 1, fill = "white", binwidth = 1) + 
  stat_function(fun = function(x) dnorm(x, mean = mean(sf$SUM), sd = sd(sf$SUM)) * nrow(sf) * 1) +
  scale_x_continuous("비가중 합산점수") + #n 뒤에 ,color = "black", size =  //  걍 이거 해 ,limits = c(-8.5,4)
  scale_y_continuous("빈도 수",sec.axis=sec_axis(
    trans = ~./(max(table(sf$SUM)) / max(density(sf$SUM)$y)),name = "밀도"))

B = ggplot(data = sf, aes(sample = SUM)) + stat_qq()  + theme_bw() +  stat_qq_line() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  scale_y_continuous("sample quantiles") + 
  scale_x_continuous("theoretical quatiles")

plot_grid(A,B,ncol=2,rel_widths = c(2.5,1),rel_heights = 0.5)
# P#C#A binwidth = 0.212
A = ggplot(data = sf, aes(x = PCA)) + 
  theme_bw() + theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) + 
  geom_histogram(aes(y = ..count..), colour = 1, fill = "white", binwidth = 0.212) + 
  stat_function(fun = function(x) dnorm(x, mean = mean(sf$PCA), sd = sd(sf$PCA)) * nrow(sf) * 0.212) +
  scale_x_continuous("가중 합산점수") + #n 뒤에 ,color = "black", size =  //  걍 이거 해 ,limits = c(-8.5,4)
  scale_y_continuous("빈도 수",sec.axis=sec_axis(
    trans = ~./(max(table(sf$PCA)) / max(density(sf$PCA)$y)),name = "밀도"))

B = ggplot(data = sf, aes(sample = PCA)) + stat_qq()  + theme_bw() +  stat_qq_line() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  scale_y_continuous("sample quantiles") + 
  scale_x_continuous("theoretical quatiles")

plot_grid(A,B,ncol=2,rel_widths = c(2.5,1),rel_heights = 0.5)
# P#C#M binwidth = 0.7
A = ggplot(data = sf, aes(x = PCM)) + 
  theme_bw() + theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) + 
  geom_histogram(aes(y = ..count..), colour = 1, fill = "white", binwidth = 0.7) + 
  stat_function(fun = function(x) dnorm(x, mean = mean(sf$PCM), sd = sd(sf$PCM)) * nrow(sf) * 0.7) +
  scale_x_continuous("비가중 특질점수") + #n 뒤에 ,color = "black", size =  //  걍 이거 해 ,limits = c(-8.5,4)
  scale_y_continuous("빈도 수",sec.axis=sec_axis(
    trans = ~./(max(table(sf$PCM)) / max(density(sf$PCM)$y)),name = "밀도"))

B = ggplot(data = sf, aes(sample = PCM)) + stat_qq()  + theme_bw() +  stat_qq_line() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  scale_y_continuous("sample quantiles") + 
  scale_x_continuous("theoretical quatiles")

plot_grid(A,B,ncol=2,rel_widths = c(2.5,1),rel_heights = 0.5)
# G#P#C#M binwidth = 0.32
A = ggplot(data = sf, aes(x = GPCM)) + 
  theme_bw() + theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) + 
  geom_histogram(aes(y = ..count..), colour = 1, fill = "white", binwidth = 0.32) + 
  stat_function(fun = function(x) dnorm(x, mean = mean(sf$GPCM), sd = sd(sf$GPCM)) * nrow(sf) * 0.32) +
  scale_x_continuous("가중 특질점수") + #n 뒤에 ,color = "black", size =  //  걍 이거 해 ,limits = c(-8.5,4)
  scale_y_continuous("빈도 수",sec.axis=sec_axis(
  trans = ~./(max(table(sf$GPCM)) / max(density(sf$GPCM)$y)),name = "밀도"))

B = ggplot(data = sf, aes(sample = GPCM)) + stat_qq()  + theme_bw() +  stat_qq_line() +
  theme(plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()) + 
  scale_y_continuous("sample quantiles") + 
  scale_x_continuous("theoretical quatiles")

plot_grid(A,B,ncol=2,rel_widths = c(2.5,1),rel_heights = 0.5)

#boxplot + qq SUM
A = ggplot(sf,aes(x = agegroup,y = SUM, fill = gender)) + 
  geom_boxplot() +
  scale_fill_manual(breaks = c("1","5"),
                    values = c("white","grey70"),labels = c("남성", "여성")) +
  stat_summary(
    fun = mean,
    geom = 'line',
    aes(group = gender),
    position = position_dodge(width = 0.75)) + 
  stat_summary(fun=mean, geom="point", aes(group=gender), position=position_dodge(.75), 
               color="black", size=2) + theme_bw() + 
  scale_y_continuous(name = "비가중 합산점수") +
  scale_x_discrete(labels = c("~69", "70~79","80~89","90~"), name = "연령") +
  theme(legend.position="right",plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + guides(fill=guide_legend(title="성별")) 


B = ggplot(data = filter(sf, sf$gender == 1, agegroup == 1), aes(sample = SUM)) + stat_qq()  + theme_bw() + stat_qq_line() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_y_continuous("sample quantiles") + 
  scale_x_continuous("theoretical quatiles")


C = ggplot(data = filter(sf, sf$gender == 1, agegroup == 2), aes(sample = SUM)) + stat_qq()  + theme_bw() + stat_qq_line() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_y_continuous("sample quantiles") + 
  scale_x_continuous("theoretical quatiles")

D = ggplot(data = filter(sf, sf$gender == 1, agegroup == 3), aes(sample = SUM)) + stat_qq()  + theme_bw() + stat_qq_line() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_y_continuous("sample quantiles") + 
  scale_x_continuous("theoretical quatiles")

E = ggplot(data = filter(sf, sf$gender == 1, agegroup == 4), aes(sample = SUM)) + stat_qq()  + theme_bw() + stat_qq_line() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_y_continuous("sample quantiles") + 
  scale_x_continuous("theoretical quatiles")

grid.arrange(A, arrangeGrob(B,C,D,E,ncol=4), nrow = 2, heights = c(1,0.5))
posterA = grid.arrange(A, arrangeGrob(B,C,D,E,ncol=4), nrow = 2, heights = c(1,0.5))
#boxplot + qq PCA
A = ggplot(sf,aes(x = agegroup,y = PCA, fill = gender)) + 
  geom_boxplot() +
  scale_fill_manual(breaks = c("1","5"),
                    values = c("white","grey70"),labels = c("남성", "여성")) +
  stat_summary(
    fun = mean,
    geom = 'line',
    aes(group = gender),
    position = position_dodge(width = 0.75)) + 
  stat_summary(fun=mean, geom="point", aes(group=gender), position=position_dodge(.75), 
               color="black", size=2) + theme_bw() + 
  scale_y_continuous(name = "가중 합산점수") +
  scale_x_discrete(labels = c("~69", "70~79","80~89","90~"), name = "연령") +
  theme(legend.position="right",plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + guides(fill=guide_legend(title="성별")) 


B = ggplot(data = filter(sf, sf$gender == 1, agegroup == 1), aes(sample = PCA)) + stat_qq()  + theme_bw() + stat_qq_line() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_y_continuous("sample quantiles") + 
  scale_x_continuous("theoretical quatiles")


C = ggplot(data = filter(sf, sf$gender == 1, agegroup == 2), aes(sample = PCA)) + stat_qq()  + theme_bw() + stat_qq_line() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_y_continuous("sample quantiles") + 
  scale_x_continuous("theoretical quatiles")

D = ggplot(data = filter(sf, sf$gender == 1, agegroup == 3), aes(sample = PCA)) + stat_qq()  + theme_bw() + stat_qq_line() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_y_continuous("sample quantiles") + 
  scale_x_continuous("theoretical quatiles")

E = ggplot(data = filter(sf, sf$gender == 1, agegroup == 4), aes(sample = PCA)) + stat_qq()  + theme_bw() + stat_qq_line() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_y_continuous("sample quantiles") + 
  scale_x_continuous("theoretical quatiles")

grid.arrange(A, arrangeGrob(B,C,D,E,ncol=4), nrow = 2, heights = c(1,0.5))
posterB = grid.arrange(A, arrangeGrob(B,C,D,E,ncol=4), nrow = 2, heights = c(1,0.5))
#boxplot + qq PCM
A = ggplot(sf,aes(x = agegroup,y = PCM, fill = gender)) + 
  geom_boxplot() +
  scale_fill_manual(breaks = c("1","5"),
                    values = c("white","grey70"),labels = c("남성", "여성")) +
  stat_summary(
    fun = mean,
    geom = 'line',
    aes(group = gender),
    position = position_dodge(width = 0.75)) + 
  stat_summary(fun=mean, geom="point", aes(group=gender), position=position_dodge(.75), 
               color="black", size=2) + theme_bw() + 
  scale_y_continuous(name = "비가중 특질점수") +
  scale_x_discrete(labels = c("~69", "70~79","80~89","90~"), name = "연령") +
  theme(legend.position="right",plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + guides(fill=guide_legend(title="성별")) 


B = ggplot(data = filter(sf, sf$gender == 1, agegroup == 1), aes(sample = PCM)) + stat_qq()  + theme_bw() + stat_qq_line() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_y_continuous("sample quantiles") + 
  scale_x_continuous("theoretical quatiles")


C = ggplot(data = filter(sf, sf$gender == 1, agegroup == 2), aes(sample = PCM)) + stat_qq()  + theme_bw() + stat_qq_line() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_y_continuous("sample quantiles") + 
  scale_x_continuous("theoretical quatiles")

D = ggplot(data = filter(sf, sf$gender == 1, agegroup == 3), aes(sample = PCM)) + stat_qq()  + theme_bw() + stat_qq_line() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_y_continuous("sample quantiles") + 
  scale_x_continuous("theoretical quatiles")

E = ggplot(data = filter(sf, sf$gender == 1, agegroup == 4), aes(sample = PCM)) + stat_qq()  + theme_bw() + stat_qq_line() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_y_continuous("sample quantiles") + 
  scale_x_continuous("theoretical quatiles")

grid.arrange(A, arrangeGrob(B,C,D,E,ncol=4), nrow = 2, heights = c(1,0.5))
posterC = grid.arrange(A, arrangeGrob(B,C,D,E,ncol=4), nrow = 2, heights = c(1,0.5))

#boxplot + qq GPCM
A = ggplot(sf,aes(x = agegroup,y = GPCM, fill = gender)) + 
  geom_boxplot() +
  scale_fill_manual(breaks = c("1","5"),
                    values = c("white","grey70"),labels = c("남성", "여성")) +
  stat_summary(
    fun = mean,
    geom = 'line',
    aes(group = gender),
    position = position_dodge(width = 0.75)) + 
  stat_summary(fun=mean, geom="point", aes(group=gender), position=position_dodge(.75), 
               color="black", size=2) + theme_bw() + 
  scale_y_continuous(name = "가중 특질점수") +
  scale_x_discrete(labels = c("~69", "70~79","80~89","90~"), name = "연령") +
  theme(legend.position="right",plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + guides(fill=guide_legend(title="성별")) 


B = ggplot(data = filter(sf, sf$gender == 1, agegroup == 1), aes(sample = GPCM)) + stat_qq()  + theme_bw() + stat_qq_line() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_y_continuous("sample quantiles") + 
  scale_x_continuous("theoretical quatiles")


C = ggplot(data = filter(sf, sf$gender == 1, agegroup == 2), aes(sample = GPCM)) + stat_qq()  + theme_bw() + stat_qq_line() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_y_continuous("sample quantiles") + 
  scale_x_continuous("theoretical quatiles")

D = ggplot(data = filter(sf, sf$gender == 1, agegroup == 3), aes(sample = GPCM)) + stat_qq()  + theme_bw() + stat_qq_line() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_y_continuous("sample quantiles") + 
  scale_x_continuous("theoretical quatiles")

E = ggplot(data = filter(sf, sf$gender == 1, agegroup == 4), aes(sample = GPCM)) + stat_qq()  + theme_bw() + stat_qq_line() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_y_continuous("sample quantiles") + 
  scale_x_continuous("theoretical quatiles")

grid.arrange(A, arrangeGrob(B,C,D,E,ncol=4), nrow = 2, heights = c(1,0.5))
posterD = grid.arrange(A, arrangeGrob(B,C,D,E,ncol=4), nrow = 2, heights = c(1,0.5))




#alpha
alpha = alpha(response[,1:19])
alpha$item.stats %>% round(2) %>% write.csv("C:/git/journeytoamastersdegree/alpha.csv")

# 평균 표준편차 왜도 첨도
describe(response) %>% round(2)%>% write.csv("C:/git/journeytoamastersdegree/describe.csv")
#PCA 상관 스크리도표
ev <- eigen(cor(response[,1:19]))
PC = c(1:19)
Eigen_Values <-ev$values
Scree <- data.frame(PC, Eigen_Values)
plot(Scree, main = "스크리 도표",ylim=c(0,8), xlab = "성분 수", ylab = "성분의 고윳값",pch = 16); lines(Scree);abline(h=1)
#PCA 로딩
results.pca$Structure %>% round(3)%>% write.csv("C:/git/journeytoamastersdegree/PCAloading.csv")
#PCA 가중치
results.pca$weights%>% write.csv("C:/git/journeytoamastersdegree/PCAweight.csv")
#PCA 가중합 반응값(표준화)
weightedsum = data.frame()
sponse = apply(response[1:19],2,scale)
sponse = scale(response[1:19])
colMeans(sponse)
apply(sponse, 2, sd)
for (i in 1:nrow(response)) {
  newone = t(sponse[i,1:19] *results.pca$weights)
  weightedsum = rbind(weightedsum,newone)
}
weightedsum$tot = apply(weightedsum, 1, sum) # 총점

#내가 계산한거랑 PCA에서 계산해준거랑 아다리 맞나 상관 보기
cor(weightedsum$tot,score.PCA)

#PCA 문항총점상관
PCAitemtot = data.frame()
for (i in 1:19){
  newone = cor(weightedsum[,i],weightedsum$tot)
  PCAitemtot = rbind(PCAitemtot,newone)
}
PCAitemtot%>%round(2)%>% write.csv("C:/git/journeytoamastersdegree/PCAitemtot.csv")

describe(weightedsum[,-20]) %>% round(2)%>% write.csv("C:/git/journeytoamastersdegree/PCAdescribe.csv")

#ICC 그림모음
for(i in 1:19) {
  assign(paste0("plot_",i),itemplot(results.gpcm,i, type = 'trace', main = paste0("문항",i) , auto.key = none, par.settings = bwtheme))
}
plot_20 = plot(results.gpcm, type = 'score',main = "검사 총점 기댓값(Expected Total Score)", theta_lim = c(-6,6), lwd=1,par.settings=bwtheme)
grid.arrange(plot_1,plot_2,plot_3,plot_4,plot_5,plot_6,plot_7,plot_8,plot_9,plot_10,plot_11,plot_12,
             plot_13,plot_14,plot_15,plot_16,plot_17,plot_18,plot_19,plot_20,ncol=4,vp=viewport(width=1, height=0.95))
KeyA<-list(text = list(as.character(levels(as.factor(1:5)))),
           lines = TRUE, columns = 5,
           lty = 1:length(levels(as.factor(1:5))))
draw.key(KeyA, draw = TRUE, vp = viewport(0.5, 0.02))

# 문항모수
coef.pcm <- coef(results.pcm, IRTpars=TRUE, simplify=TRUE)
coef.pcm$items %>% round(2)%>% write.csv("C:/git/journeytoamastersdegree/PCMitems.csv")

# 정보 함수
plot(results.pcm, type = 'info', theta_lim = c(-1.3,-1.1), lwd=1,par.settings=bwtheme, main = "검사 정보 함수")
plot(results.gpcm, type = 'info', theta_lim = c(-1.3,-1.1), lwd=1,par.settings=bwtheme, main = "검사 정보 함수")
#우도비 검정
anova(results.gpcm,results.pcm)
# 문항모수
coef.gpcm <- coef(results.gpcm, IRTpars=TRUE, simplify=TRUE)
coef.gpcm$items %>% round(2)%>% write.csv("C:/git/journeytoamastersdegree/GPCMitems.csv")

#변별도 산점도 (PCA-GPCM)
coef.gpcm <- coef(results.gpcm, IRTpars=TRUE, simplify=TRUE)
lb = 1:19
scattdisc = cbind(coef.gpcm$items[,1],results.pca$Structure,lb); colnames(scattdisc) = c("GPCM","PCA","item")
scattdisc <- as.data.frame(scattdisc)
plot(scattdisc[,1:2], xlab = "가중 특질점수", ylab = "가중 합산점수"); text(scattdisc, labels = scattdisc$item,cex= 1, pos=4) #frame = FALSE
cor(scattdisc[,1:2])
cor(scattdisc[-c(1,5,6,12,16),1:2])#1,5,6,12,16번 문항 제거
cor(scattdisc[c(1,5,6,12,16),1:2])#1,5,6,12,16번 문항간 피어슨 상관계수


# 연령별 표준화된 검사 점수
scalesf <- sf
scalesf$SUM <- scale(sf$SUM, scale = T, center = T)
scalesf$PCA <- scale(sf$PCA, scale = T, center = T)
scalesf$PCM <- scale(sf$PCM, scale = T, center = T)
scalesf$GPCM <- scale(sf$GPCM, scale = T, center = T)

scalesf %>% group_by(agegroup) %>% summarise(SUM = mean(SUM),PCA = mean(PCA),PCM = mean(PCM),GPCM = mean(GPCM)) -> ssf

ssf %>% pivot_longer(cols = c(SUM,PCA,PCM,GPCM)) -> ssf
ssf$name <- factor(ssf$name,levels = c("SUM","PCA","PCM","GPCM"))
ggplot(ssf, aes(x=agegroup, y=value, group=name)) + theme_bw()+ 
  geom_line(aes(linetype=name))+
  geom_point(aes(shape=name))+ theme_bw() + 
  theme(legend.position="bottom",legend.title=element_blank()) + 
  scale_y_continuous(name = "표준화된 검사 점수") +
  scale_x_discrete(labels = c("~69", "70~79","80~89","90~"), name = "연령")




#상관 그림 + 피어슨 + 스피어만 + 파이 #Smoothing Lines
lower.panel = function(x,y){
  points(x = x, y = y, pch = 1, cex = 0.65)
  #fit<-loess.smooth(x=x,y=y)
  #lines(fit$x,fit$y,lwd = 0.75)
  abline(v = quantile(x,cutoff))
  abline(h = quantile(y,cutoff))
}
upper.panel = function(x,y){
  usr = par("usr"); on.exit(par(usr))
  par(usr = c(0,1,0,1))
  pearson = round(cor(x,y),3)
  spearman = round(cor(x,y,method="spearman"),3)
  phi = phi(c(nrow(sf[,1:4] %>% filter(x > quantile(x,cutoff) & y > quantile(y, cutoff))),
              nrow(sf[,1:4] %>% filter(x > quantile(x,cutoff) & y <= quantile(y, cutoff))),
              nrow(sf[,1:4] %>% filter(x <= quantile(x,cutoff) & y > quantile(y, cutoff))),
              nrow(sf[,1:4] %>% filter(x <= quantile(x,cutoff) & y <= quantile(y, cutoff)))
              ),3)
  txtp = paste0("Pearson = ",pearson)
  txts = paste0("Spearman = ",spearman)
  txtphi = paste0("Phi = ",phi)
  text(0.5,0.65,txtp,cex = 2)
  text(0.5,0.50,txts,cex = 2)
  text(0.5,0.35,txtphi,cex = 2)
}
pairs(sf[,1:4],
      upper.panel = upper.panel,
      lower.panel = lower.panel)

#상관 그림 + 피어슨 + 스피어만 +카파 #Smoothing Lines
lower.panel = function(x,y){
  points(x = x, y = y, pch = 1, cex = 0.65)
  fit<-loess.smooth(x=x,y=y)
  #lines(fit$x,fit$y,lwd = 0.75)
  abline(v = quantile(x,cutoff))
  abline(h = quantile(y,cutoff))
}
upper.panel = function(x,y){
  usr = par("usr"); on.exit(par(usr))
  par(usr = c(0,1,0,1))
  pearson = round(cor(x,y),3)
  #spearman = round(cor(x,y,method="spearman"),3)
  kappa = round(Kappa(matrix(c(nrow(sf[,1:4] %>% filter(x > quantile(x,cutoff) & y > quantile(y, cutoff))),
              nrow(sf[,1:4] %>% filter(x > quantile(x,cutoff) & y <= quantile(y, cutoff))),
              nrow(sf[,1:4] %>% filter(x <= quantile(x,cutoff) & y > quantile(y, cutoff))),
              nrow(sf[,1:4] %>% filter(x <= quantile(x,cutoff) & y <= quantile(y, cutoff)))),ncol=2))[[1]][1],3)
  txtp = paste0("Pearson = ",pearson)
  #txts = paste0("Spearman = ",spearman)
  txtkap = paste0("Kappa = ",kappa)
  text(0.5,0.6,txtp,cex = 2)
  #text(0.5,0.50,txts,cex = 2)
  text(0.5,0.4,txtkap,cex = 2)
}

pairs(sf[,1:4],
      upper.panel = upper.panel,
      #text.panel = my.text.panel(c(SUM="비가중 합산점수", PCA="가중 합산점수",
      #                             PCM="비가중 특질점수", GPCM="가중 특질점수")),
      lower.panel = lower.panel)
dev.off()

Kappa(matrix(c(nrow(sf[,1:4] %>% filter(SUM > quantile(SUM,cutoff) & PCA > quantile(PCA, cutoff))),
        nrow(sf[,1:4] %>% filter(SUM > quantile(SUM,cutoff) & PCA <= quantile(PCA, cutoff))),
        nrow(sf[,1:4] %>% filter(SUM <= quantile(SUM,cutoff) & PCA > quantile(PCA, cutoff))),
        nrow(sf[,1:4] %>% filter(SUM <= quantile(SUM,cutoff) & PCA <= quantile(PCA, cutoff)))),ncol=2))[[1]][1]




#연령 집단별 피어슨
sf %>% group_by(agegroup) %>% summarise("SUM-PCA" = cor(SUM,PCA),
                                        "SUM-PCM" = cor(SUM,PCM),
                                        "SUM-GPCM" = cor(SUM,GPCM),
                                        "PCA-PCM" = cor(PCA,PCM),
                                        "PCA-GPCM" = cor(PCA,GPCM),
                                        "PCM-GPCM" = cor(PCM,GPCM)) %>% 
  pivot_longer(cols = "SUM-PCA":"PCM-GPCM") -> tablep
tablep$name = factor(tablep$name, levels = c("SUM-PCA","SUM-PCM","SUM-GPCM","PCA-PCM","PCA-GPCM","PCM-GPCM"))
ggplot(data = tablep, aes(x = agegroup, y = value, group = name)) + 
  geom_line(aes(linetype=name)) + geom_point(aes(shape=name)) + theme_bw() + 
  theme(legend.position="bottom",legend.title=element_blank()) + 
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()) +
  scale_y_continuous(name = "피어슨 상관계수") +
  scale_x_discrete(labels = c("~69", "70~79","80~89","90~"), name = "연령")
posterE = ggplot(data = tablep, aes(x = agegroup, y = value, group = name)) + 
  geom_line(aes(linetype=name)) + geom_point(aes(shape=name)) + theme_bw() + 
  theme(legend.position="bottom",legend.title=element_blank()) + 
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()) +
  scale_y_continuous(name = "피어슨 상관계수") +
  scale_x_discrete(labels = c("~69", "70~79","80~89","90~"), name = "연령")
round(tablep$value,3) %>% write.csv("C:/git/journeytoamastersdegree/tablepearson.csv")
#연령 집단별 스피어만
sf %>% group_by(agegroup) %>% summarise("SUM-PCA" = cor(SUM,PCA,method = "spearman"),
                                        "SUM-PCM" = cor(SUM,PCM,method = "spearman"),
                                        "SUM-GPCM" = cor(SUM,GPCM,method = "spearman"),
                                        "PCA-PCM" = cor(PCA,PCM,method = "spearman"),
                                        "PCA-GPCM" = cor(PCA,GPCM,method = "spearman"),
                                        "PCM-GPCM" = cor(PCM,GPCM,method = "spearman")) %>% 
  pivot_longer(cols = "SUM-PCA":"PCM-GPCM") -> tables
tables$name = factor(tables$name, levels = c("SUM-PCA","SUM-PCM","SUM-GPCM","PCA-PCM","PCA-GPCM","PCM-GPCM"))
ggplot(data = tables, aes(x = agegroup, y = value, group = name)) + 
  geom_line(aes(linetype=name)) + geom_point(aes(shape=name)) + theme_bw() + 
  theme(legend.position="bottom",legend.title=element_blank()) + 
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +scale_y_continuous(name = "스피어만 상관계수") +
  scale_x_discrete(labels = c("~69", "70~79","80~89","90~"), name = "연령")

round(tables$value,3) %>% write.csv("C:/git/journeytoamastersdegree/tablespearman.csv")
#연령 집단별 파이
sf %>% group_by(agegroup) %>% summarise("SUM-PCA" = phi(confusionMatrix(markerSUM,markerPCA)[[2]],3),
                                        "SUM-PCM" = phi(confusionMatrix(markerSUM,markerPCM)[[2]],3),
                                        "SUM-GPCM" = phi(confusionMatrix(markerSUM,markerGPCM)[[2]],3), 
                                        "PCA-PCM" = phi(confusionMatrix(markerPCA,markerPCM)[[2]],3),
                                        "PCA-GPCM" = phi(confusionMatrix(markerPCA,markerGPCM)[[2]],3),
                                        "PCM-GPCM" = phi(confusionMatrix(markerPCM,markerGPCM)[[2]],3)) %>% 
  pivot_longer(cols = "SUM-PCA":"PCM-GPCM") -> tableph
tableph$name = factor(tableph$name, levels = c("SUM-PCA","SUM-PCM","SUM-GPCM","PCA-PCM","PCA-GPCM","PCM-GPCM"))
ggplot(data = tableph, aes(x = agegroup, y = value, group = name)) + 
  geom_line(aes(linetype=name)) + geom_point(aes(shape=name)) + theme_bw() + 
  theme(legend.position="bottom",legend.title=element_blank()) + 
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +scale_y_continuous(name = "파이 계수") +
  scale_x_discrete(labels = c("~69", "70~79","80~89","90~"), name = "연령")
tableph %>% write.csv("C:/git/journeytoamastersdegree/tablephi.csv")
#연령 집단별 카파
sf %>% group_by(agegroup) %>% summarise("SUM-PCA" = round(confusionMatrix(markerSUM,markerPCA)[[3]][[2]],3),
                                        "SUM-PCM" = round(confusionMatrix(markerSUM,markerPCM)[[3]][[2]],3),
                                        "SUM-GPCM" = round(confusionMatrix(markerSUM,markerGPCM)[[3]][[2]],3), 
                                        "PCA-PCM" = round(confusionMatrix(markerPCA,markerPCM)[[3]][[2]],3),
                                        "PCA-GPCM" = round(confusionMatrix(markerPCA,markerGPCM)[[3]][[2]],3),
                                        "PCM-GPCM" = round(confusionMatrix(markerPCM,markerGPCM)[[3]][[2]],3)) %>% 
  pivot_longer(cols = "SUM-PCA":"PCM-GPCM") -> tablekp
tablekp$name = factor(tablekp$name, levels = c("SUM-PCA","SUM-PCM","SUM-GPCM","PCA-PCM","PCA-GPCM","PCM-GPCM"))
ggplot(data = tablekp, aes(x = agegroup, y = value, group = name)) + 
  geom_line(aes(linetype=name)) + geom_point(aes(shape=name)) + theme_bw() + 
  theme(legend.position="bottom",legend.title=element_blank()) + 
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +scale_y_continuous(name = "카파 계수") +
  scale_x_discrete(labels = c("~69", "70~79","80~89","90~"), name = "연령")
posterF = ggplot(data = tablekp, aes(x = agegroup, y = value, group = name)) + 
  geom_line(aes(linetype=name)) + geom_point(aes(shape=name)) + theme_bw() + 
  theme(legend.position="bottom",legend.title=element_blank()) + 
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +scale_y_continuous(name = "카파 계수") +
  scale_x_discrete(labels = c("~69", "70~79","80~89","90~"), name = "연령")
tablekp %>% write.csv("C:/git/journeytoamastersdegree/tablekapa.csv")



#전체 검사 부적절성(GPCM)
human = mean(sf$GPCM)
coef.gpcm <- coef(results.gpcm, IRTpars=TRUE, simplify=TRUE)
nando = mean(coef.gpcm$items[,2:4],na.rm=T)
nando - human

#연령집단별 검사 부적절성(GPCM)
coef.gpcm <- coef(results.gpcm, IRTpars=TRUE, simplify=TRUE)
nando = mean(coef.gpcm$items[,2:4],na.rm=T)
sf %>% group_by(agegroup) %>% summarise(TI = nando - mean(GPCM))

#연령집단별 검사 부적절성(PCM)
coef.pcm <- coef(results.pcm, IRTpars=TRUE, simplify=TRUE)
nando = mean(coef.pcm$items[,2:4],na.rm=T)
sf %>% group_by(agegroup) %>% summarise(TI = nando - mean(PCM))

#스뀨 계산
sf %>% group_by(agegroup) %>% summarise(skewSUM = skew(SUM),
                                        skewPCA = skew(PCA),
                                        skewPCM = skew(PCM),
                                        skewGPCM = skew(GPCM)) ->sk
for(i in 1:4){
print(round(sk[i,4] - sk[i,5],2))
}

sktable = data.frame()
for(i in 1:6){
  for (j in 2:5){
    a = sk[i,j] - sk[i,j+1]
    print(a)
  }
}

