
## 총점 ##
# 히스토그램 + density curve + qq plot
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
sf %>% filter(gender == 1)%>% group_by(agegroup) %>% summarise(Avg = mean(PCA)) -> meandotmale
sf %>% filter(gender == 5)%>% group_by(agegroup) %>% summarise(Avg = mean(PCA)) -> meandotfemale

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
#alpha
alpha = alpha(response[,1:19])
alpha$item.stats %>% round(2) %>% write.csv("C:/git/journeytoamastersdegree/alpha.csv")

# 평균 표준편차 왜도 첨도
describe(response) %>% round(2)%>% write.csv("C:/git/journeytoamastersdegree/describe.csv")
#PCA 로딩
results.pca$Structure %>% round(3)%>% write.csv("C:/git/journeytoamastersdegree/PCAloading.csv")
#PCA 가중치
results.pca$weights%>% write.csv("C:/git/journeytoamastersdegree/PCAweight.csv")
#PCA 가중합 반응값
weightedsum = data.frame()
for (i in 1:nrow(response)) {
  newone = response[i,1:19] *results.pca$weights
  weightedsum = rbind(weightedsum,newone)
}
weightedsum$tot = apply(weightedsum, 1, sum) # 총점
#PCA 문항총점상관
PCAitemtot = data.frame()
for (i in 1:19){
  newone = cor(weightedsum[,i],weightedsum$tot)
  PCAitemtot = rbind(PCAitemtot,newone)
}
PCAitemtot%>%round(2)%>% write.csv("C:/git/journeytoamastersdegree/PCAitemtot.csv")

describe(weightedsum[,-20]) %>% round(2)%>% write.csv("C:/git/journeytoamastersdegree/PCAdescribe.csv")

#PCM
for(i in 1:19) {
  assign(paste0("plot_",i),plot(results.gpcm, type = 'trace', main = "", which.items = c(i))) 
}
for(i in 1:19) {
  assign(paste0("plot_",i),itemplot(results.gpcm,i, type = 'trace', main = paste0("문항",i) , auto.key = none)) 
}
plot_20 = plot(results.pcm, type = 'score',main = "뭐라고적지", theta_lim = c(-4,4), lwd=2,par.settings=bwtheme)
grid.arrange(plot_1,plot_2,plot_3,plot_4,plot_5,plot_6,plot_7,plot_8,plot_9,plot_10,plot_11,plot_12,
             plot_13,plot_14,plot_15,plot_16,plot_17,plot_18,plot_19,plot_20,ncol=4,)



#jittered
jit = sf[,1:4]
jit$CTT <- jitter(jit$CTT, factor = 1)
jit$PCM <- jitter(jit$PCM, factor = 1)
plot(jit)


#우도비 검정
anova(results.gpcm,results.pcm)

#변별도 산점도 (PCA-GPCM)
coef.gpcm <- coef(results.gpcm, IRTpars=TRUE, simplify=TRUE)
lb = 1:19
scattdisc = cbind(coef.gpcm$items[,1],results.pca$Structure,lb); colnames(scattdisc) = c("GPCM","PCA","item")
scattdisc <- as.data.frame(scattdisc)
plot(scattdisc[,1:2])
text(scattdisc, labels = scattdisc$item,cex= 1, pos=4)
cor(scattdisc[,1:2])

#변별도 산점도 (CFA-GPCM)
ttt = summary(results.cfa)

coef.gpcm <- coef(results.gpcm, IRTpars=TRUE, simplify=TRUE)
lb = 1:19
scattdisc = cbind(coef.gpcm$items[,1],ttt$PE$est[1:19],lb); colnames(scattdisc) = c("GPCM","CFA","item")
scattdisc <- as.data.frame(scattdisc)
plot(scattdisc[,1:2])
text(scattdisc, labels = scattdisc$item,cex= 1, pos=4)
cor(scattdisc[,1:2])

# 각 변별도간 상관비교
coef.gpcm <- coef(results.gpcm, IRTpars=TRUE, simplify=TRUE)
lb = 1:19
scattdisc = cbind(coef.gpcm$items[,1],summary(results.gpcm)[1]$rotF[1:19],ttt$PE$est[1:19],PCAitemtot,lb); colnames(scattdisc) = c("GPCM","GPCMloading","PCA","itemtot","item")
scattdisc <- as.data.frame(scattdisc)
cor(scattdisc[,1:4])

# 표준화
scalesf <- sf
scalesf$CTT <- scale(sf$CTT, scale = T, center = T)
scalesf$PCA <- scale(sf$PCA, scale = T, center = T)
scalesf$PCM <- scale(sf$PCM, scale = T, center = T)
scalesf$GPCM <- scale(sf$GPCM, scale = T, center = T)

scalesf %>% group_by(agegroup) %>% summarise(CTT = mean(CTT),PCA = mean(PCA),PCM = mean(PCM),GPCM = mean(GPCM)) -> ssf

ssf %>% pivot_longer(cols = c(CTT,PCA,PCM,GPCM)) -> ssf

ggplot(ssf, aes(x=agegroup, y=value, group=name)) + theme_bw()+ 
  geom_line(aes(linetype=name))+
  geom_point(aes(shape=name))

# 문항특성곡선
plot(results.pcm, type = 'trace', which.items = c(1:19),par.settings=bwtheme)
plot(results.pcm, type = 'score', theta_lim = c(-4,4), lwd=2,par.settings=bwtheme)

plot(results.gpcm, type = 'trace', which.items = c(1:19),par.settings=bwtheme)
plot(results.gpcm, type = 'score', theta_lim = c(-4,4), lwd=2,par.settings=bwtheme)

# 문항모수
coef.pcm <- coef(results.pcm, IRTpars=TRUE, simplify=TRUE)
coef.gpcm <- coef(results.gpcm, IRTpars=TRUE, simplify=TRUE)
coef.pcm$items %>% round(2)%>% write.csv("C:/git/journeytoamastersdegree/PCMitems.csv")
coef.gpcm$items %>% round(2)%>% write.csv("C:/git/journeytoamastersdegree/GPCMitems.csv")

