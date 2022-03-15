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