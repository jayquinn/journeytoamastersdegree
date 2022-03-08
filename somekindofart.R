
## 총점 ##
# 히스토그램
ggplot(data = sf, aes(x = CTT)) + 
  geom_histogram(aes(y = ..density..), colour = 1, fill = "white", bins = 31) +
  stat_function(fun = dnorm, args = list(mean = 16, sd = sd(sf$CTT))) # 모델 히스토그램 + 덴시티


ggplot(data = sf, aes(x = CTT)) + 
  geom_histogram(aes(y = ..count..), colour = 1, fill = "white", bins = 31) + 
  stat_function(fun = function(x) dnorm(x, mean = 16, sd = sd(sf$CTT)) * nrow(sf)) + #n 뒤에 ,color = "black", size = 1 // 더블 y축
  scale_y_continuous("count",sec.axis=sec_axis(
    trans = ~./(max(table(sf$CTT)) / max(density(sf$CTT)$y)),name = "density"))

A = ggplot(data = sf, aes(x = CTT)) + 
  theme_bw() + 
  geom_histogram(aes(y = ..count..), colour = 1, fill = "white", bins = 31) + 
  stat_function(fun = function(x) dnorm(x, mean = 16, sd = sd(sf$CTT)) * nrow(sf)) +
  scale_y_continuous("빈도 수") + 
  scale_x_continuous("비가중 합산점수") #n 뒤에 ,color = "black", size =  //  걍 이거 해

B = ggplot(data = sf, aes(sample = CTT)) + stat_qq() + coord_flip() + stat_qq_line() + theme_bw() +
  scale_y_continuous("sample") + 
  scale_x_continuous("theoretical")

plot_grid(A,B)
grid.arrange(A, B, nrow = 2)


A = ggplot() + 
  geom_boxplot(data = filter(sf, sf$gender == 1), mapping = aes(x = agegroup, y = CTT, group = agegroup))+
  geom_point(data = meandotmale, mapping = aes(x = agegroup, y = Avg)) + 
  geom_line(data = meandotmale, mapping = aes(x = agegroup, y = Avg, group = 1))+
  scale_y_continuous(name = "비가중 합산점수") +
  scale_x_discrete(labels = abbreviate, name = "연령")  + ggtitle("남성")
B = ggplot() + 
  geom_boxplot(data = filter(sf, sf$gender == 5), mapping = aes(x = agegroup, y = CTT, group = agegroup))+
  geom_point(data = meandotmale, mapping = aes(x = agegroup, y = Avg)) + 
  geom_line(data = meandotmale, mapping = aes(x = agegroup, y = Avg, group = 1))+
  scale_y_continuous(name = "비가중 합산점수") +
  scale_x_discrete(labels = abbreviate, name = "연령")  + ggtitle("여성")
grid.arrange(A, B, nrow = 2)
