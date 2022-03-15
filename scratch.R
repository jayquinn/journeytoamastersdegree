ggplot(data = sf, aes(x = CTT)) + 
  geom_histogram(aes(y = ..density..), colour = 1, fill = "white", bins = 31) +
  stat_function(fun = dnorm, args = list(mean = 16, sd = sd(sf$CTT))) # 모델 히스토그램 + 덴시티

ggplot(data = sf, aes(x = PCA)) + 
  geom_histogram(aes(y = ..count..), colour = 1, fill = "white", bins = 31) + 
  stat_function(fun = function(x) dnorm(x, mean = median(range(sf$PCA)), sd = sd(sf$PCA)) * nrow(sf)*0.2) + #n 뒤에 ,color = "black", size = 1 // 더블 y축
  scale_y_continuous("count",sec.axis=sec_axis(
    trans = ~./(max(table(sf$PCA)) / max(density(sf$PCA)$y)),name = "density"))

ggplot(data = sf, aes(x = PCA),) + 
  geom_histogram(aes(y = ..count..), colour = 1, fill = "white", binwidth = 0.212) + 
  stat_function(fun = function(x) dnorm(x, mean = median(range(sf$PCA)), sd = sd(sf$PCA)) * nrow(sf) * 0.212) + #n 뒤에 ,color = "black", size = 1 // 더블 y축
  scale_y_continuous("count",sec.axis=sec_axis(
    trans = ~./(max(table(sf$PCA)) / max(density(sf$PCA)$y)),name = "density"))


ggplot(sf,aes(x = agegroup,y = CTT, fill = gender)) + 
  geom_boxplot() +
  scale_fill_manual(breaks = c("1","5"),
                    values = c("white","grey70"))+
  stat_summary(
    fun = mean,
    geom = 'line',
    aes(group = gender),
    position = position_dodge(width = 0.75)) + 
  stat_summary(fun=mean, geom="point", aes(group=gender), position=position_dodge(.75), 
               color="black", size=2) + theme_classic()

