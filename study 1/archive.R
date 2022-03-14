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
