library(tidyverse)
library(mirt)
library(psych) # phi 계수 https://www.r-bloggers.com/2021/07/how-to-calculate-phi-coefficient-in-r/
library(lavaan) #CFA
# cohen.kappa 랑 wkappa ?cohen.kappa
# mirt simdata
# psych https://personality-project.org/r/html/sim.html
# irt시뮬레이션 패키지: irtplay



tmphitmark1 = vector()
tmphitmark2 = vector()
tmphitmark3 = vector()
tmphifmark1 = vector()
tmphifmark2 = vector()
tmphifmark3 = vector()
tmphipmark1 = vector()
tmphipmark2 = vector()
tmphipmark3 = vector()
tmphigmark1 = vector()
tmphigmark2 = vector()
tmphigmark3 = vector()

tmpkaptmark1 = vector()
tmpkaptmark2 = vector()
tmpkaptmark3 = vector()
tmpkapfmark1 = vector()
tmpkapfmark2 = vector()
tmpkapfmark3 = vector()
tmpkappmark1 = vector()
tmpkappmark2 = vector()
tmpkappmark3 = vector()
tmpkapgmark1 = vector()
tmpkapgmark2 = vector()
tmpkapgmark3 = vector()

boat = data.frame()
