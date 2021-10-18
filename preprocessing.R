library(tidyverse)
library(mirt)
library(psych) # phi 계수 https://www.r-bloggers.com/2021/07/how-to-calculate-phi-coefficient-in-r/
library(lavaan) #CFA
library("vcd") # kappa
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

type1tmark1 = vector()
type1tmark2 = vector()
type1tmark3 = vector()
type1fmark1 = vector()
type1fmark2 = vector()
type1fmark3 = vector()
type1pmark1 = vector()
type1pmark2 = vector()
type1pmark3 = vector()
type1gmark1 = vector()
type1gmark2 = vector()
type1gmark3 = vector()



type2tmark1 = vector()
type2tmark2 = vector()
type2tmark3 = vector()
type2fmark1 = vector()
type2fmark2 = vector()
type2fmark3 = vector()
type2pmark1 = vector()
type2pmark2 = vector()
type2pmark3 = vector()
type2gmark1 = vector()
type2gmark2 = vector()
type2gmark3 = vector()



boat = data.frame()
boat2 = data.frame()
boat5 = data.frame()
