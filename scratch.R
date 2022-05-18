

sf %>% filter(agegroup==1) -> yf
confusionMatrix(yf$markerSUM,yf$markerPCA)[[2]] #0.035
confusionMatrix(yf$markerSUM,yf$markerGPCM)[[2]]#0.22
confusionMatrix(yf$markerPCA,yf$markerGPCM)[[2]]#0.015

confusionMatrix(yf$markerSUM,yf$markerPCA)[[3]][2] #0.83
confusionMatrix(yf$markerSUM,yf$markerGPCM)[[3]][2] #0.89
confusionMatrix(yf$markerPCA,yf$markerGPCM)[[3]][2] #0.93


sf %>% filter(agegroup==2) -> uf
confusionMatrix(uf$markerSUM,uf$markerPCA)[[2]] #0.049
confusionMatrix(uf$markerSUM,uf$markerGPCM)[[2]]#0.035
confusionMatrix(uf$markerPCA,uf$markerGPCM)[[2]]#0.019

confusionMatrix(uf$markerSUM,uf$markerPCA)[[3]][2] #0.9
confusionMatrix(uf$markerSUM,uf$markerGPCM)[[3]][2] #0.93
confusionMatrix(uf$markerPCA,uf$markerGPCM)[[3]][2] #0.96

sf %>% filter(agegroup==3) -> of
confusionMatrix(of$markerSUM,of$markerPCA)[[2]] #0.029
confusionMatrix(of$markerSUM,of$markerGPCM)[[2]] #0.02
confusionMatrix(of$markerPCA,of$markerGPCM)[[2]] #0.014

confusionMatrix(of$markerSUM,of$markerPCA)[[3]][2] #0.94
confusionMatrix(of$markerSUM,of$markerGPCM)[[3]][2] #0.96
confusionMatrix(of$markerPCA,of$markerGPCM)[[3]][2] #0.97


sf %>% filter(agegroup==4) -> pf
confusionMatrix(pf$markerSUM,pf$markerPCA)[[2]] #0.014
confusionMatrix(pf$markerSUM,pf$markerGPCM)[[2]] #0
confusionMatrix(pf$markerPCA,pf$markerGPCM)[[2]] #0.014

confusionMatrix(pf$markerSUM,pf$markerPCA)[[3]][2] #0.95
confusionMatrix(pf$markerSUM,pf$markerGPCM)[[3]][2] #1
confusionMatrix(pf$markerPCA,pf$markerGPCM)[[3]][2] #0.95

