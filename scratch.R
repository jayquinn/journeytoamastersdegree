
#연령 집단별 혼동행렬

sf %>% filter(agegroup == 4) -> sksf
confusionMatrix(sksf$markerPCA,sksf$markerGPCM)

              