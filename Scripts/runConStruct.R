#setwd command is required
library(devtools)
library(conStruct)

#Make all 3 inputs for conStruct
alFreq = as.matrix(data.frame(read.table("ZeaAllInfo.pmz.conS.alFreq", header=TRUE)))
coordsData = as.matrix(data.frame(read.table("ZeaAllInfo.pmz.conS.coords", header=TRUE)))
distMatrix = fields::rdist.earth(coordsData, miles=FALSE, R=NULL)

conStruct(spatial=TRUE, K=2, freqs=alFreq, geoDist=distMatrix, coords=coordsData, prefix= "ZeaPmzK2ord1")
conStruct(spatial=TRUE, K=2, freqs=alFreq, geoDist=distMatrix, coords=coordsData, prefix= "ZeaPmzK2ord2")
conStruct(spatial=TRUE, K=2, freqs=alFreq, geoDist=distMatrix, coords=coordsData, prefix= "ZeaPmzK2ord3")
conStruct(spatial=TRUE, K=2, freqs=alFreq, geoDist=distMatrix, coords=coordsData, prefix= "ZeaPmzK2ord4")
conStruct(spatial=TRUE, K=2, freqs=alFreq, geoDist=distMatrix, coords=coordsData, prefix= "ZeaPmzK2ord5")