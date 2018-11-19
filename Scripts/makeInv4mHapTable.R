#This script is designed to look at inv4m distribution across parvHC, parvAmb ,mexHC, 
#mexHC, all hybrids, and the three hybrid groups.
#Created by David E. Hufnagel

#set working directory here
data = read.table("ZeaAllInfo.pmz", skip=2, sep="\t")

#subset data and convert it to matrix
hybrids = as.matrix(subset(data, V5=="hybrid"))
parvHC = as.matrix(subset(data, V5=="parvHC"))
mexHC = as.matrix(subset(data, V5=="mexHC"))

CPG = as.matrix(subset(data, V6=="Central_Plateau"))
CBG = as.matrix(subset(data, V6=="Central_Balsas"))
SGG = as.matrix(subset(data, V6=="South_Guerrero"))

allAmb = as.matrix(subset(data, V5=="other")) 
parvAmb = subset(allAmb, allAmb[,2]=="Zea_mays_parviglumis")
mexAmb = subset(allAmb, allAmb[,2]=="Zea_mays_mexicana")

#count inversion types
hybInvs = table(hybrids[,13])
parvInvs = table(parvHC[,13])
mexInvs = table(mexHC[,13])

CPGInvs = table(CPG[,13])
CBGInvs = table(CBG[,13])
SGGInvs = table(SGG[,13])

pAmbInvs = table(parvAmb[,13])
mAmbInvs = table(mexAmb[,13])

sink("inv4mHapTable.txt")
cat("#inv4m presence within groups (P=parvType, M=mexType, H=heterozygous, R=recombinant, N=missingData)\n")
cat("          P   M   H   R  N\n")
cat(sprintf("parvHC  %d   %d   %d   %d  %d\n",parvInvs[1],0,0,parvInvs[2],0))
cat(sprintf("parvAmb %d   %d   %d   %d  %d\n",pAmbInvs[3],pAmbInvs[2],pAmbInvs[1],0,0))
cat(sprintf("mexHC     %d  %d   %d  %d  %d\n",0,mexInvs[1],0,mexInvs[2],0))
cat(sprintf("mexAmb    %d  %d   %d  %d  %d\n",mAmbInvs[3],mAmbInvs[2],mAmbInvs[1],mAmbInvs[4],0))
cat(sprintf("hybAll   %d   %d  %d   %d  %d\n",hybInvs[4],hybInvs[2],hybInvs[1],hybInvs[5],hybInvs[3]))
cat(sprintf("hybCPG    %d   %d   %d   %d  %d\n",CPGInvs[3],CPGInvs[2],CPGInvs[1],0,0))
cat(sprintf("hybCBG   %d   %d  %d   %d  %d\n",CBGInvs[4],CBGInvs[2],CBGInvs[1],CBGInvs[5],CBGInvs[3]))
cat(sprintf("hybSGG   %d   %d   %d   %d  %d\n",SGGInvs[2],0,SGGInvs[1],0,0))

cat("\n##percentages\n")
perc = "%"
cat("          P      M      H      R     N\n")
cat(sprintf("parvHC  %.1f%s   %.1f%s   %.1f%s   %.1f%s  %.1f%s\n",parvInvs[1]/sum(parvInvs)*100,perc,0,perc,0,perc,parvInvs[2]/sum(parvInvs)*100,perc,0,perc))
cat(sprintf("parvAmb %.1f%s   %.1f%s   %.1f%s   %.1f%s  %.1f%s\n",pAmbInvs[2]/sum(pAmbInvs)*100,perc,0,perc,pAmbInvs[1]/sum(pAmbInvs)*100,perc,0,perc,0,perc))
cat(sprintf("mexHC    %.1f%s  %.1f%s   %.1f%s  %.1f%s  %.1f%s\n",0, perc,mexInvs[1]/sum(mexInvs)*100,perc,0,perc,mexInvs[2]/sum(mexInvs)*100,perc,0,perc))
cat(sprintf("mexAmb   %.1f%s  %.1f%s  %.1f%s  %.1f%s  %.1f%s\n",mAmbInvs[3]/sum(mAmbInvs)*100,perc,mAmbInvs[2]/sum(mAmbInvs)*100,perc,mAmbInvs[1]/sum(mAmbInvs)*100,perc,mAmbInvs[4]/sum(mAmbInvs)*100,perc,0,perc))
cat(sprintf("hybAll  %.1f%s   %.1f%s  %.1f%s   %.1f%s  %.1f%s\n",hybInvs[4]/sum(hybInvs)*100,perc,hybInvs[2]/sum(hybInvs)*100,perc,hybInvs[1]/sum(hybInvs)*100,perc,hybInvs[5]/sum(hybInvs)*100,perc,hybInvs[3]/sum(hybInvs)*100,perc))
cat(sprintf("hybCPG  %.1f%s  %.1f%s  %.1f%s   %.1f%s  %.1f%s\n",CPGInvs[3]/sum(CPGInvs)*100,perc,CPGInvs[2]/sum(CPGInvs)*100,perc,CPGInvs[1]/sum(CPGInvs)*100,perc,0,perc,0,perc))
cat(sprintf("hybCBG  %.1f%s   %.1f%s  %.1f%s   %.1f%s  %.1f%s\n",CBGInvs[4]/sum(CBGInvs)*100,perc,CBGInvs[2]/sum(CBGInvs)*100,perc,CBGInvs[1]/sum(CBGInvs)*100,perc,CBGInvs[5]/sum(CBGInvs)*100,perc,CBGInvs[3]/sum(CBGInvs)*100,perc))
cat(sprintf("hybSGG  %.1f%s   %.1f%s   %.1f%s   %.1f%s  %.1f%s\n",SGGInvs[2]/sum(SGGInvs)*100,perc,0,perc,SGGInvs[1]/sum(SGGInvs)*100,perc,0,perc,0,perc))
sink()




