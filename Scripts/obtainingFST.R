#set working directory manually
#I'm not sure why, but each file must be run seperately

#Parv_Balsas v Parv_Jalisco
require(hierfstat)

data<-read.table("HierFstatFiles/ZeaAllInfo.pmz.hierFstat.PB_PJ", header=TRUE, check.names=FALSE)
attach(data)
require(hierfstat)
loci<-data.frame(data[3:969])
FST<-varcomp.glob(data.frame(pop), loci)
FSTperLoci<-mat.or.vec(length(FST$loc[,1]), 1)

for (i in c(1:length(FST$loc[,1]))) {
	
	FSTperLoci[i]<-(FST$loc[i,1]/(FST$loc[i,1]+FST$loc[i,2]+FST$loc[i,3]))
	
}

FST_Table<-data.frame(names(loci))
FST_Table$FST<-FSTperLoci
write.table(FST_Table,"ParvBalsasParvJal.Fst", quote=FALSE, row.names=FALSE, sep="\t" )

#Parv_Balsas v Mex_Chalco
data<-read.table("HierFstatFiles/ZeaAllInfo.pmz.hierFstat.PB_Mch", header=TRUE, check.names=FALSE)
attach(data)
require(hierfstat)
loci<-data.frame(data[3:969])
FST<-varcomp.glob(data.frame(pop), loci)
FSTperLoci<-mat.or.vec(length(FST$loc[,1]), 1)

for (i in c(1:length(FST$loc[,1]))) {
	
	FSTperLoci[i]<-(FST$loc[i,1]/(FST$loc[i,1]+FST$loc[i,2]+FST$loc[i,3]))
	
}

FST_Table<-data.frame(names(loci))
FST_Table$FST<-FSTperLoci
write.table(FST_Table,"ParvBalsasMexChalco.Fst", quote=FALSE, row.names=FALSE, sep="\t" )

#Parv_Balsas v Mex_Central_Plateau
data<-read.table("HierFstatFiles/ZeaAllInfo.pmz.hierFstat.PB_Mcp", header=TRUE, check.names=FALSE)
attach(data)
require(hierfstat)
loci<-data.frame(data[3:969])
FST<-varcomp.glob(data.frame(pop), loci)
FSTperLoci<-mat.or.vec(length(FST$loc[,1]), 1)

for (i in c(1:length(FST$loc[,1]))) {
	
	FSTperLoci[i]<-(FST$loc[i,1]/(FST$loc[i,1]+FST$loc[i,2]+FST$loc[i,3]))
	
}

FST_Table<-data.frame(names(loci))
FST_Table$FST<-FSTperLoci
write.table(FST_Table,"ParvBalsasMexCP.Fst", quote=FALSE, row.names=FALSE, sep="\t" )

#Parv_Balsas v Hyb_South_Guerrero
data<-read.table("HierFstatFiles/ZeaAllInfo.pmz.hierFstat.PB_HSG", header=TRUE, check.names=FALSE)
attach(data)
require(hierfstat)
loci<-data.frame(data[3:969])
FST<-varcomp.glob(data.frame(pop), loci)
FSTperLoci<-mat.or.vec(length(FST$loc[,1]), 1)

for (i in c(1:length(FST$loc[,1]))) {
	
	FSTperLoci[i]<-(FST$loc[i,1]/(FST$loc[i,1]+FST$loc[i,2]+FST$loc[i,3]))
	
}

FST_Table<-data.frame(names(loci))
FST_Table$FST<-FSTperLoci
write.table(FST_Table,"ParvBalsasHybSG.Fst", quote=FALSE, row.names=FALSE, sep="\t" )

#Parv_Balsas v Hyb_Central_Balsas
data<-read.table("HierFstatFiles/ZeaAllInfo.pmz.hierFstat.PB_HCB", header=TRUE, check.names=FALSE)
attach(data)
require(hierfstat)
loci<-data.frame(data[3:969])
FST<-varcomp.glob(data.frame(pop), loci)
FSTperLoci<-mat.or.vec(length(FST$loc[,1]), 1)

for (i in c(1:length(FST$loc[,1]))) {
	
	FSTperLoci[i]<-(FST$loc[i,1]/(FST$loc[i,1]+FST$loc[i,2]+FST$loc[i,3]))
	
}

FST_Table<-data.frame(names(loci))
FST_Table$FST<-FSTperLoci
write.table(FST_Table,"ParvBalsasHybCB.Fst", quote=FALSE, row.names=FALSE, sep="\t" )

#Parv_Balsas v Hyb_Central_Plateau
data<-read.table("HierFstatFiles/ZeaAllInfo.pmz.hierFstat.PB_HCP", header=TRUE, check.names=FALSE)
attach(data)
require(hierfstat)
loci<-data.frame(data[3:969])
FST<-varcomp.glob(data.frame(pop), loci)
FSTperLoci<-mat.or.vec(length(FST$loc[,1]), 1)

for (i in c(1:length(FST$loc[,1]))) {
	
	FSTperLoci[i]<-(FST$loc[i,1]/(FST$loc[i,1]+FST$loc[i,2]+FST$loc[i,3]))
	
}

FST_Table<-data.frame(names(loci))
FST_Table$FST<-FSTperLoci
write.table(FST_Table,"ParvBalsasHybCP.Fst", quote=FALSE, row.names=FALSE, sep="\t" )

#Parv_Jalisco v Mex_Chalco
data<-read.table("HierFstatFiles/ZeaAllInfo.pmz.hierFstat.PJ_Mch", header=TRUE, check.names=FALSE)
attach(data)
require(hierfstat)
loci<-data.frame(data[3:969])
FST<-varcomp.glob(data.frame(pop), loci)
FSTperLoci<-mat.or.vec(length(FST$loc[,1]), 1)

for (i in c(1:length(FST$loc[,1]))) {
	
	FSTperLoci[i]<-(FST$loc[i,1]/(FST$loc[i,1]+FST$loc[i,2]+FST$loc[i,3]))
	
}

FST_Table<-data.frame(names(loci))
FST_Table$FST<-FSTperLoci
write.table(FST_Table,"ParvJalMexChalco.Fst", quote=FALSE, row.names=FALSE, sep="\t" )

#Parv_Jalisco v Mex_Central_Plateau
data<-read.table("HierFstatFiles/ZeaAllInfo.pmz.hierFstat.PJ_Mcp", header=TRUE, check.names=FALSE)
attach(data)
require(hierfstat)
loci<-data.frame(data[3:969])
FST<-varcomp.glob(data.frame(pop), loci)
FSTperLoci<-mat.or.vec(length(FST$loc[,1]), 1)

for (i in c(1:length(FST$loc[,1]))) {
	
	FSTperLoci[i]<-(FST$loc[i,1]/(FST$loc[i,1]+FST$loc[i,2]+FST$loc[i,3]))
	
}

FST_Table<-data.frame(names(loci))
FST_Table$FST<-FSTperLoci
write.table(FST_Table,"ParvJalMexCP.Fst", quote=FALSE, row.names=FALSE, sep="\t" )

#Parv_Jalisco v Hyb_South_Guerrero
data<-read.table("HierFstatFiles/ZeaAllInfo.pmz.hierFstat.PJ_HSG", header=TRUE, check.names=FALSE)
attach(data)
require(hierfstat)
loci<-data.frame(data[3:969])
FST<-varcomp.glob(data.frame(pop), loci)
FSTperLoci<-mat.or.vec(length(FST$loc[,1]), 1)

for (i in c(1:length(FST$loc[,1]))) {
	
	FSTperLoci[i]<-(FST$loc[i,1]/(FST$loc[i,1]+FST$loc[i,2]+FST$loc[i,3]))
	
}

FST_Table<-data.frame(names(loci))
FST_Table$FST<-FSTperLoci
write.table(FST_Table,"ParvJalHybSG.Fst", quote=FALSE, row.names=FALSE, sep="\t" )

#Parv_Jalisco v Hyb_Central_Balsas
data<-read.table("HierFstatFiles/ZeaAllInfo.pmz.hierFstat.PJ_HCB", header=TRUE, check.names=FALSE)
attach(data)
require(hierfstat)
loci<-data.frame(data[3:969])
FST<-varcomp.glob(data.frame(pop), loci)
FSTperLoci<-mat.or.vec(length(FST$loc[,1]), 1)

for (i in c(1:length(FST$loc[,1]))) {
	
	FSTperLoci[i]<-(FST$loc[i,1]/(FST$loc[i,1]+FST$loc[i,2]+FST$loc[i,3]))
	
}

FST_Table<-data.frame(names(loci))
FST_Table$FST<-FSTperLoci
write.table(FST_Table,"ParvJalHybCB.Fst", quote=FALSE, row.names=FALSE, sep="\t" )

#Parv_Jalisco v Hyb_Central_Plateau
data<-read.table("HierFstatFiles/ZeaAllInfo.pmz.hierFstat.PJ_HCP", header=TRUE, check.names=FALSE)
attach(data)
require(hierfstat)
loci<-data.frame(data[3:969])
FST<-varcomp.glob(data.frame(pop), loci)
FSTperLoci<-mat.or.vec(length(FST$loc[,1]), 1)

for (i in c(1:length(FST$loc[,1]))) {
	
	FSTperLoci[i]<-(FST$loc[i,1]/(FST$loc[i,1]+FST$loc[i,2]+FST$loc[i,3]))
	
}

FST_Table<-data.frame(names(loci))
FST_Table$FST<-FSTperLoci
write.table(FST_Table,"ParvJalHybCP.Fst", quote=FALSE, row.names=FALSE, sep="\t" )

#Mex_Chalco v Mex_Central_Plateau
data<-read.table("HierFstatFiles/ZeaAllInfo.pmz.hierFstat.Mch_Mcp", header=TRUE, check.names=FALSE)
attach(data)
require(hierfstat)
loci<-data.frame(data[3:969])
FST<-varcomp.glob(data.frame(pop), loci)
FSTperLoci<-mat.or.vec(length(FST$loc[,1]), 1)

for (i in c(1:length(FST$loc[,1]))) {
	
	FSTperLoci[i]<-(FST$loc[i,1]/(FST$loc[i,1]+FST$loc[i,2]+FST$loc[i,3]))
	
}

FST_Table<-data.frame(names(loci))
FST_Table$FST<-FSTperLoci
write.table(FST_Table,"MexChalcoMexCP.Fst", quote=FALSE, row.names=FALSE, sep="\t" )

#Mex_Chalco v Hyb_South_Guerrero
data<-read.table("HierFstatFiles/ZeaAllInfo.pmz.hierFstat.Mch_HSG", header=TRUE, check.names=FALSE)
attach(data)
require(hierfstat)
loci<-data.frame(data[3:969])
FST<-varcomp.glob(data.frame(pop), loci)
FSTperLoci<-mat.or.vec(length(FST$loc[,1]), 1)

for (i in c(1:length(FST$loc[,1]))) {
	
	FSTperLoci[i]<-(FST$loc[i,1]/(FST$loc[i,1]+FST$loc[i,2]+FST$loc[i,3]))
	
}

FST_Table<-data.frame(names(loci))
FST_Table$FST<-FSTperLoci
write.table(FST_Table,"MexChalcoHybSG.Fst", quote=FALSE, row.names=FALSE, sep="\t" )

#Mex_Chalco v Hyb_Central_Balsas
data<-read.table("HierFstatFiles/ZeaAllInfo.pmz.hierFstat.Mch_HCB", header=TRUE, check.names=FALSE)
attach(data)
require(hierfstat)
loci<-data.frame(data[3:969])
FST<-varcomp.glob(data.frame(pop), loci)
FSTperLoci<-mat.or.vec(length(FST$loc[,1]), 1)

for (i in c(1:length(FST$loc[,1]))) {
	
	FSTperLoci[i]<-(FST$loc[i,1]/(FST$loc[i,1]+FST$loc[i,2]+FST$loc[i,3]))
	
}

FST_Table<-data.frame(names(loci))
FST_Table$FST<-FSTperLoci
write.table(FST_Table,"MexChalcoHybCB.Fst", quote=FALSE, row.names=FALSE, sep="\t" )

#Mex_Chalco v Hyb_Central_Plateau
data<-read.table("HierFstatFiles/ZeaAllInfo.pmz.hierFstat.Mch_HCP", header=TRUE, check.names=FALSE)
attach(data)
require(hierfstat)
loci<-data.frame(data[3:969])
FST<-varcomp.glob(data.frame(pop), loci)
FSTperLoci<-mat.or.vec(length(FST$loc[,1]), 1)

for (i in c(1:length(FST$loc[,1]))) {
	
	FSTperLoci[i]<-(FST$loc[i,1]/(FST$loc[i,1]+FST$loc[i,2]+FST$loc[i,3]))
	
}

FST_Table<-data.frame(names(loci))
FST_Table$FST<-FSTperLoci
write.table(FST_Table,"MexChalcoHybCP.Fst", quote=FALSE, row.names=FALSE, sep="\t" )

#Mex_Central_Plateau v Hyb_South_Guerrero
data<-read.table("HierFstatFiles/ZeaAllInfo.pmz.hierFstat.Mcp_HSG", header=TRUE, check.names=FALSE)
attach(data)
require(hierfstat)
loci<-data.frame(data[3:969])
FST<-varcomp.glob(data.frame(pop), loci)
FSTperLoci<-mat.or.vec(length(FST$loc[,1]), 1)

for (i in c(1:length(FST$loc[,1]))) {
	
	FSTperLoci[i]<-(FST$loc[i,1]/(FST$loc[i,1]+FST$loc[i,2]+FST$loc[i,3]))
	
}

FST_Table<-data.frame(names(loci))
FST_Table$FST<-FSTperLoci
write.table(FST_Table,"MexCPHybSG.Fst", quote=FALSE, row.names=FALSE, sep="\t" )

#Mex_Central_Plateau v Hyb_Central_Balsas
data<-read.table("HierFstatFiles/ZeaAllInfo.pmz.hierFstat.Mcp_HCB", header=TRUE, check.names=FALSE)
attach(data)
require(hierfstat)
loci<-data.frame(data[3:969])
FST<-varcomp.glob(data.frame(pop), loci)
FSTperLoci<-mat.or.vec(length(FST$loc[,1]), 1)

for (i in c(1:length(FST$loc[,1]))) {
	
	FSTperLoci[i]<-(FST$loc[i,1]/(FST$loc[i,1]+FST$loc[i,2]+FST$loc[i,3]))
	
}

FST_Table<-data.frame(names(loci))
FST_Table$FST<-FSTperLoci
write.table(FST_Table,"MexCPHybCB.Fst", quote=FALSE, row.names=FALSE, sep="\t" )

#Mex_Central_Plateau v Hyb_Central_Plateau
data<-read.table("HierFstatFiles/ZeaAllInfo.pmz.hierFstat.Mcp_HCP", header=TRUE, check.names=FALSE)
attach(data)
require(hierfstat)
loci<-data.frame(data[3:969])
FST<-varcomp.glob(data.frame(pop), loci)
FSTperLoci<-mat.or.vec(length(FST$loc[,1]), 1)

for (i in c(1:length(FST$loc[,1]))) {
	
	FSTperLoci[i]<-(FST$loc[i,1]/(FST$loc[i,1]+FST$loc[i,2]+FST$loc[i,3]))
	
}

FST_Table<-data.frame(names(loci))
FST_Table$FST<-FSTperLoci
write.table(FST_Table,"MexCPHybCP.Fst", quote=FALSE, row.names=FALSE, sep="\t" )

#Hyb_South_Guerrero v Hyb_Central_Balsas
data<-read.table("HierFstatFiles/ZeaAllInfo.pmz.hierFstat.HSG_HCB", header=TRUE, check.names=FALSE)
attach(data)
require(hierfstat)
loci<-data.frame(data[3:969])
FST<-varcomp.glob(data.frame(pop), loci)
FSTperLoci<-mat.or.vec(length(FST$loc[,1]), 1)

for (i in c(1:length(FST$loc[,1]))) {
	
	FSTperLoci[i]<-(FST$loc[i,1]/(FST$loc[i,1]+FST$loc[i,2]+FST$loc[i,3]))
	
}

FST_Table<-data.frame(names(loci))
FST_Table$FST<-FSTperLoci
write.table(FST_Table,"HybSGHybCB.Fst", quote=FALSE, row.names=FALSE, sep="\t" )

#Hyb_South_Guerrero v Hyb_Central_Plateau
data<-read.table("HierFstatFiles/ZeaAllInfo.pmz.hierFstat.HSG_HCP", header=TRUE, check.names=FALSE)
attach(data)
require(hierfstat)
loci<-data.frame(data[3:969])
FST<-varcomp.glob(data.frame(pop), loci)
FSTperLoci<-mat.or.vec(length(FST$loc[,1]), 1)

for (i in c(1:length(FST$loc[,1]))) {
	
	FSTperLoci[i]<-(FST$loc[i,1]/(FST$loc[i,1]+FST$loc[i,2]+FST$loc[i,3]))
	
}

FST_Table<-data.frame(names(loci))
FST_Table$FST<-FSTperLoci
write.table(FST_Table,"HybSGHybCP.Fst", quote=FALSE, row.names=FALSE, sep="\t" )

#Hyb_Central_Balsas v Hyb_Central_Plateau
data<-read.table("HierFstatFiles/ZeaAllInfo.pmz.hierFstat.HCB_HCP", header=TRUE, check.names=FALSE)
attach(data)
require(hierfstat)
loci<-data.frame(data[3:969])
FST<-varcomp.glob(data.frame(pop), loci)
FSTperLoci<-mat.or.vec(length(FST$loc[,1]), 1)

for (i in c(1:length(FST$loc[,1]))) {
	
	FSTperLoci[i]<-(FST$loc[i,1]/(FST$loc[i,1]+FST$loc[i,2]+FST$loc[i,3]))
	
}

FST_Table<-data.frame(names(loci))
FST_Table$FST<-FSTperLoci
write.table(FST_Table,"HybCBHybCP.Fst", quote=FALSE, row.names=FALSE, sep="\t" )


