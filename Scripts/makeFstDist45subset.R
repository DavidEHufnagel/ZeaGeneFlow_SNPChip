#This script is designed to create histograms of distance from 45 degree to compare to figures 
#  of hybrid groups compared to each other via Fst on some chromosomes to look for deviations from
#  a 45degree straight line to look for differential architectures of hybridization with mex alleles.
#  This version contains only a subset of all plots
#  Created by David E. Hufnagel

library(Hmisc)

#Calculates the euclidian distance from 2 points.  Taken from 
GetEucDist = function(x1, x2){
	sqrt(sum((x1 - x2) ^ 2))	
} 

#Calculates the distance from a point to the 45degree line and makes the
#  value negative if it lies below the 45degree
GetDist45 = function(x,y) {
	e = GetEucDist(c(x,y), c(y,x)) #get dist between point and reflection across 45degree
	dist45 = e / 2.0               #get distance to 45degree
	if(y<x){                       #make value negative if point is below 45degree
		dist45 = dist45 * -1.0
    }    
    return(dist45)
}

#Calculates dist45 for all x and y values
GetAllDist45 = function(all_x, all_y){
	all45s = c()
	for (i in 1:length(all_x)) {
		#y = parv1[i,]$fstCP 
		y = all_y[i]
		#x = parv1[i,]$fstCB
		x = all_x[i]
		dist45 = GetDist45(x,y) 
		all45s = c(all45s, dist45)
	}
	return(all45s)
}

#import data
#set working directory here
mexData = read.table("mexVhybFsts.txt", header=TRUE)

#subset data by chromosome and taxonomy
#Mex
mex1 = subset(mexData, chromo==1)
mex2 = subset(mexData, chromo==2)
mex4 = subset(mexData, chromo==4)
mex10 = subset(mexData, chromo==10)


#plot data
#CPvCB parv
pdf("subsetDist45.pdf", width=7.2, height=5.43)
par(mar=c(4,4,2,1), mfrow=c(3,4), mgp=c(1.5,.5,0), cex.main=.8, cex.lab=.9, cex.axis=.9)

#CPvCB mex
all45sMex1_CPvCB = GetAllDist45(mex1$fstCB,mex1$fstCP)
dens = density(all45sMex1_CPvCB)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CPvCB mex Chr1", col="firebrick4")
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sMex1_CPvCB))) #plot sum of all values for a summary statistic

all45sMex2_CPvCB = GetAllDist45(mex2$fstCB,mex2$fstCP)
dens = density(all45sMex2_CPvCB)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CPvCB mex Chr2", col="firebrick4")
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sMex2_CPvCB))) #plot sum of all values for a summary statistic

all45sMex4_CPvCB = GetAllDist45(mex4$fstCB,mex4$fstCP)
dens = density(all45sMex4_CPvCB)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CPvCB mex Chr4", col="firebrick4")
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sMex4_CPvCB))) #plot sum of all values for a summary statistic

all45sMex10_CPvCB = GetAllDist45(mex10$fstCB,mex10$fstCP)
dens = density(all45sMex10_CPvCB)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CPvCB mex Chr10", col="firebrick4")
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sMex10_CPvCB))) #plot sum of all values for a summary statistic

#CPvSG mex
all45sMex1_CPvSG = GetAllDist45(mex1$fstSG,mex1$fstCP)
dens = density(all45sMex1_CPvSG)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CPvSG mex Chr1", col="firebrick4")
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sMex1_CPvSG))) #plot sum of all values for a summary statistic

all45sMex2_CPvSG = GetAllDist45(mex2$fstSG,mex2$fstCP)
dens = density(all45sMex2_CPvSG)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CPvSG mex Chr2", col="firebrick4")
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sMex2_CPvSG))) #plot sum of all values for a summary statistic

all45sMex4_CPvSG = GetAllDist45(mex4$fstSG,mex4$fstCP)
dens = density(all45sMex4_CPvSG)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CPvSG mex Chr4", col="firebrick4")
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sMex4_CPvSG))) #plot sum of all values for a summary statistic

all45sMex10_CPvSG = GetAllDist45(mex10$fstSG,mex10$fstCP)
dens = density(all45sMex10_CPvSG)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CPvSG mex Chr10", col="firebrick4")
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sMex10_CPvSG))) #plot sum of all values for a summary statistic

#CBvSG mex
all45sMex1_CBvSG = GetAllDist45(mex1$fstSG,mex1$fstCB)
dens = density(all45sMex1_CBvSG)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CBvSG mex Chr1", col="firebrick4")
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sMex1_CBvSG))) #plot sum of all values for a summary statistic

all45sMex2_CBvSG = GetAllDist45(mex2$fstSG,mex2$fstCB)
dens = density(all45sMex2_CBvSG)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CBvSG mex Chr2", col="firebrick4")
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sMex2_CBvSG))) #plot sum of all values for a summary statistic

all45sMex4_CBvSG = GetAllDist45(mex4$fstSG,mex4$fstCB)
dens = density(all45sMex4_CBvSG)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CBvSG mex Chr4", col="firebrick4")
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sMex4_CBvSG))) #plot sum of all values for a summary statistic

all45sMex10_CBvSG = GetAllDist45(mex10$fstSG,mex10$fstCB)
dens = density(all45sMex10_CBvSG)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CBvSG mex Chr10", col="firebrick4")
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sMex10_CBvSG))) #plot sum of all values for a summary statistic
dev.off()





