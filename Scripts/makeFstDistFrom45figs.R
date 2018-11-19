#This script is designed to create histograms of distance from 45 degree to compare to figures 
#  of hybrid groups compared to each other via Fst on all chromosomes to look for deviations from
#  a 45degree straight line to look for differential architectures of hybridization.
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
parvData = read.table("parvVhybFsts.txt", header=TRUE)
mexData = read.table("mexVhybFsts.txt", header=TRUE)

#subset data by chromosome and taxonomy
#Parv
parv1 = subset(parvData, chromo==1)
parv2 = subset(parvData, chromo==2)
parv3 = subset(parvData, chromo==3)
parv4 = subset(parvData, chromo==4)
parv5 = subset(parvData, chromo==5)
parv6 = subset(parvData, chromo==6)
parv7 = subset(parvData, chromo==7)
parv8 = subset(parvData, chromo==8)
parv9 = subset(parvData, chromo==9)
parv10 = subset(parvData, chromo==10)

#Mex
mex1 = subset(mexData, chromo==1)
mex2 = subset(mexData, chromo==2)
mex3 = subset(mexData, chromo==3)
mex4 = subset(mexData, chromo==4)
mex5 = subset(mexData, chromo==5)
mex6 = subset(mexData, chromo==6)
mex7 = subset(mexData, chromo==7)
mex8 = subset(mexData, chromo==8)
mex9 = subset(mexData, chromo==9)
mex10 = subset(mexData, chromo==10)

#plot data
#CPvCB parv
pdf("AllPlots_dist45.pdf", width=18, height=10.86)
par(mar=c(4,4,2,1), mfrow=c(6,10), mgp=c(1.5,.5,0), cex.main=.8, cex.lab=.9, cex.axis=.9)

all45sParv1_CPvCB = GetAllDist45(parv1$fstCB,parv1$fstCP)
dens = density(all45sParv1_CPvCB)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CPvCB parv Chr1", col="blue2")
minor.tick(nx=2, ny=5, tick.ratio=.45)
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sParv1_CPvCB))) #plot sum of all values for a summary statistic

all45sParv2_CPvCB = GetAllDist45(parv2$fstCB,parv2$fstCP)
dens = density(all45sParv2_CPvCB)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CPvCB parv Chr2", col="blue2")
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sParv2_CPvCB))) #plot sum of all values for a summary statistic

all45sParv3_CPvCB = GetAllDist45(parv3$fstCB,parv3$fstCP)
dens = density(all45sParv3_CPvCB)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CPvCB parv Chr3", col="blue2")
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sParv3_CPvCB))) #plot sum of all values for a summary statistic

all45sParv4_CPvCB = GetAllDist45(parv4$fstCB,parv4$fstCP)
dens = density(all45sParv4_CPvCB)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CPvCB parv Chr4", col="blue2")
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sParv4_CPvCB))) #plot sum of all values for a summary statistic

all45sParv5_CPvCB = GetAllDist45(parv5$fstCB,parv5$fstCP)
dens = density(all45sParv5_CPvCB)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CPvCB parv Chr5", col="blue2")
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sParv5_CPvCB))) #plot sum of all values for a summary statistic

all45sParv6_CPvCB = GetAllDist45(parv6$fstCB,parv6$fstCP)
dens = density(all45sParv6_CPvCB)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CPvCB parv Chr6", col="blue2")
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sParv6_CPvCB))) #plot sum of all values for a summary statistic

all45sParv7_CPvCB = GetAllDist45(parv7$fstCB,parv7$fstCP)
dens = density(all45sParv7_CPvCB)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CPvCB parv Chr7", col="blue2")
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sParv7_CPvCB))) #plot sum of all values for a summary statistic

all45sParv8_CPvCB = GetAllDist45(parv8$fstCB,parv8$fstCP)
dens = density(all45sParv8_CPvCB)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CPvCB parv Chr8", col="blue2")
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sParv8_CPvCB))) #plot sum of all values for a summary statistic

all45sParv9_CPvCB = GetAllDist45(parv9$fstCB,parv9$fstCP)
dens = density(all45sParv9_CPvCB)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CPvCB parv Chr9", col="blue2")
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sParv9_CPvCB))) #plot sum of all values for a summary statistic

all45sParv10_CPvCB = GetAllDist45(parv10$fstCB,parv10$fstCP)
dens = density(all45sParv10_CPvCB)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CPvCB parv Chr10", col="blue2")
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sParv10_CPvCB))) #plot sum of all values for a summary statistic

#CPvSG parv
all45sParv1_CPvSG = GetAllDist45(parv1$fstSG,parv1$fstCP)
dens = density(all45sParv1_CPvSG)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CPvSG parv Chr1", col="blue2")
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sParv1_CPvSG))) #plot sum of all values for a summary statistic

all45sParv2_CPvSG = GetAllDist45(parv2$fstSG,parv2$fstCP)
dens = density(all45sParv2_CPvSG)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CPvSG parv Chr2", col="blue2")
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sParv2_CPvSG))) #plot sum of all values for a summary statistic

all45sParv3_CPvSG = GetAllDist45(parv3$fstSG,parv3$fstCP)
dens = density(all45sParv3_CPvSG)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CPvSG parv Chr3", col="blue2")
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sParv3_CPvSG))) #plot sum of all values for a summary statistic

all45sParv4_CPvSG = GetAllDist45(parv4$fstSG,parv4$fstCP)
dens = density(all45sParv4_CPvSG)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CPvSG parv Chr4", col="blue2")
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sParv4_CPvSG))) #plot sum of all values for a summary statistic

all45sParv5_CPvSG = GetAllDist45(parv5$fstSG,parv5$fstCP)
dens = density(all45sParv5_CPvSG)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CPvSG parv Chr5", col="blue2")
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sParv5_CPvSG))) #plot sum of all values for a summary statistic

all45sParv6_CPvSG = GetAllDist45(parv6$fstSG,parv6$fstCP)
dens = density(all45sParv6_CPvSG)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CPvSG parv Chr6", col="blue2")
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sParv6_CPvSG))) #plot sum of all values for a summary statistic

all45sParv7_CPvSG = GetAllDist45(parv7$fstSG,parv7$fstCP)
dens = density(all45sParv7_CPvSG)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CPvSG parv Chr7", col="blue2")
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sParv7_CPvSG))) #plot sum of all values for a summary statistic

all45sParv8_CPvSG = GetAllDist45(parv8$fstSG,parv8$fstCP)
dens = density(all45sParv8_CPvSG)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CPvSG parv Chr8", col="blue2")
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sParv8_CPvSG))) #plot sum of all values for a summary statistic

all45sParv9_CPvSG = GetAllDist45(parv9$fstSG,parv9$fstCP)
dens = density(all45sParv9_CPvSG)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CPvSG parv Chr9", col="blue2")
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sParv9_CPvSG))) #plot sum of all values for a summary statistic

all45sParv10_CPvSG = GetAllDist45(parv10$fstSG,parv10$fstCP)
dens = density(all45sParv10_CPvSG)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CPvSG parv Chr10", col="blue2")
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sParv10_CPvSG))) #plot sum of all values for a summary statistic

#CBvSG parv
all45sParv1_CBvSG = GetAllDist45(parv1$fstSG,parv1$fstCB)
dens = density(all45sParv1_CBvSG)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CBvSG parv Chr1", col="blue2")
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sParv1_CBvSG))) #plot sum of all values for a summary statistic

all45sParv2_CBvSG = GetAllDist45(parv2$fstSG,parv2$fstCB)
dens = density(all45sParv2_CBvSG)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CBvSG parv Chr2", col="blue2")
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sParv2_CBvSG))) #plot sum of all values for a summary statistic

all45sParv3_CBvSG = GetAllDist45(parv3$fstSG,parv3$fstCB)
dens = density(all45sParv3_CBvSG)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CBvSG parv Chr3", col="blue2")
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sParv3_CBvSG))) #plot sum of all values for a summary statistic

all45sParv4_CBvSG = GetAllDist45(parv4$fstSG,parv4$fstCB)
dens = density(all45sParv4_CBvSG)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CBvSG parv Chr4", col="blue2")
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sParv4_CBvSG))) #plot sum of all values for a summary statistic

all45sParv5_CBvSG = GetAllDist45(parv5$fstSG,parv5$fstCB)
dens = density(all45sParv5_CBvSG)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CBvSG parv Chr5", col="blue2")
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sParv5_CBvSG))) #plot sum of all values for a summary statistic

all45sParv6_CBvSG = GetAllDist45(parv6$fstSG,parv6$fstCB)
dens = density(all45sParv6_CBvSG)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CBvSG parv Chr6", col="blue2")
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sParv6_CBvSG))) #plot sum of all values for a summary statistic

all45sParv7_CBvSG = GetAllDist45(parv7$fstSG,parv7$fstCB)
dens = density(all45sParv7_CBvSG)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CBvSG parv Chr7", col="blue2")
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sParv7_CBvSG))) #plot sum of all values for a summary statistic

all45sParv8_CBvSG = GetAllDist45(parv8$fstSG,parv8$fstCB)
dens = density(all45sParv8_CBvSG)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CBvSG parv Chr8", col="blue2")
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sParv8_CBvSG))) #plot sum of all values for a summary statistic

all45sParv9_CBvSG = GetAllDist45(parv9$fstSG,parv9$fstCB)
dens = density(all45sParv9_CBvSG)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CBvSG parv Chr9", col="blue2")
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sParv9_CBvSG))) #plot sum of all values for a summary statistic

all45sParv10_CBvSG = GetAllDist45(parv10$fstSG,parv10$fstCB)
dens = density(all45sParv10_CBvSG)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CBvSG parv Chr10", col="blue2")
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sParv10_CBvSG))) #plot sum of all values for a summary statistic

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

all45sMex3_CPvCB = GetAllDist45(mex3$fstCB,mex3$fstCP)
dens = density(all45sMex3_CPvCB)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CPvCB mex Chr3", col="firebrick4")
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sMex3_CPvCB))) #plot sum of all values for a summary statistic

all45sMex4_CPvCB = GetAllDist45(mex4$fstCB,mex4$fstCP)
dens = density(all45sMex4_CPvCB)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CPvCB mex Chr4", col="firebrick4")
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sMex4_CPvCB))) #plot sum of all values for a summary statistic

all45sMex5_CPvCB = GetAllDist45(mex5$fstCB,mex5$fstCP)
dens = density(all45sMex5_CPvCB)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CPvCB mex Chr5", col="firebrick4")
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sMex5_CPvCB))) #plot sum of all values for a summary statistic

all45sMex6_CPvCB = GetAllDist45(mex6$fstCB,mex6$fstCP)
dens = density(all45sMex6_CPvCB)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CPvCB mex Chr6", col="firebrick4")
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sMex6_CPvCB))) #plot sum of all values for a summary statistic

all45sMex7_CPvCB = GetAllDist45(mex7$fstCB,mex7$fstCP)
dens = density(all45sMex7_CPvCB)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CPvCB mex Chr7", col="firebrick4")
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sMex7_CPvCB))) #plot sum of all values for a summary statistic

all45sMex8_CPvCB = GetAllDist45(mex8$fstCB,mex8$fstCP)
dens = density(all45sMex8_CPvCB)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CPvCB mex Chr8", col="firebrick4")
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sMex8_CPvCB))) #plot sum of all values for a summary statistic

all45sMex9_CPvCB = GetAllDist45(mex9$fstCB,mex9$fstCP)
dens = density(all45sMex9_CPvCB)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CPvCB mex Chr9", col="firebrick4")
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sMex9_CPvCB))) #plot sum of all values for a summary statistic

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

all45sMex3_CPvSG = GetAllDist45(mex3$fstSG,mex3$fstCP)
dens = density(all45sMex3_CPvSG)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CPvSG mex Chr3", col="firebrick4")
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sMex3_CPvSG))) #plot sum of all values for a summary statistic

all45sMex4_CPvSG = GetAllDist45(mex4$fstSG,mex4$fstCP)
dens = density(all45sMex4_CPvSG)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CPvSG mex Chr4", col="firebrick4")
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sMex4_CPvSG))) #plot sum of all values for a summary statistic

all45sMex5_CPvSG = GetAllDist45(mex5$fstSG,mex5$fstCP)
dens = density(all45sMex5_CPvSG)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CPvSG mex Chr5", col="firebrick4")
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sMex5_CPvSG))) #plot sum of all values for a summary statistic

all45sMex6_CPvSG = GetAllDist45(mex6$fstSG,mex6$fstCP)
dens = density(all45sMex6_CPvSG)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CPvSG mex Chr6", col="firebrick4")
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sMex6_CPvSG))) #plot sum of all values for a summary statistic

all45sMex7_CPvSG = GetAllDist45(mex7$fstSG,mex7$fstCP)
dens = density(all45sMex7_CPvSG)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CPvSG mex Chr7", col="firebrick4")
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sMex7_CPvSG))) #plot sum of all values for a summary statistic

all45sMex8_CPvSG = GetAllDist45(mex8$fstSG,mex8$fstCP)
dens = density(all45sMex8_CPvSG)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CPvSG mex Chr8", col="firebrick4")
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sMex8_CPvSG))) #plot sum of all values for a summary statistic

all45sMex9_CPvSG = GetAllDist45(mex9$fstSG,mex9$fstCP)
dens = density(all45sMex9_CPvSG)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CPvSG mex Chr9", col="firebrick4")
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sMex9_CPvSG))) #plot sum of all values for a summary statistic

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

all45sMex3_CBvSG = GetAllDist45(mex3$fstSG,mex3$fstCB)
dens = density(all45sMex3_CBvSG)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CBvSG mex Chr3", col="firebrick4")
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sMex3_CBvSG))) #plot sum of all values for a summary statistic

all45sMex4_CBvSG = GetAllDist45(mex4$fstSG,mex4$fstCB)
dens = density(all45sMex4_CBvSG)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CBvSG mex Chr4", col="firebrick4")
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sMex4_CBvSG))) #plot sum of all values for a summary statistic

all45sMex5_CBvSG = GetAllDist45(mex5$fstSG,mex5$fstCB)
dens = density(all45sMex5_CBvSG)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CBvSG mex Chr5", col="firebrick4")
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sMex5_CBvSG))) #plot sum of all values for a summary statistic

all45sMex6_CBvSG = GetAllDist45(mex6$fstSG,mex6$fstCB)
dens = density(all45sMex6_CBvSG)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CBvSG mex Chr6", col="firebrick4")
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sMex6_CBvSG))) #plot sum of all values for a summary statistic

all45sMex7_CBvSG = GetAllDist45(mex7$fstSG,mex7$fstCB)
dens = density(all45sMex7_CBvSG)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CBvSG mex Chr7", col="firebrick4")
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sMex7_CBvSG))) #plot sum of all values for a summary statistic

all45sMex8_CBvSG = GetAllDist45(mex8$fstSG,mex8$fstCB)
dens = density(all45sMex8_CBvSG)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CBvSG mex Chr8", col="firebrick4")
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sMex8_CBvSG))) #plot sum of all values for a summary statistic

all45sMex9_CBvSG = GetAllDist45(mex9$fstSG,mex9$fstCB)
dens = density(all45sMex9_CBvSG)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CBvSG mex Chr9", col="firebrick4")
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sMex9_CBvSG))) #plot sum of all values for a summary statistic

all45sMex10_CBvSG = GetAllDist45(mex10$fstSG,mex10$fstCB)
dens = density(all45sMex10_CBvSG)
plot(dens, xlim=c(-0.72, 0.72), ylim=c(0,18), xlab="Distance from 45deg line", main="Density of point distance from\n 45deg line for CBvSG mex Chr10", col="firebrick4")
lines(x=c(0,0), y=c(0,100)) #make a straight line at 0
text(x=.45, y=16.8, cex=.8, labels=sprintf("avg: %.3f",mean(all45sMex10_CBvSG))) #plot sum of all values for a summary statistic
dev.off()






