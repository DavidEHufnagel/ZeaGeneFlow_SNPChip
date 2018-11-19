#This script is designed to create figures of hybrid groups compared to each other 
#via Fst on all chromosomes to look for differential architectures of hybridization.
#Created by David E. Hufnagel

#define functions
#This function extracts the p-value from an lm summary object.  It is taken from http://stackoverflow.com/questions/5587676/pull-out-p-values-and-r-squared-from-a-linear-regression
GetPval = function (modelobject) {
    if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
    f <- summary(modelobject)$fstatistic
    p <- pf(f[1],f[2],f[3],lower.tail=F)
    attributes(p) <- NULL
    return(p)
}

#This function creates a y=mx+b style formula from an lm summary object. 
#my version
GetForm = function (model) {
	sprintf("y = %.4f * %s + %.4f", coefficients(model)[-1], names(coefficients(model)[-1]), coefficients(model)[1]) 
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
pdf("AllplotsInR.pdf", width=18, height=10.86)
par(mar=c(4,4,2,1), mfrow=c(6,10), mgp=c(1.5,.3,0), cex.main=1.4, cex.lab=1.5, cex.axis=1.6, cex=.32, ps=10.5)
#CPvCB 1 parv
fit = lm(fstCP~fstCB, data=parv1) #do a linear regression
plot(parv1$fstCB, parv1$fstCP, col="blue2", ylim=c(0,1), xlim=c(0,1), ylab="HybCP vs. ParvBal Fst", xlab="HybCB vs. ParvBal Fst", main="Locus-by-locus Fst comparisons for Chormosome 1", lwd=.5)
lines(x=c(0,1), y=c(0,1), lwd=.6) #45 degree line
abline(fit, col="blue2", lwd=.6) #fit line
p = GetPval(fit); text(x=.65,y=.97, labels=sprintf("p-value: %.8f",p), cex=1.05) #fit line p-val
form = GetForm(fit); text(x=.25,y=1.01, labels=form)                             #fit line formula
r2 = summary(fit)$r.squared; text(x=.59,y=1.01, labels=sprintf("R^2: %.4f",r2))  #fite line R^2

fit = lm(fstCP~fstCB, data=parv2) #do a linear regression
plot(parv2$fstCB, parv2$fstCP, col="blue2", ylim=c(0,1), xlim=c(0,1), ylab="HybCP vs. ParvBal Fst", xlab="HybCB vs. ParvBal Fst", main="Locus-by-locus Fst comparisons for Chormosome 2", lwd=.5)
lines(x=c(0,1), y=c(0,1), lwd=0.6)
abline(fit, col="blue2", lwd=0.6)
p = GetPval(fit); text(x=.65,y=.97, labels=sprintf("p-value: %.8f",p), cex=1.05) #fit line p-val
form = GetForm(fit); text(x=.25,y=1.01, labels=form)                             #fit line formula
r2 = summary(fit)$r.squared; text(x=.59,y=1.01, labels=sprintf("R^2: %.4f",r2))  #fite line R^2

fit = lm(fstCP~fstCB, data=parv3) #do a linear regression
plot(parv3$fstCB, parv3$fstCP, col="blue2", ylim=c(0,1), xlim=c(0,1), ylab="HybCP vs. ParvBal Fst", xlab="HybCB vs. ParvBal Fst", main="Locus-by-locus Fst comparisons for Chormosome 3", lwd=.5)
lines(x=c(0,1), y=c(0,1), lwd=0.6)
abline(fit, col="blue2", lwd=0.6)
p = GetPval(fit); text(x=.65,y=.97, labels=sprintf("p-value: %.8f",p), cex=1.05) #fit line p-val
form = GetForm(fit); text(x=.25,y=1.01, labels=form)                             #fit line formula
r2 = summary(fit)$r.squared; text(x=.59,y=1.01, labels=sprintf("R^2: %.4f",r2))  #fite line R^2

fit = lm(fstCP~fstCB, data=parv4) #do a linear regression
plot(parv4$fstCB, parv4$fstCP, col="blue2", ylim=c(0,1), xlim=c(0,1), ylab="HybCP vs. ParvBal Fst", xlab="HybCB vs. ParvBal Fst", main="Locus-by-locus Fst comparisons for Chormosome 4", lwd=.5)
lines(x=c(0,1), y=c(0,1), lwd=0.6)
abline(fit, col="blue2", lwd=0.6)
p = GetPval(fit); text(x=.65,y=.97, labels=sprintf("p-value: %.8f",p), cex=1.05) #fit line p-val
form = GetForm(fit); text(x=.25,y=1.01, labels=form)                             #fit line formula
r2 = summary(fit)$r.squared; text(x=.59,y=1.01, labels=sprintf("R^2: %.4f",r2))  #fite line R^2

fit = lm(fstCP~fstCB, data=parv5) #do a linear regression
plot(parv5$fstCB, parv5$fstCP, col="blue2", ylim=c(0,1), xlim=c(0,1), ylab="HybCP vs. ParvBal Fst", xlab="HybCB vs. ParvBal Fst", main="Locus-by-locus Fst comparisons for Chormosome 5", lwd=.5)
lines(x=c(0,1), y=c(0,1), lwd=0.6)
abline(fit, col="blue2", lwd=0.6)
p = GetPval(fit); text(x=.65,y=.97, labels=sprintf("p-value: %.8f",p), cex=1.05) #fit line p-val
form = GetForm(fit); text(x=.25,y=1.01, labels=form)                             #fit line formula
r2 = summary(fit)$r.squared; text(x=.59,y=1.01, labels=sprintf("R^2: %.4f",r2))  #fite line R^2

fit = lm(fstCP~fstCB, data=parv6) #do a linear regression
plot(parv6$fstCB, parv6$fstCP, col="blue2", ylim=c(0,1), xlim=c(0,1), ylab="HybCP vs. ParvBal Fst", xlab="HybCB vs. ParvBal Fst", main="Locus-by-locus Fst comparisons for Chormosome 6", lwd=.5)
lines(x=c(0,1), y=c(0,1), lwd=0.6)
abline(fit, col="blue2", lwd=0.6)
p = GetPval(fit); text(x=.65,y=.97, labels=sprintf("p-value: %.8f",p), cex=1.05) #fit line p-val
form = GetForm(fit); text(x=.25,y=1.01, labels=form)                             #fit line formula
r2 = summary(fit)$r.squared; text(x=.59,y=1.01, labels=sprintf("R^2: %.4f",r2))  #fite line R^2

fit = lm(fstCP~fstCB, data=parv7) #do a linear regression
plot(parv7$fstCB, parv7$fstCP, col="blue2", ylim=c(0,1), xlim=c(0,1), ylab="HybCP vs. ParvBal Fst", xlab="HybCB vs. ParvBal Fst", main="Locus-by-locus Fst comparisons for Chormosome 7", lwd=.5)
lines(x=c(0,1), y=c(0,1), lwd=0.6)
abline(fit, col="blue2", lwd=0.6)
p = GetPval(fit); text(x=.65,y=.97, labels=sprintf("p-value: %.8f",p), cex=1.05) #fit line p-val
form = GetForm(fit); text(x=.25,y=1.01, labels=form)                             #fit line formula
r2 = summary(fit)$r.squared; text(x=.59,y=1.01, labels=sprintf("R^2: %.4f",r2))  #fite line R^2

fit = lm(fstCP~fstCB, data=parv8) #do a linear regression
plot(parv8$fstCB, parv8$fstCP, col="blue2", ylim=c(0,1), xlim=c(0,1), ylab="HybCP vs. ParvBal Fst", xlab="HybCB vs. ParvBal Fst", main="Locus-by-locus Fst comparisons for Chormosome 8", lwd=.5)
lines(x=c(0,1), y=c(0,1), lwd=0.6)
abline(fit, col="blue2", lwd=0.6)
p = GetPval(fit); text(x=.65,y=.97, labels=sprintf("p-value: %.8f",p), cex=1.05) #fit line p-val
form = GetForm(fit); text(x=.25,y=1.01, labels=form)                             #fit line formula
r2 = summary(fit)$r.squared; text(x=.59,y=1.01, labels=sprintf("R^2: %.4f",r2))  #fite line R^2

fit = lm(fstCP~fstCB, data=parv9) #do a linear regression
plot(parv9$fstCB, parv9$fstCP, col="blue2", ylim=c(0,1), xlim=c(0,1), ylab="HybCP vs. ParvBal Fst", xlab="HybCB vs. ParvBal Fst", main="Locus-by-locus Fst comparisons for Chormosome 9", lwd=.5)
lines(x=c(0,1), y=c(0,1), lwd=0.6)
abline(fit, col="blue2", lwd=0.6)
p = GetPval(fit); text(x=.65,y=.97, labels=sprintf("p-value: %.8f",p), cex=1.05) #fit line p-val
form = GetForm(fit); text(x=.25,y=1.01, labels=form)                             #fit line formula
r2 = summary(fit)$r.squared; text(x=.59,y=1.01, labels=sprintf("R^2: %.4f",r2))  #fite line R^2

fit = lm(fstCP~fstCB, data=parv10) #do a linear regression
plot(parv10$fstCB, parv10$fstCP, col="blue2", ylim=c(0,1), xlim=c(0,1), ylab="HybCP vs. ParvBal Fst", xlab="HybCB vs. ParvBal Fst", main="Locus-by-locus Fst comparisons for Chormosome 10", lwd=.5)
lines(x=c(0,1), y=c(0,1), lwd=0.6)
abline(fit, col="blue2", lwd=0.6)
p = GetPval(fit); text(x=.65,y=.97, labels=sprintf("p-value: %.8f",p), cex=1.05) #fit line p-val
form = GetForm(fit); text(x=.25,y=1.01, labels=form)                             #fit line formula
r2 = summary(fit)$r.squared; text(x=.59,y=1.01, labels=sprintf("R^2: %.4f",r2))  #fite line R^2

#CPvSG parv
fit = lm(fstCP~fstSG, data=parv1) #do a linear regression
plot(parv1$fstSG, parv1$fstCP, col="blue2", ylim=c(0,1), xlim=c(0,1), ylab="HybCP vs. ParvBal Fst", xlab="HybSG vs. ParvBal Fst", main="Locus-by-locus Fst comparisons for Chormosome 1", lwd=.5)
lines(x=c(0,1), y=c(0,1), lwd=0.6)
abline(fit, col="blue2", lwd=0.6)
p = GetPval(fit); text(x=.65,y=.97, labels=sprintf("p-value: %.8f",p), cex=1.05) #fit line p-val
form = GetForm(fit); text(x=.25,y=1.01, labels=form)                             #fit line formula
r2 = summary(fit)$r.squared; text(x=.59,y=1.01, labels=sprintf("R^2: %.4f",r2))  #fite line R^2

fit = lm(fstCP~fstSG, data=parv2) #do a linear regression
plot(parv2$fstSG, parv2$fstCP, col="blue2", ylim=c(0,1), xlim=c(0,1), ylab="HybCP vs. ParvBal Fst", xlab="HybSG vs. ParvBal Fst", main="Locus-by-locus Fst comparisons for Chormosome 2", lwd=.5)
lines(x=c(0,1), y=c(0,1), lwd=0.6)
abline(fit, col="blue2", lwd=0.6)
p = GetPval(fit); text(x=.65,y=.97, labels=sprintf("p-value: %.8f",p), cex=1.05) #fit line p-val
form = GetForm(fit); text(x=.25,y=1.01, labels=form)                             #fit line formula
r2 = summary(fit)$r.squared; text(x=.59,y=1.01, labels=sprintf("R^2: %.4f",r2))  #fite line R^2

fit = lm(fstCP~fstSG, data=parv3) #do a linear regression
plot(parv3$fstSG, parv3$fstCP, col="blue2", ylim=c(0,1), xlim=c(0,1), ylab="HybCP vs. ParvBal Fst", xlab="HybSG vs. ParvBal Fst", main="Locus-by-locus Fst comparisons for Chormosome 3", lwd=.5)
lines(x=c(0,1), y=c(0,1), lwd=0.6)
abline(fit, col="blue2", lwd=0.6)
p = GetPval(fit); text(x=.65,y=.97, labels=sprintf("p-value: %.8f",p), cex=1.05) #fit line p-val
form = GetForm(fit); text(x=.25,y=1.01, labels=form)                             #fit line formula
r2 = summary(fit)$r.squared; text(x=.59,y=1.01, labels=sprintf("R^2: %.4f",r2))  #fite line R^2

fit = lm(fstCP~fstSG, data=parv4) #do a linear regression
plot(parv4$fstSG, parv4$fstCP, col="blue2", ylim=c(0,1), xlim=c(0,1), ylab="HybCP vs. ParvBal Fst", xlab="HybSG vs. ParvBal Fst", main="Locus-by-locus Fst comparisons for Chormosome 4", lwd=.5)
lines(x=c(0,1), y=c(0,1), lwd=0.6)
abline(fit, col="blue2", lwd=0.6)
p = GetPval(fit); text(x=.65,y=.97, labels=sprintf("p-value: %.8f",p), cex=1.05) #fit line p-val
form = GetForm(fit); text(x=.25,y=1.01, labels=form)                             #fit line formula
r2 = summary(fit)$r.squared; text(x=.59,y=1.01, labels=sprintf("R^2: %.4f",r2))  #fite line R^2

fit = lm(fstCP~fstSG, data=parv5) #do a linear regression
plot(parv5$fstSG, parv5$fstCP, col="blue2", ylim=c(0,1), xlim=c(0,1), ylab="HybCP vs. ParvBal Fst", xlab="HybSG vs. ParvBal Fst", main="Locus-by-locus Fst comparisons for Chormosome 5", lwd=.5)
lines(x=c(0,1), y=c(0,1), lwd=0.6)
abline(fit, col="blue2", lwd=0.6)
p = GetPval(fit); text(x=.65,y=.97, labels=sprintf("p-value: %.8f",p), cex=1.05) #fit line p-val
form = GetForm(fit); text(x=.25,y=1.01, labels=form)                             #fit line formula
r2 = summary(fit)$r.squared; text(x=.59,y=1.01, labels=sprintf("R^2: %.4f",r2))  #fite line R^2

fit = lm(fstCP~fstSG, data=parv6) #do a linear regression
plot(parv6$fstSG, parv6$fstCP, col="blue2", ylim=c(0,1), xlim=c(0,1), ylab="HybCP vs. ParvBal Fst", xlab="HybSG vs. ParvBal Fst", main="Locus-by-locus Fst comparisons for Chormosome 6", lwd=.5)
lines(x=c(0,1), y=c(0,1), lwd=0.6)
abline(fit, col="blue2", lwd=0.6)
p = GetPval(fit); text(x=.65,y=.97, labels=sprintf("p-value: %.8f",p), cex=1.05) #fit line p-val
form = GetForm(fit); text(x=.25,y=1.01, labels=form)                             #fit line formula
r2 = summary(fit)$r.squared; text(x=.59,y=1.01, labels=sprintf("R^2: %.4f",r2))  #fite line R^2

fit = lm(fstCP~fstSG, data=parv7) #do a linear regression
plot(parv7$fstSG, parv7$fstCP, col="blue2", ylim=c(0,1), xlim=c(0,1), ylab="HybCP vs. ParvBal Fst", xlab="HybSG vs. ParvBal Fst", main="Locus-by-locus Fst comparisons for Chormosome 7", lwd=.5)
lines(x=c(0,1), y=c(0,1), lwd=0.6)
abline(fit, col="blue2", lwd=0.6)
p = GetPval(fit); text(x=.65,y=.97, labels=sprintf("p-value: %.8f",p), cex=1.05) #fit line p-val
form = GetForm(fit); text(x=.25,y=1.01, labels=form)                             #fit line formula
r2 = summary(fit)$r.squared; text(x=.59,y=1.01, labels=sprintf("R^2: %.4f",r2))  #fite line R^2

fit = lm(fstCP~fstSG, data=parv8) #do a linear regression
plot(parv8$fstSG, parv8$fstCP, col="blue2", ylim=c(0,1), xlim=c(0,1), ylab="HybCP vs. ParvBal Fst", xlab="HybSG vs. ParvBal Fst", main="Locus-by-locus Fst comparisons for Chormosome 8", lwd=.5)
lines(x=c(0,1), y=c(0,1), lwd=0.6)
abline(fit, col="blue2", lwd=0.6)
p = GetPval(fit); text(x=.65,y=.97, labels=sprintf("p-value: %.8f",p), cex=1.05) #fit line p-val
form = GetForm(fit); text(x=.25,y=1.01, labels=form)                             #fit line formula
r2 = summary(fit)$r.squared; text(x=.59,y=1.01, labels=sprintf("R^2: %.4f",r2))  #fite line R^2

fit = lm(fstCP~fstSG, data=parv9) #do a linear regression
plot(parv9$fstSG, parv9$fstCP, col="blue2", ylim=c(0,1), xlim=c(0,1), ylab="HybCP vs. ParvBal Fst", xlab="HybSG vs. ParvBal Fst", main="Locus-by-locus Fst comparisons for Chormosome 9", lwd=.5)
lines(x=c(0,1), y=c(0,1), lwd=0.6)
abline(fit, col="blue2", lwd=0.6)
p = GetPval(fit); text(x=.65,y=.97, labels=sprintf("p-value: %.8f",p), cex=1.05) #fit line p-val
form = GetForm(fit); text(x=.25,y=1.01, labels=form)                             #fit line formula
r2 = summary(fit)$r.squared; text(x=.59,y=1.01, labels=sprintf("R^2: %.4f",r2))  #fite line R^2

fit = lm(fstCP~fstSG, data=parv10) #do a linear regression
plot(parv10$fstSG, parv10$fstCP, col="blue2", ylim=c(0,1), xlim=c(0,1), ylab="HybCP vs. ParvBal Fst", xlab="HybSG vs. ParvBal Fst", main="Locus-by-locus Fst comparisons for Chormosome 10", lwd=.5)
lines(x=c(0,1), y=c(0,1), lwd=0.6)
abline(fit, col="blue2", lwd=0.6)
p = GetPval(fit); text(x=.65,y=.97, labels=sprintf("p-value: %.8f",p), cex=1.05) #fit line p-val
form = GetForm(fit); text(x=.25,y=1.01, labels=form)                             #fit line formula
r2 = summary(fit)$r.squared; text(x=.59,y=1.01, labels=sprintf("R^2: %.4f",r2))  #fite line R^2

#CBvSG parv
fit = lm(fstCB~fstSG, data=parv1) #do a linear regression
plot(parv1$fstSG, parv1$fstCB, col="blue2", ylim=c(0,1), xlim=c(0,1), ylab="HybCB vs. ParvBal Fst", xlab="HybSG vs. ParvBal Fst", main="Locus-by-locus Fst comparisons for Chormosome 1", lwd=.5)
lines(x=c(0,1), y=c(0,1), lwd=0.6)
abline(fit, col="blue2", lwd=0.6)
p = GetPval(fit); text(x=.65,y=.97, labels=sprintf("p-value: %.8f",p), cex=1.05) #fit line p-val
form = GetForm(fit); text(x=.25,y=1.01, labels=form)                             #fit line formula
r2 = summary(fit)$r.squared; text(x=.59,y=1.01, labels=sprintf("R^2: %.4f",r2))  #fite line R^2

fit = lm(fstCB~fstSG, data=parv2) #do a linear regression
plot(parv2$fstSG, parv2$fstCB, col="blue2", ylim=c(0,1), xlim=c(0,1), ylab="HybCB vs. ParvBal Fst", xlab="HybSG vs. ParvBal Fst", main="Locus-by-locus Fst comparisons for Chormosome 2", lwd=.5)
lines(x=c(0,1), y=c(0,1), lwd=0.6)
abline(fit, col="blue2", lwd=0.6)
p = GetPval(fit); text(x=.65,y=.97, labels=sprintf("p-value: %.8f",p), cex=1.05) #fit line p-val
form = GetForm(fit); text(x=.25,y=1.01, labels=form)                             #fit line formula
r2 = summary(fit)$r.squared; text(x=.59,y=1.01, labels=sprintf("R^2: %.4f",r2))  #fite line R^2

fit = lm(fstCB~fstSG, data=parv3) #do a linear regression
plot(parv3$fstSG, parv3$fstCB, col="blue2", ylim=c(0,1), xlim=c(0,1), ylab="HybCB vs. ParvBal Fst", xlab="HybSG vs. ParvBal Fst", main="Locus-by-locus Fst comparisons for Chormosome 3", lwd=.5)
lines(x=c(0,1), y=c(0,1), lwd=0.6)
abline(fit, col="blue2", lwd=0.6)
p = GetPval(fit); text(x=.65,y=.97, labels=sprintf("p-value: %.8f",p), cex=1.05) #fit line p-val
form = GetForm(fit); text(x=.25,y=1.01, labels=form)                             #fit line formula
r2 = summary(fit)$r.squared; text(x=.59,y=1.01, labels=sprintf("R^2: %.4f",r2))  #fite line R^2

fit = lm(fstCB~fstSG, data=parv4) #do a linear regression
plot(parv4$fstSG, parv4$fstCB, col="blue2", ylim=c(0,1), xlim=c(0,1), ylab="HybCB vs. ParvBal Fst", xlab="HybSG vs. ParvBal Fst", main="Locus-by-locus Fst comparisons for Chormosome 4", lwd=.5)
lines(x=c(0,1), y=c(0,1), lwd=0.6)
abline(fit, col="blue2", lwd=0.6)
p = GetPval(fit); text(x=.65,y=.97, labels=sprintf("p-value: %.8f",p), cex=1.05) #fit line p-val
form = GetForm(fit); text(x=.25,y=1.01, labels=form)                             #fit line formula
r2 = summary(fit)$r.squared; text(x=.59,y=1.01, labels=sprintf("R^2: %.4f",r2))  #fite line R^2

fit = lm(fstCB~fstSG, data=parv5) #do a linear regression
plot(parv5$fstSG, parv5$fstCB, col="blue2", ylim=c(0,1), xlim=c(0,1), ylab="HybCB vs. ParvBal Fst", xlab="HybSG vs. ParvBal Fst", main="Locus-by-locus Fst comparisons for Chormosome 5", lwd=.5)
lines(x=c(0,1), y=c(0,1), lwd=0.6)
abline(fit, col="blue2", lwd=0.6)
p = GetPval(fit); text(x=.65,y=.97, labels=sprintf("p-value: %.8f",p), cex=1.05) #fit line p-val
form = GetForm(fit); text(x=.25,y=1.01, labels=form)                             #fit line formula
r2 = summary(fit)$r.squared; text(x=.59,y=1.01, labels=sprintf("R^2: %.4f",r2))  #fite line R^2

fit = lm(fstCB~fstSG, data=parv6) #do a linear regression
plot(parv6$fstSG, parv6$fstCB, col="blue2", ylim=c(0,1), xlim=c(0,1), ylab="HybCB vs. ParvBal Fst", xlab="HybSG vs. ParvBal Fst", main="Locus-by-locus Fst comparisons for Chormosome 6", lwd=.5)
lines(x=c(0,1), y=c(0,1), lwd=0.6)
abline(fit, col="blue2", lwd=0.6)
p = GetPval(fit); text(x=.65,y=.97, labels=sprintf("p-value: %.8f",p), cex=1.05) #fit line p-val
form = GetForm(fit); text(x=.25,y=1.01, labels=form)                             #fit line formula
r2 = summary(fit)$r.squared; text(x=.59,y=1.01, labels=sprintf("R^2: %.4f",r2))  #fite line R^2

fit = lm(fstCB~fstSG, data=parv7) #do a linear regression
plot(parv7$fstSG, parv7$fstCB, col="blue2", ylim=c(0,1), xlim=c(0,1), ylab="HybCB vs. ParvBal Fst", xlab="HybSG vs. ParvBal Fst", main="Locus-by-locus Fst comparisons for Chormosome 7", lwd=.5)
lines(x=c(0,1), y=c(0,1), lwd=0.6)
abline(fit, col="blue2", lwd=0.6)
p = GetPval(fit); text(x=.65,y=.97, labels=sprintf("p-value: %.8f",p), cex=1.05) #fit line p-val
form = GetForm(fit); text(x=.25,y=1.01, labels=form)                             #fit line formula
r2 = summary(fit)$r.squared; text(x=.59,y=1.01, labels=sprintf("R^2: %.4f",r2))  #fite line R^2

fit = lm(fstCB~fstSG, data=parv8) #do a linear regression
plot(parv8$fstSG, parv8$fstCB, col="blue2", ylim=c(0,1), xlim=c(0,1), ylab="HybCB vs. ParvBal Fst", xlab="HybSG vs. ParvBal Fst", main="Locus-by-locus Fst comparisons for Chormosome 8", lwd=.5)
lines(x=c(0,1), y=c(0,1), lwd=0.6)
abline(fit, col="blue2", lwd=0.6)
p = GetPval(fit); text(x=.65,y=.97, labels=sprintf("p-value: %.8f",p), cex=1.05) #fit line p-val
form = GetForm(fit); text(x=.25,y=1.01, labels=form)                             #fit line formula
r2 = summary(fit)$r.squared; text(x=.59,y=1.01, labels=sprintf("R^2: %.4f",r2))  #fite line R^2

fit = lm(fstCB~fstSG, data=parv9) #do a linear regression
plot(parv9$fstSG, parv9$fstCB, col="blue2", ylim=c(0,1), xlim=c(0,1), ylab="HybCB vs. ParvBal Fst", xlab="HybSG vs. ParvBal Fst", main="Locus-by-locus Fst comparisons for Chormosome 9", lwd=.5)
lines(x=c(0,1), y=c(0,1), lwd=0.6)
abline(fit, col="blue2", lwd=0.6)
p = GetPval(fit); text(x=.65,y=.97, labels=sprintf("p-value: %.8f",p), cex=1.05) #fit line p-val
form = GetForm(fit); text(x=.25,y=1.01, labels=form)                             #fit line formula
r2 = summary(fit)$r.squared; text(x=.59,y=1.01, labels=sprintf("R^2: %.4f",r2))  #fite line R^2

fit = lm(fstCB~fstSG, data=parv10) #do a linear regression
plot(parv10$fstSG, parv10$fstCB, col="blue2", ylim=c(0,1), xlim=c(0,1), ylab="HybCB vs. ParvBal Fst", xlab="HybSG vs. ParvBal Fst", main="Locus-by-locus Fst comparisons for Chormosome 10", lwd=.5)
lines(x=c(0,1), y=c(0,1), lwd=0.6)
abline(fit, col="blue2", lwd=0.6)
p = GetPval(fit); text(x=.65,y=.97, labels=sprintf("p-value: %.8f",p), cex=1.05) #fit line p-val
form = GetForm(fit); text(x=.25,y=1.01, labels=form)                             #fit line formula
r2 = summary(fit)$r.squared; text(x=.59,y=1.01, labels=sprintf("R^2: %.4f",r2))  #fite line R^2

#CPvCB mex
fit = lm(fstCP~fstCB, data=mex1) #do a linear regression
plot(mex1$fstCB, mex1$fstCP, col="firebrick4", ylim=c(0,1), xlim=c(0,1), ylab="HybCP vs. MexCP Fst", xlab="HybCB vs. MexCP Fst", main="Locus-by-locus Fst comparisons for Chormosome 1", lwd=.5)
lines(x=c(0,1), y=c(0,1), lwd=0.6)
abline(fit, col="firebrick4", lwd=0.6)
p = GetPval(fit); text(x=.65,y=.97, labels=sprintf("p-value: %.8f",p), cex=1.05) #fit line p-val
form = GetForm(fit); text(x=.25,y=1.01, labels=form)                             #fit line formula
r2 = summary(fit)$r.squared; text(x=.59,y=1.01, labels=sprintf("R^2: %.4f",r2))  #fite line R^2

fit = lm(fstCP~fstCB, data=mex2) #do a linear regression
plot(mex2$fstCB, mex2$fstCP, col="firebrick4", ylim=c(0,1), xlim=c(0,1), ylab="HybCP vs. MexCP Fst", xlab="HybCB vs. MexCP Fst", main="Locus-by-locus Fst comparisons for Chormosome 2", lwd=.5)
lines(x=c(0,1), y=c(0,1), lwd=0.6)
abline(fit, col="firebrick4", lwd=0.6)
p = GetPval(fit); text(x=.65,y=.97, labels=sprintf("p-value: %.8f",p), cex=1.05) #fit line p-val
form = GetForm(fit); text(x=.25,y=1.01, labels=form)                             #fit line formula
r2 = summary(fit)$r.squared; text(x=.59,y=1.01, labels=sprintf("R^2: %.4f",r2))  #fite line R^2

fit = lm(fstCP~fstCB, data=mex3) #do a linear regression
plot(mex3$fstCB, mex3$fstCP, col="firebrick4", ylim=c(0,1), xlim=c(0,1), ylab="HybCP vs. MexCP Fst", xlab="HybCB vs. MexCP Fst", main="Locus-by-locus Fst comparisons for Chormosome 3", lwd=.5)
lines(x=c(0,1), y=c(0,1), lwd=0.6)
abline(fit, col="firebrick4", lwd=0.6)
p = GetPval(fit); text(x=.65,y=.97, labels=sprintf("p-value: %.8f",p), cex=1.05) #fit line p-val
form = GetForm(fit); text(x=.25,y=1.01, labels=form)                             #fit line formula
r2 = summary(fit)$r.squared; text(x=.59,y=1.01, labels=sprintf("R^2: %.4f",r2))  #fite line R^2

fit = lm(fstCP~fstCB, data=mex4) #do a linear regression
plot(mex4$fstCB, mex4$fstCP, col="firebrick4", ylim=c(0,1), xlim=c(0,1), ylab="HybCP vs. MexCP Fst", xlab="HybCB vs. MexCP Fst", main="Locus-by-locus Fst comparisons for Chormosome 4", lwd=.5)
lines(x=c(0,1), y=c(0,1), lwd=0.6)
abline(fit, col="firebrick4", lwd=0.6)
p = GetPval(fit); text(x=.65,y=.97, labels=sprintf("p-value: %.8f",p), cex=1.05) #fit line p-val
form = GetForm(fit); text(x=.25,y=1.01, labels=form)                             #fit line formula
r2 = summary(fit)$r.squared; text(x=.59,y=1.01, labels=sprintf("R^2: %.4f",r2))  #fite line R^2

fit = lm(fstCP~fstCB, data=mex5) #do a linear regression
plot(mex5$fstCB, mex5$fstCP, col="firebrick4", ylim=c(0,1), xlim=c(0,1), ylab="HybCP vs. MexCP Fst", xlab="HybCB vs. MexCP Fst", main="Locus-by-locus Fst comparisons for Chormosome 5", lwd=.5)
lines(x=c(0,1), y=c(0,1), lwd=0.6)
abline(fit, col="firebrick4", lwd=0.6)
p = GetPval(fit); text(x=.65,y=.97, labels=sprintf("p-value: %.8f",p), cex=1.05) #fit line p-val
form = GetForm(fit); text(x=.25,y=1.01, labels=form)                             #fit line formula
r2 = summary(fit)$r.squared; text(x=.59,y=1.01, labels=sprintf("R^2: %.4f",r2))  #fite line R^2

fit = lm(fstCP~fstCB, data=mex6) #do a linear regression
plot(mex6$fstCB, mex6$fstCP, col="firebrick4", ylim=c(0,1), xlim=c(0,1), ylab="HybCP vs. MexCP Fst", xlab="HybCB vs. MexCP Fst", main="Locus-by-locus Fst comparisons for Chormosome 6", lwd=.5)
lines(x=c(0,1), y=c(0,1), lwd=0.6)
abline(fit, col="firebrick4", lwd=0.6)
p = GetPval(fit); text(x=.65,y=.97, labels=sprintf("p-value: %.8f",p), cex=1.05) #fit line p-val
form = GetForm(fit); text(x=.25,y=1.01, labels=form)                             #fit line formula
r2 = summary(fit)$r.squared; text(x=.59,y=1.01, labels=sprintf("R^2: %.4f",r2))  #fite line R^2

fit = lm(fstCP~fstCB, data=mex7) #do a linear regression
plot(mex7$fstCB, mex7$fstCP, col="firebrick4", ylim=c(0,1), xlim=c(0,1), ylab="HybCP vs. MexCP Fst", xlab="HybCB vs. MexCP Fst", main="Locus-by-locus Fst comparisons for Chormosome 7", lwd=.5)
lines(x=c(0,1), y=c(0,1), lwd=0.6)
abline(fit, col="firebrick4", lwd=0.6)
p = GetPval(fit); text(x=.65,y=.97, labels=sprintf("p-value: %.8f",p), cex=1.05) #fit line p-val
form = GetForm(fit); text(x=.25,y=1.01, labels=form)                             #fit line formula
r2 = summary(fit)$r.squared; text(x=.59,y=1.01, labels=sprintf("R^2: %.4f",r2))  #fite line R^2

fit = lm(fstCP~fstCB, data=mex8) #do a linear regression
plot(mex8$fstCB, mex8$fstCP, col="firebrick4", ylim=c(0,1), xlim=c(0,1), ylab="HybCP vs. MexCP Fst", xlab="HybCB vs. MexCP Fst", main="Locus-by-locus Fst comparisons for Chormosome 8", lwd=.5)
lines(x=c(0,1), y=c(0,1), lwd=0.6)
abline(fit, col="firebrick4", lwd=0.6)
p = GetPval(fit); text(x=.65,y=.97, labels=sprintf("p-value: %.8f",p), cex=1.05) #fit line p-val
form = GetForm(fit); text(x=.25,y=1.01, labels=form)                             #fit line formula
r2 = summary(fit)$r.squared; text(x=.59,y=1.01, labels=sprintf("R^2: %.4f",r2))  #fite line R^2

fit = lm(fstCP~fstCB, data=mex9) #do a linear regression
plot(mex9$fstCB, mex9$fstCP, col="firebrick4", ylim=c(0,1), xlim=c(0,1), ylab="HybCP vs. MexCP Fst", xlab="HybCB vs. MexCP Fst", main="Locus-by-locus Fst comparisons for Chormosome 9", lwd=.5)
lines(x=c(0,1), y=c(0,1), lwd=0.6)
abline(fit, col="firebrick4", lwd=0.6)
p = GetPval(fit); text(x=.65,y=.97, labels=sprintf("p-value: %.8f",p), cex=1.05) #fit line p-val
form = GetForm(fit); text(x=.25,y=1.01, labels=form)                             #fit line formula
r2 = summary(fit)$r.squared; text(x=.59,y=1.01, labels=sprintf("R^2: %.4f",r2))  #fite line R^2

fit = lm(fstCP~fstCB, data=mex10) #do a linear regression
plot(mex10$fstCB, mex10$fstCP, col="firebrick4", ylim=c(0,1), xlim=c(0,1), ylab="HybCP vs. MexCP Fst", xlab="HybCB vs. MexCP Fst", main="Locus-by-locus Fst comparisons for Chormosome 10", lwd=.5)
lines(x=c(0,1), y=c(0,1), lwd=0.6)
abline(fit, col="firebrick4", lwd=0.6)
p = GetPval(fit); text(x=.65,y=.97, labels=sprintf("p-value: %.8f",p), cex=1.05) #fit line p-val
form = GetForm(fit); text(x=.25,y=1.01, labels=form)                             #fit line formula
r2 = summary(fit)$r.squared; text(x=.59,y=1.01, labels=sprintf("R^2: %.4f",r2))  #fite line R^2

#CPvSG mex
fit = lm(fstCP~fstSG, data=mex1) #do a linear regression
plot(mex1$fstSG, mex1$fstCP, col="firebrick4", ylim=c(0,1), xlim=c(0,1), ylab="HybCP vs. MexCP Fst", xlab="HybSG vs. MexCP Fst", main="Locus-by-locus Fst comparisons for Chormosome 1", lwd=.5)
lines(x=c(0,1), y=c(0,1), lwd=0.6)
abline(fit, col="firebrick4", lwd=0.6)
p = GetPval(fit); text(x=.65,y=.97, labels=sprintf("p-value: %.8f",p), cex=1.05) #fit line p-val
form = GetForm(fit); text(x=.25,y=1.01, labels=form)                             #fit line formula
r2 = summary(fit)$r.squared; text(x=.59,y=1.01, labels=sprintf("R^2: %.4f",r2))  #fite line R^2

fit = lm(fstCP~fstSG, data=mex2) #do a linear regression
plot(mex2$fstSG, mex2$fstCP, col="firebrick4", ylim=c(0,1), xlim=c(0,1), ylab="HybCP vs. MexCP Fst", xlab="HybSG vs. MexCP Fst", main="Locus-by-locus Fst comparisons for Chormosome 2", lwd=.5)
lines(x=c(0,1), y=c(0,1), lwd=0.6)
abline(fit, col="firebrick4", lwd=0.6)
p = GetPval(fit); text(x=.65,y=.97, labels=sprintf("p-value: %.8f",p), cex=1.05) #fit line p-val
form = GetForm(fit); text(x=.25,y=1.01, labels=form)                             #fit line formula
r2 = summary(fit)$r.squared; text(x=.59,y=1.01, labels=sprintf("R^2: %.4f",r2))  #fite line R^2

fit = lm(fstCP~fstSG, data=mex3) #do a linear regression
plot(mex3$fstSG, mex3$fstCP, col="firebrick4", ylim=c(0,1), xlim=c(0,1), ylab="HybCP vs. MexCP Fst", xlab="HybSG vs. MexCP Fst", main="Locus-by-locus Fst comparisons for Chormosome 3", lwd=.5)
lines(x=c(0,1), y=c(0,1), lwd=0.6)
abline(fit, col="firebrick4", lwd=0.6)
p = GetPval(fit); text(x=.65,y=.97, labels=sprintf("p-value: %.8f",p), cex=1.05) #fit line p-val
form = GetForm(fit); text(x=.25,y=1.01, labels=form)                             #fit line formula
r2 = summary(fit)$r.squared; text(x=.59,y=1.01, labels=sprintf("R^2: %.4f",r2))  #fite line R^2

fit = lm(fstCP~fstSG, data=mex4) #do a linear regression
plot(mex4$fstSG, mex4$fstCP, col="firebrick4", ylim=c(0,1), xlim=c(0,1), ylab="HybCP vs. MexCP Fst", xlab="HybSG vs. MexCP Fst", main="Locus-by-locus Fst comparisons for Chormosome 4", lwd=.5)
lines(x=c(0,1), y=c(0,1), lwd=0.6)
abline(fit, col="firebrick4", lwd=0.6)
p = GetPval(fit); text(x=.65,y=.97, labels=sprintf("p-value: %.8f",p), cex=1.05) #fit line p-val
form = GetForm(fit); text(x=.25,y=1.01, labels=form)                             #fit line formula
r2 = summary(fit)$r.squared; text(x=.59,y=1.01, labels=sprintf("R^2: %.4f",r2))  #fite line R^2

fit = lm(fstCP~fstSG, data=mex5) #do a linear regression
plot(mex5$fstSG, mex5$fstCP, col="firebrick4", ylim=c(0,1), xlim=c(0,1), ylab="HybCP vs. MexCP Fst", xlab="HybSG vs. MexCP Fst", main="Locus-by-locus Fst comparisons for Chormosome 5", lwd=.5)
lines(x=c(0,1), y=c(0,1), lwd=0.6)
abline(fit, col="firebrick4", lwd=0.6)
p = GetPval(fit); text(x=.65,y=.97, labels=sprintf("p-value: %.8f",p), cex=1.05) #fit line p-val
form = GetForm(fit); text(x=.25,y=1.01, labels=form)                             #fit line formula
r2 = summary(fit)$r.squared; text(x=.59,y=1.01, labels=sprintf("R^2: %.4f",r2))  #fite line R^2

fit = lm(fstCP~fstSG, data=mex6) #do a linear regression
plot(mex6$fstSG, mex6$fstCP, col="firebrick4", ylim=c(0,1), xlim=c(0,1), ylab="HybCP vs. MexCP Fst", xlab="HybSG vs. MexCP Fst", main="Locus-by-locus Fst comparisons for Chormosome 6", lwd=.5)
lines(x=c(0,1), y=c(0,1), lwd=0.6)
abline(fit, col="firebrick4", lwd=0.6)
p = GetPval(fit); text(x=.65,y=.97, labels=sprintf("p-value: %.8f",p), cex=1.05) #fit line p-val
form = GetForm(fit); text(x=.25,y=1.01, labels=form)                             #fit line formula
r2 = summary(fit)$r.squared; text(x=.59,y=1.01, labels=sprintf("R^2: %.4f",r2))  #fite line R^2

fit = lm(fstCP~fstSG, data=mex7) #do a linear regression
plot(mex7$fstSG, mex7$fstCP, col="firebrick4", ylim=c(0,1), xlim=c(0,1), ylab="HybCP vs. MexCP Fst", xlab="HybSG vs. MexCP Fst", main="Locus-by-locus Fst comparisons for Chormosome 7", lwd=.5)
lines(x=c(0,1), y=c(0,1), lwd=0.6)
abline(fit, col="firebrick4", lwd=0.6)
p = GetPval(fit); text(x=.65,y=.97, labels=sprintf("p-value: %.8f",p), cex=1.05) #fit line p-val
form = GetForm(fit); text(x=.25,y=1.01, labels=form)                             #fit line formula
r2 = summary(fit)$r.squared; text(x=.59,y=1.01, labels=sprintf("R^2: %.4f",r2))  #fite line R^2

fit = lm(fstCP~fstSG, data=mex8) #do a linear regression
plot(mex8$fstSG, mex8$fstCP, col="firebrick4", ylim=c(0,1), xlim=c(0,1), ylab="HybCP vs. MexCP Fst", xlab="HybSG vs. MexCP Fst", main="Locus-by-locus Fst comparisons for Chormosome 8", lwd=.5)
lines(x=c(0,1), y=c(0,1), lwd=0.6)
abline(fit, col="firebrick4", lwd=0.6)
p = GetPval(fit); text(x=.65,y=.97, labels=sprintf("p-value: %.8f",p), cex=1.05) #fit line p-val
form = GetForm(fit); text(x=.25,y=1.01, labels=form)                             #fit line formula
r2 = summary(fit)$r.squared; text(x=.59,y=1.01, labels=sprintf("R^2: %.4f",r2))  #fite line R^2

fit = lm(fstCP~fstSG, data=mex9) #do a linear regression
plot(mex9$fstSG, mex9$fstCP, col="firebrick4", ylim=c(0,1), xlim=c(0,1), ylab="HybCP vs. MexCP Fst", xlab="HybSG vs. MexCP Fst", main="Locus-by-locus Fst comparisons for Chormosome 9", lwd=.5)
lines(x=c(0,1), y=c(0,1), lwd=0.6)
abline(fit, col="firebrick4", lwd=0.6)
p = GetPval(fit); text(x=.65,y=.97, labels=sprintf("p-value: %.8f",p), cex=1.05) #fit line p-val
form = GetForm(fit); text(x=.25,y=1.01, labels=form)                             #fit line formula
r2 = summary(fit)$r.squared; text(x=.59,y=1.01, labels=sprintf("R^2: %.4f",r2))  #fite line R^2

fit = lm(fstCP~fstSG, data=mex10) #do a linear regression
plot(mex10$fstSG, mex10$fstCP, col="firebrick4", ylim=c(0,1), xlim=c(0,1), ylab="HybCP vs. MexCP Fst", xlab="HybSG vs. MexCP Fst", main="Locus-by-locus Fst comparisons for Chormosome 10", lwd=.5)
lines(x=c(0,1), y=c(0,1), lwd=0.6)
abline(fit, col="firebrick4", lwd=0.6)
p = GetPval(fit); text(x=.65,y=.97, labels=sprintf("p-value: %.8f",p), cex=1.05) #fit line p-val
form = GetForm(fit); text(x=.25,y=1.01, labels=form)                             #fit line formula
r2 = summary(fit)$r.squared; text(x=.59,y=1.01, labels=sprintf("R^2: %.4f",r2))  #fite line R^2

#CBvSG mex
fit = lm(fstCB~fstSG, data=mex1) #do a linear regression
plot(mex1$fstSG, mex1$fstCB, col="firebrick4", ylim=c(0,1), xlim=c(0,1), ylab="HybCB vs. MexCP Fst", xlab="HybSG vs. MexCP Fst", main="Locus-by-locus Fst comparisons for Chormosome 1", lwd=.5)
lines(x=c(0,1), y=c(0,1), lwd=0.6)
abline(fit, col="firebrick4", lwd=0.6)
p = GetPval(fit); text(x=.65,y=.97, labels=sprintf("p-value: %.8f",p), cex=1.05) #fit line p-val
form = GetForm(fit); text(x=.25,y=1.01, labels=form)                             #fit line formula
r2 = summary(fit)$r.squared; text(x=.59,y=1.01, labels=sprintf("R^2: %.4f",r2))  #fite line R^2

fit = lm(fstCB~fstSG, data=mex2) #do a linear regression
plot(mex2$fstSG, mex2$fstCB, col="firebrick4", ylim=c(0,1), xlim=c(0,1), ylab="HybCB vs. MexCP Fst", xlab="HybSG vs. MexCP Fst", main="Locus-by-locus Fst comparisons for Chormosome 2", lwd=.5)
lines(x=c(0,1), y=c(0,1), lwd=0.6)
abline(fit, col="firebrick4", lwd=0.6)
p = GetPval(fit); text(x=.65,y=.97, labels=sprintf("p-value: %.8f",p), cex=1.05) #fit line p-val
form = GetForm(fit); text(x=.25,y=1.01, labels=form)                             #fit line formula
r2 = summary(fit)$r.squared; text(x=.59,y=1.01, labels=sprintf("R^2: %.4f",r2))  #fite line R^2

fit = lm(fstCB~fstSG, data=mex3) #do a linear regression
plot(mex3$fstSG, mex3$fstCB, col="firebrick4", ylim=c(0,1), xlim=c(0,1), ylab="HybCB vs. MexCP Fst", xlab="HybSG vs. MexCP Fst", main="Locus-by-locus Fst comparisons for Chormosome 3", lwd=.5)
lines(x=c(0,1), y=c(0,1), lwd=0.6)
abline(fit, col="firebrick4", lwd=0.6)
p = GetPval(fit); text(x=.65,y=.97, labels=sprintf("p-value: %.8f",p), cex=1.05) #fit line p-val
form = GetForm(fit); text(x=.25,y=1.01, labels=form)                             #fit line formula
r2 = summary(fit)$r.squared; text(x=.59,y=1.01, labels=sprintf("R^2: %.4f",r2))  #fite line R^2

fit = lm(fstCB~fstSG, data=mex4) #do a linear regression
plot(mex4$fstSG, mex4$fstCB, col="firebrick4", ylim=c(0,1), xlim=c(0,1), ylab="HybCB vs. MexCP Fst", xlab="HybSG vs. MexCP Fst", main="Locus-by-locus Fst comparisons for Chormosome 4", lwd=.5)
lines(x=c(0,1), y=c(0,1), lwd=0.6)
abline(fit, col="firebrick4", lwd=0.6)
p = GetPval(fit); text(x=.65,y=.97, labels=sprintf("p-value: %.8f",p), cex=1.05) #fit line p-val
form = GetForm(fit); text(x=.25,y=1.01, labels=form)                             #fit line formula
r2 = summary(fit)$r.squared; text(x=.59,y=1.01, labels=sprintf("R^2: %.4f",r2))  #fite line R^2

fit = lm(fstCB~fstSG, data=mex5) #do a linear regression
plot(mex5$fstSG, mex5$fstCB, col="firebrick4", ylim=c(0,1), xlim=c(0,1), ylab="HybCB vs. MexCP Fst", xlab="HybSG vs. MexCP Fst", main="Locus-by-locus Fst comparisons for Chormosome 5", lwd=.5)
lines(x=c(0,1), y=c(0,1), lwd=0.6)
abline(fit, col="firebrick4", lwd=0.6)
p = GetPval(fit); text(x=.65,y=.97, labels=sprintf("p-value: %.8f",p), cex=1.05) #fit line p-val
form = GetForm(fit); text(x=.25,y=1.01, labels=form)                             #fit line formula
r2 = summary(fit)$r.squared; text(x=.59,y=1.01, labels=sprintf("R^2: %.4f",r2))  #fite line R^2

fit = lm(fstCB~fstSG, data=mex6) #do a linear regression
plot(mex6$fstSG, mex6$fstCB, col="firebrick4", ylim=c(0,1), xlim=c(0,1), ylab="HybCB vs. MexCP Fst", xlab="HybSG vs. MexCP Fst", main="Locus-by-locus Fst comparisons for Chormosome 6", lwd=.5)
lines(x=c(0,1), y=c(0,1), lwd=0.6)
abline(fit, col="firebrick4", lwd=0.6)
p = GetPval(fit); text(x=.65,y=.97, labels=sprintf("p-value: %.8f",p), cex=1.05) #fit line p-val
form = GetForm(fit); text(x=.25,y=1.01, labels=form)                             #fit line formula
r2 = summary(fit)$r.squared; text(x=.59,y=1.01, labels=sprintf("R^2: %.4f",r2))  #fite line R^2

fit = lm(fstCB~fstSG, data=mex7) #do a linear regression
plot(mex7$fstSG, mex7$fstCB, col="firebrick4", ylim=c(0,1), xlim=c(0,1), ylab="HybCB vs. MexCP Fst", xlab="HybSG vs. MexCP Fst", main="Locus-by-locus Fst comparisons for Chormosome 7", lwd=.5)
lines(x=c(0,1), y=c(0,1), lwd=0.6)
abline(fit, col="firebrick4", lwd=0.6)
p = GetPval(fit); text(x=.65,y=.97, labels=sprintf("p-value: %.8f",p), cex=1.05) #fit line p-val
form = GetForm(fit); text(x=.25,y=1.01, labels=form)                             #fit line formula
r2 = summary(fit)$r.squared; text(x=.59,y=1.01, labels=sprintf("R^2: %.4f",r2))  #fite line R^2

fit = lm(fstCB~fstSG, data=mex8) #do a linear regression
plot(mex8$fstSG, mex8$fstCB, col="firebrick4", ylim=c(0,1), xlim=c(0,1), ylab="HybCB vs. MexCP Fst", xlab="HybSG vs. MexCP Fst", main="Locus-by-locus Fst comparisons for Chormosome 8", lwd=.5)
lines(x=c(0,1), y=c(0,1), lwd=0.6)
abline(fit, col="firebrick4", lwd=0.6)
p = GetPval(fit); text(x=.65,y=.97, labels=sprintf("p-value: %.8f",p), cex=1.05) #fit line p-val
form = GetForm(fit); text(x=.25,y=1.01, labels=form)                             #fit line formula
r2 = summary(fit)$r.squared; text(x=.59,y=1.01, labels=sprintf("R^2: %.4f",r2))  #fite line R^2

fit = lm(fstCB~fstSG, data=mex9) #do a linear regression
plot(mex9$fstSG, mex9$fstCB, col="firebrick4", ylim=c(0,1), xlim=c(0,1), ylab="HybCB vs. MexCP Fst", xlab="HybSG vs. MexCP Fst", main="Locus-by-locus Fst comparisons for Chormosome 9", lwd=.5)
lines(x=c(0,1), y=c(0,1), lwd=0.6)
abline(fit, col="firebrick4", lwd=0.6)
p = GetPval(fit); text(x=.65,y=.97, labels=sprintf("p-value: %.8f",p), cex=1.05) #fit line p-val
form = GetForm(fit); text(x=.25,y=1.01, labels=form)                             #fit line formula
r2 = summary(fit)$r.squared; text(x=.59,y=1.01, labels=sprintf("R^2: %.4f",r2))  #fite line R^2

fit = lm(fstCB~fstSG, data=mex10) #do a linear regression
plot(mex10$fstSG, mex10$fstCB, col="firebrick4", ylim=c(0,1), xlim=c(0,1), ylab="HybCB vs. MexCP Fst", xlab="HybSG vs. MexCP Fst", main="Locus-by-locus Fst comparisons for Chormosome 10", lwd=.5)
lines(x=c(0,1), y=c(0,1), lwd=0.6)
abline(fit, col="firebrick4", lwd=0.6)
p = GetPval(fit); text(x=.65,y=.97, labels=sprintf("p-value: %.8f",p), cex=1.05) #fit line p-val
form = GetForm(fit); text(x=.25,y=1.01, labels=form)                             #fit line formula
r2 = summary(fit)$r.squared; text(x=.59,y=1.01, labels=sprintf("R^2: %.4f",r2))  #fite line R^2
dev.off()




