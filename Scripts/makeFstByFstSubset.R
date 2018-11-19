#This script is designed to create figures of hybrid groups compared to each other 
#via Fst on all chromosomes to look for differential architectures of hybridization.
#This version plots only some comparisons.
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
mexData = read.table("mexVhybFsts.txt", header=TRUE)

#subset data by chromosome and taxonomy
#Mex
mex1 = subset(mexData, chromo==1)
mex2 = subset(mexData, chromo==2)
mex4 = subset(mexData, chromo==4)
mex10 = subset(mexData, chromo==10)

#plot data
pdf("FstxFstSubset.pdf", width=7.2, height=5.43)
par(mar=c(4,4,2,1), mfrow=c(3,4), mgp=c(1.5,.3,0), cex.main=1.4, cex.lab=1.5, cex.axis=1.6, cex=.32, ps=10.5)

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

fit = lm(fstCP~fstCB, data=mex4) #do a linear regression
plot(mex4$fstCB, mex4$fstCP, col="firebrick4", ylim=c(0,1), xlim=c(0,1), ylab="HybCP vs. MexCP Fst", xlab="HybCB vs. MexCP Fst", main="Locus-by-locus Fst comparisons for Chormosome 4", lwd=.5)
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

fit = lm(fstCP~fstSG, data=mex4) #do a linear regression
plot(mex4$fstSG, mex4$fstCP, col="firebrick4", ylim=c(0,1), xlim=c(0,1), ylab="HybCP vs. MexCP Fst", xlab="HybSG vs. MexCP Fst", main="Locus-by-locus Fst comparisons for Chormosome 4", lwd=.5)
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

fit = lm(fstCB~fstSG, data=mex4) #do a linear regression
plot(mex4$fstSG, mex4$fstCB, col="firebrick4", ylim=c(0,1), xlim=c(0,1), ylab="HybCB vs. MexCP Fst", xlab="HybSG vs. MexCP Fst", main="Locus-by-locus Fst comparisons for Chormosome 4", lwd=.5)
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





