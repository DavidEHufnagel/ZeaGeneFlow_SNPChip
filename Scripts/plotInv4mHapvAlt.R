#This script is designed to generate linear regression plots of 
#altitude verses  inv4m haplotype frequencies. M=2,P=0,H=1
#Created by David E. Hufnagel

#set working directory here
data = read.table("inv4mHapByAcc.txt", sep="\t", header=T)

alt = data$altitude
p = data$inv4mPcount
m = data$inv4mMcount
h = data$inv4mHcount
x = (2*m+h)/(2*(m+p+h))
color = as.character(data$hybStatus)

#This function creates a y=mx+b style formula from an lm summary object. 
#my version
GetForm = function (model) {
	sprintf("y = %.4f * %s + %.4f", coefficients(model)[-1], names(coefficients(model)[-1]), coefficients(model)[1]) 
}

#plot P type
pdf("altVhapFreq.pdf")
plot(x,alt, col=color, lwd=1.5, ylab="altitude (m)", xlab="frequency of inv4m mexicana haplotypes")
fit = lm(alt~x);abline(fit, col="black", lwd=0.6) #do a linear regression
form = GetForm(fit);text(0.8,420, labels=form) #fit line formula
dev.off()
