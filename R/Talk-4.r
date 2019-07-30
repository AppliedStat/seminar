#####################################################################
# Phone-lines Example from Welch (1987)
#####################################################################
source("https://raw.githubusercontent.com/AppliedStat/seminar/master/R/Rsec.R")
library(MASS);
#---------------------
data  = c(-988,-135,-78,3,59,83,93,110,189,197,204,229,269,310)
n = length(data)
MU = mean(data);
SD = sqrt( (n-1)/n*var(data))   # MLE 

h = 1.7*mad(data)*n^(-0.2) 
Xmin = min(data)-1.5*h
Xmax = max(data)+3*h
X = seq(Xmin, Xmax, l=100)
#--------------------
pdf(file="Telephone.pdf",   height=1.5, width=5.5)
par(mar=c(4, 4, 0, 0), omi=c(0,0,0,0), cex=0.5,mex=1/4)
truehist(data, nbins=10, xlim=c(Xmin,Xmax), ylim=c(0,0.00305), 
         ymax=0.0028, col="aliceblue", prob=T, xlab="")
lines(X, dnorm(X, mean=MU,sd=SD),             lty=1,lwd=2.0, col="grey")
lines(X, dnorm(X, mean=HL(data), sd=SH(data)),lty=2,lwd=1.5, col="black")
lines( density(data,adjust=2),                lty=1,lwd=1.5, col="red")
lines( density(data,adjust=0.5),              lty=1,lwd=1.5, col="green")
points(data, rep(-0.00006,length(data)), cex=0.7)
legend(-1200,0.0025,
       legend=c("MLE","Robust method (HL & Shamos)",
                "KDE (bandwidth*2)","KDE (bandwidth/2)"), 
       lty=c(1,2,1,1), lwd=c(2.0,1.5,1.5,1.5), 
       col=c("grey","black","red","green"), bty="n")
dev.off()
#####################################################################



#####################################################################
# Darwin's Zea Mays experiment Data.
#####################################################################
#------------------------------------------------------------------
X = c(23.5,   12,     21,    22, 19.125, 21.5,   22.125, 20.375,
      18.25,  21.625, 23.25, 21, 22.125, 23,     12 )
Y = c(17.375, 20.375, 20,    20, 18.375, 18.625, 18.625, 15.25,
      16.5,   18,     16.25, 18, 12.75,  15.5,   18 )
Diff = X - Y
#------------------------------------------------------------------
 pdf (file="ZeaMaysBoxQQ.pdf",  width=6, height=3.0) 
par(mfrow=c(1,2), mar=c(5,5,3,1), omi=c(0,0,0,0), cex=0.5, mex=0.5)
#------------------------------------------------------------------
boxplot(Diff,outline=T, notch=TRUE, col="bisque", ylab="Difference in heights")

qqnorm(Diff)
qqline(Diff, col="blue")

dev.off()
#==================================================================


#==================================================================
library(rt.test)
library(PairedData)
#------------------------------------------------------------------
Delta = seq(10, 25, by=0.1)
nN = length(Delta) 

pval1 = pval2 = pval3 = pval4 = pval5 = pval6 = pval7 = numeric( nN )

for ( i  in 1:nN ) {
     Ynoised = c(Y[1:14],Delta[i])

     tmp = t.test(X,Ynoised, paired = TRUE)
     pval1[i] = tmp$p.value

     tmp = stats::wilcox.test(X,Ynoised, paired = TRUE)
     pval2[i] = tmp$p.value

     tmp = yuen.t.test(X,Ynoised, paired = TRUE, tr=0.1)
     pval3[i] = tmp$p.value

     tmp = yuen.t.test(X,Ynoised, paired = TRUE, tr=0.2)
     pval4[i] = tmp$p.value

     tmp = yuen.t.test(X,Ynoised, paired = TRUE, tr=0.29)
     pval5[i] = tmp$p.value

     tmp = rt.test(X-Ynoised)
     pval6[i] = tmp$p.value

     tmp = rt.test(X-Ynoised, test.stat="TB")
     pval7[i] = tmp$p.value
}

#====================================================================
pdf(file="ZeaMays-pval.pdf", width=5.0, height=3.0) 
par(mar=c(5,5,1,1),omi=c(0,0,0,0),cex=0.5,mex=0.5)
#--------------------------------------------------------------------
 plot(Delta, pval1, type="l", ylim=c(0,0.10), 
      xlab=expression(Y[15]), ylab="p-value", col="grey" ) 

lines(Delta, pval2, lty=1, col="red" ) 

lines(Delta, pval3, lty=1, col="brown" ) 
lines(Delta, pval4, lty=2, col="brown" ) 
lines(Delta, pval5, lty=3, col="brown" ) 

lines(Delta, pval6, lty=1, col="green3" ) 
lines(Delta, pval7, lty=2, col="green3", lwd=2 ) 

abline(h=0.05, col="bisque1", lty=1 )
lines( c(18.028, 18.028), c(-0.5,0.05), col="bisque1", lty=1)
text(18.35, -0.0025, "18.028", cex=0.75)

lines( c(19.000, 19.000), c(-0.5,0.05), col="bisque1", lty=1)
text(19.1, -0.0025, "19", cex=0.75)

lines( c(17.693, 17.693), c(-0.5,0.05), col="bisque1", lty=1)
text(17.35, -0.0025, "17.693", cex=0.75)

legend(10, 0.10, 
      legend=c("t-test(Student)", "Wilcoxon", 
               "Yuen(0.1)", "Yuen(0.2)", "Yuen(0.29)",
               "rt-test(TA)", "rt-test(TB)" ), bty="n", 
      lty=c(1,1,1,2,3,1,2), 
      col=c("grey","red","brown","brown","brown", "green3", "green3") )
#========================================================================

