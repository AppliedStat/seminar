#####################################################################
# pdf: Phone-lines Example from Welch (1987)
#####################################################################

source("https://raw.githubusercontent.com/AppliedStat/seminar/master/R/Rsec.R")
library(MASS)

##
##- Check the examples of D.G. Simpson (1989) JASA v.84 pp.107-113
##
data  <- c(-988,-135,-78,3,59,83,93,110,189,197,204,229,269,310)
##--------
h = bw.nrd0(data)
Xmin = min(data)-1.5*h; Xmax <- max(data)+3*h
X = seq(Xmin, Xmax, l=100)
#--------------------
pdf(file="phone.pdf",   height=1.5, width=5.5)
par(mar=c(4, 4, 0, 0), omi=c(0,0,0,0), cex=0.5,mex=1/4)
#------
truehist(data, nbins=10, xlim=c(Xmin,Xmax), ymax=0.0028, ylim=c(0,0.00305),
         col="aliceblue", prob=T, xlab="")
lines(X, dnorm(X, mean=mean(data),sd=sd(data)),lty=1,lwd=2.0, col="grey")
lines(X, dnorm(X, mean=HL(data), sd=SH(data)), lty=2,lwd=1.5, col="black")
lines( density(data,adjust=2),                  lty=1,lwd=1.5, col="red")
lines( density(data,adjust=0.5),                lty=1,lwd=1.5, col="blue")
points(data, rep(-0.00006,length(data)), cex=0.7)
legend(-1100,0.0030,
  legend=c("MLE", "Robust method (HL & Shamos)", "KDE (bandwidth*2)", "KDE (bandwidth/2)"),
       lty=c(1,2,1,1), lwd=c(2.0,1.5,1.5,1.5), col=c("grey","black","red","blue"), bty="n")
dev.off()
##------


#####################################################################
# CDF: Phone-lines Example from Welch (1987)
#####################################################################
source("https://raw.githubusercontent.com/AppliedStat/seminar/master/R/Rsec.R")
#---------------------
data  = c(-988,-135,-78,3,59,83,93,110,189,197,204,229,269,310)

OUT = rbind( c(mean(data),sd(data)), c(HL(data),SH(data)) )
colnames(OUT) = c("location",  "scale") 
rownames(OUT) = c("MLE", "Robust method") 
OUT

# ===================================================================
# Find P[X < a]
a = -500   # say

# -------------------------------------------------------------------
# Using plain/obsolete empirical cdf 
Fn = ecdf(data)
Fn(a)

# -------------------------------------------------------------------
# Using MLE 
pnorm(a, mean=mean(data), sd=sd(data) )  

# -------------------------------------------------------------------
# Using robust estimation (HL and Shamos)
pnorm(a, mean=HL(data), sd=SH(data) )


# -------------------------------------------------------------------
# Kernel (direct integral of kde): common mistake
# Using the crude integration of KDE (not recommended b/c improper integral)

# large bandwidth (double the default bandwidth)
kde = density(data, n=1024, adjust=2) 
fk  = approxfun(kde$x, kde$y, yleft=0, yright=0)
integrate(fk, lower=-Inf, upper=a, subdivisions=1000L)

# default bandwidth 
kde = density(data, n=1024)
fk  = approxfun(kde$x, kde$y, yleft=0, yright=0)
integrate(fk, lower=-Inf, upper=a, subdivisions=1000L)

# small bandwidth (half the default bandwidth)
kde = density(data, n=1024, adjust=1/2) 
fk  = approxfun(kde$x, kde$y, yleft=0, yright=0)
integrate(fk, lower=-Inf, upper=a, subdivisions=1000L)

# -------------------------------------------------------------------
# Correct calculation of P[X < a]
# Using CDF of Kernel 
# x = a single number only 
Fhat1 = function(x, data, bw) { 
   if (missing(bw)) bw=bw.nrd0(data)
   mean( pnorm(x,data,bw) ) 
}

# large bandwidth (double the default bandwidth)
Fhat1(a, data=data, bw=bw.nrd0(data)*2)

# default bandwidth 
Fhat1(a, data=data)

# large bandwidth (double the default bandwidth)
Fhat1(a, data=data, bw=bw.nrd0(data)*0.5)


########################################################################
# CDF plots
########################################################################
# x = a vector 
Fhat2 = function(x, data, bw) {
   if (missing(bw)) bw=bw.nrd0(data)
   n = length(data)
   p = numeric(length(x)) 
   for ( i in 1:length(data) ) {
     p = p + pnorm(x,data[i],bw) 
   }
   return(p/n)
}
#-----------------------------------------------------------------------
pdf(file="phone-cdf.pdf", width=5.0, height=3.25)
par( mfrow=c(1,1), mar=c(5, 5, 1, 1), omi=c(0,0,0,0), cex=0.6, mex=0.6 )

x = seq(-1150, 600, by=10)
plot( ecdf(data), ylim=c(0,1), xlim=range(x), ylab="CDF", main=NA )
legend(-1200, 0.9,
  legend=c("MLE", "Robust method (HL & Shamos)", "KDE (bandwidth*2)", 
           "KDE (bandwidth/2)"), 
  lty=c(1,2,1,1), lwd=c(2.0,1.5,1.5,1.5), 
  col=c("grey","black","red","blue"), bty="n")
text(-1000, 0.12, "outlier")
lines(x, Fhat2(x,data, bw=bw.nrd0(data)*2), col="red" )
lines(x, Fhat2(x,data, bw=bw.nrd0(data)*0.5), col="blue" )
lines(x, pnorm(x,mean(data),sd(data)), col="grey", lwd=2)
lines(x, pnorm(x,  HL(data),SH(data)), lty=2)
#=======================================================================
