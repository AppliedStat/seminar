# 
# Install library from R package repository or CRAN
#   https://cran.r-project.org/web/packages/rt.test/
#   https://github.com/AppliedStat/R (latest version)

 install.packages("rt.test")  # Install directly from CRAN

 library(rt.test)


# ----------------------------------
# Darwin Data 
X = c(23.5, 12, 21, 22, 19.125, 21.5, 22.125, 20.375, 18.25, 21.625,
      23.25, 21, 22.125, 23, 12)
Y = c(17.375, 20.375, 20, 20, 18.375, 18.625, 18.625, 15.25, 16.5,
      18, 16.25, 18, 12.75, 15.5, 18)
d = X-Y 

#--------------------------------------------
 t.test(d)
rt.test(d)

boxplot(d)

#--------------------------------------------
# After deleting two probable outliers (2nd and 15th values).
d0 = d[c(1, 3:14)]

 t.test(d0)
rt.test(d0)


#=================================================


#---------------------------------
Delta = seq(10, 25, by=0.1)
nN = length(Delta)

CI0 = array( dim=c(nN,2) )
CIA = array( dim=c(nN,2) )
CIB = array( dim=c(nN,2) )
pvalue0 = numeric( nN )
pvalueA = numeric( nN )
pvalueB = numeric( nN )

for ( i  in 1:nN ) {
     Ynoised = c(Y[1:14],Delta[i])

     tmp = t.test(X-Ynoised)
     CI0[i,] = tmp$conf.int
     pvalue0[i] = tmp$p.value

     tmp =rt.test(X-Ynoised, test.stat="TA")
     CIA[i,] = tmp$conf.int
     pvalueA[i] = tmp$p.value

     tmp =rt.test(X-Ynoised, test.stat="TB")
     CIB[i,] = tmp$conf.int
     pvalueB[i] = tmp$p.value
}


#------------------------------------------------------------------
par(mfrow=c(1,2), mar=c(7,5,3,1),omi=c(0,0,0,0),cex=0.5,mex=0.5)
#------------------------------------------------------------------

#==========================================================================
 plot(Delta, CI0[,1], type="l", ylim=range(CI0,CIA,CIB), sub="(a)",
      xlab=expression(delta), ylab="Lower and Upper ends" )
lines(Delta, CI0[,2] )

lines(Delta, CIA[,1], lty=2 )
lines(Delta, CIA[,2], lty=2 )

lines(Delta, CIB[,1], lty=3, col="red" )
lines(Delta, CIB[,2], lty=3, col="red" )

legend(20, 1.3, "Lower End of CI", bty="n")
legend(20, 5.4, "Upper End of CI", bty="n")

legend(10, 3.5,
      legend=c("CI using t-test(paired)", "CI using t-test(TA)", "CI using t-test(TB)"),
      bty="n", lty=1:3, col=c("black","black","red") )
abline(h=0, col="green", lty=2 )
lines( c(18.028, 18.028), c(-1.5,0), col="green", lty=2)
text(18.028, -1.0, "18.028")
#==========================================================================



#==========================================================================
 plot(Delta, pvalue0, type="l", ylim=c(0,0.14), sub="(b)",
      xlab=expression(delta), ylab="p-value" )
lines(Delta, pvalueA, lty=2 )
lines(Delta, pvalueB, lty=3, col="red" )
abline(h=0.05, col="green", lty=2 )
lines( c(18.028, 18.028), c(-0.5,0.05), col="green", lty=2)
text(18.028, 0.0, "18.028")

legend(10, 0.10,
      legend=c("CI using t-test(Student)", "CI using t-test(TA)", "CI using t-test(TB)"),
      bty="n", lty=1:3, col=c("black","black","red") )
#==========================================================================



