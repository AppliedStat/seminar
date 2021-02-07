#########################################################################
#- Hodges-Lehmann location estimator (i<j)
HLold <- function(x) {
   y = x[-1] + x[1] ;
   n = length(y) ;
   while (n > 1) {
          x = x[-1]
          y = c( y, x[-1] + x[1] ) ;
          n = n - 1 ;
   }
   median(y/2)
}
HL <- function (x, IncludeEqual = FALSE, na.rm = FALSE) {
    if (na.rm) x = x[!is.na(x)]
    w = outer(x, x, "+")
    0.5 * median(w[lower.tri(w, diag = IncludeEqual)])
}
#- Shamos scale estimator 
SH <- function (x, b=1.048358, IncludeEqual=FALSE, na.rm=FALSE) {
    if (na.rm) x <- x[!is.na(x)]
    w1 = outer(x, x, "-")
    w2 = abs(w1[lower.tri(w1, diag = IncludeEqual)])
    b * median(w2)
}
#########################################################################
c4 = function(n) {
     tmp = lgamma(n/2) - lgamma( (n-1)/2)
     sqrt(2/(n-1)) * exp(tmp)
}
#-----------------------------------
d2 = function(n) {
    integrand = function(x) {
                 Phi = pnorm(x)
                 2*(1 - (1-Phi)^n - Phi^n)
    }
    tmp = integrate(integrand, lower=0, upper=Inf)
    return(tmp$value)
}
#-----------------------------------
d3 = function(n) {
   mu = d2(n)
   joint2 = function(x,y) {(y-mu)^2*exp(-(x^2+(x+y)^2)/2)*(pnorm(x+y)-pnorm(x))^(n-2)}
   tmp = integrate(function(y) {
     sapply(y,function(y) {integrate(function(x) joint2(x,y),-Inf,Inf)$value})
   }, 0, Inf)
   sqrt( n*(n-1)/2/pi*tmp$value )
}
#-----------------------------------
A  = function(n) {3/sqrt(n)}
A2 = function(n) {3/d2(n)/sqrt(n)}  
A3 = function(n) {3/c4(n)/sqrt(n)}   

#-----------------------------------
B3 = function(n) {max(0,1-3*sqrt(1-c4(n)^2)/c4(n))} 
B4 = function(n) {1+3*sqrt(1-c4(n)^2)/c4(n)}        

B5 = function(n) {max(0,c4(n)-3*sqrt(1-c4(n)^2))} 
B6 = function(n) {c4(n)+3*sqrt(1-c4(n)^2)}        

##-----------------------------------
D1 = function(n) {max(0,d2(n)-3*d3(n))}  
D2 = function(n) {d2(n)+3*d3(n)}         

D3 = function(n) {max(0,1-3*d3(n)/d2(n))} 
D4 = function(n) {1+3*d3(n)/d2(n)}        
##-----------------------------------



