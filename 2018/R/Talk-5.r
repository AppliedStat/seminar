##############################################################
# (1) Estimation of the probability when tossing a die or dice
##############################################################

# Toss a fair die. 
 set.seed(1)
 ITER = 10
 x = sample(1:6, size=ITER, replace=TRUE)
 table(x) / ITER
 table( factor(x, levels=1:6)) / ITER  # cosmetic

# Toss two dice. Prob. of sum of two
 set.seed(1)
 ITER = 10
 x = sample(1:6, size=ITER, replace=TRUE)
 y = sample(1:6, size=ITER, replace=TRUE)
 table(x+y) / ITER 
 table( factor(x+y, levels=1:12)) / ITER  # cosmetic

# Toss two dice. Prob. of difference of two
 set.seed(1)
 ITER = 1000
 x = sample(1:6, size=ITER, replace=TRUE)
 y = sample(1:6, size=ITER, replace=TRUE)
 prob = table( abs(x-y)) / ITER 
 barplot( height=prob )
 barplot( height=prob, col=1:6 )  # cosmetic

# Toss two UNFAIR dice. Prob. of sum of two
 set.seed(1)
 ITER = 1000
 p1 = c(0.3, 0.1, 0.1, 0.1, 0.1, 0.3) 
 p2 = c(0.1, 0.1, 0.1, 0.1, 0.3, 0.3) 
 x = sample(1:6, size=ITER, replace=TRUE, prob=p1)
 y = sample(1:6, size=ITER, replace=TRUE, prob=p2)
 prob = table(x+y) / ITER 
 barplot( height=prob, col=1:12 )  # cosmetic


##############################################################
# (2) Ratio of the circumference of a circle, PI
##############################################################
 set.seed(1)
 ITER = 1000
 x = runif(ITER, min=-1, max=1)
 y = runif(ITER, min=-1, max=1)
 4 * sum (x^2+y^2 < 1) / ITER 


##############################################################
# (3) Efficiency of median and Hodges-Lehmann
##############################################################

# 
# Load Hodges-Lehmann and Shamos estimators.
source("https://raw.githubusercontent.com/AppliedStat/seminar/master/R/Rsec.R")
# 

ITER = 1000
n = 50
means = medians = HLs = numeric(ITER)

for ( i in 1:ITER ) { 
    x = rnorm(n)
    means[i]   = mean(x)
    medians[i] = median(x)
    HLs[i]     = HL(x)
}
c( var(means) / var(medians),  var(means) / var(HLs) )



#--------------------------
# Misc. : Computing times
#--------------------------

# (a) Computing time using HL()
ITER = 1000; n=100
start = proc.time()
for ( i in 1:ITER ) { 
    x = rnorm(n)
    HLs[i]     = HL(x)
}
end = proc.time()
end - start


# (b) Computing time using HLold()
ITER = 1000; n=100
start = proc.time()
for ( i in 1:ITER ) { 
    x = rnorm(n)
    HLs[i]     = HLold(x)
}
end = proc.time()
end - start




##############################################################
# (4) Breakdown point
##############################################################

n = 100   # Number of observations
k =  20   # Number of contaminated obs. 
x = c(runif(n-k), rep(Inf,k))
c(median(x), HL(x) )
k / n 

# Repeat the above with a different k.
n = 100   # Number of observations

for ( k in 1: (n/2) ) {
     x = c(runif(n-k), rep(Inf,k))
     print( c(k/n,median(x),HL(x)) )
}



##############################################################
# (5) Bias correction factors (d2 and c4)
##############################################################

#-------------------------------------------------------------
# (5-1): d2
#-------------------------------------------------------------
set.seed(1)
n = 10         # sample size 
ITER = 1E4     # The larger, the more accurate d2
MAX = MIN = numeric(ITER)
for ( i in 1:ITER ) {  
    Z = rnorm(n) 
    MAX[i] = max(Z)
    MIN[i] = min(Z)
}
mean(MAX) - mean(MIN)

#-------------------------------------------------------------
# (5-2): c4
#-------------------------------------------------------------
set.seed(1)
n = 10         # sample size 
ITER = 1E4     # The larger, the more accurate d2
S = numeric(ITER)
for ( i in 1:ITER ) {  
    Z = rnorm(n) 
    S[i] = sd(Z)
}
mean(S)


#-------------------------------------------------------------
# (5-3): s vs. s/c4 vs vs R vs R/d2 
#-------------------------------------------------------------
# Load d2 and c4. Or use Table in Talk-5-supplemental.pdf
source("https://raw.githubusercontent.com/AppliedStat/seminar/master/R/Rsec.R")
set.seed(1) 
ITER = 1E5 
R = Rd2 = SD = SDc4 = numeric(ITER) 
for ( i in 1:ITER ) {
    X = rnorm(n)
    R[i]  = diff(range(X))
    SD[i] = sd(X)
}
Rd2 = R / d2(n)
SDc4 = SD / c4(n) 

# Bias of estimators
c( mean(R)-1, mean(Rd2)-1, mean(SD)-1, mean(SDc4)-1 )
# Var. of estimators 
c( var(R), var(Rd2), var(SD), var(SDc4) )



##############################################################
# (6) Power of statistical hypothesis testing
##############################################################

#-------------------------------------------------------------
# (6-1): Example 8.5-2 from Hogg/Tanis/Zimmerman (2015)
#-------------------------------------------------------------

# Theoretical power 
K = function(mu) { 1-pnorm( (62-mu)/2 ) } 

# Empirical power (through simulation)
set.seed(1); 
ITER = 10000
n=25; sigma=10; mu=65
count = 0
for ( i in 1:ITER ) { 
    X = rnorm(n=n, mean=mu, sd=sigma)
    # count if it is in a critical region.
    if (mean(X) > 62) count = count+1  
}
c( count / ITER, K(65) )

#-------------
# Figure 8.5-2
#-------------

# Empirical power (through simulation)
set.seed(1)
ITER = 1000
n=25; sigma=10; MU=seq(60, 68, length=81)
power = numeric(length(MU)) 
for ( j in 1:length(MU) ) { 
    mu = MU[j] 
    for ( i in 1:ITER ) { 
        X = rnorm(n=n, mean=mu, sd=sigma)
        if ( mean(X) >= 62 ) power[j] = power[j] + 1/ITER
   }
}
# Compare the empirical power with the theoretical power 
plot(MU, K(MU), xlim=c(58,68), ylim=c(0,1),  type="l" )
lines(MU, power, col="red")

#-------------------------------------------------------------
# (6-2): Empirical power of z-test (sigma is known)
#-------------------------------------------------------------
# Theoretical Power 
Kz = function(mu, alpha, mu0, sigma, n) {
    z.cut = qnorm(1-alpha/2)
    tmp = (mu-mu0)/(sigma/sqrt(n))
    pnorm(z.cut + tmp, lower.tail=FALSE) + pnorm(-z.cut + tmp)
}
# Empirical Power
set.seed(1); ITER = 1E4
mu=1.5; mu0=0.5; alpha=0.05; sigma=1; n=5
power = 0
zcut = qnorm(1-alpha/2)
for ( i in 1:ITER ) { 
  X = rnorm(n, mean=mu, sd=1)
  if(abs(mean(X)-mu0)/(sigma/sqrt(n)) > zcut) power=power+1/ITER
}
# Compare 
c( Kz(mu, alpha, mu0, sigma, n), power ) 


#-------------------------------------------------------------
# (6-3): Empirical power of t-test (sigma is NOT known)
#-------------------------------------------------------------
# Theoretical Power 
Kt = function(mu, alpha, mu0, sigma, n) {
    t.cut = qt(1-alpha/2,df=n-1)
    ncp = (mu-mu0)/(sigma/sqrt(n))
    pt(t.cut,df=n-1,ncp=ncp, lower.tail=FALSE) + pt(-t.cut,df=n-1,ncp=ncp)
}
# Empirical Power
set.seed(1); ITER = 1E4
mu=1.5; mu0=0.5; alpha=0.05; sigma=1; n=5
power = 0
tcut = qt(1-alpha/2, df=n-1)  # tcut is used instead of zcut
for ( i in 1:ITER ) {
  X = rnorm(n, mean=mu, sd=1)
  # sd(X) is used instead of known sigma
  if(abs(mean(X)-mu0)/(sd(X)/sqrt(n)) > tcut) power=power+1/ITER 
}
# Compare 
c( Kt(mu, alpha, mu0, sigma, n), power )


##############################################################
# (7) Development of rt.test R Package
##############################################################

#-------------------------------------------------------------
# (7-1): Warming-up
#-------------------------------------------------------------

set.seed(1)
ITER = 1.0E4; n=6;
Tstat = numeric(ITER)
for ( i in 1:ITER ) { 
    Z = rnorm(n) 
    Tstat[i] = mean(Z) / (sd(Z)/sqrt(n))
}

# Critical values 
quantile(Tstat, prob=0.975)
qt(0.975, df=n-1)
qnorm(0.975)  # 1.96

# Plot 
hist(Tstat,breaks=100,probability=T,xlim=c(-5,5),ylim=c(0,0.4))
lines( density(Tstat))
curve( dnorm(x), xlim=c(-5,5), add=T, col="red" )

#-------------------------------------------------------------
# (7-2): rt.test R package
#-------------------------------------------------------------

set.seed(1)
ITER = 1.0E4; n=6;
TA = numeric(ITER)
for ( i in 1:ITER ) { 
    Z = rnorm(n) 
    TA[i] = sqrt(2*n/pi)*qnorm(3/4)*median(Z)/mad(Z,constant=1)
}

# Install rt.test package
install.packages("rt.test")
library("rt.test")

# Critical values 
quantile(TA, prob=0.975)
q.robustified.t(0.975, n=n) # This Package used ITER = 1.0E8

qnorm(0.975)  # 1.96 (compare it with the above)



##############################################################
# (8) Development of weibullness R Package
##############################################################

set.seed(1)
ITER = 1.0E4; n=23;
cors = numeric(ITER)
for ( i in 1:ITER ) {
   x = sort( rexp(n) )    # Step 1-2
   p = ppoints(n)         # Step 3
   cors[i] = cor(log(-log(1-p)),log(x))    # Step 4
}  # Repeat (i=1,2,...,ITER)               # Step 5

alphas = c(0.01,0.02,0.025,0.5,0.1,0.2) 
quantile(cors, prob=alphas)                # Step 6

# Install weibullness package
install.packages("weibullness")
library("weibullness")
wp.test.critical(alphas,n)   # This package used ITER=1E8









