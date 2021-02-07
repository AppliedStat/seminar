# =======================================================
# Talk-1
# =======================================================
# Estimation of the probability when tossing a die or dice

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



# =======================================================
# Talk-2
# =======================================================

# ------------------------------------
# Install rQCC Package
#
# Package Source: 
#      https://cran.r-project.org/web/packages/rQCC/
#
# Vignettes: 
#      https://cran.r-project.org/web/packages/rQCC/vignettes/factors.cc.pdf
#      https://cran.r-project.org/web/packages/rQCC/vignettes/rcc.pdf
# ------------------------------------
  install.packages("rQCC")  

# Call library
  library("rQCC")    

# Help
  help(package="rQCC")   

# ------------------------------------
# Finite-sample breakdown point and RE 
# ------------------------------------
  finite.breakdown (n=10, method="median")   
  RE (n=10, method="median")   

# ------------------------------------
# Fisher-consistent and unbiased MAD 
# ------------------------------------
  x = c(0:5, 50)
  mad(x)          # Fisher-consistent MAD 
  mad.unbiased(x) # unbiased MAD 

# ------------------------------------
# Fisher-consistent and unbiased shamos 
# ------------------------------------
  x = c(0:5, 50)
  shamos(x)          # Fisher-consistent Shamos
  shamos.unbiased(x) # unbiased Shamos 

# ------------------------------------
# Miscellaneous 
# ------------------------------------
  Y0 = c(-2,   -1, 0, 1,  2)
  Y1 = c(-102, -1, 0, 1,102)  

  library("rQCC")

  c(var(Y0), mad(Y0), mad.unbiased(Y0), shamos(Y0) )

  c(var(Y1), mad(Y1), mad.unbiased(Y1), shamos(Y1) )

  finite.breakdown (n=5, method="mad")

  finite.breakdown (n=5, method="shamos")


# =======================================================
# Talk-3
# =======================================================

# Data sets
  x1 = c(2, 2.2,2.4,2.6, NA, NA,3.2, NA,3.6,3.8,  4,4.2)
  x2 = c(93,115, 96,116, NA, NA, NA,103, 95,112,100,120)
  x3 = c(NA, NA, 26, 28, 30, 32, 34, 36, NA, 40, 42, 44)

  V = cov(cbind(x1,x2,x3), use="pairwise")
  cov2cor(V)

# We calculate the above manually. (same result). 
  x1p = c(2, 2.2,2.4,2.6, 3.2, 3.6,3.8,  4,4.2)
  x3p = c(26, 28, 30, 32, 34, 36,  40, 42, 44)
  s1 = sd(x1p)
  s3 = sd(x3p)
  
  x1c = c(2.4, 2.6, 3.2, 3.8, 4.0, 4.2 )
  x3c = c(26,   28,  34,  40,  42,  44 )
  
  x1cbar = mean(x1c)
  x3cbar = mean(x3c)
  NUM = sum( (x1c-mean(x1c))*(x3c-mean(x3c)) ) / 5
  NUM / (s1*s3)


# =======================================================
# Talk-4
# =======================================================

# -------------------------------------------------------
#  Example: Application (t-test)
# -------------------------------------------------------
#  The example below is for constructing X-bar control chart
#      with Hodges-Lehmann and Shamos estimator 
#           instead of sample mean and variance.

   install.packages("rQCC")  # if rQCC is not installed

   library("rQCC")   

   help(rcc)

   tmp = c(
   72, 84, 79, 49, 56, 87, 33, 42, 55, 73, 22, 60, 44, 80, 54, 74,
   97, 26, 48, 58, 83, 89, 91, 62, 47, 66, 53, 58, 88, 50, 84, 69,
   57, 47, 41, 46, 13, 10, 30, 32, 26, 39, 52, 48, 46, 27, 63, 34,
   49, 62, 78, 87, 71, 63, 82, 55, 71, 58, 69, 70, 67, 69, 70, 94,
   55, 63, 72, 49, 49, 51, 55, 76, 72, 80, 61, 59, 61, 74, 62, 57 )
   data2 = matrix(tmp, ncol=4, byrow=TRUE)
   
   out = rcc(data2, loc="HL2", scale="shamos")
   summary(out)
   plot(out)


# -------------------------------------------------------
#  Example: Application (Competing Risks with censoring, masking, etc.)
# -------------------------------------------------------
   source("https://raw.githubusercontent.com/AppliedStat/R-code/master/2006b/Rpp5.R")

   X=c(1.9, 2.1, 3.2, 1.1, 2.1, 1.0, 2.0, 6.1, 3)     # lifetime observation
   M=list(1, 1, 1, 2, 2, 3, 3, 0, c(1,2,3))           # failure modes

#  --------------------
#  We assume that there is full masking in M. 
   expo.cm.EM(X,M)      # Exponential Model
   norm.cm.EM(X,M)      # Normal Model
   norm.cm.EM(log(X),M) # Lognormal Model
   weibull.cm.EM(X,M)   # Weibull Model
   wald.cm.EM(X,M)      # Wald (inverse Gaussian) Model (need to be improved)

#  --------------------
#  We assume that there is partial masking in M. 
   M = list(1, 1, 0, c(2,3), 2, 3, 3, c(1,2), c(1,2,3)) 

   expo.cm.EM(X,M)      # Exponential Model
   norm.cm.EM(X,M)      # Normal Model
   norm.cm.EM(log(X),M) # Lognormal Model
   weibull.cm.EM(X,M)   # Weibull Model
   wald.cm.EM(X,M)      # Wald (inverse Gaussian) Model (need to be improved)


# -------------------------------------------------------
#  Example: Application (grouped data in Nelson, 1982)
# -------------------------------------------------------
source("https://raw.githubusercontent.com/AppliedStat/R-code/master/2018/Rs2.R")

## Data from Nelson (1982). "Applied Life Data Analysis"
X1 = c( 0, 6.12, 19.92, 29.64, 35.40, 39.72, 45.24, 52.32, 63.48 )
X2 = c(    6.12, 19.92, 29.64, 35.40, 39.72, 45.24, 52.32, 63.48, Inf )
f  = c( 5, 16, 12, 18, 18, 2, 6, 17, 73 )

#-----------------------------------------------
Xinterval = cbind( rep(X1,f), rep(X2,f) )
para.exp  = ic.exp.EM(Xinterval, start=1, eps=1E-5)
para.wei  = ic.weibull.QEM(Xinterval, beta0=1, lam0=1, eps=1E-5)

cat("\n\n***** Exponential Model *****\n")
print(para.exp)


cat("\n\n***** Weibull Model ***** \n")
print(para.wei)


