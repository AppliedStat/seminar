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
# Install R Package
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

######################################################
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





