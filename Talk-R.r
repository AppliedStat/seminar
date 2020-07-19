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


