# ====================================
# Talk-2
# ====================================

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
mad(x)  # Fisher-consistent
mad.unbiased(x) # unbiased

# ------------------------------------
# Fisher-consistent and unbiased shamos 
# ------------------------------------
x = c(0:5, 50)
shamos(x) # Fisher-consistent
shamos.unbiased(x) # unbiased


