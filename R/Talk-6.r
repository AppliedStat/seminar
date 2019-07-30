##############################################################
# Case study: Cauchy 
# NB: No contamination (outliers are surprising values)
##############################################################

set.seed(14)  # Change this seed.
n = 10;       # n = 10000
x = rcauchy(n)
y = rnorm(n)

OUT = rbind( c(mean(x),sd(x)), c(mean(y),sd(y)) )
colnames(OUT) = c("mean",  "SD") 
rownames(OUT) = c("Cauchy(x)","Normal(y)") 
OUT
x
y

#==============================================================
# With a large sample
#--------------------------------------------------------------

n = 10000
x = rcauchy(n)
y = rnorm(n)

OUT = rbind( c(mean(x),sd(x)), c(mean(y),sd(y)) )
colnames(OUT) = c("mean",  "SD")
rownames(OUT) = c("Cauchy(x)","Normal(y)")
OUT
x
y

#==============================================================






