# ------------------------------------------------------------
library("mirtCAT")
set.seed(1)

# multidimensional IRT model with simple structure (continuation from Example 2)
nitems <- 100
a <- rlnorm(nitems, .2, .3) # from Example 2
d <- runif(nitems, -3, 3)
g <- rep(0.2, nitems)

a <- matrix(c(a, numeric(nitems*2), rlnorm(nitems, .2, .3)), 200)
d <- c(d, runif(nitems, -3, 3))
g <- c(g, rep(0.2, nitems))

pars <- data.frame(a1 = a[,1], a2=a[,2], d = d, g = g)
head(pars)
pars[98:103, ]
tail(pars)

# correlation between traits
lc <- matrix(c(1,.6,.6,1), 2, 2)

mirt_object <- generate.mirt_object(pars, itemtype = "3PL", 
									latent_covariance = lc)
coef(mirt_object, simplify=TRUE)
plot(mirt_object)
itemplot(mirt_object, 1)
itemplot(mirt_object, 1, drop.zeros=TRUE)

## generate random response pattern
pattern <- generate_pattern(mirt_object, Theta = c(0.5, -0.5))

# multidimensional CAT with seq (terminated when SEM < .3 for BOTH estimates)
result_seq <- mirtCAT(mo = mirt_object, local_pattern = pattern)
result_seq
plot(result_seq)

# multidimensional CAT with DPrule (note the start item)
result <- mirtCAT(mo = mirt_object, local_pattern = pattern, 
				  start_item = 'Trule', criteria = 'DPrule')
result
summary(result)
plot(result)

# multidimensional CAT with APrule
result2 <- mirtCAT(mo = mirt_object, local_pattern = pattern, 
				   start_item = 'Trule', criteria = 'APrule')
result2
summary(result2)
plot(result2)

#-------------------------------------------------------------

### Monte Carlo simulation example

library("mirtCAT")
set.seed(1)

### Define population parameters for CAT ###
nitems <- 200
a <- rlnorm(nitems, .2, .3)
d <- rnorm(nitems)
g <- rep(0.2, nitems)
pars <- data.frame(a1 = a, d = d, g = g)
head(pars)

## Generate mirt_object for CAT session
mirt_object <- generate.mirt_object(pars, itemtype = "3PL")

# multiple response patterns, and a parallel option

# pass Theta as a matrix, where each row is a participant 
Thetas <- matrix(rnorm(100))
head(Thetas)
patterns <- generate_pattern(mirt_object, Theta = Thetas)
head(patterns)

# select items according to maximum information criteria
system.time({
	result <- mirtCAT(mo = mirt_object, local_pattern = patterns, 
						 start_item = 'MI', criteria = 'MI')
})

# result is a list or mirtCAT objects
class(result)
length(result)

# first element
result[[1L]]
plot(result[[1L]])

# first three
result[1:3]

# the above is kind of slow, so lets use R's build in parallel computation schemes
library(parallel)
cl <- makeCluster(4) # see also detectCores()

# executed in parallel
system.time({
	result2 <- mirtCAT(mo = mirt_object, local_pattern = patterns, 
					  start_item = 'MI', criteria = 'MI', cl=cl)
})

stopCluster(cl)
