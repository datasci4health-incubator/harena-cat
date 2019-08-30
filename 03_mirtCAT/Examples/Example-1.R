### Basic introduction to mirtCAT

library("mirtCAT")
set.seed(1)

### Define population parameters for CAT ###
nitems <- 100
a <- rlnorm(nitems, .2, .3)
d <- rnorm(nitems)
g <- rep(0.2, nitems)
pars <- data.frame(a1 = a, d = d, g = g)
head(pars)

## Generate mirt_object for CAT session
args(generate.mirt_object)
help(generate.mirt_object)
mirt_object <- generate.mirt_object(pars, itemtype = "3PL")
print(mirt_object)

# where to learn about itemtype
help(mirt)

# inspect general properties of item bank
coef(mirt_object, simplify = TRUE)
plot(mirt_object)
plot(mirt_object, type = 'info')
plot(mirt_object, type = 'SE')

# add a cuttoff line at SE = .5
library(latticeExtra)
plot(mirt_object, type = 'SE') + layer(panel.abline(h=.5, col='red'))

# bundles of items
plot(mirt_object, type = 'trace', which.items = 1:12)
plot(mirt_object, type = 'infotrace', which.items = 1:12)
itemplot(mirt_object, 1)
plot(mirt_object, which.items = 1:12)
plot(mirt_object, type = 'info', which.items = 1:12)

## generate random response pattern
args(generate_pattern)
help(generate_pattern)
pattern <- generate_pattern(mirt_object, Theta = 1)
str(pattern)

# --------------------------------------------------------------------
# fscores can use the simulated pattern as an input
fscores(mirt_object, method = 'ML', response.pattern = pattern)
fscores(mirt_object, method = 'WLE', response.pattern = pattern)
fscores(mirt_object, method = 'MAP', response.pattern = pattern)

# run the CAT for this response pattern using the default 'seq' criteria
args(mirtCAT)
help(mirtCAT)

# by default mirtCAT() will try to terminate early, so force it to use all items
result <- mirtCAT(mo = mirt_object, local_pattern = pattern,
				  design = list(min_items = 100))
print(result)

# plot the CAT session
plot(result)
plot(result, SE = qnorm(.95))
plot(result, SE = qnorm(.975))
plot(result, scales=list(x = list(rot=90))) # turn x-axis labels 90 degrees
plot(result, scales=list(x = list(at=NULL))) # ... or just remove

# summary() returns a list, and indexing from said list can be used for other things
summary(result)
