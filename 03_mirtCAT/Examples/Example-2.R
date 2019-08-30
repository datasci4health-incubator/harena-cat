### Off-line adaptive testing

library("mirtCAT")
set.seed(1)

### Define population parameters for CAT ###
nitems <- 100
a <- rlnorm(nitems, .2, .3)
d <- runif(nitems, -3, 3)
g <- rep(0.2, nitems)
pars <- data.frame(a1 = a, d = d, g = g)
head(pars)

## Generate mirt_object for CAT session
mirt_object <- generate.mirt_object(pars, itemtype = "3PL")

## generate random response pattern
pattern <- generate_pattern(mirt_object, Theta = 1)

# select items using seq criteria (by default, terminated when SE(theta) < .3)
result_seq <- mirtCAT(mo = mirt_object, local_pattern = pattern)
result_seq
plot(result_seq)

# select items according to maximum information criteria
result_MI <- mirtCAT(mo = mirt_object, local_pattern = pattern, 
					 start_item = 'MI', criteria = 'MI')
result_MI
plot(result_MI)

# same as above, but with ML ability estimator (note: MAP used until a plausible ML optim exists)
result_MI.ML <- mirtCAT(mo = mirt_object, local_pattern = pattern, 
						method = 'ML', start_item = 'MI', criteria = 'MI')
result_MI.ML
plot(result_MI.ML)

# change default arguments
result <- mirtCAT(mo = mirt_object, local_pattern = pattern, 
				  method = 'ML', start_item = 'MI', criteria = 'MI',
				  design = list(min_SEM = .4, thetas.start = -1))
result
plot(result)
