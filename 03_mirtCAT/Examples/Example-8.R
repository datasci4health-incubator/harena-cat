### shadow CAT example

library("mirtCAT")

set.seed(1234)
nitems <- 50
itemnames <- paste0('Item.', 1:nitems)
a <- matrix(rlnorm(nitems, .2, .3))
d <- matrix(rnorm(nitems))
pars <- data.frame(a1=a, d=d)
mod <- generate.mirt_object(pars, itemtype='2PL')

# find maximum information subject to constraints
#  20 <= sum(xi) <= 30        ### between 20 and 30 items
#  x1 + x2 <= 1               ### items 1 and 2 can't be together
#  x4 == 0                    ### item 4 not included
#  x5 + x6 == 1               ### item 5 or 6 must be included, but not both

# constraint function
constr_fun <- function(design, person, test){
	
	# left hand side constrains 
	#    - 1 row per constraint, and ncol must equal number of items
	mo <- extract.mirtCAT(test, 'mo')
	nitems <- extract.mirt(mo, 'nitems')
	lhs <- matrix(0, 5, nitems)
	lhs[1:2,] <- 1
	lhs[3,c(1,2)] <- 1
	lhs[4, 4] <- 1
	lhs[5, c(5,6)] <- 1
	
	# relationship direction
	dirs <- c(">=", "<=", "<=", '==', '==')
	
	#right hand side
	rhs <- c(20, 30, 1, 0, 1)
	
	#all together
	constraints <- data.frame(lhs, dirs, rhs)
	# browser()
	constraints
}

customNextItem <- function(design, person, test){
	objective <- computeCriteria(person=person, design=design, test=test, 
								 criteria = 'MI') 
	item <- findNextItem(person=person, design=design, test=test,
						 objective=objective)
	item
}

response <- generate_pattern(mod, 1)
result <- mirtCAT(mo=mod, local_pattern = response, start_item = 'MI',
					 design = list(constr_fun=constr_fun,
					 			  customNextItem=customNextItem))
result
plot(result)
summary(result)
summary(result)$items_answered


response <- generate_pattern(mod, 0)
result <- mirtCAT(mo=mod, local_pattern = response, start_item = 'MI',
				  design = list(constr_fun=constr_fun,
				  			  customNextItem=customNextItem))
result
plot(result)
summary(result)
summary(result)$items_answered
