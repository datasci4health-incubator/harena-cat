### customNextItem and customUpdateThetas example

library("mirtCAT")

set.seed(1234)
nitems <- 50
itemnames <- paste0('Item.', 1:nitems)
a <- matrix(rlnorm(nitems, .2, .3))
d <- matrix(rnorm(nitems))
pars <- data.frame(a1=a, d=d)
mod <- generate.mirt_object(pars, itemtype='2PL')

# equivalent to criteria = 'MI'
help(findNextItem)
customNextItem <- function(design, person, test){
	item <- findNextItem(person=person, design=design, test=test,
	                     criteria = 'MI')
	item
}

response <- generate_pattern(mod, 1)
result <- mirtCAT(mo=mod, local_pattern = response, start_item = 'MI',
					 design = list(customNextItem=customNextItem))
result
plot(result)

result2 <- mirtCAT(mo=mod, local_pattern = response, start_item = 'MI',
                  criteria = 'MI')
plot(result2)

#-----------------------------------------------------------
# stop after 10 items
customNextItem <- function(person, design, test){
    items_answered <- extract.mirtCAT(person, 'items_answered')
    total <- sum(!is.na(items_answered))
    ret <- NA
    if(total < 10)
        ret <- findNextItem(person=person, test=test, design=design, criteria = 'MI')
    ret
}

res <- mirtCAT(mo=mod, local_pattern=response, start_item = 'MI',
               design = list(customNextItem=customNextItem))
plot(res)

# equivalent to the following
res2 <- mirtCAT(mo=mod, local_pattern=response, start_item = 'MI',
                criteria = 'MI', design = list(max_items = 10))
plot(res2)


#--------------------------------------------------------

# update theta values using a customized map. 
#  If response variability method = 'ML' else 'EAP'
myfun <- function(design, person, test){
    mo <- extract.mirtCAT(test, 'mo')
    responses <- extract.mirtCAT(person, 'responses')
    var_resp <- var(responses, na.rm=TRUE)
    method <- 'EAP'
    if(!is.na(var_resp) && var_resp > 0) method <- 'ML'
    tmp <- fscores(mo, response.pattern = responses, method=method)
    
    # person is a refClass object; hence, can modify elements directly
    person$thetas <- matrix(tmp[,'F1'], 1L)
    person$thetas_SE_history <- rbind(person$thetas_SE_history,
                                      tmp[,'SE_F1', drop=FALSE])
    person$thetas_history <- rbind(person$thetas_history, person$thetas)
    
    # NOTE: mirtCAT version 1.5+ has a much better approach than above 4 lines
    # person$Update_thetas(theta=tmp[,'F1'], theta_SE=tmp[,'SE_F1', drop=FALSE])
    invisible()
}

res <- mirtCAT(mo=mod, local_pattern=response, start_item = 'MI', criteria='MI',
               design = list(customUpdateThetas=myfun))
plot(res)
summary(res)
