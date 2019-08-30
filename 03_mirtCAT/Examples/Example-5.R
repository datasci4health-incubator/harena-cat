### Formative assessment CAT

library("mirtCAT")
options(stringsAsFactors = FALSE)

# define IRT parameters
set.seed(1234)
nitems <- 120
itemnames <- paste0("Item.", 1:nitems)
a <- matrix(c(rlnorm(nitems/2, 0.2, 0.3), rnorm(nitems/4, 0, 0.3), numeric(nitems/2),
			  rnorm(nitems/4, 0, 0.3), rlnorm(nitems/2, 0.2, 0.3)), nitems)
d <- matrix(rnorm(nitems))
pars <- data.frame(a, d)
colnames(pars) <- c("a1", "a2", "d")
head(pars)
tail(pars)
pars[57:62, ]
View(pars)

# latent trait relationship
trait_cov <- matrix(c(1, 0.5, 0.5, 1), 2, 2)

# create mirt_object
mod <- generate.mirt_object(pars, itemtype = "2PL", latent_covariance = trait_cov)
plot(mod)
plot(mod, type = 'info')

# bundles
plot(mod, type = 'info', which.items = 1:30)
plot(mod, type = 'info', which.items = 31:60)
plot(mod, type = 'info', which.items = 61:90)
plot(mod, type = 'info', which.items = 91:120)

# --------------------------------------------------------

# math items definitions addition for one factor and multiplication for the other
questions <- answers <- character(nitems)
options <- matrix("", nitems, 5)
spacing <- floor(d - min(d)) + 1  #easier items have more variation

for (i in 1:nitems) {
	if (i < 31) {
		# addition
		n1 <- sample(1:100, 1)
		n2 <- sample(101:200, 1)
		ans <- n1 + n2
		questions[i] <- paste0(n1, " + ", n2, " = ?")
	} else if (i < 61) {
		# addition and multiplication
		n1 <- sample(1:50, 1)
		n2 <- sample(51:100, 1)
		m1 <- sample(1:10, 1)
		m2 <- sample(1:10, 1)
		ans <- n1 + n2 + m1 * m2
		questions[i] <- paste0(n1, " + ", n2, " + ", m1, " * ", m2, " = ?")
	} else if (i < 91) {
		# multiplication and addition
		n1 <- sample(1:10, 1)
		n2 <- sample(1:10, 1)
		m1 <- sample(1:25, 1)
		m2 <- sample(1:25, 1)
		ans <- n1 + n2 + m1 * m2
		questions[i] <- paste0(m1, " * ", m2, " + ", n1, " + ", n2, " = ?")
	} else {
		# multiplication
		m1 <- sample(1:50, 1)
		m2 <- sample(1:50, 1)
		ans <- n1 + n2 + m1 * m2
		questions[i] <- paste0(m1, " * ", m2, " = ?")
	}
	answers[i] <- as.character(ans)
	ch <- ans + sample(c(-5:-1, 1:5) * spacing[i, ], 5)
	ch[sample(1:5, 1)] <- ans
	options[i, ] <- as.character(ch)
}

# load list of items and their answers
df <- data.frame(Question = questions, Option = options, Answer = answers,
                 Type = rep(c("radio", "select"), each=60))
head(df)
tail(df)

# --------------------------------------------------------
# some options set to override defaults

design <- list(max_items = 5)

# change aesthetics of GUI, including title, authors, header, and initial message
title <- "Example Test"
authors <- "I. M. D. Author"
firstpage <- list(h2("Example Test"), h5("Please answer each item to the best of your ability.\n
										 The results of this test will remain completely anonymous\n
										 and are only used for research purposes."))
shinyGUI <- list(title = title, authors = authors, firstpage = firstpage)

# run the customized GUI interface
results <- mirtCAT(df = df, mo = mod, criteria = "Drule", start_item = "DPrule",
				   shinyGUI = shinyGUI, design=design)
summary(results)
plot(results)

# --------------------------------------------------------

# customized answer function. Useful when input is text-based
df$Type[1] <- 'text'
AnswerFuns <- as.list(rep(NA, nrow(df)))
AnswerFuns[[1]] <- function(text){
	text <- tolower(text)
	text == '147' || (grepl('one', text) && grepl('forty', text) && grepl('seven', text))
}
results2 <- mirtCAT(df = df, mo = mod, criteria = "Drule", AnswerFuns=AnswerFuns,
					shinyGUI = shinyGUI, design = list(max_items=1))
summary(results2)

# --------------------------------------------------------

### last page function to print results

# person is an internal refClass object (we'll talk more about this later)
lastpagefun <- function(person){
	#browser() ## browser can be helpful here to see the contents of 'person'
	est <- extract.mirtCAT(person, 'thetas')
	return(list(h4("You have successfully completed the interface."),
				h5(sprintf("Your final ability estimates to two decimal places are %.2f and %.2f.",
						   est[1], est[2]))))
}
results <- mirtCAT(df = df, mo=mod, design = list(max_items=3),
				   shinyGUI = list(lastpage=lastpagefun))
summary(results)
