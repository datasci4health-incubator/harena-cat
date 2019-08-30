### Simple GUI example

library("mirtCAT")

# why this isn't the default these days I'll never know....
options(stringsAsFactors = FALSE)

# simple interface
options <- matrix(c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree"),
				  nrow = 3, ncol = 5, byrow = TRUE)
questions <- c("Building CATs with mirtCAT is difficult.",
			   "mirtCAT requires a substantial amount of coding.",
			   "I would use mirtCAT in my research.")
df <- data.frame(Question = questions, Option = options, Type = "radio")
df

# survey with defaults
results <- mirtCAT(df = df)
results
summary(results)

# no neutral option for first item
df2 <- df
df2$Option.3[1L] <- NA
df2
results <- mirtCAT(df = df2)
summary(results)

#--------------------------------------------------------
# Math Equations
df3 <- df
df3$Question[1] <- 'Which Greek letter is missing in the following linear regression equation? 
      \\begin{equation} y = (ANSWER) + \\beta_1 x_1 + \\beta_2 x_2 \\end{equation}'
df3$Option.1[1] <- '\\(\\gamma_0\\)'
df3$Option.2[1] <- '\\(\\beta_0\\)'
df3$Option.3[1] <- '\\(\\theta_0\\)'
df3$Option.4[1] <- '\\(\\delta_0\\)'
df3$Option.5[1] <- '\\(\\psi_0\\)'

# set firt item radio buttons inline rather than stacked
?radioButtons
df3$inline <- FALSE
df3$inline[1] <- TRUE
View(df3)

# max 2 items, forced-choice disabled
results <- mirtCAT(df = df3, shinyGUI = list(forced_choice = FALSE),
				   design = list(max_items = 2))
summary(results)

#---------------------------------------------------

# theme, password, and temp file (to restore interupted session)
library(shinythemes)
?shinythemes

results <- mirtCAT(df=df, shinyGUI=list(theme="superhero",
										password = data.frame('1234', '4321'),
										forced_choice = FALSE,
										temp_file = 'temp.rds'))
summary(results)
dir()
readRDS('temp.rds') #internal state of 'person' object (more on this later)

# rerun original code to continue
dir()
summary(results)


