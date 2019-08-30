# Customized item stimuli

library("mirtCAT")

options(stringsAsFactors = FALSE)
options <- rbind(c(10, 20, 1236), c('True', 'False', NA), c('True', 'False', NA))

# external files
df <- data.frame(Question = c("", "", "Just a standard stem."), Option = options, Type = "radio",
				 Stem = c('Math-stem.html', 'Question.md', ''), inline = TRUE)
df
results <- mirtCAT(df = df, shinyGUI = list(forced_choice = FALSE))

#----------------------------------------------------------

# expressions
# see https://shiny.rstudio.com/articles/tag-glossary.html for a list of tags in shiny with htmltools
df <- data.frame(Question = c('', 'tags$h1("My header")', 'tags$b("This text is bold.")'),
				 Option = options, Type = "radio",
				 Stem = c('Math-stem.html', '', ''),
				 StemExpression = c(FALSE, TRUE, TRUE))
df
results <- mirtCAT(df = df, shinyGUI = list(forced_choice = FALSE))

# divs
df <- data.frame(Question = c('div(HTML("Here is <strong>one</strong> way to insert <em>arbitrary</em> HTML."))',
							  'div(tags$style("#text { font-size: 35px; height: 200px; overflow: auto; }"),
                                         div(id = "text", paste(names(tags), collapse = ", ")))',
							  'div(tags$style("#text { font-size: 20px; height: 65px; overflow: auto; }"),
                                         div(id = "text", paste(names(tags), collapse = ", ")))'),
				 Option = options, Type = "radio",
				 Stem = c('', '', ''),
				 StemExpression = rep(TRUE, 3))
results <- mirtCAT(df = df, shinyGUI = list(forced_choice = FALSE))

#-----------------------------------------------------------------------
# audio/video
dirname <- paste0(getwd(), '/www')
shiny::addResourcePath('www', dirname)
df <- data.frame(Question = c('',
							  'tags$audio(src = "www/clip.mp3", type = "audio/mp3",
                                    controls = TRUE)',
							  'tags$video(src = "www/vid.mp4", type = "video/mp4",
                                    autoplay = TRUE, controls = TRUE, height=300, width=400)'),
				 Option = options, Type = "radio",
				 Stem = c('Math-stem.html', '', ''),
				 StemExpression = c(FALSE, TRUE, TRUE))
results <- mirtCAT(df = df[2:3, ], shinyGUI = list(forced_choice = FALSE))

#-----------------------------------------------------------------------

# example of DOGS
# creates a multi-tab type stimuli,
myfun <- function(inputId, df_row){
	#browser()   ## uncomment to test the objects manually
	with(df_row,
		 tabsetPanel(tabPanel("Panel 1", tab.1),
		 			if(!is.na(tab.2)) tabPanel("Panel 2", tab.2) else "",
		 			if(!is.na(tab.3)) tabPanel("Panel 3", tab.3) else "",
		 			tabPanel("Response", Question,
		 					 radioButtons(inputId = inputId, label='',
		 					 			 choices = c(Options.1, Options.2), selected = ''))))
}

questions <- c('1) Text for response','2) More text for response','3) Text for radio response')
tabs <- rbind(c('Text for tab1', 'Text for tab2', 'Text for tab3'),
			  c('Different text for tab1', 'Different for tab2', NA), NA)
options <- matrix(c('True', 'False'), 3, 2, byrow=TRUE)

df <- data.frame(Question = questions, Options=options, Type = c('myQ', 'myQ', 'radio'),
				 tab=tabs, stringsAsFactors = FALSE)
df

results <- mirtCAT(df = df, customTypes=list(myQ=myfun))
summary(results)
