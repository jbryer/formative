library(shiny)
library(ggplot2)

source('ShinyAssessment.R')
source('radioButtons_withHTML.R')

# Math items from: http://stattrek.com/ap-statistics/practice-test.aspx
math.items <- read.csv('items.csv', stringsAsFactors=FALSE)
mass.items <- read.csv('mass.csv', stringsAsFactors=FALSE)
read.items <- read.csv('ReadingItems.csv', stringsAsFactors=FALSE)

math.items[math.items$E == '',]$E <- NA

read.stems <- list()
read.stems[[1]] <- div(
			div(includeText('2013-08-B.txt'), style="white-space: pre; word-wrap: normal; 
                       overflow-x: auto; font-size: 11pt; padding: 10px; background: #fffff8;
					   border-style: solid; border-size:1px; border-color: #111111"),
			p(read.items[1,]$Stem))
for(i in 2:nrow(read.items)) {
	read.stems[[i]] <- p(read.items[i,]$Stem)
}

##### User Interface ###########################################################
ui <- shinyUI(fluidPage(
	withMathJax(),
	uiOutput('ui')
))

##### Server ###################################################################
server <- shinyServer(function(input, output, session) {
	# Save the most recent assessment results to display
	assmt.results <- reactiveValues(
		math = logical(),
		mass = integer(),
		reading = logical()
	)
	
	# This function will be called when the assessment is completed.
	saveResults <- function(results) {
		assmt.results$math <- results == math.items$Answer
	}
	
	saveMASSResults <- function(results) {
		assmt.results$mass <- factor(results,
									 levels = names(mass.items)[2:6],
									 ordered = TRUE)
	}
	
	saveReadingResults <- function(results) {
		assmt.results$reading <- results == read.items$Answer
	}
	
	# Provide some basic feedback to students
	output$math.results <- renderText({
		txt <- ''
		if(length(assmt.results$math) > 0) {
			txt <- paste0('You got ', sum(assmt.results$math, na.rm=TRUE),
						  ' of ', length(assmt.results$math), ' items correct.')
		} else {
			txt <- 'No results found. Please complete the statistics assessment.'
		}
		return(txt)
	})
	
	output$mass.results <- renderText({
		txt <- ''
		if(length(assmt.results$mass) == 0) {
			txt <- 'No results found. Please complete the statistics assessment.'
		}
		return(txt)
	})
	
	output$reading.results <- renderText({
		txt <- ''
		if(length(assmt.results$reading) > 0) {
			txt <- paste0('You got ', sum(assmt.results$reading, na.rm=TRUE),
						  ' of ', length(assmt.results$reading), ' items correct.')
		} else {
			txt <- 'No results found. Please complete the reading assessment.'
		}
		return(txt)
	})
	
	output$mass.plot <- renderPlot({
		if(length(assmt.results$mass) > 0) {
			df <- data.frame(Item = mass.items$stem,
							 Response = assmt.results$mass)
			p <- ggplot(df, aes(x=Response, y=Item)) + geom_point()
			return(p)
		} else {
			return(NULL)
		}
	})
	
	# Multiple choice test example
	test <- ShinyAssessment(input, output, session,
							name = 'Statistics',
							item.stems = math.items$Stem,
							item.choices = math.items[,c(4:8)],
							callback = saveResults,
							start.label = 'Start the Statistics Assessment',
							itemsPerPage = 1,
							inline = FALSE,
							useMathJax = TRUE,
							RADIO_FUN = radioButtons_withHTML)
	
	# Likert scale example
	mass <- ShinyAssessment(input, output, session,
							name = 'MASS',
							item.stems = mass.items$stem,
							item.choices = mass.items[,2:6],
							callback = saveMASSResults,
							start.label = 'Take the Math Anxiety Survey',
							itemsPerPage = 7,
							inline = TRUE)
	
	reading <- ShinyAssessment(input, output, session,
							name = 'Reading',
							item.stems = read.stems,
							item.choices = read.items[,6:9],
							callback = saveReadingResults,
							start.label = 'Take the Reading Assessment',
							itemsPerPage = 6,
							inline = FALSE)
	
	output$ui <- renderUI({
		if(SHOW_ASSESSMENT$show) { # The assessment will take over the entire page.
			fluidPage(width = 12, uiOutput(SHOW_ASSESSMENT$assessment))
		} else { # Put other ui components here
			fluidPage(	 			
				titlePanel("Shiny Assessment Example"),
				sidebarLayout(
					sidebarPanel(
						# Show the start assessment link
						h4('Example multiple choice assessment'),
						p('You can use a link'),
						uiOutput(test$link.name),
						p('Or a button to start the assessment'),
						uiOutput(test$button.name),
						hr(),
						h4('Reading assessment with custom stems'),
						uiOutput(reading$button.name),
						hr(),
						h4('Example of a likert survey'),
						uiOutput(mass$button.name)
					),
					mainPanel(
						h3('Statistics Assessment Results'),
						textOutput('math.results'),
						hr(),
						h3('Reading Assessment Results'),
						textOutput('reading.results'),
						hr(),
						h3('Math Anxiety Survey Results'),
						textOutput('mass.results'),
						plotOutput('mass.plot')
					)
			)
			)
		}
	})
})

##### Run the application ######################################################
shinyApp(ui = ui, server = server)

