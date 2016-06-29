library(rmarkdown)
library(shinyjs)

source('config.R')

ui <- bootstrapPage(
	shinyjs::useShinyjs(), # Allows us to disable the download button from the server
	downloadButton('downloadReport'),
	htmlOutput('report')
)

server <- function(input, output, session) {
	getResponseID <- function(url_search) {
		query <- parseQueryString(url_search)
		if('responseId' %in% names(query)) {
			return(query[['responseId']])
		} else if('responseid' %in% names(query)) {
			return(query[['responseid']])
		} else {
			return(NULL)
		}
	}
	
	output$report <- reactive({
		responseId <- getResponseID(session$clientData$url_search)
		if(is.null(responseId)) {
			shinyjs::disable("downloadReport")
			# TODO: need a more robust page for when a result is not found.
			return(reportNotFoundMsg)
		} else {
			filename <- paste0(cacheDir, '/', reportPrefix, responseId, '.html')
			if(!file.exists(filename)) { # Render the file
				rmd.envir <- new.env()
				assign('responseId', responseId, envir=rmd.envir)
				rmarkdown::render(reportFile, 
								  html_document(),
								  output_file=filename, 
								  envir=rmd.envir)
			}
			html <- scan(filename, what=character(), sep='\n')
			shinyjs::enable("downloadReport")
			return(paste0(html, collapse='\n'))
		}
	})
	
	output$downloadReport <- downloadHandler(
		filename = function() {
			responseId <- getResponseID(session$clientData$url_search)
			paste0(reportPrefix, responseId, '.html')
		},
		
		content = function(file) {
			responseId <- getResponseID(session$clientData$url_search)
			rmd.envir <- new.env()
			assign('responseId', responseId, envir=rmd.envir)
			out <- rmarkdown::render(reportFile, 
									 html_document(),
							         envir=rmd.envir)
			file.rename(out, file)
		}
	)
}

shinyApp(ui = ui, server = server)

