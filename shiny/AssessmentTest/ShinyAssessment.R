#' Create Shiny UI for multiple choice assessment.
#' 
#' 
#' NOTE: This function will create an object \code{SHOW_ASSESSMENT} in the 
#' calling environment. This object is used to determine whether the assessment
#' should be shown or not. This object will be shared across multiple
#' \code{ShinyAssessment} instances.
#' 
#' @param input from \code{shinyServer}.
#' @param output from \code{shinyServer}.
#' @param session from \code{shinyServer}.
#' @param name the name of the assessment. This should be a name that follows
#'        R's naming rules (i.e. does not start with a number, no spaces, etc).
#' @param callback function called when the user submits the assessment. Used
#'        for saving the results.
#' @param item.stems a character vector or list with the item stems. If a list,
#'        any valid Shiny output is allowed (e.g. \code{p}, \code{div}, 
#'        \code{fluidRow}, etc.). For character vectors HTML is allowed.
#' @param item.choices a data frame with the item answers. For items that have 
#'        fewer choices than the total number of
#'        columns, place \code{NA} in that column's value. The results will be
#'        passed to the \code{callback} function as named list where the value
#'        is the name of the column selected.
#' @param start.label The label used for the link and button created to start
#'        the assessment.
#' @param itemsPerPage the number of items to display per page.
#' @param inline If \code{TRUE}, render the choices inline (i.e. horizontally).
#' @param width The width of the radio button input.
#' @param cancelButton should a cancel button be displayed on the assessment.
#' @param useMathJax set to \code{TRUE} if any of the items contain MathJax
#'        equations. You will also need to call \code{withMathJax()} in your UI.
#' @param RADIO_FUN the function to use to generate the radio buttons. Typically
#'        would be \code{shiny::radioButtons} but could be \code{radioButtons_withHTML}.
#'        See http://stackoverflow.com/questions/35783446/can-you-have-an-image-as-a-radiobutton-choice-in-shiny
#'        for this function.
#' @return Returns a list with the following values:
#'         \itemize{
#'         \item{ui.name}{the name of the UI put on the output object for the items.}
#'         \item{link.name}{the name of the UI element for the start assessment link.}
#'         \item{button.name}{the name of the UI element for the start assessment button.}
#'         }
#' @export
ShinyAssessment <- function(input, output, session,
							name, callback,
							item.stems, item.choices,
							start.label = 'Start the Assessment',
							itemsPerPage = 1,
							inline = FALSE,
							width = '100%',
							cancelButton = TRUE,
							useMathJax = FALSE,
							RADIO_FUN = radioButtons
) {
	stopifnot(length(item.stems) == nrow(item.choices))
	
	if(!exists('SHOW_ASSESSMENT', envir = parent.env(environment()))) {
		# A bit of a hack and knowingly bad form. This will put an object in
		# the calling environment. This will allow for multiple asssessments
		# to be run in the same Shiny app.
		assign('SHOW_ASSESSMENT', 
			   value = reactiveValues(show = FALSE, assessment = NULL, 
			   					      unique = format(Sys.time(), '%Y%m%d%H%M%S')),
			   envir = parent.env(environment())
		)
	}
	
	ASSESSMENT <- reactiveValues(
		currentPage = 1,
		responses = rep(as.integer(NA), length(item.stems))
	)
	
	# Names of various UI elements. Note that for radio, next, cancel, and done
	# buttons the name has SHOW_ASSESSMENT$unique concatenated, which is the
	# current time in seconds when the assessment was started. This ensures that
	# a unique set of buttons are created for each assessment. Otherwise, answers
	# would be carried over from prior assessments. This is especially problematic
	# since the buttons remain in the session but previous responses not shown
	# to the user.

	link.name <- paste0('Start', name, 'Link')
	button.name <- paste0('Start', name, 'Button')
	cancel.name <- paste0('Cancel', name, 'Button')
	ui.name <- paste0(name, 'Items')
	save.name <- paste0(name, 'Save')
	page.name <- paste0(name, 'Page')
	totalPages <- ceiling(length(item.stems) / itemsPerPage)
	
	output[[link.name]] <- renderUI({
		observe({
			if(!is.null(input[[paste0(link.name, SHOW_ASSESSMENT$unique)]])) {
				if(input[[paste0(link.name, SHOW_ASSESSMENT$unique)]] == 1) {
					SHOW_ASSESSMENT$show <- TRUE
					SHOW_ASSESSMENT$assessment <- ui.name
				}
			}
		})
		
		actionLink(paste0(link.name, SHOW_ASSESSMENT$unique), start.label)
	})

	output[[button.name]] <- renderUI({
		observe({
			if(!is.null(input[[paste0(button.name, SHOW_ASSESSMENT$unique)]])) {
				if(input[[paste0(button.name, SHOW_ASSESSMENT$unique)]] == 1) {
					SHOW_ASSESSMENT$show <- TRUE
					SHOW_ASSESSMENT$assessment <- ui.name
				}
			}
		})
		
		actionButton(paste0(button.name, SHOW_ASSESSMENT$unique), start.label)
	})

	output[[cancel.name]] <- renderUI({
		observe({
			if(!is.null(input[[paste0(cancel.name, SHOW_ASSESSMENT$unique)]])) {
				if(input[[paste0(cancel.name, SHOW_ASSESSMENT$unique)]] == 1) {
					# TODO: Should the callback function be called with the
					#       incomplete results?
					SHOW_ASSESSMENT$show <- FALSE
					SHOW_ASSESSMENT$assessment <- NULL
					SHOW_ASSESSMENT$unique <- format(Sys.time(), '%Y%m%d%H%M%S')
					ASSESSMENT$currentPage <- 1
					ASSESSMENT$responses <- rep(as.integer(NA), length(item.stems))
				}
			}
		})
		
		actionButton(paste0(cancel.name, SHOW_ASSESSMENT$unique), 'Cancel')
	})
	
	output[[ui.name]] <- renderUI({
		# Build a list of radioButtons for each item.
		buttons <- list()
		for(i in seq_len(length(item.stems))) {
			choices <- character()
			for(j in 1:ncol(item.choices)) {
				if(!is.na(item.choices[i,j])) {
					choices[(j)] <- names(item.choices)[j]
					names(choices)[(j)] <- HTML(item.choices[i,j])
				}
			}
			button.label <- ''
			if(is.character(item.stems)) {
				button.label <- HTML(item.stems[i])
			} else {
				button.label <- item.stems[[i]]
			}
			buttons[[i]] <- RADIO_FUN(inputId = paste0(name, i, SHOW_ASSESSMENT$unique), 
										 label = button.label,
										 choices = choices, 
										 inline = inline,
										 selected = character(),
										 width = width)
		}

		startPos <- ((ASSESSMENT$currentPage - 1) * itemsPerPage) + 1
		pos <- seq(startPos, min( (startPos + itemsPerPage - 1), length(buttons)))
	
		observe({
			# Save the results
			if(SHOW_ASSESSMENT$show & 
			   !is.null(input[[paste0(save.name, SHOW_ASSESSMENT$unique)]])
			) {
				if(input[[paste0(save.name, SHOW_ASSESSMENT$unique)]] == 1) {
					results <- character(length(item.stems))
					for(i in seq_len(length(buttons))) {
						ans <- input[[paste0(name, i, SHOW_ASSESSMENT$unique)]]
						results[i] <- ifelse(is.null(ans), NA, ans)
					}
					# Do callback
					callback(results)
					# Reset for another assessment
					SHOW_ASSESSMENT$show <- FALSE
					SHOW_ASSESSMENT$assessment <- NULL
					SHOW_ASSESSMENT$unique <- format(Sys.time(), '%Y%m%d%H%M%S')
					ASSESSMENT$currentPage <- 1
					ASSESSMENT$responses <- rep(as.integer(NA), length(item.stems))
				}
			}
		})
	
		# Increment the page
		nextButtonName <- paste(page.name, ASSESSMENT$currentPage, SHOW_ASSESSMENT$unique)
		if(!is.null(input[[nextButtonName]])) {
			if(input[[nextButtonName]] == 1) {
				for(i in seq( ((ASSESSMENT$currentPage - 1) * itemsPerPage) + 1,
							  ASSESSMENT$currentPage * itemsPerPage, by=1) ) {
					ans <- input[[paste0(name, i, SHOW_ASSESSMENT$unique)]]
					ASSESSMENT$responses[i] <- ifelse(is.null(ans), NA, ans)
				}
				ASSESSMENT$currentPage <- ASSESSMENT$currentPage + 1
				nextButtonName <- paste0(page.name, ASSESSMENT$currentPage)
			}
		}
		
		# Next or Done button
		if(ASSESSMENT$currentPage == totalPages) {
			nextButton <- actionButton(paste0(save.name, SHOW_ASSESSMENT$unique), 'Done')
		} else {
			nextButton <- actionButton(nextButtonName, 'Next')
		}
		
		# Force the page to scroll to the top on the next page and to
		# render any MathJax equations.
		if(useMathJax) {
			js <- '<script language="JavaScript">
				   document.body.scrollTop = document.documentElement.scrollTop = 0;
				   MathJax.Hub.Queue(["Typeset",MathJax.Hub]);
				   </script>'
		} else {
			js <- '<script language="JavaScript">
				   document.body.scrollTop = document.documentElement.scrollTop = 0;
			       </script>'
		}
		
		mainPanel(width=12,
			br(),
			buttons[pos],
			br(),
			fluidRow(
				column(width=2, uiOutput(cancel.name)),
				column(width=8, p(paste0('Page ', ASSESSMENT$currentPage, ' of ', totalPages)), 
					   align='center'),
				column(width=2, nextButton)
			),
			br(),
			HTML(js)
		)
	})
	
	return(list(ui.name = ui.name,
				link.name = link.name,
				button.name = button.name
	))
}
