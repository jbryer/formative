shinyServer(function(input, output, session, clientData) {
source("Login.R",  local=TRUE)

output$tabs <- renderUI({
	tabs <- list()
	tabs[['title']] <- 'Shiny Authentication Test'
	tabs[['id']] <- 'navbar'
	tabs[['theme']] <- 'bootstrap.css'
	tabs[['inverse']] <- FALSE
	tabs[['windowTitle']] <- 'Shiny Authentication Test'

	if(USER$Logged) { 
##### Display application ######################################################
		tabs[['myPage']] <- tabPanel('myPage',
			h4(paste0('You are logged in as ', USER$Username)),
			p(paste0('Email: ', USER$Email)),
			p(paste0('Group: ', USER$Group)),
			uiOutput('uiLogout')
		)
	} else { 
##### Display login and create account tab #####################################
		tabs[['Login']] <- tabPanel('Login',
			fluidRow(
				column(4, offset=1, h3('Login'),
					   uiOutput('uiLogin')),
				column(4, offset=1, h3('Create new account'),
					   uiOutput('uiNewAccount') ) 
			) )
	}

	do.call(navbarPage, tabs)
}) # End renderUI
}) # End shinyServer
