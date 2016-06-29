shinyUI(			 
	tagList(
		tags$head( # Needed to encrypt the password when sent to the server
			tags$link(rel="stylesheet", type="text/css",href="style.css"),
			tags$script(type="text/javascript", src = "md5.js"),
			tags$script(type="text/javascript", src = "passwdInputBinding.js")
		),
		tags$style(type="text/css",
				   "pre { white-space: pre; word-wrap: normal; overflow-x: auto; font-size: 10pt; }"
		),
		uiOutput('tabs')
	)
)
