# Set the configuration parameters in this file and rename to config.R for
# your specific deployment.

user <- ''
pass <- ''
shinyappsio.secret <- ''

reportFile <- 'daacs-report.Rmd'
cacheDir <- 'results' # Make sure this directory already exists
reportPrefix <- 'DAACS_'
reportNotFoundMsg <- paste0('<p>Survey result not found. You can complete the Diagnostic ',
							'Assessment and Achievement (DAACS)  at ',
							'<a href="http://daacs.net">www.daacs.net</a>.')
