#' This over rides the function built into Shiny to allow for local MathJax
#' access.
withMathJax <- function (...) {
	#path <- "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
	path <- '/MathJax/MathJax.js?config=TeX-AMS-MML_HTMLorMML'
	tagList(tags$head(singleton(tags$script(src = path, type = "text/javascript"))), 
			..., tags$script(HTML("MathJax.Hub.Queue([\"Typeset\", MathJax.Hub]);")))
}
