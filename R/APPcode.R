# Project: visualizationEXTRA
# 
# Author: vvanbell
###############################################################################


#' Run R Shiny app
#' 
#' Run a selected R Shiny application to illustrate the working of the VRPM package.
#' 
#' Different applications are possible.  To illustrate the visualization of a logistic regression model, 
#' an application on the Intermittent Claudication model (see references) can be loaded using 
#'    \code{example}=	"IntermittentClaudication". Two illustrations for the visualization of support vector machine 
#' classifiers can be loaded using \code{example}="Iris" and \code{example}="Pima".  To illustrate the possibilities 
#' for visualizing multinomial logistic regression models, use \code{example}="xsnz".
#' 
#' @param example The name of the application that should be loaded.  \code{example} should be one of
#' "IntermittentClaudication", "Iris", "Pima" or "xsnz".  
#' @example /inst/examples/Appex.R
#' @author Vanya Van Belle
#' @importFrom shiny runApp
#' @references Van Belle V., Van Calster B., \emph{Visualizing risk prediction models}, PLoS ONE, 10(7):e0132614. doi:10.1371/journal.pone.0132614 (2015).
#' @references Van Belle V., Van Calster B., Suykens J.A.K., Van Huffel S. and Lisboa P., \emph{Explaining support vector machines: a color based nomogram}, Internal Report 16-27, ESAT-Stadius, KU Leuven (Leuven, Belgium), 2016
#' @references Van Belle V., Van Huffel S., Timmerman D., Froyman W., Bourne T. and Van Calster B., \emph{A color based nomogram for Multinomial Logistic Regression}, Internal Report 16-28, ESAT-Stadius, KU Leuven (Leuven, Belgium), 2016
#' @export
runVRPMexample <- function(example) {
	# locate all the shiny app examples that exist
	validExamples <- list.files(system.file("shiny-examples", package = "VRPM"))
	
	validExamplesMsg <-
			paste0(
					"Valid examples are: '",
					paste(validExamples, collapse = "', '"),
					"'")
	
	# if an invalid example is given, throw an error
	if (missing(example) || !nzchar(example) ||
			!example %in% validExamples) {
		stop(
				'Please run `runExample()` with a valid example app as an argument.n',
				validExamplesMsg,
				call. = FALSE)
	}
	
	# find and launch the app
	appDir <- system.file("shiny-examples", example, package = "VRPM")
	shiny::runApp(appDir, display.mode = "normal")
}
