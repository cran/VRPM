shinyUI(fluidPage(
				tags$script('$(document).on("shiny:connected", function(e) {
								var jsWidth = screen.width;
								Shiny.onInputChange("GetScreenWidth",jsWidth);
								});
								'),
				tags$head(
						tags$style(HTML("
												.shiny-output-error-validation {
												color: red;
												}
												"))
				),
				
				span(h1("Explaining SVM models: an illustration on the IRIS data"),align="center",style="color:darkblue"),
				titlePanel("",windowTitle = "IRIS"),
				sidebarLayout(position = "left",
						sidebarPanel( 
								actionButton("godisclaimer", label = "Read disclaimer"),actionButton("analyse", label = "Analyse"),
								textOutput("value"),
								h4("Options"),
								tabsetPanel(
										tabPanel("Kernel",
												helpText(span(h5("Select options for the SVM model."),style="color:darkblue")),
												selectInput("kernel", label = h6("Select kernel type."),
														choices = list("linear","polynomial","RBF"), selected = "RBF"),
												uiOutput("input_ui")
										),
										tabPanel("Plot",
												helpText(span(h5("Select options for the visualization."),style="color:darkblue")),
												selectInput("zerolevel", label = h6("Select level of functional form corresponding to zero."),
														choices = list("zero","min","mean","median"), selected = "zero"),
												selectInput("coloroptions", label = h6("Select the color map."),
														choices = list("Rainbow","Sequential","Diverging","Black-White","Viridis"), selected = "Sequential"),
												span(h5("Specify the ranges of the continuous predictors."),style="color:darkblue"),
												sliderInput("slRange",label = h6("Range of sepal length (cm)"),min=4.3,max=7.9,step=0.5,value=c(4.3,7.9)),
												sliderInput("swRange",label = h6("Range of sepal width (cm)"),min=2,max=4.4,value=c(2,4.4)),
												sliderInput("plRange",label = h6("Range of petal length (cm)"),min=1,max=6.9,value=c(1,6.9)),
												sliderInput("pwRange",label = h6("Range of petal width (cm)"),min=0.1,max=2.5,value=c(0.1,2.5))
										),
										
										tabPanel("Observation",
												helpText(span(h5("Add observation characteristics."),style="color:darkblue")),
												numericInput("sl", label = h6("What is the sepal length (cm)?"), value = 6),
												numericInput("sw", label = h6("What is the sepal width (cm)?"), value = 2),
												numericInput("pl", label = h6("What is the petal length (cm)?"), value = 3),
												numericInput("pw", label = h6("What is the petal width (cm)?"), value = 2)
										)
								)
						),
						mainPanel(
								h4(textOutput("temp"),align="center",style='color:red'),
								
								navbarPage(	id="righttabs",span("Results",style="color:black") ,
										tabPanel("Colorplot", 
												imageOutput("image")
										), 
										tabPanel("cchart", 
												imageOutput("cchart")
										), 
										tabPanel("ccchart", 
												imageOutput("ccchart")
										),
										navbarMenu("performance", 
												tabPanel("all",
														span(h4("Summary of performance graphics"),
																align="center",style="color:darkblue"),
														fluidRow(
																column(6,
																		span(h6("Visualization of the (dis)agreement between the latent variables of the SVM model and the approximation"),
																				align="center",style="color:darkblue"),
																		span(imageOutput("lp",height="100%"),align="center")
																),
																column(6,
																		span(h6("Visualization of the (dis)agreement between the estimated probabilites of the SVM model and the approximation"),
																				align="center",style="color:darkblue"),
																		span(imageOutput("p",height="100%"),align="center")
																),
																br(),br(),
																column(6,
																		span(h6("Visualization of the (dis)agreement between predicted output of the SVM model and the approximation"),
																				align="center",style="color:darkblue"),
																		span(imageOutput("outcomes",height="100%"),align="center")
																),
																column(6,
																		span(h6("Plot of the contributions of the approximation"),align="center",style="color:darkblue"),
																		span(imageOutput("contributions",height="100%"),align="center")
																),
																column(6,
																		span(h6("ROC curve for SVM model and the approximation"),align="center",style="color:darkblue"),
																		span(imageOutput("roc",height="100%"),align="center")
																),
																column(6,
																		span(h6("Scatter plot of the terms in the approximation and the rest term."),align="center",style="color:darkblue"),
																		span(imageOutput("correlations",height="100%"),align="center")
																)
														)
												),								
												
												tabPanel("latent variables",
														span(h4("Visualization of the (dis)agreement between the latent variables of the SVM model and the approximation"),
																align="center",style="color:darkblue"),
														span(imageOutput("lp2",height="100%"),align="center"),
														br(),
														"The latent variable of the approximation is plotted against the latent variable of the SVM model (circles).  The latent variable
																of the approximation is calculated as the sum of all contributions for one observation, b (i.e. the bias of 
																the SVM b), and the median of the rest term.  The line indicates the ideal situation in which both latent variables
																variables are the same.  The approximation is able to explain the SVM model when all points are near this reference line."
												),
												tabPanel("probabilities",
														span(h4("Visualization of the (dis)agreement between the estimated probabilites of the SVM model and the approximation"),
																align="center",style="color:darkblue"),
														span(imageOutput("p2",height="100%"),align="center"),
														br(),
														"The estimated probabilities of the approximation are plotted against the estimated probabilities of the SVM model (circles).  
																A perfect match is indicated by means of the reference line.  The approximation is able to explain the model when the circles are near the reference line."
												),
												tabPanel("outcomes",
														span(h4("Visualization of the (dis)agreement between predicted output of the SVM model and the approximation"),
																align="center",style="color:darkblue"),
														span(imageOutput("outcomes2",height="100%"),align="center"),
														br(),
														"A bubble plot represents in how many cases the SVM model and its approximation agree on the predicted outcomes."
												),
												tabPanel("contributions",
														span(h4("Plot of the contributions of the approximation"),align="center",style="color:darkblue"),
														span(imageOutput("contributions2",height="100%"),align="center"),
														br(),
														"For each of the contributions the median is substracted, such that all medians equal zero and the 
																boxplots align.  Contributions with a broad range are most important.  Contributions with a 
																small range are less important.  The approximation is a good explanation of the SVM model 
																whenever the rest term can be ignored w.r.t. the other contributions."),
												
												tabPanel("roc",
														span(h4("ROC curve for SVM model and the approximation"),align="center",style="color:darkblue"),
														span(imageOutput("roc2",height="100%"),align="center"),
														br(),
														"If the ROC curves are close to each other, the approximation is able to obtain a similar discimination performance
																as the original model."),
												
												tabPanel("correlations",
														span(h4("Scatter plot of the terms in the approximation and the rest term."),align="center",style="color:darkblue"),
														span(imageOutput("correlations2",height="100%"),align="center"),
														br(),
														"If the rest term is highly correlated with one of the other contributions, a better approximation might be possible by chosing
																other parameters or by selecting the most important input variables.",
														br(),br(),br(),
														"The pearson correlation between the different terms of the approximation and the rest term is given in the Table below.",
														span(tableOutput('corrtable'),align="center")),
												
												tabPanel("summary",
														span(h4("Performance summary of SVM model and approximation on training data"),align="center",style="color:darkblue"),
														fluidRow(
																column(8,
																		"The accuracy of the SVM model on the training data is:",
																		br(),
																		"The accuracy of the approximation on the training data is:",
																		br(),
																		"The accuracy of the approximation w.r.t. the SVM model  on the training data is:"
																),
																column(1,
																		textOutput("acctrainSVM"),
																		textOutput("acctrainAPPROX"),
																		textOutput("myaccwrtSVMtrain")
																)
														
														)
												
												)),
										tabPanel("References",
												tags$ul(
														tags$li("R A Fisher.", strong("The use of multiple measurements in taxonomic problems."), em("Annals of Eugenics,"),"7(Part II):179-188, 1936."),
														tags$li("V Van Belle and B Van Calster.", strong("Visualizing risk prediction models."), em("PLoS ONE"),",
																		10(7):e0132614, 2015."),
														tags$li("V Van Belle, B Van Calster B, S Van Huffel, JAK Suykens and P Lisboa.", strong("Explaining support vector machines: a color based nomogram."), "Internal Report
																		16-21, ESAT-Stadius, KU Leuven (Leuven, Belgium), 2016.")
												)
										),
										tabPanel("Disclaimer",
												fluidRow(
														column(9,
																checkboxInput("accepted", label = strong("Accept disclaimer"), value = FALSE, width = NULL),
																
																p("The software is made available under the GPLv3 license agreement as available on",
																		a("http://www.gnu.org/licenses/gpl-3.0.en.html", href="http://www.gnu.org/licenses/gpl-3.0.en.html"),", whereby the articles below prevail on contradictory 
																				provisions in articles 15 and 16 of the GPLv3 license agreement."),
																br(),
																tags$u("No warranty and limitation of liability"),
																br(),
																p("THIS SOFTWARE IS EXPERIMENTAL IN NATURE AND IS THE RESULT OF ACADEMIC RESEARCH. 
																				HEALTH RELATED INFORMATION CHANGES FREQUENTLY AND THEREFORE INFORMATION CONTAINED 
																				IN THE SOFTWARE MAY BE OUTDATED, INCOMPLETE OR INCORRECT."),
																br(),
																p("THIS SOFTWARE IS NOT INTENDED TO BE USED AND MAY NOT BE USED AS A SUBSTITUTE FOR
																				MEDICAL ADVICE, DIAGNOSIS, PREVENTION, MONITORING OR TREATMENT OF ANY HEALTH CONDITION 
																				OR PROBLEM."),
																br(),
																p("WHEN USING THE SOFTWARE YOU AGREE THAT THE SOFTWARE IS SUPPLIED BY KU LEUVEN FOR
																				NON-COMMERCIAL TESTING PURPOSES WITHIN THE SCOPE OF YOUR INTERNAL RESEARCH ACTIVITIES 
																				AND ONLY WITHIN THE FIELD OF STATISTICS AND EPIDEMIOLOGY. YOU ACKNOWLEDGE AND AGREE THAT THE SOFTWARE HAS 
																				NEITHER BEEN VALIDATED NOR RELEASED FOR CLINICAL USE. YOU HEREBY EXPLICITLY AGREE THAT 
																				THE RESULTS OF THE SOFTWARE CANNOT BE USED FOR CLINICAL INTERPRETATION OR PHYSICIAN SERVICES."),
																br(),
																p("The Software is provided 'as is' by KU LEUVEN without warranty of any kind, whether express 
																				or implied.  KU LEUVEN specifically disclaims the implied warranties of merchantability and 
																				fitness for a particular purpose or that the use of the Software will not infringe any patents, 
																				copyrights or trademarks or other rights of third parties. The entire risk as to the quality 
																				and performance of the Software is borne by you."),
																br(),
																p("KU LEUVEN shall not be liable for any loss or any direct, indirect, special, incidental, 
																				liquidated or consequential damages or other liability incurred by you in connection with 
																				the Software licensed by KU LEUVEN under this Agreement or arising out of any performance 
																				of this Agreement, whether such damages are based on contract, tort or any other legal theory 
																				(including but not limited to damages to third parties, loss of profits or loss of contracts). 
																				KU LEUVEN's total liability to you shall in no case exceed the amount of 10.000 EUR."),
																br(),
																p("Notwithstanding the foregoing, the liability of KU LEUVEN shall not be limited to the extent 
																				that such limitation is not permitted by law or to the extent that damages are caused by willful 
																				misconduct of KU LEUVEN. "),
																br(),
																tags$u("Indemnification"),
																br(),
																p("You will indemnify, defend and hold harmless KU LEUVEN, its directors, officers, employees 
																				and agents from and against all liability, losses, damages and expenses (including attorney's 
																				fees and costs) arising out of any claims, demands, actions or other proceedings made or 
																				instituted by any third party against any of them and arising out of or relating to any breach 
																				of this Agreement by you, or any use or disclosure of the Software by you, unless such claims 
																				or liability result from KU LEUVEN's willful misconduct."),
																br(),
																tags$u("Severability"),
																br(),
																p("Should any (part of a) provision of this Agreement become invalid, illegal or unenforceable, 
																				it shall not affect the validity of the remaining (part of the) provision(s) of this Agreement. 
																				In such a case, the Parties concerned shall be entitled to request that a valid and practicable 
																				provision be negotiated which fulfils the purpose of the original provision and the intent of the 
																				Parties.")
														)
												)
										
										)
								)
						
						)
				)
		))