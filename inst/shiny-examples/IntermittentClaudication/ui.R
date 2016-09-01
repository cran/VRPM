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
				
				
				span(h1("Visualizing the risk on Intermittent Claudication (IC)"),align="center",style="color:darkblue"),
				titlePanel("",windowTitle = "IC"),
				sidebarLayout(position = "left",
						sidebarPanel( 
								actionButton("godisclaimer", label = "Read disclaimer"),actionButton("analyse", label = "Analyse"),
								#actionButton("action", label = "Action"),
								textOutput("value"),
								h4("Inputs"),
								tabsetPanel(
										tabPanel("Plot options",
												helpText(span(h5("Select options for the visualization."),style="color:darkblue")),
												selectInput("zerolevel", label = h6("Select level of functional form corresponding to zero."),
														choices = list("zero","min","mean","median"), selected = "zero"),
												selectInput("coloroptions", label = h6("Select the color map."),
														choices = list("Rainbow","Sequential","Diverging","Black-White","Viridis"), selected = "Sequential"),
												span(h5("Specify the ranges of the continuous predictors."),style="color:darkblue"),
												sliderInput("ageRange",label = h6("Range of age (year)"),min=45,max=84,value=c(45,84)),
												sliderInput("cholRange",label = h6("Range of cholesterol value (mg/dL)"),min=150,max=300,value=c(150,300)),
												sliderInput("cigRange",label = h6("Range of number of cigarettes smoked daily (n)."),min=0,max=30,value=c(0,30))
										),
										
										tabPanel("Patient characteristics",
												helpText(span(h5("Add patient characteristics."),style="color:darkblue")),
												selectInput("sex", label = h6("What is the gender of the patient?"), 
														choices = list("female","male"), selected = "male"),
												selectInput("blood", label = h6("What type of blood pressure does the patient have?"),
														choices = list("normal","high","stage 1","stage 2"), selected = "normal"),
												selectInput("diabetes", label = h6("Is the patient diabetic?"),
														choices = list("no","yes"), selected = "no"),
												selectInput("chd", label = h6("Does the patient suffer from coronary heart disease?"),
														choices = list("no","yes"), selected = "no"),
												numericInput("age", label = h6("What is the age of the patient (year)?"), value = 58),
												numericInput("cigarettes", label = h6("How many cigarettes does the patient smoke daily (n)?"), value = 5),
												numericInput("cholesterol", label = h6("What is cholesterol level of the patient (mg/dL)?"), value = 210)
										)
								)
						),
						mainPanel(
								h4(textOutput("temp"),align="center",style='color:red'),
								
								span(h5(textOutput("text")),align="center",style="color:red"),
								
								navbarPage(	id="righttabs",span("Results",style="color:black") ,
										
										tabPanel("Colorplot", 
												imageOutput("image"),
												br(),br(),br(),br(),br(),br(),br(),br()
										), 
										tabPanel("cchart", 
												imageOutput("cchart")
										), 
										tabPanel("ccchart", 
												imageOutput("ccchart"),
												br(),br(),br(),br(),br()
										),
										tabPanel("References",
												tags$ul(tags$li("JM Murabito, RB D'Agostino, H Silbershatz, WF Wilson.", strong("Intermittent claudication: 
																				a risk profile from The Framingham Heart Study."),em("Circulation,"),"96(1):44-49, 1997."),
														tags$li("V Van Belle and B Van Calster.", strong("Visualizing risk prediction models."), em("PLoS ONE"),",
																		10(7):e0132614, 2015.")														
												)
										),
										tabPanel("Disclaimer",
												fluidRow(
														column(9,
																checkboxInput("accepted", label = strong("Accept disclaimer"), value = FALSE, width = NULL),
																
																#p("This is the disclaimer.")
																
																
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