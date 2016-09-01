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
				
				
				span(h1("Visualizing the chance on marital status"),align="center",style="color:darkblue"),
				titlePanel("",windowTitle = "Marital status"),
				sidebarLayout(position = "left",
						sidebarPanel( width = 2,
								actionButton("godisclaimer", label = "Read disclaimer"),actionButton("analyse", label = "Analyse"),
								textOutput("value"),
								h4("Inputs"),
								tabsetPanel(
										tabPanel("Plot options",
												helpText(span(h5("Select options for the visualization."),style="color:darkblue")),
												selectInput("zerolevel", label = h6("Select level of functional form corresponding to zero."),
														choices = list("zero","min"), selected = "zero"),
												selectInput("coloroptions", label = h6("Select the color map."),
														choices = list("Rainbow","Sequential","Diverging","Black-White","Viridis"), selected = "Sequential"),
												span(h5("Specify the ranges of the continuous predictors."),style="color:darkblue"),
												sliderInput("ageRange",label = h6("Range of age (year)"),min=16,max=88,value=c(16,88)),
												sliderInput("heightRange",label = h6("Range of height (m)"),min=1.40,max=2.05,value=c(1.40,2.05), step=0.05),
												sliderInput("weightRange",label = h6("Range of weight (kg)"),min=33.6,max=130,value=c(33.6,130))
										),
										
										tabPanel("Patient characteristics",
												helpText(span(h5("Add patient characteristics."),style="color:darkblue")),
												selectInput("sex", label = h6("What is the gender of the person?"), 
														choices = list("female","male"), selected = "male"),
												numericInput("age", label = h6("What is the age of the person (year)?"), value = 38),
												numericInput("height", label = h6("What is the height of the person (m)?"), value = 1.78),
												numericInput("weight", label = h6("What is the weight of the person (kg)?"), value = 85)
										)
								)
						),
						mainPanel(
								h4(textOutput("temp"),align="center",style='color:red'),
								
								navbarPage(	id="righttabs",span("Results",style="color:black") ,
										
										tabPanel("Colorplot summary",
												span(HTML("<p align=center style='padding:6px; color: white; background-color: darkgray; border: 
																		CornflowerBlue 2px solid'>Linear predictors</p>")),
												
												fluidRow(
														column(6,
																span(imageOutput("imageb",height="100%"),align="center")
														),
														column(6,
																span(imageOutput("image2b",height="100%"),align="center")
														),
														column(12,
																span(imageOutput("image3b",height="100%"),align="center")
														)
												),
												br(),
												span(HTML("<p align=center style='padding:6px; color: white; background-color: darkgray; border: 
																		CornflowerBlue 2px solid'>Risk on reference output level</p>")),
												br(),
												
												span(imageOutput("image4b",height="100%"),align="center"),
												
												br(),
												span(HTML("<p align=center style='padding:6px; color: white; background-color: darkgray; border: 
																		CornflowerBlue 2px solid'>Risk on non-reference output levels</p>")),
												br(),
												fluidRow(
														column(6,
																span(imageOutput("image5b",height="100%"),align="center")
														),
														column(6,
																span(imageOutput("image6b",height="100%"),align="center")
														),
														column(12,
																span(imageOutput("image7b",height="100%"),align="center")
														)
												)
										),
										navbarMenu("Colorplot", 
												tabPanel("Alternative summary",
														span(HTML("<p align=center style='padding:6px; color: white; background-color: darkgray; border: 
																				CornflowerBlue 2px solid'>Linear predictors</p>")),
														
														fluidRow(
																column(6,
																		span(imageOutput("imagec",height="100%"),align="center")
																),
																column(6,
																		span(imageOutput("image2c",height="100%"),align="center")
																),
																column(12,
																		span(imageOutput("image3c",height="100%"),align="center")
																)
														),
														br(),
														span(HTML("<p align=center style='padding:6px; color: white; background-color: darkgray; border: 
																				CornflowerBlue 2px solid'>Risk on reference output level</p>")),
														br(),
														
														span(imageOutput("image4c",height="100%"),align="center"),
														
														br(),
														span(HTML("<p align=center style='padding:6px; color: white; background-color: darkgray; border: 
																				CornflowerBlue 2px solid'>Risk on non-reference output levels</p>")),
														br(),
														fluidRow(
																column(6,
																		span(imageOutput("image5c",height="100%"),align="center")
																),
																column(6,
																		span(imageOutput("image6c",height="100%"),align="center")
																),
																column(12,
																		span(imageOutput("image7c",height="100%"),align="center")
																)
														)
												
												),
												tabPanel("Linear predictor: married",
														span(imageOutput("image",height="100%"),align="center")
												),
												tabPanel("Linear predictor: divorced",
														span(imageOutput("image2",height="100%"),align="center")
												),
												tabPanel("Linear predictor: widowed",
														span(imageOutput("image3",height="100%"),align="center")
												),
												tabPanel("Chance: single",
														span(imageOutput("image4",height="100%"),align="center")
												),
												tabPanel("Chance: married",
														span(imageOutput("image5",height="100%"),align="center")
												),
												tabPanel("Chance: divorced",
														span(imageOutput("image6",height="100%"),align="center")
												),
												tabPanel("Chance: widowed",
														span(imageOutput("image7",height="100%"),align="center")
												)
										),
										navbarMenu("Cchart", 
												tabPanel("Summary",
														span(HTML("<p align=center style='padding:6px; color: white; background-color: darkgray; border: 
																				CornflowerBlue 2px solid'>Linear predictors</p>")),
														
														fluidRow(
																column(6,
																		span(h4("Linear predictor for married"),
																				align="center",style="color:darkblue"),
																		span(imageOutput("cchartb",height="100%"),align="center")
																),
																column(6,
																		span(h4("Linear predictor for divorced"),
																				align="center",style="color:darkblue"),
																		span(imageOutput("cchart2b",height="100%"),align="center")
																),
																column(12,
																		span(h4("Linear predictor for widowed"),
																				align="center",style="color:darkblue"),
																		span(imageOutput("cchart3b",height="100%"),align="center")
																)
														),
														br(),
														span(HTML("<p align=center style='padding:6px; color: white; background-color: darkgray; border: 
																				CornflowerBlue 2px solid'>Risk on reference output level</p>")),
														br(),
														
														span(imageOutput("cchart4b",height="100%"),align="center"),
														
														br(),
														span(HTML("<p align=center style='padding:6px; color: white; background-color: darkgray; border: 
																				CornflowerBlue 2px solid'>Risk on non-reference output levels</p>")),
														br(),
														fluidRow(
																column(6,
																		span(h4("Estimated risk on being married"),
																				align="center",style="color:darkblue"),
																		span(imageOutput("cchart5b",height="100%"),align="center")
																),
																column(6,
																		span(h4("Estimated risk on being divorced"),
																				align="center",style="color:darkblue"),
																		span(imageOutput("cchart6b",height="100%"),align="center")
																),
																column(12,
																		span(h4("Estimated risk on being widowed"),
																				align="center",style="color:darkblue"),
																		span(imageOutput("cchart7b",height="100%"),align="center")
																)
														)
												),
												tabPanel("Linear predictor: married",
														span(imageOutput("cchart",height="100%"),align="center")
												),
												tabPanel("Linear predictor: divorced",
														span(imageOutput("cchart2",height="100%"),align="center")
												),
												tabPanel("Linear predictor: widowed",
														span(imageOutput("cchart3",height="100%"),align="center")
												),
												tabPanel("Chance: single",
														span(imageOutput("cchart4",height="100%"),align="center")
												),
												tabPanel("Chance: married",
														span(imageOutput("cchart5",height="100%"),align="center")
												),
												tabPanel("Chance: divorced",
														span(imageOutput("cchart6",height="100%"),align="center")
												),
												tabPanel("Chance: widowed",
														span(imageOutput("cchart7",height="100%"),align="center")
												)
										
										), 
										navbarMenu("Ccchart", 
												tabPanel("Summary", 
														span(HTML("<p align=center style='padding:6px; color: white; background-color: darkgray; border: 
																				CornflowerBlue 2px solid'>Linear predictors</p>")),
														
														fluidRow(
																column(6,
																		span(h4("Linear predictor for married"),
																				align="center",style="color:darkblue"),
																		span(imageOutput("ccchartb",height="100%"),align="center")
																),
																column(6,
																		span(h4("Linear predictor for divorced"),
																				align="center",style="color:darkblue"),
																		span(imageOutput("ccchart2b",height="100%"),align="center")
																),
																column(12,
																		span(h4("Linear predictor for widowed"),
																				align="center",style="color:darkblue"),
																		span(imageOutput("ccchart3b",height="100%"),align="center")
																)
														),
														br(),
														span(HTML("<p align=center style='padding:6px; color: white; background-color: darkgray; border: 
																				CornflowerBlue 2px solid'>Risk on reference output level</p>")),
														br(),
														
														span(imageOutput("ccchart4b",height="100%"),align="center"),
														
														br(),
														span(HTML("<p align=center style='padding:6px; color: white; background-color: darkgray; border: 
																				CornflowerBlue 2px solid'>Risk on non-reference output levels</p>")),
														br(),
														fluidRow(
																column(6,
																		span(h4("Estimated risk on being married"),
																				align="center",style="color:darkblue"),
																		span(imageOutput("ccchart5b",height="100%"),align="center")
																),
																column(6,
																		span(h4("Estimated risk on being divorced"),
																				align="center",style="color:darkblue"),
																		span(imageOutput("ccchart6b",height="100%"),align="center")
																),
																column(12,
																		span(h4("Estimated risk on being widowed"),
																				align="center",style="color:darkblue"),
																		span(imageOutput("ccchart7b",height="100%"),align="center")
																)
														)
												),
												tabPanel("Linear predictor: married",
														span(imageOutput("ccchart",height="100%"),align="center")
												),
												tabPanel("Linear predictor: divorced",
														span(imageOutput("ccchart2",height="100%"),align="center")
												),
												tabPanel("Linear predictor: widowed",
														span(imageOutput("ccchart3",height="100%"),align="center")
												),
												tabPanel("Chance: single",
														span(imageOutput("ccchart4",height="100%"),align="center")
												),
												tabPanel("Chance: married",
														span(imageOutput("ccchart5",height="100%"),align="center")
												),
												tabPanel("Chance: divorced",
														span(imageOutput("ccchart6",height="100%"),align="center")
												),
												tabPanel("Chance: widowed",
														span(imageOutput("ccchart7",height="100%"),align="center")
												)
										),
										tabPanel("Observation summary",
												fluidRow(
														column(6,
																span(imageOutput("summary",height="100%"),align="center")
														),
														column(6,
																span(imageOutput("summary2",height="100%"),align="center")
														
														)
												)
										),
										tabPanel("Table", 
												span(h4("Coefficients for output level = married"),
														align="center",style="color:darkblue"),
												dataTableOutput("table"),
												span(h4("Coefficients for output level = divorced"),
														align="center",style="color:darkblue"),
												dataTableOutput("table2"),
												span(h4("Coefficients for output level = widowed"),
														align="center",style="color:darkblue"),
												dataTableOutput("table3")
										),
										tabPanel("References",
												tags$ul(
														tags$li("V Van Belle and B Van Calster.", strong("Visualizing risk prediction models."), em("PLoS ONE"),",
																		10(7):e0132614, 2015."),
														tags$li("V Van Belle S Van Huffel, D Timmerman, W Froyman, T Bourne and B Van Calster.", strong("A color based nomogram for Multinomial Logistic Regression."), "Internal Report
																		16-28, ESAT-Stadius, KU Leuven (Leuven, Belgium), 2016")
												
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