# load data and libraries here: some for all users and sessions
library(png)

source("helpers.R")
mydata<-IC_data()
xmin=apply(mydata$x,2,min)
xmax=apply(mydata$x,2,max)


shinyServer(function(input, output, clientData, session) {
			# define user specific objects here: same for all sessions of one user
			
			coloroptions <- eventReactive(input$analyse, {
						coloroptions <- switch(input$coloroptions, 
								"Rainbow" = 1,
								"Sequential" = 2,
								"Diverging" = 3,
								"Black-White" = 4,
								"Viridis" = 5)
					})
			
			obs <- eventReactive(input$analyse,{	
						validate(
								need(input$age != "", "Please specify the age of the patient."),
								need(input$cholesterol != "", "Please specify the cholesterol level of the patient."),
								need(input$cigarettes != "", "Please specify how many cigarettes the patient smokes daily.")
						)
						
						blood <- switch(input$blood,
								"normal"=0,
								"high"=1,
								"stage 1"=2,
								"stage 2"=3
						)
						
						obs=c(as.numeric(input$sex=="male"),input$age,blood,as.numeric(input$diabetes=="yes"),
								input$cigarettes,input$cholesterol,as.numeric(input$chd=="yes"))
						
					})
		
			
			output$text <- renderText({
						
						text <- switch(input$zerolevel,
								"zero"="",
								"min"="",
								"mean"="!! The data used for illustration in this application is simulated.  The mean does not correspond to the mean in the study population.  !!",
								"median"="!! The data used for illustration in this application is simulated.  The median does not correspond to the median in the study population.  !!",
						)			
						
					})

			
			
			output$value<- eventReactive(input$godisclaimer, {
						# reset tab to disclaimer
						updateNavbarPage(session, "righttabs", selected = "Disclaimer")
						""
					})
			
			
			mymodel<- eventReactive(input$analyse, {
						if (input$accepted == FALSE)
							return()
						
						# reset tab to colplot
						updateNavbarPage(session, "righttabs", selected = "Colorplot")
						
						withProgress(message = 'Making plot', value = 0, {
									
									xmin[2]=input$ageRange[1]
									xmin[5]=input$cigRange[1]
									xmin[6]=input$cholRange[1]
									
									xmax[2]=input$ageRange[2]
									xmax[5]=input$cigRange[2]
									xmax[6]=input$cholRange[2]
									
									colplot(mydata, zerolevel=input$zerolevel,filename="colplot",coloroptions=coloroptions(),
											risklabel="Estimated risk on intermittent claudication",obs=obs(),xmin=xmin,xmax=xmax)	
									
									
									cchart(mydata,obs=obs(),filename="cchart", zerolevel=input$zerolevel,
											risklabel="Estimated risk on IC",xmin=xmin,xmax=xmax)
									
									
									ccchart(mydata,obs=obs(),filename="ccchart", zerolevel=input$zerolevel,
											risklabel="Estimated risk on intermittent claudication",xmin=xmin,xmax=xmax)
									
									
									
									# Increment the progress bar, and update the detail text.
									incProgress(1, detail = paste("Done"))
								})
						1
					})
			
			
			
			
			
			
			
			
			output$image <- renderImage({

						mymodel()
						if (sum(mymodel()) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							
							# Return a list containing information about the image
							list(src = "colplot.png",
									contentType = "image/png",
									width = input$GetScreenWidth/2,
									#											height = 700,
									alt = "Error: Unable to display plot!")
						}
					}, deleteFile = FALSE)
			
			output$cchart <- renderImage({
						
						mymodel()
						if (sum(mymodel()) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "cchart.png",
									contentType = "image/png",
									width = input$GetScreenWidth/2,
#											height = 560,
									alt = "Error: Unable to display plot!")
						}
					}, deleteFile = FALSE)
			
			output$ccchart <- renderImage({
						mymodel()
						if (sum(mymodel()) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							
							# Return a list containing information about the image
							list(src = "ccchart.png",
									contentType = "image/png",
									width = input$GetScreenWidth/2,
									#											height = 900,
									alt = "Error: Unable to display plot!")
						}
					}, deleteFile = FALSE)
			
			
			output$temp <- renderText({
						if (sum(mymodel()) == 0){ 
							updateNavbarPage(session, "righttabs", selected = "Disclaimer")
							return("Disclaimer needs to be accepted before calculations can proceed.")
							
						} else{
							return("")
							
						}
						
					})
		})


