# load data and libraries here: some for all users and sessions
# library(VRPM)
library(png)
library(xtable)
library(data.table)
library(ROCR)
library(hexbin)
library(ggplot2)
library(kernlab)


library(MASS)
data(Pima.tr)
mydata=Pima.tr
mydatatest=Pima.te
indy=8
names(mydata)[indy]="y"
names(mydatatest)[indy]="y"
mydata$y=as.factor(as.integer(mydata$y=="Yes"))
mydatatest$y=as.factor(as.integer(mydatatest$y=="Yes"))

mysig=0.03125
myC=1


myxmin=apply(mydata[,-indy],2,min)
myxmax=apply(mydata[,-indy],2,max)

risklabel="Risk on diabetes"

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
			
			
			kernel <- eventReactive(input$analyse, {
						kernel <- switch(input$kernel,
								"linear"="linear",
								"polynomial"="poly",
								"RBF"="rbf")
					})
			
			zerolevel <- eventReactive(input$analyse, {
						zerolevel <- input$zerolevel
					})
			
			
			obs <- eventReactive(input$analyse, {	
						validate(
								
								need(input$npreg != "", "Please specify the number of pregnancies."),
								need(input$glu != "", "Please specify the plasma glucose concentration."),
								need(input$bp != "", "Please specify the diastolic blood pressure (mmHg)."),
								need(input$skin != "", "Please specify the triceps skin fold thickness (mm)."),
								need(input$bmi != "", "Please specify the body mass index (weight in kg/(height in m)^2)."),
								need(input$ped != "", "Please specify the diabetes pedigree function."),
								need(input$age != "", "Please specify the age (years).")
						)
						obs=data.frame(npreg=input$npreg,glu=input$glu,bp=input$bp,skin=input$skin,bmi=input$bmi,ped=input$ped,age=input$age)
					})
			
			xmin <- eventReactive(input$analyse, {	
						temp=myxmin
						temp[1]=input$npregRange[1]
						temp[2]=input$gluRange[1]
						temp[3]=input$bpRange[1]
						temp[4]=input$skinRange[1]
						temp[5]=input$bmiRange[1]
						temp[6]=input$pedRange[1]
						temp[7]=input$ageRange[1]
						temp
					})
			
			xmax <- eventReactive(input$analyse, {	
						temp=myxmax
						temp[1]=input$npregRange[2]
						temp[2]=input$gluRange[2]
						temp[3]=input$bpRange[2]
						temp[4]=input$skinRange[2]
						temp[5]=input$bmiRange[2]
						temp[6]=input$pedRange[2]
						temp[7]=input$ageRange[2]
						temp
					})
			
			output$value<- eventReactive(input$godisclaimer, {
						# reset tab to disclaimer
						updateNavbarPage(session, "righttabs", selected = "Disclaimer")
						""
					})
			
			pars <- eventReactive(input$analyse, {	
						pars <- switch(input$kernel,
								"linear"=c(input$par_1),
								"RBF"=c(input$par_1,input$par_2),
								"polynomial"=c(input$par_1,input$par_2,input$par_3))
					})
			
			output$input_ui <- renderUI({
						num <- switch(input$kernel,
								"linear"=1,
								"RBF"=2,
								"polynomial"=3)
						
						names <- switch(input$kernel,
								"linear"=c("regularization constant"),
								"RBF"=c("regularization constant","inverse kernel width"),
								"polynomial"=c("regularization constant","scale","degree"))
						
						value <- switch(input$kernel,
								"linear"=c(1),
								"RBF"=c(myC,mysig),
								"polynomial"=c(1,1,2))
						
						lapply(1:num, function(i) {
									numericInput(paste0("par_", i), label = names[i], value = value[i])
								}
						)
					})
			
			mymodel<- eventReactive(input$analyse, {
						if (input$accepted == FALSE)
							return()
						
						# reset tab to colplot
						updateNavbarPage(session, "righttabs", selected = "Colorplot")
						
						# Start the clock!
						ptm <- proc.time()
						
						withProgress(message = 'Making plot', value = 0, {
									# train the model
									set.seed(100) 
									if(kernel()=="rbf"){
										model <-ksvm(y ~ ., data = mydata,prob.model=TRUE,
												kpar=list(pars()[2]),
												C=pars()[1])
									} else if (kernel()=="poly"){
										if (!pars()[3]==round(pars()[3]) | pars()[3]<0) {
											stop("The degree of the polynomial kernel should be a positive integer.")
										}
										model <-ksvm(y ~ ., data = mydata,prob.model=TRUE,kernel="polydot",
												kpar=list(degree=pars()[3], scale=pars()[2], offset=1),
												C=pars()[1])
									} else if (kernel()=="linear"){
										model <-ksvm(y ~ ., data = mydata,prob.model=TRUE,kernel="polydot",
												kpar=list(degree=1, scale=1, offset=0),
												C=pars()[1])
									}
									
									# from model to list to be able to plot
									tempmodel=preplotperf(model,mydata,indy,zerolevel=zerolevel(),risklabel=risklabel,adverse=FALSE)
									
									colplot(tempmodel, zerolevel=zerolevel(),filename="colplot",coloroptions=coloroptions(),
											risklabel=risklabel,obs=obs(),xmin=xmin(),xmax=xmax())	
									
									cchart(tempmodel,obs=obs(),filename="cchart", zerolevel=zerolevel(),
											risklabel=risklabel,xmin=xmin(),xmax=xmax())
									
									ccchart(tempmodel,obs=obs(),filename="ccchart", zerolevel=zerolevel(),
											risklabel=risklabel,xmin=xmin(),xmax=xmax())
									
									plotperf(tempmodel,mydata, indy=indy)
									
									# Increment the progress bar, and update the detail text.
									incProgress(1, detail = paste("Done"))
								})
						return<-list(tempmodel,1,proc.time()[3] - ptm[3])
						
					})
			
			
			output$image <- renderImage({
						mymodel()
						if (sum(mymodel()[[2]]) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "colplot.png",
									contentType = "image/png",
									width = input$GetScreenWidth/3,
#											height = 700,
									alt = "Error: Unable to display plot!")
						}
					}, deleteFile = FALSE)
			
			
			output$image2 <- renderImage({
						mymodel()
						if (sum(mymodel()[[2]]) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "colplot_B.png",
									contentType = "image/png",
									width = input$GetScreenWidth/3,
#											height = 700,
									alt = "Error: Unable to display plot!")
						}
					}, deleteFile = FALSE)					
			
			
			output$cchart <- renderImage({
						mymodel()
						if (sum(mymodel()[[2]]) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "cchart.png",
									contentType = "image/png",
									width = input$GetScreenWidth/3,
#											height = 560,
									alt = "Error: Unable to display plot!")
						}
					}, deleteFile = FALSE)
			
			output$ccchart <- renderImage({
						mymodel()
						if (sum(mymodel()[[2]]) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "ccchart.png",
									contentType = "image/png",
									width = input$GetScreenWidth/3,
#											height = 900,
									alt = "Error: Unable to display plot!")
						}
					}, deleteFile = FALSE)
			
			output$lp <- renderImage({
						mymodel()
						if (sum(mymodel()[[2]]) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "lps.png",
									contentType = "image/png",
									width = input$GetScreenWidth/4,
#											height = 900,
									alt = "Error: Unable to display plot!")
						}
					}, deleteFile = FALSE)
			
			output$lp2 <- renderImage({
						mymodel()
						if (sum(mymodel()[[2]]) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "lps.png",
									contentType = "image/png",
									width = input$GetScreenWidth/3,
#											height = 900,
									alt = "Error: Unable to display plot!")
						}
					}, deleteFile = FALSE)
			
			output$p <- renderImage({
						mymodel()
						if (sum(mymodel()[[2]]) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
						# Return a list containing information about the image
						list(src = "probs.png",
								contentType = "image/png",
								width = input$GetScreenWidth/4,
#											height = 900,
								alt = "Error: Unable to display plot!")
					}
					}, deleteFile = FALSE)
			
			output$p2 <- renderImage({
						mymodel()
						if (sum(mymodel()[[2]]) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "probs.png",
									contentType = "image/png",
									width = input$GetScreenWidth/3,
#											height = 900,
									alt = "Error: Unable to display plot!")
						}
					}, deleteFile = FALSE)
			
			output$outcomes <- renderImage({
						mymodel()
						if (sum(mymodel()[[2]]) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "outcomes.png",
									contentType = "image/png",
									width = input$GetScreenWidth/4,
#											height = 900,
									alt = "Error: Unable to display plot!")
						}
					}, deleteFile = FALSE)
			
			output$outcomes2 <- renderImage({
						mymodel()
						if (sum(mymodel()[[2]]) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "outcomes.png",
									contentType = "image/png",
									width = input$GetScreenWidth/3,
#											height = 900,
									alt = "Error: Unable to display plot!")
						}
					}, deleteFile = FALSE)
			
			output$contributions <- renderImage({
						mymodel()
						if (sum(mymodel()[[2]]) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "contributions2.png",
									contentType = "image/png",
									width = input$GetScreenWidth/4,
#											height = 900,
									alt = "Error: Unable to display plot!")
						}
					}, deleteFile = FALSE)
			
			output$contributions2 <- renderImage({
						mymodel()
						if (sum(mymodel()[[2]]) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "contributions2.png",
									contentType = "image/png",
									width = input$GetScreenWidth/3,
#											height = 900,
									alt = "Error: Unable to display plot!")
						}
					}, deleteFile = FALSE)
			
			
			output$roc <- renderImage({
						mymodel()
						if (sum(mymodel()[[2]]) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "ROC.png",
									contentType = "image/png",
									width = input$GetScreenWidth/4,
#											height = 900,
									alt = "Error: Unable to display plot!")
						}
					}, deleteFile = FALSE)
			
			output$roc2 <- renderImage({
						mymodel()
						if (sum(mymodel()[[2]]) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "ROC.png",
									contentType = "image/png",
									width = input$GetScreenWidth/3,
#											height = 900,
									alt = "Error: Unable to display plot!")
						}
					}, deleteFile = FALSE)
			
			
			output$correlations <- renderImage({
						mymodel()
						if (sum(mymodel()[[2]]) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "corrplot.png",
									contentType = "image/png",
									width = input$GetScreenWidth/4,
#											height = 900,
									alt = "Error: Unable to display plot!")
						}
					}, deleteFile = FALSE)
			
			output$correlations2 <- renderImage({
						mymodel()
						if (sum(mymodel()[[2]]) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "corrplot.png",
									contentType = "image/png",
									width = input$GetScreenWidth/3,
#											height = 900,
									alt = "Error: Unable to display plot!")
						}
					}, deleteFile = FALSE)
			
			
			
			output$acctrainSVM <- renderText({	
						mymodel()[[1]]$acctrainSVM
					})
			
			output$acctrainAPPROX <- renderText({
						mymodel()[[1]]$acctrainAPPROX
					})
			
			output$myaccwrtSVMtrain <- renderText({
						mymodel()[[1]]$myaccwrtSVMtrain
					})
			
			output$corrtable <- renderTable({
						cor(cbind(mymodel()[[1]]$fall))
					})
			
			output$temp <- renderText({
						if (sum(mymodel()[[2]]) == 0){ 
							updateNavbarPage(session, "righttabs", selected = "Disclaimer")
							return("Disclaimer needs to be accepted before calculations can proceed.")
							
						} else{
							return("")
							
						}
						
					})
			
			
		})


