


# load data and libraries here: same for all users and sessions

library(VGAMdata)
library(nnet)
library(data.table)


data(xs.nz)
marital.nz <- xs.nz[,c("marital","sex","age","height","weight")]
mydata <- marital.nz[complete.cases(marital.nz),]

# remove people that seem to be smaller than 1.4m (probably mistakes)
toremove=which(mydata$height<1.4)
mydata=mydata[-toremove,]
# remove people that seem to be weigh more than 130kg (outliers)
toremove=which(mydata$weight>130)
mydata=mydata[-toremove,]

fit <- multinom(marital ~ sex + age + height + weight, data = mydata,model=TRUE)

outnames=colnames(fitted(fit))
labels=c(paste("Linear predictor for",outnames[-1]),paste
				("Predicted chance of being",outnames))

xmin=apply(mydata,2,min)[-which(names(mydata)=="marital")]
xmax=apply(mydata,2,max)[-which(names(mydata)=="marital")]

myxmin=mydata[1,-1]
myxmax=mydata[1,-1]
for (i in seq(1,length(xmin))) {
	myxmin[i]=xmin[i]
	myxmax[i]=xmax[i]
}

shinyServer(function(input, output, clientData, session) {
			# define user specific objects here: same for all sessions of one user
			
			coloroptions <- reactive({
						coloroptions <- switch(input$coloroptions, 
								"Rainbow" = 1,
								"Sequential" = 2,
								"Diverging" = 3,
								"Black-White" = 4,
								"Viridis" = 5)
					})
			
			obs <- reactive({	
						validate(
								need(input$age != "", "Please specify the age of the person."),
								need(input$height != "", "Please specify the height of the person."),
								need(input$weight != "", "Please specify weight of the person.")
						)
						
						sex <- switch(input$sex,
								"male"="M",
								"female"="F"
						)
						
						obs=data.frame(sex=sex,age=input$age,height=input$height, weight=input$weight)
						
					})
			
			xmin <- eventReactive(input$analyse, {	
						temp=myxmin
						temp[2]=input$ageRange[1]
						temp[3]=input$heightRange[1]
						temp[4]=input$weightRange[1]
						temp
					})
			
			xmax <- eventReactive(input$analyse, {	
						temp=myxmax
						temp[2]=input$ageRange[2]
						temp[3]=input$heightRange[2]
						temp[4]=input$weightRange[2]
						temp
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
									
									colplot(fit, zerolevel=input$zerolevel,filename="colplot",coloroptions=coloroptions(),
											risklabel=labels,obs=obs(),xmin=xmin(),xmax=xmax())	
									cchart(fit,obs=obs(),filename="cchart",risklabel=labels,zerolevel=input$zerolevel,xmin=xmin(),xmax=xmax())
									ccchart(fit,obs=obs(),filename="ccchart",risklabel=labels,zerolevel=input$zerolevel,xmin=xmin(),xmax=xmax())
									
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
							list(src = "colplot_married.png",
									contentType = "image/png",
									width = "600",
#											height = 700,
									alt = "Error: Unable to display plot!")
						}
					}, deleteFile = FALSE)
			
			output$image2 <- renderImage({
						mymodel()
						if (sum(mymodel()) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "colplot_divorced.png",
									contentType = "image/png",
									width = "600",
									#											height = 700,
									alt = "Error: Unable to display plot!")
						}
					}, deleteFile = FALSE)
			
			output$image3 <- renderImage({
						mymodel()
						if (sum(mymodel()) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "colplot_widowed.png",
									contentType = "image/png",
									width = "600",
									#											height = 700,
									alt = "Error: Unable to display plot!")
						}
					}, deleteFile = FALSE)
			
			output$image4 <- renderImage({
						mymodel()
						if (sum(mymodel()) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "colplotp_single.png",
									contentType = "image/png",
									width = "600",
									#											height = 700,
									alt = "Error: Unable to display plot!")
						}
					}, deleteFile = FALSE)
			
			output$image5 <- renderImage({
						mymodel()
						if (sum(mymodel()) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "colplotp_married.png",
									contentType = "image/png",
									width = "600",
									#											height = 700,
									alt = "Error: Unable to display plot!")
						}
					}, deleteFile = FALSE)
			
			output$image6 <- renderImage({
						mymodel()
						if (sum(mymodel()) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "colplotp_divorced.png",
									contentType = "image/png",
									width = "600",
									#											height = 700,
									alt = "Error: Unable to display plot!")
						}
					}, deleteFile = FALSE)
			
			output$image7 <- renderImage({
						mymodel()
						if (sum(mymodel()) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "colplotp_widowed.png",
									contentType = "image/png",
									width = "600",
									#											height = 700,
									alt = "Error: Unable to display plot!")
						}
					}, deleteFile = FALSE)
			
			output$imageb <- renderImage({
						mymodel()
						if (sum(mymodel()) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "colplot_married.png",
									contentType = "image/png",
									width = input$GetScreenWidth/4,
									#											height = 700,
									alt = "Error: Unable to display plot!")
						}
					}, deleteFile = FALSE)
			
			output$image2b <- renderImage({
						mymodel()
						if (sum(mymodel()) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "colplot_divorced.png",
									contentType = "image/png",
									width = input$GetScreenWidth/4,
									#											height = 700,
									alt = "Error: Unable to display plot!")
						}
					}, deleteFile = FALSE)
			
			output$image3b <- renderImage({
						mymodel()
						if (sum(mymodel()) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "colplot_widowed.png",
									contentType = "image/png",
									width = input$GetScreenWidth/4,
									#											height = 700,
									alt = "Error: Unable to display plot!")
						}
					}, deleteFile = FALSE)
			
			output$image4b <- renderImage({
						mymodel()
						if (sum(mymodel()) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "colplotp_single.png",
									contentType = "image/png",
									width = input$GetScreenWidth/4,
									#											height = 700,
									alt = "Error: Unable to display plot!")
						}
					}, deleteFile = FALSE)
			
			output$image5b <- renderImage({
						mymodel()
						if (sum(mymodel()) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "colplotp_married.png",
									contentType = "image/png",
									width = input$GetScreenWidth/4,
									#											height = 700,
									alt = "Error: Unable to display plot!")
						}
					}, deleteFile = FALSE)
			
			output$image6b <- renderImage({
						mymodel()
						if (sum(mymodel()) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "colplotp_divorced.png",
									contentType = "image/png",
									width = input$GetScreenWidth/4,
									#											height = 700,
									alt = "Error: Unable to display plot!")
						}
					}, deleteFile = FALSE)
			
			output$image7b <- renderImage({
						mymodel()
						if (sum(mymodel()) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "colplotp_widowed.png",
									contentType = "image/png",
									width = input$GetScreenWidth/4,
									#											height = 700,
									alt = "Error: Unable to display plot!")
						}
					}, deleteFile = FALSE)
			
			
			output$imagec <- renderImage({
						mymodel()
						if (sum(mymodel()) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "colplot_married.png",
									contentType = "image/png",
									width = input$GetScreenWidth/4,
									#											height = 700,
									alt = "Error: Unable to display plot!")
						}
					}, deleteFile = FALSE)
			
			output$image2c <- renderImage({
						mymodel()
						if (sum(mymodel()) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "colplot_divorced.png",
									contentType = "image/png",
									width = input$GetScreenWidth/4,
									#											height = 700,
									alt = "Error: Unable to display plot!")
						}
					}, deleteFile = FALSE)
			
			output$image3c <- renderImage({
						mymodel()
						if (sum(mymodel()) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "colplot_widowed.png",
									contentType = "image/png",
									width = input$GetScreenWidth/4,
									#											height = 700,
									alt = "Error: Unable to display plot!")
						}
					}, deleteFile = FALSE)
			
			output$image4c <- renderImage({
						mymodel()
						if (sum(mymodel()) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "colplotp_single.png",
									contentType = "image/png",
									width = input$GetScreenWidth/4,
									#											height = 700,
									alt = "Error: Unable to display plot!")
						}
					}, deleteFile = FALSE)
			
			output$image5c <- renderImage({
						mymodel()
						if (sum(mymodel()) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "colplotp_married_wing.png",
									contentType = "image/png",
									width = input$GetScreenWidth/4,
									#											height = 700,
									alt = "Error: Unable to display plot!")
						}
					}, deleteFile = FALSE)
			
			output$image6c <- renderImage({
						mymodel()
						if (sum(mymodel()) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "colplotp_divorced_wing.png",
									contentType = "image/png",
									width = input$GetScreenWidth/4,
									#											height = 700,
									alt = "Error: Unable to display plot!")
						}
					}, deleteFile = FALSE)
			
			output$image7c <- renderImage({
						mymodel()
						if (sum(mymodel()) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "colplotp_widowed_wing.png",
									contentType = "image/png",
									width = input$GetScreenWidth/4,
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
							list(src = "cchart_married_cchart.png",
									contentType = "image/png",
									width = 600,
#											height = 560,
									alt = "Error: Unable to display plot!")
						}
						
					}, deleteFile = FALSE)
			
			output$cchart2 <- renderImage({
						mymodel()
						if (sum(mymodel()) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "cchart_divorced_cchart.png",
									contentType = "image/png",
									width = 600,
#											height = 560,
									alt = "Error: Unable to display plot!")
						}
						
					}, deleteFile = FALSE)
			
			output$cchart3 <- renderImage({
						mymodel()
						if (sum(mymodel()) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "cchart_widowed_cchart.png",
									contentType = "image/png",
									width = 600,
#											height = 560,
									alt = "Error: Unable to display plot!")
						}
						
					}, deleteFile = FALSE)
			
			output$cchart4 <- renderImage({
						mymodel()
						if (sum(mymodel()) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "cchartp_single_cchart.png",
									contentType = "image/png",
									width = 600,
#											height = 560,
									alt = "Error: Unable to display plot!")
						}
						
					}, deleteFile = FALSE)
			
			output$cchart5 <- renderImage({
						mymodel()
						if (sum(mymodel()) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "cchartp_married_cchart.png",
									contentType = "image/png",
									width = 600,
#											height = 560,
									alt = "Error: Unable to display plot!")
						}
						
					}, deleteFile = FALSE)
			
			output$cchart6 <- renderImage({
						mymodel()
						if (sum(mymodel()) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "cchartp_divorced_cchart.png",
									contentType = "image/png",
									width = 600,
#											height = 560,
									alt = "Error: Unable to display plot!")
						}
					}, deleteFile = FALSE)
			
			output$cchart7 <- renderImage({
						mymodel()
						if (sum(mymodel()) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "cchartp_widowed_cchart.png",
									contentType = "image/png",
									width = 600,
#											height = 560,
									alt = "Error: Unable to display plot!")
						}
					}, deleteFile = FALSE)
			
			output$cchartb <- renderImage({
						mymodel()
						if (sum(mymodel()) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "cchart_married_cchart.png",
									contentType = "image/png",
									width = input$GetScreenWidth/4,
									#											height = 560,
									alt = "Error: Unable to display plot!")
						}
					}, deleteFile = FALSE)
			
			output$cchart2b <- renderImage({
						mymodel()
						if (sum(mymodel()) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "cchart_divorced_cchart.png",
									contentType = "image/png",
									width = input$GetScreenWidth/4,
									#											height = 560,
									alt = "Error: Unable to display plot!")
						}
					}, deleteFile = FALSE)
			
			output$cchart3b <- renderImage({
						mymodel()
						if (sum(mymodel()) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "cchart_widowed_cchart.png",
									contentType = "image/png",
									width = input$GetScreenWidth/4,
									#											height = 560,
									alt = "Error: Unable to display plot!")
						}
					}, deleteFile = FALSE)
			
			output$cchart4b <- renderImage({
						mymodel()
						if (sum(mymodel()) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "cchartp_single_cchart.png",
									contentType = "image/png",
									width = input$GetScreenWidth/4,
									#											height = 560,
									alt = "Error: Unable to display plot!")
						}
					}, deleteFile = FALSE)
			
			output$cchart5b <- renderImage({
						mymodel()
						if (sum(mymodel()) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "cchartp_married_cchart.png",
									contentType = "image/png",
									width = input$GetScreenWidth/4,
									#											height = 560,
									alt = "Error: Unable to display plot!")
						}
					}, deleteFile = FALSE)
			
			output$cchart6b <- renderImage({
						mymodel()
						if (sum(mymodel()) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "cchartp_divorced_cchart.png",
									contentType = "image/png",
									width = input$GetScreenWidth/4,
									#											height = 560,
									alt = "Error: Unable to display plot!")
						}
					}, deleteFile = FALSE)
			
			output$cchart7b <- renderImage({
						mymodel()
						if (sum(mymodel()) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "cchartp_widowed_cchart.png",
									contentType = "image/png",
									width = input$GetScreenWidth/4,
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
							list(src = "ccchart_married_ccchart.png",
									contentType = "image/png",
									width = 600,
#											height = 560,
									alt = "Error: Unable to display plot!")
						}
					}, deleteFile = FALSE)
			
			output$ccchart2 <- renderImage({
						mymodel()
						if (sum(mymodel()) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "ccchart_divorced_ccchart.png",
									contentType = "image/png",
									width = 600,
#											height = 560,
									alt = "Error: Unable to display plot!")
						}
					}, deleteFile = FALSE)
			
			output$ccchart3 <- renderImage({
						mymodel()
						if (sum(mymodel()) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "ccchart_widowed_ccchart.png",
									contentType = "image/png",
									width = 600,
#											height = 560,
									alt = "Error: Unable to display plot!")
						}
					}, deleteFile = FALSE)
			
			output$ccchart4 <- renderImage({
						mymodel()
						if (sum(mymodel()) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "ccchartp_single_ccchart.png",
									contentType = "image/png",
									width = 600,
#											height = 560,
									alt = "Error: Unable to display plot!")
						}
					}, deleteFile = FALSE)
			
			output$ccchart5 <- renderImage({
						mymodel()
						if (sum(mymodel()) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "ccchartp_married_ccchart.png",
									contentType = "image/png",
									width = 600,
#											height = 560,
									alt = "Error: Unable to display plot!")
						}
					}, deleteFile = FALSE)
			
			output$ccchart6 <- renderImage({
						mymodel()
						if (sum(mymodel()) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "ccchartp_divorced_ccchart.png",
									contentType = "image/png",
									width = 600,
#											height = 560,
									alt = "Error: Unable to display plot!")
						}
					}, deleteFile = FALSE)
			
			output$ccchart7 <- renderImage({
						mymodel()
						if (sum(mymodel()) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "ccchartp_widowed_ccchart.png",
									contentType = "image/png",
									width = 600,
#											height = 560,
									alt = "Error: Unable to display plot!")
						}
					}, deleteFile = FALSE)
			
			output$ccchartb <- renderImage({
						mymodel()
						if (sum(mymodel()) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "ccchart_married_ccchart.png",
									contentType = "image/png",
									width = input$GetScreenWidth/4,
									#											height = 560,
									alt = "Error: Unable to display plot!")
						}
					}, deleteFile = FALSE)
			
			output$ccchart2b <- renderImage({
						mymodel()
						if (sum(mymodel()) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "ccchart_divorced_ccchart.png",
									contentType = "image/png",
									width = input$GetScreenWidth/4,
									#											height = 560,
									alt = "Error: Unable to display plot!")
						}
					}, deleteFile = FALSE)
			
			output$ccchart3b <- renderImage({
						mymodel()
						if (sum(mymodel()) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "ccchart_widowed_ccchart.png",
									contentType = "image/png",
									width = input$GetScreenWidth/4,
									#											height = 560,
									alt = "Error: Unable to display plot!")
						}
					}, deleteFile = FALSE)
			
			output$ccchart4b <- renderImage({
						mymodel()
						if (sum(mymodel()) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "ccchartp_single_ccchart.png",
									contentType = "image/png",
									width = input$GetScreenWidth/4,
									#											height = 560,
									alt = "Error: Unable to display plot!")
						}
					}, deleteFile = FALSE)
			
			output$ccchart5b <- renderImage({
						mymodel()
						if (sum(mymodel()) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "ccchartp_married_ccchart.png",
									contentType = "image/png",
									width = input$GetScreenWidth/4,
									#											height = 560,
									alt = "Error: Unable to display plot!")
						}
					}, deleteFile = FALSE)
			
			output$ccchart6b <- renderImage({
						mymodel()
						if (sum(mymodel()) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "ccchartp_divorced_ccchart.png",
									contentType = "image/png",
									width = input$GetScreenWidth/4,
									#											height = 560,
									alt = "Error: Unable to display plot!")
						}
					}, deleteFile = FALSE)
			
			output$ccchart7b <- renderImage({
						mymodel()
						if (sum(mymodel()) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "ccchartp_widowed_ccchart.png",
									contentType = "image/png",
									width = input$GetScreenWidth/4,
									#											height = 560,
									alt = "Error: Unable to display plot!")
						}
					}, deleteFile = FALSE)
			
			output$summary <- renderImage({
						mymodel()
						if (sum(mymodel()) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "colplot_patientsummary.png",
									contentType = "image/png",
									width = input$GetScreenWidth/3,
									#											height = 560,
									alt = "Error: Unable to display plot!")
						}
					}, deleteFile = FALSE)
			
			output$summary2 <- renderImage({
						mymodel()
						if (sum(mymodel()) == 0){
							list(src = "www/empty.png",
									contentType = "image/png",
									width = "0",
#											height = 700,
									alt = "Error: Unable to display plot!")
						} else {
							# Return a list containing information about the image
							list(src = "colplot_patientsummary2.png",
									contentType = "image/png",
									width = input$GetScreenWidth/3,
									#											height = 560,
									alt = "Error: Unable to display plot!")
						}
					}, deleteFile = FALSE)
			
			output$table <- renderDataTable({
						i=1
						temp=cbind(coef(fit)[i,],exp(cbind(OR = coef(fit)[i,], confint(fit)[,,i])))
						temp=temp[-1,]
						colnames(temp)[1]="beta"
						temp=data.table(round(temp,4),keep.rownames=TRUE)
						setnames(temp,"rn",'variable')
						temp[1,1]='sex (male vs. female)'
						temp				
					})
			
			output$table2 <- renderDataTable({
						i=2
						temp=cbind(coef(fit)[i,],exp(cbind(OR = coef(fit)[i,], confint(fit)[,,i])))	
						colnames(temp)[1]="beta"
						temp=temp[-1,]
						temp=data.table(round(temp,4),keep.rownames=TRUE)
						setnames(temp,"rn",'variable')
						temp[1,1]='sex (male vs. female)'
						temp					
					})
			
			output$table3 <- renderDataTable({
						i=3
						temp=cbind(coef(fit)[i,],exp(cbind(OR = coef(fit)[i,], confint(fit)[,,i])))
						colnames(temp)[1]="beta"
						temp=temp[-1,]
						temp=data.table(round(temp,4),keep.rownames=TRUE)
						setnames(temp,"rn",'variable')
						temp[1,1]='sex (male vs. female)'
						temp
					})
			
			output$temp <- renderText({
						if (sum(mymodel()) == 0){ 
							updateNavbarPage(session, "righttabs", selected = "Disclaimer")
							return("Disclaimer needs to be accepted before calculations can proceed.")
							
						} else{
							return("")
							
						}
						
					})
			
		})


