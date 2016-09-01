#' Visualize a risk prediction model by means of colored bars.
#' 
#' Display a graph (color based nomogram) in which the contributions of each predictor or set of predictors is represented in a colored bar.  
#' 	The color indicates the value of the contribution.
#' 
#' The colorplot is a chart (similar to a nomogam) that visualizes the contribution of a predictor or a set of predictors by means of 
#' 	colored bars.  Depending on the value of \code{zerolevel}, the visualized contributions are slightly different.  
#' 	If \code{zerolevel}="zero", the contribution for variable \eqn{x^p} is \eqn{\beta_pf_p(x^p)}, with \eqn{\beta_p} the model
#' 	coefficient corresponding to this predictor and \eqn{f_p(x^p)} a (possible) transformation of \eqn{x^p}.  If \code{zerolevel} is "min", "median" or "mean", a value equal to 
#' 	the minimum, median and mean of the contribution \eqn{\beta_pf_p(x^p)} in the training data, respectively, is substracted from the contribution.  See the references for more information.
#' 	\code{coloroptions} enables to choose between diffent color maps.  It is recommended to use the sequential or the viridis color map when \code{zerolevel}="min" 
#' 	and a diverging color map when \code{zerolevel} is "median" or "mean".  For the latter case, a white color will correspond 
#' 	to zero points.  In the color bar converting the score to a risk, white will correspond to the median or mean observed risk 
#' 	in the training data, respectively.
#' 
#' @param x \code{glm}, \code{coxph}, \code{mfp}, \code{multinom} or \code{ksvm} object. 
#' @param filename The name of the resulting file (default: colplot).
#' @param coloroptions If 1, the rainbow color map is used.  If 2, a sequential color map is used.  If 3, a diverging color map is used. 
#' 	If 4, a black-and-white color map is used. If 5, the viridis color map is used. (default=2)
#' @param zerolevel The value of the contributions that should be put to zero.  If "zero", the contributions are represented 
#' 	as they are.  If "min", for each predictor or set of predictors contributing to an interaction, the minimal observed value 
#' 	of the contribution in the training data is substracted from the contribution to ensure that the contribution is always positive.  
#'  If "median" or "mean", the median or mean value is substracted from the contributions, respectively (default="zero").  See below for more details.
#' @param risklabel A character string representing the label for the represented risk. For  multinomial logistic regression models, a vector of risk labels should be provided. 
#'  See the examples for an illustration of the approach.
#' @param xmin Minimal values of input variables to be represented on the visualization.  These values only have an influence on continuous input variables.
#' @param xmax Maximal values of input variables to be represented on the visualization.  These values only have an influence on continuous input variables.
#' @param adverse A logical indicating whether the score and risk range in the adverse direction (default=FALSE, i.e. high score corresponds to a high risk). 
#' @param obs A data.frame containing the predictor values of the observation that should be added to the plot.
#' @param q5 A data.frame containing the predictor values of the 5th percentiles of the predictors that should be added to the plot. This only impacts the plot for continous variables.
#' @param q95 A data.frame containing the predictor values of the 95th percentiles of the predictors that should be added to the plot. This only impacts the plot for continous variables.
#' @param time The time at which the estimated survival should be calculated.  As default, the estimated survival at median survival time is reported.  If the median 
#' 	survival time can not be calculated, the estimated survival at the latest event time is reported.  For objects that are not a member of the \code{coxph} class, this is redundant.
#' @author Vanya Van Belle
#' @aliases colplot.glm colplot.coxph colplot.default colplot.multinom colplot.ksvm
#' @importFrom Hmisc cut2
#' @importFrom survival survfit
#' @importFrom fields tim.colors
#' @importFrom fields designer.colors
#' @importFrom viridis viridis
#' @importFrom stats coefficients 
#' @importFrom methods hasArg 
#' @importFrom grDevices png dev.off
#' @import graphics
#' @export
#' @seealso \code{\link{cchart}}, \code{\link{ccchart}}
#' @example /inst/examples/colplotex.R
#' @note This graph can not be used for cox proportional hazard regression including strata.
#' @note For \code{coxph} models, it is necessary to include \code{model=TRUE} in the model fit.
#' @note For \code{multinom} models, it is necessary to include \code{model=TRUE} in the model fit.
#' @note For \code{multinom} models, more than one output file is generated.  A first series of plots visualizes how the linear predictors are obtained.  The files
#' 	are named "filename_outcome_level", with "outcome_level" the name of the outcome level for which the linear predictor is visualized.  A second series of plots
#' 	visualizes how the linear predictors are transformed into a risk prediction for each outcome level.  The files are named "filename_p_outcome_level".  A third 
#' 	series of plots uses an alternative way to represent the calculation of the risk of the non-reference outcome levels.  These plots are named "filename_outcome_level_wing".
#' @note For \code{multinom} models,  a vector of risk labels needs to be made and provided to the \code{colplot()} function.  See the examples for an illustration of the approach.
#' @note For \code{ksvm} models, it is necessary to include \code{prob.model=TRUE} in the model fit.
#' @note The plot is not shown in a graphical window but saved in the current working directory.
#' @references Van Belle V., Van Calster B., \emph{Visualizing risk prediction models}, PLoS ONE, 10(7):e0132614. doi:10.1371/journal.pone.0132614 (2015).
#' @references Van Belle V., Van Calster B., Suykens J.A.K., Van Huffel S. and Lisboa P., \emph{Explaining support vector machines: a color based nomogram}, Internal Report 16-27, ESAT-Stadius, KU Leuven (Leuven, Belgium), 2016
#' @references Van Belle V., Van Huffel S., Timmerman D., Froyman W., Bourne T. and Van Calster B., \emph{A color based nomogram for Multinomial Logistic Regression}, Internal Report 16-28, ESAT-Stadius, KU Leuven (Leuven, Belgium), 2016
colplot<-function(x,filename, coloroptions=2,zerolevel="zero",
		risklabel,xmin,xmax,adverse,obs,q5,q95,time) UseMethod("colplot")

#' @export
colplot.default<-function(x,filename, coloroptions=2,zerolevel="zero",
		risklabel="Predicted risk",xmin,xmax,adverse=FALSE,obs,q5,q95,time) {
	
	
	# if x is a score model: plot as scoring system
	if ("points"%in%names(x)&"cutoffs"%in%names(x)) {
		colplot_score(x, filename, coloroptions,zerolevel,
				risklabel,xmin,xmax,adverse, obs,q5,q95,time)
	} else {
		
		# prepare to plot
		if (!"multinom"%in%names(x)) { 
			x2<-precolplot(x, filename, coloroptions,zerolevel,
					risklabel,xmin,xmax,adverse,obs,q5,q95)
		} else
		{
			x2=x
		}
		
		
		pcp<-check_data(x2,type="colplot")
		
		colplot_core(pcp=pcp)
	}
	
}

#' @export
colplot.glm<-function(x,filename, coloroptions=2,zerolevel="zero",
		risklabel="Predicted risk",xmin,xmax,adverse=FALSE,obs,q5,q95,time){
	
	x2<-glm2data(x,zerolevel,risklabel,adverse)
	
	if(hasArg(obs)){
		obs=adaptObs(x2,obs)
	}	
	if(hasArg(q5)){
		q5=adaptObs(x2,q5)
	}
	if(hasArg(q95)){
		q95=adaptObs(x2,q95)
	}
	if(hasArg(xmin)){
		xmin=as.numeric(adaptObs(x2,xmin))
	}
	if(hasArg(xmax)){
		xmax=as.numeric(adaptObs(x2,xmax))
	}
	
	# if only categorical x are present in the model, plot the model as a scoring system
	if (all(x2$vartypes=="cat")) {
		# define the cutoff-values and points for main effects
		indfactors=which(attr(x$terms,"dataClasses")=="factor")-1
		indlogicals=which(attr(x$terms,"dataClasses")=="logical")-1
		indother=c(1:x2$d)
		if(length(indfactors)+length(indlogicals)>0){
			indother=indother[-c(indfactors,indlogicals)]
		}
		cutoffs=list()
		points=list()
		alllevels=numeric(length(x2$vartypes))
		for (i in seq(1,length(x2$vartypes),1)) {
			alllevels[i]=ifelse(i%in%indfactors,length(x$xlevels[[i]])-1,1) # indicates the number of coefficients for variable i
		}
		cumlevels=cumsum(alllevels)
		if (length(indfactors)>0) {
			for (i in seq(1,length(indfactors))) {
				cutoffs[[indfactors[i]]]=c(1:length(x$xlevels[[i]]))[-1]
				points[[indfactors[i]]]=c(0,x$coefficients[seq(ifelse(indfactors[i]==1,1,cumlevels[indfactors[i]-1]+1),cumlevels[indfactors[i]],1)+1])
			}
		}
		if (length(indlogicals)>0) {
			for (i in seq(1,length(indlogicals))) {
				cutoffs[[indlogicals[i]]]=c(1)
				points[[indlogicals[i]]]=c(0,x$coefficients[seq(ifelse(indlogicals[i]==1,1,cumlevels[indlogicals[i]-1]+1),cumlevels[indlogicals[i]],1)+1])
			}
		}
		if (length(indother)>0) {
			for (i in seq(1,length(indother))) {
				cutoffs[[indother[i]]]=c(1)
				points[[indother[i]]]=c(0,x$coefficients[seq(ifelse(indother[i]==1,1,cumlevels[indother[i]-1]+1),cumlevels[indother[i]],1)+1])
			}
		}
		
		# generate list of cutoffs and points for the interactions
		if (length(x2$posint)>0) { # there are interaction effects
			icutoffs=list()
			ipoints=list()
			ipoints2=list()
			
			for (i in seq(along = seq(1,dim(x2$interactions)[1]))) {
				icutoffs[[i]]=list()
				icutoffs[[i]][[1]]=c(0)
				icutoffs[[i]][[2]]=c(0)
				ipoints[[i]]=rbind(c(0,0),c(0,0)) 
			}
			
			for (i in seq(1,length(x2$posint),1)) {
				i1=x2$interactions[x2$posint[i],1]
				i2=x2$interactions[x2$posint[i],2]
				icutoffs[[x2$posint[i]]][[1]]=cutoffs[[i1]]
				icutoffs[[x2$posint[i]]][[2]]=cutoffs[[i2]]
				xi=expand.grid(x2$levelnames[[2]][[i1]],x2$levelnames[[2]][[i2]])
				
				xtemp=x2$x[seq(1,dim(xi)[1],1),]
				xtemp[,i1]=xi[,1]
				xtemp[,i2]=xi[,2]
				fi=getfuncformGLM(x2,xtemp)
				ipoints2[[i]]=fi[,x2$posint[i]+x2$d]
				ipoints[[x2$posint[i]]]=matrix(fi[,x2$posint[i]+x2$d],
						nrow = length(icutoffs[[x2$posint[i]]][[1]])+1, 
						ncol = length(icutoffs[[x2$posint[i]]][[2]])+1, byrow = FALSE)
				
			}
			
			intcontr=do.call(expand.grid, ipoints2)
			names(intcontr)=paste(names(intcontr),'x',sep='')
			x2$ipoints=ipoints
			x2$icutoffs=icutoffs
			
		}	
		
		x2$points=points
		x2$cutoffs=cutoffs
		
		colplot_score(x2, filename, coloroptions,zerolevel,
				risklabel,xmin,xmax,adverse,obs,q5,q95,time)
		
	} else {
		colplot(x2, filename, coloroptions,zerolevel,
				risklabel,xmin,xmax,adverse,obs,q5,q95,time)
	}
	
}

#' @export
colplot.coxph<-function(x,filename,coloroptions=2,zerolevel="zero",
		risklabel="Predicted Survival",xmin,xmax,adverse=TRUE,obs,q5,q95,time){
	
	x2<-coxph2data(x,zerolevel,risklabel,adverse,time)
	
	if(hasArg(obs)){
		obs=adaptObs(x2,obs)
	}
	if(hasArg(q5)){
		q5=adaptObs(x2,q5)
	}
	if(hasArg(q95)){
		q95=adaptObs(x2,q95)
	}
	if(hasArg(xmin)){
		xmin=as.numeric(adaptObs(x2,xmin))
	}
	if(hasArg(xmax)){
		xmax=as.numeric(adaptObs(x2,xmax))
	}
	
	# if only categorical x are present in the model, plot the model as a scoring system
	if (all(x2$vartypes=="cat")) {
		# define the cutoff-values and points for main effects
		indfactors=which(attr(x$terms,"xClasses")=="factor")-1
		indlogicals=which(attr(x$terms,"xClasses")=="logical")-1
		indother=c(1:x2$d)
		if(length(indfactors)+length(indlogicals)>0){
			indother=indother[-c(indfactors,indlogicals)]
		}
		cutoffs=list()
		points=list()
		alllevels=numeric(length(x2$vartypes))
		for (i in seq(1,length(x2$vartypes),1)) {
			alllevels[i]=ifelse(i%in%indfactors,length(x$xlevels[[i]])-1,1) # indicates the number of coefficients for variable i
		}
		cumlevels=cumsum(alllevels)
		if (length(indfactors)>0) {
			for (i in seq(1,length(indfactors))) {
				cutoffs[[indfactors[i]]]=c(1:length(x$xlevels[[i]]))[-1]
				points[[indfactors[i]]]=c(0,x$coefficients[seq(ifelse(indfactors[i]==1,1,cumlevels[indfactors[i]-1]+1),cumlevels[indfactors[i]],1)+1])
			}
		}
		if (length(indlogicals)>0) {
			for (i in seq(1,length(indlogicals))) {
				cutoffs[[indlogicals[i]]]=c(1)
				points[[indlogicals[i]]]=c(0,x$coefficients[seq(ifelse(indlogicals[i]==1,1,cumlevels[indlogicals[i]-1]+1),cumlevels[indlogicals[i]],1)+1])
			}
		}
		if (length(indother)>0) {
			for (i in seq(1,length(indother))) {
				cutoffs[[indother[i]]]=c(1)
				points[[indother[i]]]=c(0,x$coefficients[seq(ifelse(indother[i]==1,1,cumlevels[indother[i]-1]+1),cumlevels[indother[i]],1)+1])
			}
		}
		
		
		# generate list of cutoffs and points for the interactions
		if (length(x2$posint)>0) { # there are interaction effects
			icutoffs=list()
			ipoints=list()
			ipoints2=list()
			
			for (i in seq(along = seq(1,dim(x2$interactions)[1]))) {
				icutoffs[[i]]=list()
				icutoffs[[i]][[1]]=c(0)
				icutoffs[[i]][[2]]=c(0)
				ipoints[[i]]=rbind(c(0,0),c(0,0)) 
			}
			
			for (i in seq(1,length(x2$posint),1)) {
				i1=x2$interactions[x2$posint[i],1]
				i2=x2$interactions[x2$posint[i],2]
				icutoffs[[x2$posint[i]]][[1]]=cutoffs[[i1]]
				icutoffs[[x2$posint[i]]][[2]]=cutoffs[[i2]]
				xi=expand.grid(x2$levelnames[[2]][[i1]],x2$levelnames[[2]][[i2]])
				
				xtemp=x2$x[seq(1,dim(xi)[1],1),]
				xtemp[,i1]=xi[,1]
				xtemp[,i2]=xi[,2]
				fi=getfuncformCPH(x2,xtemp)
				ipoints2[[i]]=fi[,x2$posint[i]+x2$d]
				ipoints[[x2$posint[i]]]=matrix(fi[,x2$posint[i]+x2$d],
						nrow = length(icutoffs[[x2$posint[i]]][[1]])+1, 
						ncol = length(icutoffs[[x2$posint[i]]][[2]])+1, byrow = FALSE)
				
			}
			
			intcontr=do.call(expand.grid, ipoints2)
			names(intcontr)=paste(names(intcontr),'x',sep='')
			x2$ipoints=ipoints
			x2$icutoffs=icutoffs
			
		}	
		
		x2$points=points
		x2$cutoffs=cutoffs
		
		colplot_score(x2, filename, coloroptions,zerolevel,
				risklabel=x2$risklabel,xmin,xmax,adverse,obs,q5,q95,time)
		
	} else {
		colplot(x2, filename, coloroptions,zerolevel,
				risklabel=x2$risklabel,xmin,xmax,adverse,obs,q5,q95,time)
	}
	
}


#' @export
colplot.multinom<-function(x,filename, coloroptions=2,zerolevel="zero",
		risklabel="Predicted risk",xmin,xmax,adverse=FALSE,obs,q5,q95,time) {
	
	message(paste("More than one figure will be created within the directory",getwd()))
	
	if(hasArg(obs)){
		obs=adaptObs(x,obs)
	}	
	if(hasArg(q5)){
		q5=adaptObs(x,q5)
	}
	if(hasArg(q95)){
		q95=adaptObs(x,q95)
	}
	if(hasArg(xmin)){
		xmin=as.numeric(adaptObs(x,xmin))
	}
	if(hasArg(xmax)){
		xmax=as.numeric(adaptObs(x,xmax))
	}
	
	coeffs=coefficients(x)
	
	# prevalance of outcome levels
	pbase=summary(x$model[,1])/dim(x$model)[1]
	outlevels=levels(x$model[,1])
	
	################################
	# create list of lists (containing all the information for colplot.default)
	################################
	
	allfits=getallfitsMLR(x,filename, coloroptions,zerolevel,
			risklabel,xmin,xmax,adverse,obs,q5,q95,time)
	
	# visualization of linear predictors for non-reference outcome levels
	for (i in seq(1,dim(coeffs)[1],1)) {
		# if only categorical x are present in the model, plot the model as a scoring system
		if (all(allfits[[i]]$vartypes=="cat")) {
			colplot_score(allfits[[i]],filename=names(allfits)[i],coloroptions,zerolevel,
					risklabel=allfits[[i]]$risklabel,xmin,xmax,adverse,obs=allfits[[i]]$obs,q5,q95,time)
		} else {
			
			colplot(allfits[[i]],filename=names(allfits)[i],coloroptions,zerolevel,
					risklabel=allfits[[i]]$risklabel,xmin,xmax,adverse,obs=allfits[[i]]$obs,q5,q95,time)
		}
	}
	
	# calculation of predicted chance for reference outcome level
	i=dim(coeffs)[1]+1
	
	colplot(allfits[[i]],filename=names(allfits)[i],coloroptions,zerolevel,
			risklabel=allfits[[i]]$risklabel,,,adverse,obs=allfits[[i]]$obs,q5,q95,time)
	
	# calculation of predicted chance for non-reference outcome levels
	for (i in dim(coeffs)[1]+1+seq(1,dim(coeffs)[1],1)) {
		# generate the plot
		colplot(allfits[[i]],filename=names(allfits)[i],coloroptions,zerolevel,
				risklabel=allfits[[i]]$risklabel,,,adverse,obs=allfits[[i]]$obs,q5,q95,time)

	}
	
	# alternative plots for chance on non-reference outcome level
	wingplot(allfits,coeffs,ylabel=risklabel)
	
	################################
	# generate summary for the observation
	################################
	if(hasArg(obs)){
		patientsummary(allfits,filename,pbase,outlevels)
	}
	
}

colplot_core<-function(pcp) {
	
	pcp<-check_data(pcp,type="colplot")
	
	# start building the graph
	devwidth=0
	devheight=0
	
	# the number of interaction effects
	if (pcp$d2>0) {
		if(pcp$d2==1){
			d3=sum(max(pcp$f[,seq(pcp$d+1,pcp$d+pcp$d2,1)])-min(pcp$f[,seq(pcp$d+1,pcp$d+pcp$d2,1)])!=0)
		}else{
			d3=sum(apply(pcp$f[,seq(pcp$d+1,pcp$d+pcp$d2,1)],2,max)-apply(pcp$f[,seq(pcp$d+1,pcp$d+pcp$d2,1)],2,min)!=0)
		}
	} else {
		d3=0
	}
	
	
	if (d3==0 |d3>6) { # only main effects
		devwidth=10
		devheight=1.4+1.4*ceiling(pcp$d/ceiling(pcp$d/5))
		
		png(paste0(ifelse("filename"%in%names(pcp),pcp$filename,"colplot"),".png"),width=devwidth,height=devheight, units = "in",res=120)
		
		par(mar = rep(2, 4))	
		
		# first divide figure in top and bottom (small)
		if(all(!is.nan(pcp$risks[[1]]))){
			split.screen(rbind(c(0,1,1.4/(1.4+1.4*ceiling(pcp$d/ceiling(pcp$d/5)))+0.05,1),c(0, 1,0.05,1.4/(1.4+1.4*ceiling(pcp$d/ceiling(pcp$d/5))))))
			
		} else { # no conversion from score to risk
			split.screen(rbind(c(0,1,0,0.95),c(0, 1,0.95,1)))
		}
				
		# first screen: upper plot 
		# divide first screen into the figure region (left) and legend region (right)
		split.screen( rbind(c(0, .9,0,1), c(.9,1,0,1)),screen=1)->ind
		
		# main effects
		# divide the upper left part in d subregions and plot a color bar for each predictor, max 5 color bars underneath eachother
		split.screen(c(ceiling(pcp$d/ceiling(pcp$d/5)),ceiling(pcp$d/5)), screen=ind[1])-> indmain
		
		
	} else{ # there are interaction effects
		
		devwidth=10
		devheight=10
		
		png(paste0(ifelse("filename"%in%names(pcp),pcp$filename,"colplot"),".png"),width=devwidth,height=devheight, units = "in",res=120)
		
		
		par(mar = rep(2, 4))
		if (d3<3) { # only one or two interaction effects
			# first divide figure in top and bottom (small)
			if(all(!is.nan(pcp$risks[[1]]))){
				split.screen(rbind(c(0,1,0.25,1),c(0, 1,0.05,0.2)))
			} else { # no conversion from score to risk
				split.screen(rbind(c(0,1,0,0.95),c(0, 1,0.95,1)))
				title(main=pcp$risklabel)
				
			}
			
			
			# divide first screen into the figure region (left) and legend region (right)
			split.screen( rbind(c(0, .9,0,1), c(.9,1,0,1)),screen=1)->ind
			
			# divide the plot into two regions (left, right) for main/ interaction effects
			# the number of columns equals the number of interactions
			split.screen(rbind(c(0,0.5,0,1), c(0.5,1,0,1)),screen=ind[1])->indmainint
			
			
			# main effects
			# divide the upper left part in d subregions and plot a color bar for each predictor, 
			split.screen(c(pcp$d,1), screen=indmainint[1])-> indmain
			# interaction effects below main effects
			split.screen(c(2,1), screen=indmainint[2])-> indint
		} else {
			# first divide figure in top and bottom (small)
			split.screen(rbind(c(0,1,0.25,1),c(0, 1,0.05,0.2)))
			
			# divide first screen into the figure region (left) and legend region (right)
			split.screen( rbind(c(0, .9,0,1), c(.9,1,0,1)),screen=1)->ind
			
			# divide the plot into two regions (top, bottom) for main/ interaction effects
			# the number of columns equals the number of interactions
			split.screen(rbind(c(0,1,0.5,1), c(0,1,0,0.5)),screen=ind[1])->indmainint
			# main effects
			# divide the upper left part in d subregions and plot a color bar for each predictor, 
			split.screen(c(ceiling(pcp$d/min(d3,3)),min(d3,3)), screen=indmainint[1])-> indmain
			
			# interaction effects below main effects
			split.screen(c(ceiling(d3/min(d3,3)),min(d3,3)), screen=indmainint[2])-> indint
			
		}
		
	}
	
	# plot the main effects
	for (i in seq(along = seq(1:pcp$d))) {
		screen(indmain[i])
		
		if (pcp$vartypes[i]=="cont") {

			xi=pcp$maincont$xsteps[,i]
			xi=xi[c(1:length(unique(xi)))]
			fi=pcp$maincont$fsteps[,i]
			fi=fi[c(1:length(unique(xi)))]
			
			if ("points"%in%names(pcp)) {# score system
				image(xi,1,as.matrix(c(1:length(fi))),axes=F,xlim=range(xi),xlab = NA, ylab = NA,col=pcp$cols1[match(round(fi,2),round(pcp$allcolors,2))],useRaster=TRUE)
				
			} else { # not a score system
				image(xi,1,as.matrix(fi),axes=F,xlim=range(xi),xlab = NA, ylab = NA,col=pcp$cols1,useRaster=TRUE,zlim=pcp$frange)
			}
			
			# add patient if necessary
			if ("obs"%in%names(pcp)) {
				if (pcp$obs[i]>=min(xi) & pcp$obs[i]<=max(xi)) {
					points(pcp$obs[i],0.7,pch=24, col="white",bg="white",cex=3)
					points(pcp$obs[i],0.7,pch=24, col="black",bg="black",cex=2)
				} else if (pcp$obs[i]>max(xi)) {
					points(max(xi)-0.05*(max(xi)-min(xi)),0.8,pch=-9658 , col="white",bg="white",cex=3)
					points(max(xi)-0.05*(max(xi)-min(xi)),0.8,pch=-9658 , col="black",bg="black",cex=2)
				} else if (pcp$obs[i]<min(xi)) {
					points(min(xi)+0.05*(max(xi)-min(xi)),0.8,pch=-9668  , col="white",bg="white",cex=3)
					points(min(xi)+0.05*(max(xi)-min(xi)),0.8,pch=-9668  , col="black",bg="black",cex=2)
				}
				
			}
			# add percentiles if necessary
			if ("q5"%in%names(pcp)) {
				if(pcp$coloroptions==4){
					lines(c(pcp$q5[i],pcp$q5[i]),c(-1,2),col="white",bg="white",lty=1,lwd=4)
					lines(c(pcp$q5[i],pcp$q5[i]),c(-1,2),col="black",bg="black",lty=2,lwd=3)
				}else{
					lines(c(pcp$q5[i],pcp$q5[i]),c(-1,2),col="gray",lty=2,lwd=3)
				}
				
			}
			if ("q95"%in%names(pcp)) {
				if(pcp$coloroptions==4){
					lines(c(pcp$q95[i],pcp$q95[i]),c(-1,2),col="white",bg="white",lty=1,lwd=4)
					lines(c(pcp$q95[i],pcp$q95[i]),c(-1,2),col="black",bg="black",lty=2,lwd=3)
				}else{
					lines(c(pcp$q95[i],pcp$q95[i]),c(-1,2),col="gray",lty=2,lwd=3)
				}
			}
			if ("cutoffs"%in%names(pcp)) { # score model
				# add cutpoints 
				axis(side = 1,lwd.ticks=0,padj=-1,at=pcp$cutoffs[[i]],labels=pcp$cutoffs[[i]])
				
				# add points 
				tempcuts=sort(unique(c(min(pcp$x[,i]),pcp$cutoffs[[i]],max(pcp$x[,i]))))
				
				printfirstcolor=which(round(pcp$points[[i]]-pcp$fmin[i],2)%in%round(pcp$allcolors[pcp$printcolor],2))
				printsecondcolor=which(round(pcp$points[[i]]-pcp$fmin[i],2)%in%round(pcp$allcolors[!pcp$printcolor],2))
				if (length(printfirstcolor)>0) {
					text((tempcuts[1:length(tempcuts)-1]+diff(tempcuts)/2)[printfirstcolor],1,labels=pcp$points[[i]][printfirstcolor]-pcp$fmin[i], col="white",font=2)	
				}
				if (length(printsecondcolor)>0) {
					text((tempcuts[1:length(tempcuts)-1]+diff(tempcuts)/2)[printsecondcolor],1,labels=pcp$points[[i]][printsecondcolor]-pcp$fmin[i], col="black",font=2)	
				}
			} else { # continuous model
				axis(side = 1,lwd.ticks=0,padj=-1)
				
			}
		} else {
			
			xi=pcp$maincont$xsteps[,i]
			xi=xi[c(1:length(unique(xi)))]
			fi=pcp$maincont$fsteps[,i]
			fi=fi[c(1:length(unique(xi)))]
			
			if ("points"%in%names(pcp)) {# score system
				image(as.integer(xi),1,as.matrix(c(1:length(fi))),axes=F,xlim=range(as.integer(xi))+c(-0.5,+0.5),xlab = NA, ylab = NA,col=pcp$cols1[match(round(fi,2),round(pcp$allcolors,2))])
				
			} else { # not a score system
				image(as.integer(xi),1,as.matrix(fi),axes=F,xlim=range(as.integer(xi))+c(-0.5,+0.5),xlab = NA, ylab = NA,col=pcp$cols1,zlim=pcp$frange)
			}
			
			# add patient if necessary
			if ("obs"%in%names(pcp)) {
				if(pcp$coloroptions==4){
					points(pcp$obs[i],0.7,pch=24, col="white",bg="white",cex=3)
					points(pcp$obs[i],0.7,pch=24, col="black",bg="black",cex=2)
				}else{
					points(pcp$obs[i],0.7,pch=24, col="white",bg="white",cex=3)
					points(pcp$obs[i],0.7,pch=24, col="black",bg="black",cex=2)
				}
			}
			
			
			if ("cutoffs"%in%names(pcp)) { # score model
				axis(side = 1,lwd.ticks=0,padj=-1,at=xi,labels=pcp$levelnames[[1]][[i]])
				
				printfirstcolor=which(round(pcp$points[[i]]-pcp$fmin[i],2)%in%round(pcp$allcolors[pcp$printcolor],2))
				printsecondcolor=which(round(pcp$points[[i]]-pcp$fmin[i],2)%in%round(pcp$allcolors[!pcp$printcolor],2))
				if (length(printfirstcolor)>0) {
					text(xi[printfirstcolor],1,labels=round(pcp$points[[i]][printfirstcolor]-pcp$fmin[i],2), col="white",font=2)	
				}
				if (length(printsecondcolor)>0) {
					text(xi[printsecondcolor],1,labels=round(pcp$points[[i]][printsecondcolor]-pcp$fmin[i],2), col="black",font=2)	
				}
			} else {
				axis(side = 1,lwd.ticks=0,padj=-1,at=as.integer(xi),labels=pcp$levelnames[[1]][[i]])
			}
		}
		box()
		mtext(side=3,pcp$names[i],line=0.5,font=2)
	}
	
	
	if (d3<=6) {
		#plot interaction effects
		plotInteractions(pcp,d3,indint)
	}
	
	
	
	#left figure=color legend
	screen(ind[2])
	
	
	if ("cutoffs"%in%names(pcp)) { # score model
		prev_par=par()
		par(plt=c(0.2,0.6,0.05,0.95))
		allcols2=c(1:length(pcp$allcolors))
		image(c(0),c(allcols2),t(as.matrix(c(allcols2))),col=pcp$cols1,axes=F,xlab=NA,ylab=NA)
		
		
		mtext(side=2,"Color Legend",line=0.5,font=2)
		box()
		if (sum(pcp$printcolor)>0) {
			text(0,allcols2[pcp$printcolor],labels=round(pcp$allcolors[pcp$printcolor],2), col="white",font=2)
		}
		if (sum(!pcp$printcolor)>0) {
			text(0,allcols2[!pcp$printcolor],labels=round(pcp$allcolors[!pcp$printcolor],2), col="black",font=2)
		}
		
		par=prev_par
	} else {
		prev_par=par()
		par(plt=c(0.2,0.6,0.05,0.95))
		toplot=seq(min(pcp$frange),max(pcp$frange),length=100)
		image(c(0),toplot,t(as.matrix(toplot)),col=pcp$cols1,axes=F,xlab=NA,ylab=NA,useRaster=TRUE)
		axis(side = 4,padj=-1.5,lwd.ticks=0)
		mtext(side=2,"Color Legend",line=0.5,font=2)
		box()
		par=prev_par
	}
	
	## only do this if risk estimates can be plotted
	# bottom figure: add convertion from score (=sum of functional form for all predictors) to risk estimate
	# divide region into 3 parts (to provide some empty space)	
	
	if(all(!is.nan(pcp$risks[[1]]))){
		
		temp=as.matrix(pcp$risks[[1]])
		temprisk=as.matrix(pcp$risks[[2]])
		temp=temp[temprisk>=0.01]
		temprisk=temprisk[temprisk>=0.01]
		temp=temp[temprisk<=0.99]
		temprisk=temprisk[temprisk<=0.99]
		
		screen(2)
		
		if (pcp$adverse==FALSE) {
			image(temp,1,as.matrix(temprisk),axes=F,xlim=range(temp),
					xlab = NA, ylab = NA,col=pcp$cols2,zlim=range(temprisk),useRaster=TRUE)
			
		} else {
			image(temp,1,as.matrix(temprisk),axes=F,xlim=range(temp),
					xlab = NA, ylab = NA,col=pcp$cols2b,zlim=range(temprisk),useRaster=TRUE)
			
		}
		
		
		# add score of this patient
		if ("obs"%in%names(pcp)) {
			thisscore=sum(pcp$fpatient)
			if (thisscore<min(temp)) {
				thisscore=min(temp)
			}
			if (thisscore>max(temp)) {
				thisscore=max(temp)
			}
			if(pcp$coloroptions==4){
				points(thisscore,0.7,pch=24, col="white",bg="white",cex=3)	
				points(thisscore,0.7,pch=24, col="black",bg="black",cex=2)	
			}else{
				points(thisscore,0.7,pch=24, col="white",bg="white",cex=3)	
				points(thisscore,0.7,pch=24, col="black",bg="black",cex=2)	
			}
		}
		box()
		
		# add median risk observed in the data and the corresponding score		
		axis(side = 3,padj=1,lwd.ticks=0,at=seq(floor(min(temp)),ceiling(max(temp)),by=0.5),labels=seq(floor(min(temp)),ceiling(max(temp)),by=0.5))
		axis(side = 1,padj=-1,lwd.ticks=0,at=(temp[c(round(seq(1,length(temp),length=12)))]),labels=round(temprisk[c(round(seq(1,length(temp),length=12)))],2))
		
		mtext(side=3,"Score",line=2,font=2)
		mtext(side=1,pcp$risklabel,line=2,font=2)
		
	} else {
		
		screen(2,FALSE)
		
		title(main=pcp$risklabel)
	}
	
	close.screen(all.screens=TRUE)
	
	garbage<-dev.off()
	
	# if there are more than 6 interaction effects, a separate plot only containing the interactions effects is plotted
	if (d3>6) {

		devwidth=10
		devheight=10
		
		png(paste0(ifelse("filename"%in%names(pcp),paste0(pcp$filename,"_B"),"colplot_B"),".png"),width=devwidth,height=devheight, units = "in",res=120)
		
		par(mar = rep(2, 4))
		# max 3 interaction effects
		#close.screen(all.screens=TRUE)
		split.screen(c(ceiling(sqrt(d3)),ceiling(d3/ceiling(sqrt(d3)))))-> indint
		
		plotInteractions(pcp,d3,indint)

		
		close.screen(all.screens=TRUE)
		
		garbage<-dev.off()
		
	}

}


#' @importFrom stats median
#' @importFrom grDevices colorRampPalette gray.colors 
precolplot<-function(x, filename, coloroptions,zerolevel,
		risklabel,xmin,xmax,adverse,obs,q5,q95){
	
	# check x structure
	pcp=x
	
	if (hasArg(xmax)&hasArg(xmin)) {
		if(!all(xmax[which(x$vartypes=="cont")]-xmin[which(x$vartypes=="cont")]>0,na.rm = TRUE)){
			stop("There is something wrong with the ranges of the continuous variables.")
		}
	}
	
	
	if(!hasArg(coloroptions)){
		pcp$coloroptions=NULL
	}
	if(hasArg(filename)){
		pcp$filename=filename
	}
	if(!hasArg(zerolevel)){
		pcp$zerolevel=NULL
	}
	if(!hasArg(q5)){
		pcp$q5=NULL
	}
	if(!hasArg(q95)){
		pcp$q95=NULL
	}
	if(!hasArg(risklabel)){
		pcp$risklabel=NULL
	}
	if(!hasArg(adverse)){
		pcp$adverse=NULL
	}
	if(!hasArg(obs)){
		pcp$obs=NULL
	}
	
	# initializations
	d=dim(x$x)[2]
	n=dim(x$x)[1]
	
	if (!hasArg(coloroptions) & "coloroptions" %in% names(x)) {
		coloroptions=x$coloroptions
	}
	
	#obtain functional forms
	f=x$getfuncform(x)
	
	
	# the action is different depending on te value of zerolevel 
	if (zerolevel=="min") {
		fmin=apply(f,2,min)
	} else if (zerolevel=="median") {
		if ("glm"%in%names(x)) {
			if ("prior.weight"%in%names(x$glm)) {
				f2=apply(f,2,rep,times=x$glm$prior.weight)
				fmin=apply(f2,2,median)
			}else{
				fmin=apply(f,2,median)
				
			}
		} else {
			fmin=apply(f,2,median)
			
		}
		
	}else if (zerolevel=="mean") {
		if ("glm"%in%names(x)) {
			if ("prior.weight"%in%names(x$glm)) {
				f2=apply(f,2,rep,times=x$glm$prior.weight)
				fmin=apply(f2,2,mean)
			}else{
				fmin=apply(f,2,mean)
			}
		} else {
			fmin=apply(f,2,mean)
		}
		
	} else { # zerolevel=="zero"
		fmin=seq(0,0,length=dim(f)[2])
	}
	
	pcp$fmin=fmin
	
	# calculate risks
	risks<-x$getriskestimate(rowSums(f),x) 
	#risks[[1]]<-risks[[1]]-sum(fmin)
	
	# number of interactions
	d2=dim(f)[2]-d
	# check
	if (d2!=0) { # there are interactions
		if(d2!=d*(d-1)/2){
			stop("Something wrong with getfuncform.  The number of interactions does not equal d*(d-1)/2")
		}
		
		if(!is.matrix(x$interactions)){
			stop("Something wrong with interactions. ")
		}
		if (d2!=dim(x$interactions)[1]) {
			stop("Something wrong with getfuncform and/or interactions.  The number of interactions does not equal d*(d-1)/2")
		}
	}
	if (d2!=0 & !is.matrix(x$interactions)) { 
		stop("No matrix of interactions is specified.")
	}
	
	pcp$d2=d2
	pcp$f=f
	
	
	######################
	# set range of contributions
	######################
	
	# extract range of contributions for the main effects
	nbsteps=50	
	
	xsteps=data.frame(matrix(NaN,nrow=nbsteps,ncol=pcp$d))
	
	
	for (i in seq(along = seq(1:pcp$d))) {
		minx=ifelse("factor"%in%class(pcp$x[,i]),min(as.integer(pcp$x[,i])),min(pcp$x[,i]))
		maxx=ifelse("factor"%in%class(pcp$x[,i]),max(as.integer(pcp$x[,i])),max(pcp$x[,i]))
		
		if (pcp$vartypes[i]=="cont") {
			# adapt to user defined boundary of input variables
			if (hasArg(xmin)) {
				minx=ifelse(!is.nan(xmin[i]),max(minx,xmin[i]),minx)	
			}
			if (hasArg(xmax)) {
				maxx=ifelse(!is.nan(xmax[i]),min(maxx,xmax[i]),maxx)
			}
		
			if("colplotpref"%in%names(pcp)){
				maxx=min(max(pcp$x[pcp$x[,i]+pcp$beta0[i]<=3,i]),maxx)
			}
			
			if (sum(pcp$x[,i]==round(pcp$x[,i]))==length(pcp$x[,i])&length(unique(pcp$x[,i]))<50) {
				# all integers
				tempx=seq(minx,maxx,by=ifelse(maxx-minx<nbsteps,1,ceiling((maxx-minx)/nbsteps)))
				if(length(tempx)>nbsteps){
					tempx=tempx[c(1:nbsteps)]
				}
				xsteps[,i]=max(tempx)
				xsteps[c(1:length(tempx)),i]=tempx
			} else{
				xsteps[,i]=seq(minx,maxx,length=nbsteps)	
			}
		} else { 
			tempx=sort(unique(pcp$x[,i]))
			xsteps[,i]=tempx[length(tempx)]
			xsteps[c(1:length(tempx)),i]=tempx
			if (class(pcp$x[,i])=="factor") {
				xsteps[,i]=factor(xsteps[,i],labels=levels(pcp$x[,i]))
			}
		}
	}
	
	x2=pcp
	x2$x=xsteps
	if(!is.null(names(pcp$x))){
		names(x2$x)=names(pcp$x)
	}
	fsteps=pcp$getfuncform(x2);
	
	if (zerolevel=="min") {
		fmin2=apply(fsteps,2,min)
		pcp$fmin=fmin2
	}
	
	fsteps=fsteps-matrix(1,nrow=nbsteps,ncol=1)%*%pcp$fmin
	
	# if zerolevel is mean or median and xmin or xmax are supplied by the user: check whether mean or median is 
	# within the range of the plot, if not: throw an error
	if (zerolevel%in%c("mean","median")&(hasArg(xmin)|hasArg(xmax))) {
		temp=apply(fsteps[,c(1:pcp$d)],2,range)
		if(!all((temp[1,]<=0 & temp[2,]>=0))){
			stop("The range of the input variables is such that the zerolevel will not appear on the plot.  Adapt this range or select another zerolevel.")
		}
	}
	
	maincont=list(xsteps=xsteps,fsteps=fsteps[,c(1:pcp$d)])
	
	frange=range(fsteps)
	
	pcp$maincont=maincont
	
	
# extract range of contributions for interaction effects
	# the number of interaction effects
	if (pcp$d2>0) {
		if(pcp$d2==1){
			d3=sum(max(pcp$f[,seq(pcp$d+1,pcp$d+pcp$d2,1)])-min(pcp$f[,seq(pcp$d+1,pcp$d+pcp$d2,1)])!=0)
		}else{
			d3=sum(apply(pcp$f[,seq(pcp$d+1,pcp$d+pcp$d2,1)],2,max)-apply(pcp$f[,seq(pcp$d+1,pcp$d+pcp$d2,1)],2,min)!=0)
		}

		intcont=vector(mode="list", length=d3)
		
		if (pcp$d2==1) {
			posint=which(max(pcp$f[,seq(pcp$d+1,pcp$d+pcp$d2,1)])-min(pcp$f[,seq(pcp$d+1,pcp$d+pcp$d2,1)])!=0)
			trueeffects=which(max(pcp$f[,seq(pcp$d+1,pcp$d+pcp$d2,1)])-min(pcp$f[,seq(pcp$d+1,pcp$d+pcp$d2,1)])!=0)
			
		} else{
			posint=which(apply(pcp$f[,seq(pcp$d+1,pcp$d+pcp$d2,1)],2,max)-apply(pcp$f[,seq(pcp$d+1,pcp$d+pcp$d2,1)],2,min)!=0)
			trueeffects=which(apply(pcp$f[,seq(pcp$d+1,pcp$d+pcp$d2,1)],2,max)-apply(pcp$f[,seq(pcp$d+1,pcp$d+pcp$d2,1)],2,min)!=0)
			
		}
		
		for (i in seq(along = seq(1:d3))) {
			i1=pcp$interactions[trueeffects[i],1]
			i2=pcp$interactions[trueeffects[i],2]
			
			xi1=unique(xsteps[,i1])
			xi2=unique(xsteps[,i2])
			
			
			xy=meshgrid(xi1,xi2 );
			xtemp=pcp$x[rep(1,length=prod(dim(xy[[1]]))),]
			xtemp[,i1]=c(xy[[1]])
			xtemp[,i2]=c(xy[[2]])
			pcp2=pcp
			pcp2$x=xtemp
			
			fi=pcp$getfuncform(pcp2)[,pcp$d+posint[i]];
			
		
			if (zerolevel=="min") {
				fmin2=min(fi)
				pcp$fmin[pcp$d+posint[i]]=fmin2
			}
			
			fi=fi-pcp$fmin[pcp$d+posint[i]];
			
			
			intcont[[i]]$xi1=xi1
			intcont[[i]]$xi2=xi2
			intcont[[i]]$fi=fi
			
			frange=range(c(frange,range(fi)))
			
			# if zerolevel is mean or median and xmin or xmax are supplied by the user: check whether mean or median is 
			# within the range of the plot, if not: throw an error
			if (zerolevel%in%c("mean","median")&(hasArg(xmin)|hasArg(xmax))) {
				temp=range(fi)
				if(!all((temp[1]<=0 & temp[2]>=0) )){
					stop("The range of the input variables is such that the zerolevel will not appear on the plot.  Adapt this range or select another zerolevel.")
				}
			}
			
		}
		
		pcp$intcont=intcont
	}
	
	# define frange
	pcp$frange=frange
	pcp$nbsteps=nbsteps	
	fmin=pcp$fmin
	
	
	fzero=f-t(matrix(rep(fmin,n),nrow=dim(f)[2],ncol=n))
	
	# if a patient needs to be added to the plot
	if (hasArg(obs)) {
		# calculate functional form for this patient
		
		fpatient=x$getfuncform(x,obs=obs)
		
		# calculate prognostic index for this patient
		indexpatient=sum(fpatient)
		# calculate risk for this patient
		thisrisk<-x$getriskestimate(rowSums(f),x,indexpatient) 
		
		thisrisk=thisrisk[[2]][1]
		# adapt fpatient to zerolevel
		fpatient=fpatient-pcp$fmin
		indexpatient=sum(fpatient)
		
		pcp$obs=obs
		pcp$fpatient=fpatient
		pcp$thisrisk=thisrisk
		pcp$indexpatient=indexpatient
	}
	
	pcp$f=f
	pcp$fzero=fzero
	risks[[1]]<-risks[[1]]-sum(pcp$fmin)
	pcp$risks=risks

	
	#############
	# Generate colormaps
	#############
	# calculate the means of all functional forms
	if ("glm"%in%names(x)) {
		if("prior.weights"%in%names(x$glm)){
			fzero2=apply(fzero,2,rep,times=x$glm$prior.weights)
			medians=apply(fzero2,2,median)
			# calculate the means of the risk predictions
			f2=apply(f,2,rep,times=x$glm$prior.weights)
		} else {
			fzero2=apply(fzero,2,rep,times=1)
			medians=apply(fzero2,2,median)
			# calculate the means of the risk predictions
			f2=apply(f,2,rep,times=1)
		}
		
		allrisks<-x$getriskestimate(rowSums(f2),x,rowSums(f2)) 
		medianpreds=median(allrisks[[2]])	
		meanpreds=mean(allrisks[[2]])	
		
	} else {
		# calculate the means of all functional forms
		medians=apply(fzero,2,median)
		# calculate the means of the risk predictions
		allrisks<-x$getriskestimate(rowSums(f),x,rowSums(f)) 
		medianpreds=median(allrisks[[2]])	
		meanpreds=mean(allrisks[[2]])	
		
	}
	
	if ("points"%in%names(pcp)) { # score model
		allcolors=c()
		for (i in seq(along = c(1:d))) {
			allcolors=c(allcolors,pcp$points[[i]]-fmin[i])
		}
		for (i in seq(along = c(1:d2))) {
			allcolors=c(allcolors,c(pcp$ipoints[[i]]-fmin[i+d]))
			
		}
		allcolors=(sort(unique(round(allcolors,2))))
		
		pcp$allcolors=allcolors
	}
	
	if ("fzeroALL"%in%names(pcp)) {
		fzeroALL=pcp$fzeroALL
	} else {
		fzeroALL=pcp$frange
	}
	
	switch(coloroptions,
			# if coloroptions == 1, then do this
			{# rainbow				
				if ("points"%in%names(pcp)) { # score model
					cols1=fields::tim.colors(length(allcolors))
				} else {
					cols1=fields::tim.colors(100);
					
				}
				cols2=colorRampPalette( c("green", "yellow", "red"), space="rgb")(100)
				cols2b=rev(cols2) # reversed scale
			},
			# if coloroptions == 2, then do this
			{# sequential colormap
				if ("points"%in%names(pcp)) { # score model
					cols1=fields::designer.colors(col=c(rgb(255,255,229,maxColorValue = 255),rgb(0,104,55,maxColorValue = 255)),n=length(allcolors));
					cols1=rev(cols1)
				} else {
					cols1=fields::designer.colors(col=c(rgb(255,255,229,maxColorValue = 255),rgb(0,104,55,maxColorValue = 255)));
					cols1=rev(cols1)		
				}
				
				cols2b=fields::designer.colors(col=c(rgb(255,255,229,maxColorValue = 255),rgb(0,104,55,maxColorValue = 255))) 
				cols2=rev(cols2b) # reversed scale
				
			},
			# if coloroptions == 3, then do this
			{# diverging color map
				if ("points"%in%names(pcp)) { # score model
					cols1=c(fields::designer.colors(col=c(rgb(5,113,176,maxColorValue = 255),rgb(209,229,240,maxColorValue = 255)),n=sum(allcolors<0)),
							rgb(247,247,247,maxColorValue = 255),rgb(247,247,247,maxColorValue = 255),
							fields::designer.colors(col=c(rgb(253,219,199,maxColorValue = 255),
											rgb(202,0,32,maxColorValue = 255)),n=sum(allcolors>0)))
				} else {# not a score model
					cols1=c(fields::designer.colors(col=c(rgb(5,113,176,maxColorValue = 255),rgb(209,229,240,maxColorValue = 255)),n=round(abs(min(fzeroALL))/abs(max(fzeroALL)-min(fzeroALL))*100)),
							rgb(247,247,247,maxColorValue = 255),rgb(247,247,247,maxColorValue = 255),
							fields::designer.colors(col=c(rgb(253,219,199,maxColorValue = 255),
											rgb(202,0,32,maxColorValue = 255)),n=round(abs(max(fzeroALL))/abs(max(fzeroALL)-min(fzeroALL))*100)))
				}
				if (!zerolevel%in%c("mean","median")) {
					
					cols2=fields::designer.colors(col=c(rgb(5,113,176,maxColorValue = 255),rgb(247,247,247,maxColorValue = 255),
									rgb(202,0,32,maxColorValue = 255)))
					cols2b=fields::designer.colors(col=c(rgb(202,0,32,maxColorValue = 255),rgb(247,247,247,maxColorValue = 255),
									rgb(5,113,176,maxColorValue = 255))) # reversed scale
				} else { # zerolevel=mean, or zerolevel=median
					
					minpred=max(0.01,min(risks[[2]]))
					maxpred=min(0.99,max(risks[[2]]))
					
					if (!is.nan(maxpred)) {
						if (zerolevel=="median") {
							cols2=c(fields::designer.colors(col=c(rgb(5,113,176,maxColorValue = 255),rgb(209,229,240,maxColorValue = 255)),n=max(0,round((medianpreds-minpred)/(maxpred-minpred)*100))),
									rgb(247,247,247,maxColorValue = 255),rgb(247,247,247,maxColorValue = 255),
									fields::designer.colors(col=c(rgb(253,219,199,maxColorValue = 255),
													rgb(202,0,32,maxColorValue = 255)),n=max(0,round((maxpred-medianpreds)/(maxpred-minpred)*100))))
							
							cols2b=c(fields::designer.colors(col=c(rgb(202,0,32,maxColorValue = 255),
													rgb(253,219,199,maxColorValue = 255)),n=max(0,round((medianpreds-minpred)/(maxpred-minpred)*100))),
									rgb(247,247,247,maxColorValue = 255),rgb(247,247,247,maxColorValue = 255),
									fields::designer.colors(col=c(rgb(209,229,240,maxColorValue = 255),rgb(5,113,176,maxColorValue = 255)),n=max(0,round((maxpred-medianpreds)/(maxpred-minpred)*100))))		
							
						}
						if (zerolevel=="mean") {
							cols2=c(fields::designer.colors(col=c(rgb(5,113,176,maxColorValue = 255),rgb(209,229,240,maxColorValue = 255)),n=max(0,round((meanpreds-minpred)/(maxpred-minpred)*100))),
									rgb(247,247,247,maxColorValue = 255),rgb(247,247,247,maxColorValue = 255),
									fields::designer.colors(col=c(rgb(253,219,199,maxColorValue = 255),
													rgb(202,0,32,maxColorValue = 255)),n=max(0,round((maxpred-meanpreds)/(maxpred-minpred)*100))))
							
							cols2b=c(fields::designer.colors(col=c(rgb(202,0,32,maxColorValue = 255),
													rgb(253,219,199,maxColorValue = 255)),n=max(0,round((meanpreds-minpred)/(maxpred-minpred)*100))),
									rgb(247,247,247,maxColorValue = 255),rgb(247,247,247,maxColorValue = 255),
									fields::designer.colors(col=c(rgb(209,229,240,maxColorValue = 255),rgb(5,113,176,maxColorValue = 255)),n=max(0,round((maxpred-meanpreds)/(maxpred-minpred)*100))))		
							
						}
					} else {
						cols2=c()
						cols2b=c()
					}	
					
				}
				
				
			},
			# if coloroptions == 4, then do this
			{# sequential black and white color map
				if ("points"%in%names(pcp)) { # score model
					cols1=gray.colors(n=length(allcolors));
					cols1=rev(cols1)
				} else {
					cols1=gray.colors(n=100);
					cols1=rev(cols1)		
				}
				cols2b=gray.colors(n=100) 
				cols2=rev(cols2b) # reversed scale
			},
			# if coloroptions == 5, then do this
			{# viridis (alternative of matlab to jet colormap)
				if ("points"%in%names(pcp)) { # score model
					cols1=viridis::viridis(length(allcolors))
				} else {
					cols1=viridis::viridis(100);
					
				}
				cols2=viridis::viridis(100)
				cols2b=rev(cols2) # reversed scale
			}
	)
	
	###########
	# adapt list
	###########
	
	pcp$f=f
	pcp$fzero=fzero
	pcp$risks=risks
	pcp$d2=d2
	pcp$medians=medians
	pcp$medianpreds=medianpreds
	pcp$cols1=cols1
	pcp$cols2=cols2
	pcp$cols2b=cols2b
	pcp$fmin=fmin
	pcp$coloroptions=coloroptions
	pcp$zerolevel=zerolevel
	pcp$risklabel=risklabel
	pcp$adverse=adverse
	if (all(!is.nan(allrisks[[2]]))) {
		if (sum(allrisks[[2]]==medianpreds)>0) {
			pcp$medianscores=allrisks[[1]][which(allrisks[[2]]==medianpreds)][1]-sum(fmin)
			
		} else {
			ndx <- order(abs(allrisks[[2]]-medianpreds))[1:2]
			pcp$medianscores=mean(allrisks[[1]][ndx])-sum(fmin)
		}
	}
	
	
	if (hasArg(q5)) {
		pcp$q5=q5
	}
	if (hasArg(q95)) {
		pcp$q95=q95
	}
	
	return<-pcp
}

colplot_score<-function(x, filename,coloroptions,zerolevel,
		risklabel,xmin,xmax,adverse, obs,q5,q95,time){
	
	# prepare to plot
	if (!"multinom"%in%names(x)) { 
		x2<-precolplot(x, filename, coloroptions,zerolevel,
				risklabel,adverse,obs,q5,q95)
	} else
	{
		x2=x
	}
	
	pcp<-check_data(x2,type="colplot")
	
	
	#check whether it is a proper score model
	if (!"cutoffs" %in% names(x)) {
		stop("No cutoffs are given for the score model.  Cutoffs should be a list containing 
						the cutoffs at which the allocated points change, for each input vairable")
	}
	if (!"points" %in% names(x)) {
		stop("No points are given for the score model.")
	}
		
	# allocate the color of the points to be printed on the color bars
	printcolor=logical(length=length(pcp$allcolors))
	
	if (min(pcp$allcolors)<0 & max(pcp$allcolors)>0) {
		switch(pcp$coloroptions,
				# coloroption=1
				{printcolor=(abs(pcp$allcolors)>0.65*max(abs(pcp$allcolors)))},
				# coloroption=2
				{printcolor=(pcp$allcolors<mean(pcp$allcolors))},
				# coloroption=3
				{printcolor=(abs(pcp$allcolors)>0.65*max(abs(pcp$allcolors)))}
		)
	}
	if (min(pcp$allcolors)>=0 & max(pcp$allcolors)>=0) {
		switch(pcp$coloroptions,
				# coloroption=1
				{printcolor=(pcp$allcolors<0.25*max(pcp$allcolors))|(pcp$allcolors>0.75*max(pcp$allcolors))},
				# coloroption=2
				{	printcolor=(pcp$allcolors<mean(pcp$allcolors))},
				# coloroption=3
				{printcolor=(pcp$allcolors>mean(pcp$allcolors))}
		)
	}
	
	#pcp$frange=frange
	pcp$printcolor=printcolor
	
	# predefine x-values and effect contributions to plot later
	# extract range of contributions for the main effects

	colplot_core(pcp)
	
}


plotInteractions<-function(pcp,d3,indint){
	# plot interaction effects
	if (d3>0) {
		# interaction effects
		if (pcp$d2==1) {
			posint=which(max(pcp$f[,seq(pcp$d+1,pcp$d+pcp$d2,1)])-min(pcp$f[,seq(pcp$d+1,pcp$d+pcp$d2,1)])!=0)
			trueeffects=which(max(pcp$f[,seq(pcp$d+1,pcp$d+pcp$d2,1)])-min(pcp$f[,seq(pcp$d+1,pcp$d+pcp$d2,1)])!=0)
			
		} else{
			posint=which(apply(pcp$f[,seq(pcp$d+1,pcp$d+pcp$d2,1)],2,max)-apply(pcp$f[,seq(pcp$d+1,pcp$d+pcp$d2,1)],2,min)!=0)
			trueeffects=which(apply(pcp$f[,seq(pcp$d+1,pcp$d+pcp$d2,1)],2,max)-apply(pcp$f[,seq(pcp$d+1,pcp$d+pcp$d2,1)],2,min)!=0)
			
		}
		for (i in seq(along = seq(1:d3))) {
			screen(indint[i])
			i1=pcp$interactions[trueeffects[i],1]
			i2=pcp$interactions[trueeffects[i],2]
			
			# both continuous variables
			if (pcp$vartypes[i1]=="cont" & pcp$vartypes[i2]=="cont") {

				xi1=pcp$intcont[[i]]$xi1
				xi2=pcp$intcont[[i]]$xi2
				fi=pcp$intcont[[i]]$fi
				
			
				if ("points"%in%names(pcp)) {# score system
					image(xi1,xi2,matrix(c(1:length(fi)),nrow = length(xi1), ncol = length(xi2), byrow = TRUE),axes=F,
							xlim=range(xi1),ylim=range(xi2),xlab = NA, ylab = NA,col=pcp$cols1[match(round(fi,2),round(pcp$allcolors,2))],useRaster=TRUE)
					
				} else { # not a score system
					image(xi1,xi2,matrix(fi,nrow = length(xi1), ncol = length(xi2), byrow = TRUE),axes=F,
							xlim=range(xi1),ylim=range(xi2),xlab = NA, ylab = NA,col=pcp$cols1,useRaster=TRUE,zlim=pcp$frange)
				}
				
				axis(side = 1,lwd.ticks=0,padj=-1)
				axis(side = 2,lwd.ticks=0,padj=1)
			} 
			
			# both categorical variables
			if (pcp$vartypes[i1]=="cat" & pcp$vartypes[i2]=="cat") {
				
				xi1=pcp$intcont[[i]]$xi1
				xi2=pcp$intcont[[i]]$xi2
				fi=pcp$intcont[[i]]$fi
				
				if ("points"%in%names(pcp)) {# score system
					image(as.numeric(xi1),as.numeric(xi2),matrix(c(1:length(fi)),nrow = length(xi1), ncol = length(xi2), byrow = TRUE),axes=F,
							xlim=range(as.numeric(xi1))+c(-0.5,0.5),ylim=range(as.numeric(xi2))+c(-0.5,0.5),xlab = NA, ylab = NA,col=pcp$cols1[match(round(fi,2),round(pcp$allcolors,2))],useRaster=TRUE)
					
				} else { # not a score system
					image(as.numeric(xi1),as.numeric(xi2),matrix(fi,nrow = length(xi1), ncol = length(xi2), byrow = TRUE),axes=F,
							xlim=range(as.numeric(xi1))+c(-0.5,0.5),ylim=range(as.numeric(xi2))+c(-0.5,0.5),xlab = NA, ylab = NA,col=pcp$cols1,useRaster=TRUE,zlim=pcp$frange)
				}
				axis(side = 1,lwd.ticks=0,padj=-1,at=xi1,labels=pcp$levelnames[[1]][[i1]])
				axis(side = 2,lwd.ticks=0,padj=1,at=xi2,labels=pcp$levelnames[[1]][[i2]])
			}
			# first continuous variable, second cat. variable
			if (pcp$vartypes[i1]=="cont" & pcp$vartypes[i2]=="cat") {

				xi1=pcp$intcont[[i]]$xi1
				xi2=pcp$intcont[[i]]$xi2
				fi=pcp$intcont[[i]]$fi
				
				if ("points"%in%names(pcp)&!"glm"%in%names(pcp)) {# score system, no GLM model
					image(xi1,as.numeric(xi2),matrix(c(1:length(fi)),nrow = length(xi1), ncol = length(xi2), byrow = TRUE),axes=F,
							xlim=range(xi1),ylim=range(as.numeric(xi2))+c(-0.5,0.5),xlab = NA, ylab = NA,col=pcp$cols1[match(round(fi,2),round(pcp$allcolors,2))],useRaster=TRUE)
					
				} else { # not a score system
					image(xi1,as.numeric(xi2),matrix(fi,nrow = length(xi1), ncol = length(xi2), byrow = TRUE),axes=F,
							xlim=range(xi1),ylim=range(as.numeric(xi2))+c(-0.5,0.5),xlab = NA, ylab = NA,col=pcp$cols1,useRaster=TRUE,zlim=pcp$frange)
					
				}
				
				axis(side = 1,lwd.ticks=0,padj=-1)
				axis(side = 2,lwd.ticks=0,padj=1,at=xi2,labels=pcp$levelnames[[1]][[i2]])
			} 
			# first cat variable, second cont variable
			if (pcp$vartypes[i1]=="cat" & pcp$vartypes[i2]=="cont") {
				
				xi1=pcp$intcont[[i]]$xi1
				xi2=pcp$intcont[[i]]$xi2
				fi=pcp$intcont[[i]]$fi
				
				if ("points"%in%names(pcp)&!"glm"%in%names(pcp)) {# score system, no glm
					image(as.numeric(xi1),xi2,matrix(c(1:length(fi)),nrow = length(xi1), ncol = length(xi2), byrow = TRUE),axes=F,
							xlim=range(as.numeric(xi1))+c(-0.5,0.5),ylim=range(xi2),xlab = NA, ylab = NA,col=pcp$cols1[match(round(fi,2),round(pcp$allcolors,2))],useRaster=TRUE)					
				} else { # not a score system
					image(as.numeric(xi1),xi2,matrix(fi,nrow = length(xi1), ncol = length(xi2), byrow = TRUE),axes=F,
							xlim=range(as.numeric(xi1))+c(-0.5,0.5),ylim=range(xi2),xlab = NA, ylab = NA,col=pcp$cols1,useRaster=TRUE,zlim=pcp$frange)
				}
				axis(side = 1,lwd.ticks=0,padj=-1,at=xi1,labels=pcp$levelnames[[1]][[i1]])
				axis(side = 2,lwd.ticks=0,padj=1)
			} 
			box()
			
			mtext(side=1,pcp$names[i1],line=1.,font=2)
			mtext(side=2,pcp$names[i2],line=1.2,font=2)
			# add patient if necessary
			if ("obs"%in%names(pcp)) {
				if(pcp$coloroptions==4){
					points(pcp$obs[i1],pcp$obs[i2],pch=23, col="white",bg="white",cex=3)	
					points(pcp$obs[i1],pcp$obs[i2],pch=23, col="black",bg="black",cex=2)	
				}else{
					points(pcp$obs[i1],pcp$obs[i2],pch=23, col="white",bg="white",cex=3)	
					points(pcp$obs[i1],pcp$obs[i2],pch=23, col="black",bg="black",cex=2)	
				}
			}
			title(paste(pcp$names[i1],':',pcp$names[i2],sep=""))
			
		}
	}
}