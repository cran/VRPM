#' Contribution chart.
#' 
#' Display a graph explaining how the risk prediction of a new observation is obtained from the model.
#' 	All contributions to the linear predictor or latent variable are visualized and summed to obtain the linear predictor.  The latter is then 
#' 	transformed into a risk estimate. 
#' 
#' The contribution chart is a bar plot representing the contribution of each predictor or set of predictors to the score (translated linear predictor)
#' 	by means of bars.   Depending on the value of \code{zerolevel}, the visualized contributions are slightly different.  
#' 	If \code{zerolevel}="zero", the contribution for variable \eqn{x^p} is \eqn{\beta_pf_p(x^p)}, with \eqn{\beta_p} the model
#' 	coefficient corresponding to this predictor and \eqn{f_p(x^p)} a (possible) transformation of \eqn{x^p}.  If \code{zerolevel} is "min", "median" or "mean", a value equal to 
#' 	the minimum, median and mean of the contribution \eqn{\beta_pf_p(x^p)} in the training data, respectively, is substracted from the contribution.  See the references for more information.
#' 	The sum of all the contributions is the score (i.e. translated linear predictor or latent variable) which is transformed into the risk estimate.  
#' 	The range of all contributions in the training set are visualized by means of black horizontal lines. 
#' 
#' @inheritParams colplot
#' @param filename The name of the resulting file (default: cchart).
#' @param obs A data.frame containing the predictor values of the observation for which the chart should be made.
#' @param sorted logical.  If TRUE the contributions are sorted in increasing order (default=FALSE).
#' @author Vanya Van Belle
#' @aliases cchart.glm cchart.coxph cchart.default cchart.multinom cchart.ksvm
#' @export 
#' @seealso \code{\link{colplot}}, \code{\link{ccchart}}
#' @example /inst/examples/cchartex.R
#' @note This graph can not be used for cox proportional hazard regression including strata.
#' @note For \code{coxph} models, it is necessary to include \code{model=TRUE} in the model fit.
#' @note For \code{multinom} models, it is necessary to include \code{model=TRUE} in the model fit.
#' @note For \code{multinom} models, more than one output file is generated.  A first series of plots visualizes how the linear predictors are obtained.  The files
#' 	are named "filename_outcome_level_cchart", with "outcome_level" the name of the outcome level for which the linear predictor is visualized.  A second series of plots
#' 	visualizes how the linear predictors are transformed into a risk prediction for each outcome level.  The files are named "filename_p_outcome_level_cchart". 
#' @note For \code{multinom} models,  a vector of risk labels needs to be made and provided to the \code{cchart()} function.  See the examples for an illustration of the approach.
#' @note For \code{ksvm} models, it is necessary to include \code{prob.model=TRUE} in the model fit.
#' @note The plot is not shown in a graphical window but saved in the current working directory.
#' @note In case \code{zerolevel="min"} and \code{xmin} and \code{xmax} are provided by the user, it is possible to have negative contributions within the training data.
#' @references Van Belle V., Van Calster B., \emph{Visualizing risk prediction models}, PLoS ONE, 10(7):e0132614. doi:10.1371/journal.pone.0132614 (2015).
#' @references Van Belle V., Van Calster B., Suykens J.A.K., Van Huffel S. and Lisboa P., \emph{Explaining support vector machines: a color based nomogram}, Internal Report 16-27, ESAT-Stadius, KU Leuven (Leuven, Belgium), 2016
#' @references Van Belle V., Van Huffel S., Timmerman D., Froyman W., Bourne T. and Van Calster B., \emph{A color based nomogram for Multinomial Logistic Regression}, Internal Report 16-28, ESAT-Stadius, KU Leuven (Leuven, Belgium), 2016
cchart<-function(x, obs, filename, zerolevel="zero",
		risklabel,sorted=FALSE,time,xmin,xmax) UseMethod("cchart")

#' @export
cchart.glm<-function(x,  obs, filename, zerolevel="zero",
		risklabel="Predicted risk",sorted=FALSE,time,xmin,xmax) {
	x2<-glm2data(x,zerolevel,risklabel)
	obs<-adaptObs(x2,obs)
	
	cchart(x2, obs=obs, filename,zerolevel,
			risklabel=x2$risklabel,sorted,time,xmin,xmax)
}

#' @export
cchart.coxph<-function(x,  obs, filename, zerolevel="zero",
		risklabel="Predicted survival",sorted=FALSE,time,xmin,xmax) {
	x2<-coxph2data(x,zerolevel, risklabel,time=time)
	obs<-adaptObs(x2,obs)
	
	cchart(x2, obs=obs, filename, zerolevel,
			risklabel=x2$risklabel,sorted,time,xmin,xmax)
	
	
}

#' @export
cchart.default<-function(x, obs, filename, zerolevel="zero",
		risklabel="Predicted risk",sorted=FALSE,time,xmin,xmax) {
	
	
	x$sorted=sorted
	x$zerolevel=zerolevel
	x<-check_data(x,type="cchart")
	
	if (dim(x$x)[2]!=length(obs)) {
		stop("The dimension of the given data set does not match with the length of obs.  Please adapt in order to continue.")
	}
	
	x2=precolplot(x, filename, coloroptions=1, zerolevel=zerolevel,
			risklabel=risklabel,adverse=FALSE,obs=obs,xmin,xmax)
	
	f=x2$f
	fpatient=x2$fpatient
	fzero=x2$fzero
	d=x2$d
	n=x2$n
	thisrisk=x2$thisrisk
	
	
	devheight=ifelse(dim(f)[2]>2,0.8*dim(f)[2]+1,0.8*2.5+1)
	devwidth=10
	
	png(paste0(ifelse(hasArg(filename),filename,"cchart"),".png"),width=devwidth,height=devheight, units = "in",res=120)
	
	
	# maximal and minimal values in dataset
	maxdata=apply(fzero,2,max)
	mindata=apply(fzero,2,min)
	
	# expand the names with the names of the interaction effects
	# number of interactions
	d2=dim(f)[2]-d
	# position of interaction effects
	if (d2>0) {
		if (d2==1) {
			posint=which(max(f[,seq(d+1,d+d2,1)])-min(f[,seq(d+1,d+d2,1)])!=0)
			
		} else{
			posint=which(apply(f[,seq(d+1,d+d2,1)],2,max)-apply(f[,seq(d+1,d+d2,1)],2,min)!=0)
			
		}
		
		names2=rep('',times=d+d2)
		names2[1:d]=x$names
		
		
		for (i in seq(d+1,dim(f)[2])) {
			names2[i]=paste(x$names[x$interactions[i-d,1]],' : ', x$names[x$interactions[i-d,2]])
		}
		
		names2=names2[c(seq(1,d,by=1),posint+d)]
		fpatient=fpatient[c(seq(1,d,by=1),posint+d)]
		mindata=mindata[c(seq(1,d,by=1),posint+d)]
		maxdata=maxdata[c(seq(1,d,by=1),posint+d)]
		
	} else {
		names2=x$names
	}
	
	# sort the predictors
	if (x$sorted==TRUE) {
		xorder=order(fpatient)
	} else {
		xorder=seq(1,length(fpatient),1)
	}
	
	# reverse order of bars
	thisseq=rev(xorder)
	
	if (!class(fpatient)%in%c("vector","matrix")) {
		fpatient=as.matrix(fpatient)
	}
	par(mar=c(5+2,8+8,4,2)) # increase y-axis margin and x-axis margin
	temp=barplot(fpatient[thisseq],horiz=TRUE,xlim=range(fzero)+c(0.0,0.5), width=rep(10,length(fpatient)),axes=FALSE, names.arg=c(rep("",length(fpatient))))
	
	axis(side=2, at=temp, labels=names2[thisseq], las=2,lty="blank",font=2,cex.axis=1.5)
	axis(side=1,cex.axis=1.5)
	for (i in seq(along = seq(1,length(fpatient)))) {
		lines(c(mindata[thisseq[i]],maxdata[thisseq[i]]),cbind(temp[i],temp[i]),lwd=3,col="black")
		# main effects
		if (thisseq[i]<=d) {
			if (x$vartypes[thisseq[i]]=="cont") {
				text(max(0,fpatient[thisseq[i]]),temp[i]+diff(temp)[1]/5,round(obs[thisseq[i]],2),col="dodgerblue",pos=4, font=4)#,srt=20)
			} else {
				if ("glm"%in%names(x)) {
					if (class(x$x[,thisseq[i]])=="factor") {
						text(max(0,fpatient[thisseq[i]]),temp[i]+diff(temp)[1]/5,x$levelnames[[2]][[thisseq[i]]][as.integer(obs[thisseq[i]])],col="dodgerblue",pos=4, font=4)#,srt=20)
					} else {
						text(max(0,fpatient[thisseq[i]]),temp[i]+diff(temp)[1]/5,obs[thisseq[i]],col="dodgerblue",pos=4, font=4)#,srt=20)
						
					}
					
				} else {
					text(max(0,fpatient[thisseq[i]]),temp[i]+diff(temp)[1]/5,x$levelnames[[1]][[thisseq[i]]][[match(unlist(obs[thisseq[i]]),x$levelnames[[2]][[thisseq[i]]])]],col="dodgerblue",pos=4, font=4)#,srt=20)
					
				}
			}
		}
		# interaction effects are not labeled with the value of the variables
		
	}
	
	mtext(side=1,"Contributions",line=2.5,font=2,cex=1.5)
	if(!is.nan(thisrisk)){
		mtext(side=1,paste("Score =",round(sum(fpatient),3),"      ",paste(risklabel, "=",round(thisrisk,3))),line=4,font=2,cex=1.5)
	} else {
		mtext(side=1,paste("Score =",round(sum(fpatient),3)),line=4,font=2,cex=1.5)
		
	}
	
	garbage <-dev.off()
}

