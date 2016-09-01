#' Cumulative contribution chart.
#' 
#' Display a graph explaining how the risk prediction for a new observation is obtained from the risk prediction model.
#' 	All contributions are added in a cumulative way to end up with the linear predictor that is transformed into a risk 
#' 	estimate.   
#' 
#'  The cumulative contribution chart constitutes from different bars, representing the contribution of the predictors to the
#' 	score (translated linear predictor) in a cumulative way. Depending on the value of \code{zerolevel}, the visualized contributions are slightly different.  
#' 	If \code{zerolevel}="zero", the contribution for variable \eqn{x^p} is \eqn{\beta_pf_p(x^p)}, with \eqn{\beta_p} the model
#' 	coefficient corresponding to this predictor and \eqn{f_p(x^p)} a (possible) transformation of \eqn{x^p}.  If \code{zerolevel} is "min", "median" or "mean", a value equal to 
#' 	the minimum, median and mean of the contribution \eqn{\beta_pf_p(x^p)} in the training data, respectively, is substracted from the contribution.  See the references for more information.
#' 	At the bottom of the chart, the score (i.e. the translated linear predictor or latant variable) of this observation is reported next to the maximal score observed in the training
#' 	data.  The risk corresponding to both of these scores are represented as well.  Risk predictions above the riskcutoff result in red 
#' 	bars, risk predictions below this cutoff result in green bars. 
#' 

#' @inheritParams colplot
#' @param filename The name of the resulting file (default: ccchart).
#' @param obs A data.frame containing the predictor values of the observation for which the chart should be made.
#' @param riskcutoff A value between 0 and 1, indicating the change in color for the represented score and risk bars (default: 0.1).   A risk lower than the riskcutoff is visualized in green, a risk higher than the riskcutoff is visualized in red.
#' @param type A string specifying the type of plot: "logistic" for logistic regression models, and "survival" for cox proportional hazard models.  This should generally not be provided by the user.
#' @param sorted logical.  If TRUE the contributions are sorted in increasing order (default=FALSE).
#' @author Vanya Van Belle
#' @aliases ccchart.glm ccchart.coxph ccchart.default ccchart.multinom ccchart.ksvm
#' @export
#' @seealso \code{\link{colplot}}, \code{\link{cchart}}
#' @example /inst/examples/ccchartex.R
#' @note This graph can not be used for cox proportional hazard regression including strata.
#' @note For \code{coxph} models, it is necessary to include \code{model=TRUE} in the model fit.
#' @note For \code{multinom} models, it is necessary to include \code{model=TRUE} in the model fit.
#' @note For \code{multinom} models, more than one output file is generated.  A first series of plots visualizes how the linear predictors are obtained.  The files
#' 	are named "filename_outcome_level_cchart", with "outcome_level" the name of the outcome level for which the linear predictor is visualized.  A second series of plots
#' 	visualizes how the linear predictors are transformed into a risk prediction for each outcome level.  The files are named "filename_p_outcome_level_cchart". 
#' @note For \code{multinom} models,  a vector of risk labels needs to be made and provided to the \code{ccchart()} function.  See the examples for an illustration of the approach.
#' @note For \code{ksvm} models, it is necessary to include \code{prob.model=TRUE} in the model fit.
#' @note The plot is not shown in a graphical window but saved in the current working directory.
#' @references Van Belle V., Van Calster B., \emph{Visualizing risk prediction models}, PLoS ONE, 10(7):e0132614. doi:10.1371/journal.pone.0132614 (2015).
#' @references Van Belle V., Van Calster B., Suykens J.A.K., Van Huffel S. and Lisboa P., \emph{Explaining support vector machines: a color based nomogram}, Internal Report 16-27, ESAT-Stadius, KU Leuven (Leuven, Belgium), 2016
#' @references Van Belle V., Van Huffel S., Timmerman D., Froyman W., Bourne T. and Van Calster B., \emph{A color based nomogram for Multinomial Logistic Regression}, Internal Report 16-28, ESAT-Stadius, KU Leuven (Leuven, Belgium), 2016
ccchart<-function(x, obs, filename, zerolevel='zero',
		risklabel,riskcutoff=0.1,type,sorted=FALSE,time,xmin,xmax) UseMethod("ccchart")

#' @export
ccchart.glm<-function(x, obs, filename, zerolevel='zero',
		risklabel="Predicted risk",riskcutoff=0.1,type="logistic",sorted=FALSE,time,xmin,xmax) {
	x2<-glm2data(x,zerolevel,risklabel)
	obs<-adaptObs(x2,obs)
	if(hasArg(xmin)){
		xmin=as.numeric(adaptObs(x,xmin))
	}
	if(hasArg(xmax)){
		xmax=as.numeric(adaptObs(x,xmax))
	}
	
	ccchart(x2, obs=obs, filename,zerolevel,
			risklabel=x2$risklabel,riskcutoff,type=type,sorted,time,xmin,xmax)
}

#' @export
ccchart.coxph<-function(x, obs, filename,zerolevel='zero',
		risklabel="Survival",riskcutoff=0.1,type="survival",sorted=FALSE,time,xmin,xmax) {
	x2<-coxph2data(x,zerolevel,risklabel,time=time)
	
	obs<-adaptObs(x2,obs)
	if(hasArg(xmin)){
		xmin=as.numeric(adaptObs(x,xmin))
	}
	if(hasArg(xmax)){
		xmax=as.numeric(adaptObs(x,xmax))
	}
	
	ccchart(x2, obs, filename, zerolevel, risklabel=x2$risklabel,riskcutoff,type,sorted,time,xmin,xmax)
}

#' @export
ccchart.default<-function(x, obs, filename,zerolevel='zero',
		risklabel="Estimated risk",riskcutoff=0.1,type="logistic",sorted=FALSE,time,xmin,xmax) {
	
	x$sorted=sorted
	x$zerolevel=zerolevel
	
	x<-check_data(x,type="ccchart")
	
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
	
	devheight=dim(f)[2]+2
	devwidth=10
	
	png(paste0(ifelse(hasArg(filename),filename,"ccchart"),".png"),width=devwidth,height=devheight, units = "in",res=120)
	
	
	if(!class(fpatient)%in%c("vector","matrix")){
		fpatient=as.matrix(fpatient)
	}
	
	# sort the predictors
	if (x$sorted==TRUE) {
		xorder=order(fpatient)
	} else {
		xorder=seq(1,length(fpatient),1)
	}
	
	# maximal values in dataset
	maxdata=apply(fzero,2,max)
	
	# define maximal score in data set
	maxscore=max(apply(f,1,sum)) # we need f to calculate risk
	minscore=min(apply(f,1,sum)) # we need f to calculate risk
	
	allrisks<-x$getriskestimate(rowSums(f),x,rowSums(f))[[2]]
	
	maxrisk=max(allrisks)
	minrisk=min(allrisks)
	
	
	# the represented score takes zerolevel into account
	maxscore=max(apply(fzero,1,sum))
	minscore=min(apply(fzero,1,sum))
	
	if(!is.nan(minrisk) ){
		if (type=="survival") {
			#if (type=="survival" | minrisk>maxrisk) {
			maxscore=max(apply(f,1,sum)) # we need f to calculate risk
			minscore=min(apply(f,1,sum)) # we need f to calculate risk
			maxrisk<-x$getriskestimate(rowSums(f),x,maxscore)[[2]][1]
			minrisk<-x$getriskestimate(rowSums(f),x,minscore)[[2]][1]
			
			minrisk=1-minrisk
			maxrisk=1-maxrisk
			thisrisk=1-thisrisk
			risklabel=paste0("1-",risklabel)
		} 
	}
	
	
	
	# indicate risk color
	if(!is.nan(thisrisk)){
		if (thisrisk<riskcutoff) {
			riskcolor="green"
		} else {
			riskcolor="red"
		}
	} else {
		riskcolor="orange"
	}
	
	
	# expand the names with the names of the interaction effects
	# number of interactions
	d2=dim(f)[2]-d
	
	if (d2>0) {
		# position of interaction effects
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
		fzero=fzero[c(seq(1,d,by=1),posint+d)]
		
		# sort the predictors
		if (x$sorted==TRUE) {
			xorder=order(fpatient)
		} else {
			xorder=seq(1,length(fpatient),1)
		}		
		
		
	} else {
		names2=x$names
	}
	
	# reverse order of bars
	thisseq=c(length(fpatient):1)
	
	par(mar=c(5+2,8+8,4,2)) # increase y-axis margin and x-axis margin
	cumsumfpatient=cumsum(fpatient[xorder])
	
	# preprare bars for plotting
	plotgrey=numeric(length=length(fpatient))
	plotblack=numeric(length=length(fpatient))
	plotgrey2=numeric(length=length(fpatient))
	plotblack2=numeric(length=length(fpatient))
	plotwhite=numeric(length=length(fpatient))
	
	# for first entry
	i=1
	if (fpatient[xorder[i]]>0) {
		plotgrey[i]=cumsumfpatient[i]
	}
	if (fpatient[xorder[i]]<0) {
		plotblack[i]=cumsumfpatient[i]
	}
	
	for (i in seq(2,length(fpatient),1)) {
		if (cumsumfpatient[i]==0) {
			if (fpatient[xorder[i]]>0) {
				plotgrey[i]=cumsumfpatient[i-1]
			} 
			if (fpatient[xorder[i]]<0) {
				plotblack[i]=cumsumfpatient[i-1]
			} 
		}
		if (cumsumfpatient[i]>0) {
			if (fpatient[xorder[i]]>0) {
				plotgrey[i]=cumsumfpatient[i]
				plotwhite[i]=cumsumfpatient[i-1]
			} 
			if (fpatient[xorder[i]]<0) {
				plotblack[i]=cumsumfpatient[i-1]
				plotwhite[i]=cumsumfpatient[i]
			} 
		}
		if (cumsumfpatient[i]<0) {
			if (fpatient[xorder[i]]>0) {
				plotgrey[i]=cumsumfpatient[i-1]
				plotwhite[i]=cumsumfpatient[i]
			} 
			if (fpatient[xorder[i]]<0) {
				plotblack[i]=cumsumfpatient[i]
				plotwhite[i]=cumsumfpatient[i-1]
			} 
		}
	}
	
	# if cumsumfpatient changes sign, additional bars need to be drawn
	diffsign=c("FALSE",abs(diff(sign(cumsumfpatient)))==2)
	diffinds=which(diffsign=="TRUE")
	for (i in seq(along = diffinds)) {
		if (cumsumfpatient[diffinds[i]]>0 & fpatient[xorder[diffinds[i]]]>0) {
			plotgrey2[diffinds[i]]=cumsumfpatient[diffinds[i]-1]
			plotwhite[diffinds[i]]=0
		}
		if (cumsumfpatient[diffinds[i]]<0 & fpatient[xorder[diffinds[i]]]<0) {
			plotblack2[diffinds[i]]=cumsumfpatient[diffinds[i]-1]
			plotwhite[diffinds[i]]=0
		}
	}
	
	plotgrey=plotgrey[thisseq]
	plotblack=plotblack[thisseq]
	plotwhite=plotwhite[thisseq]
	plotgrey2=plotgrey2[thisseq]
	plotblack2=plotblack2[thisseq]
	
	yaxiscolor=rep("black",length(thisseq))
	yaxiscolor[which(cumsumfpatient<min(-0.5,minscore))]="grey"
	
	if(!is.nan(thisrisk)){
		# plot grey bars
		temp=barplot(c(min(0,cumsumfpatient), 0, min(0,cumsumfpatient), min(0,cumsumfpatient),plotgrey),horiz=TRUE,
				xlim=c(min(0,cumsumfpatient),max(0,maxscore)+0.5), 
				width=rep(10,length(fpatient)+4),axes=FALSE,space=0, border=NA, names.arg=c(rep("",length(fpatient)+4)))
		
		# plot grey bars and edges for risk bars
		barplot(c(maxscore, min(0,cumsumfpatient), maxscore, min(0,cumsumfpatient),plotgrey2),
				horiz=TRUE,xlim=c(min(0,cumsumfpatient),max(0,maxscore)+0.5), 	
				width=rep(10,length(fpatient)+1),axes=FALSE, 
				col=c("grey",riskcolor,"grey",riskcolor,rep("grey",length(fpatient))),add=TRUE,space=0, border=NA, names.arg=c(rep("",length(fpatient)+4)))
		
		if(maxscore>=0){
			barplot(c(maxscore, thisrisk/maxrisk*maxscore, maxscore, sum(fpatient),plotblack),
					horiz=TRUE,xlim=c(min(0,cumsumfpatient),max(0,maxscore)+0.5), 	
					width=rep(10,length(fpatient)+1),axes=FALSE, 
					col=c("grey",riskcolor,"grey",riskcolor,rep("black",length(fpatient))),add=TRUE,space=0, border=NA, names.arg=c(rep("",length(fpatient)+4)))
			
			barplot(c(maxscore, thisrisk/maxrisk*maxscore, maxscore, sum(fpatient),plotblack2),
					horiz=TRUE,xlim=c(min(0,cumsumfpatient),max(0,maxscore)+0.5), 	
					width=rep(10,length(fpatient)+1),axes=FALSE, 
					col=c("grey",riskcolor,"grey",riskcolor,rep("black",length(fpatient))),add=TRUE,space=0, border=NA, names.arg=c(rep("",length(fpatient)+4)))
			
			# plot white bars
			barplot(c(0, 0, 0,0,plotwhite),
					horiz=TRUE,xlim=c(min(0,cumsumfpatient),max(0,maxscore)+0.5), 	width=rep(10,length(fpatient)+1),axes=FALSE, 
					col=c("grey",riskcolor,"grey",riskcolor,rep("white",length(fpatient))),add=TRUE,space=0, border=NA, names.arg=c(rep("",length(fpatient)+4)))
			
		}
		
		if(maxscore<0){
			barplot(c(min(0,cumsumfpatient), thisrisk/maxrisk*maxscore, min(0,cumsumfpatient), sum(fpatient),plotblack),
					horiz=TRUE,xlim=c(min(0,cumsumfpatient),max(0,maxscore)+0.5), 	
					width=rep(10,length(fpatient)+1),axes=FALSE, 
					col=c("grey",riskcolor,"grey",riskcolor,rep("black",length(fpatient))),add=TRUE,space=0, border=NA, names.arg=c(rep("",length(fpatient)+4)))
			
			barplot(c(min(0,cumsumfpatient), thisrisk/maxrisk*maxscore, min(0,cumsumfpatient), sum(fpatient),plotblack2),
					horiz=TRUE,xlim=c(min(0,cumsumfpatient),max(0,maxscore)+0.5), 	
					width=rep(10,length(fpatient)+1),axes=FALSE, 
					col=c("grey",riskcolor,"grey",riskcolor,rep("black",length(fpatient))),add=TRUE,space=0, border=NA, names.arg=c(rep("",length(fpatient)+4)))
			
			# plot white bars
			barplot(c(maxscore, min(0,cumsumfpatient)+(thisrisk)/maxrisk*abs(maxscore-min(0,cumsumfpatient)), maxscore,0,plotwhite),
					horiz=TRUE,xlim=c(min(0,cumsumfpatient),max(0,maxscore)+0.5), 	width=rep(10,length(fpatient)+1),axes=FALSE, 
					col=c("white","white","white","white",rep("white",length(fpatient))),add=TRUE,space=0, border=NA, names.arg=c(rep("",length(fpatient)+4)))
			
		}
		
		# chunk score bar for this patient
		if (sum(fpatient)<0) {
			barplot(c(0, 0, 0, sum(fpatient),numeric(length=length(plotwhite))),
					horiz=TRUE,xlim=c(min(0,cumsumfpatient),max(0,maxscore)+0.5), 	width=rep(10,length(fpatient)+1),axes=FALSE, 
					col=c("grey","white","grey","white",rep("white",length(fpatient))),add=TRUE,space=0, border=NA, names.arg=c(rep("",length(fpatient)+4)))
		}
		
		# add zero line for contributions
		lines(c(0,0),c(temp[4],temp[length(temp)])+diff(temp)[1]/2,lty=2)
		
		if(nchar(risklabel)<20){
			axis(side=2, at=c(temp), labels=c("",risklabel,"","Score",names2[xorder[thisseq]]), las=2,lty="blank",font=2,cex.axis=1.5)			
		} else {
			axis(side=2, at=c(temp), labels=c("","Risk","","Score",names2[xorder[thisseq]]), las=2,lty="blank",font=2,cex.axis=1.5)
			mtext(paste("Risk equals",risklabel),col="black",font=2,side=1,line=2,adj=0)
		}
		
		# add x-axis on top of the graph
		xrange=c(min(0,minscore),max(0,maxscore)+0.5)
		xrange[1]=ceiling(xrange[1])
		xrange[2]=floor(xrange[2])
		if("colplotpref"%in%names(x)){
			axis(side=3)
			
		}else{
			axis(side=3,at=seq(xrange[1],xrange[2],1))
			
		}
		
		# add line for y-axis
		lines(c(min(0,cumsumfpatient),min(0,cumsumfpatient)),c(temp[1]-diff(temp)[1]/2,temp[length(temp)]+diff(temp)[1]/2),lty=1)
		
		for (i in seq(along = seq(1,length(fpatient)))) {
			color="dodgerblue"
			if (xorder[i]<=d) {
				# add predictor values or levels for main effects
				if (x$vartypes[xorder[i]]=="cont") {
					# predictor level/value
					text(min(0,cumsumfpatient),temp[thisseq[i]+4],round(obs[xorder[i]],2),col=color,pos=4, font=4,srt=20)
				} else {
					# predictor level/value
					if ("glm"%in%names(x)) {
						if (class(x$x[,i])=="factor") {
							text(min(0,cumsumfpatient),temp[thisseq[i]+4],x$levelnames[[2]][[xorder[i]]][as.integer(obs[xorder[i]])],col=color,pos=4, font=4,srt=20)
						} else {
							text(min(0,cumsumfpatient),temp[thisseq[i]+4],obs[xorder[i]],col=color,pos=4, font=4,srt=20)
						}
					} else {
						text(min(0,cumsumfpatient),temp[thisseq[i]+4],x$levelnames[[1]][[xorder[i]]][[match(unlist(obs[xorder[i]]),x$levelnames[[2]][[xorder[i]]])]],col=color,pos=4, font=4,srt=20)	
						
					}
				}
			}
			
			# add value of fpatient = contribution of each predictor to the score
			if(maxscore>=0){
				if (cumsumfpatient[i]>maxscore) {
					text(maxscore,temp[thisseq[i]+4]-3,round(fpatient[xorder[i]],2),col="black",pos=2, font=2)
					
				} else if (cumsumfpatient[i]<min(0,minscore)) {
					text(min(0,cumsumfpatient),temp[thisseq[i]+4]-3,round(fpatient[xorder[i]],2),col="grey",pos=4, font=2)
				} else {
					text(cumsumfpatient[i],temp[thisseq[i]+4]-3,round(fpatient[xorder[i]],2),col="grey",pos=4, font=2)
					
				}
			}
			else{
				if (cumsumfpatient[i]>maxscore) {
					text(fpatient[xorder[i]],temp[thisseq[i]+4]-3,round(fpatient[xorder[i]],2),col="black",pos=2, font=2)
					
				} else if (cumsumfpatient[i]<min(0,minscore)) {
					text(min(0,cumsumfpatient),temp[thisseq[i]+4]-3,round(fpatient[xorder[i]],2),col="grey",pos=4, font=2)
				} else {
					text(cumsumfpatient[i],temp[thisseq[i]+4]-3,round(fpatient[xorder[i]],2),col="grey",pos=4, font=2)
					
				}
			}
			
			# add line representing the water fall
			lines(c(cumsumfpatient[i],cumsumfpatient[i]),c(temp[thisseq[i]+4],temp[thisseq[i]+3])+diff(temp)[1]/2)
			lines(c(cumsumfpatient[i],cumsumfpatient[i+1]),c(temp[thisseq[i]+3],temp[thisseq[i]+3])+diff(temp)[1]/2)
			
		}
		
		# add value of the score for this patient
		# thisscore between min and max score
		if (sum(fpatient)<0.9*maxscore & sum(fpatient)>=minscore) {
			text(sum(fpatient),temp[4],round(sum(fpatient),2),col=riskcolor,font=2,pos=4)
		} # thiscore larger than maxscore
		else if(sum(fpatient)>=0.9*maxscore) {
			text(maxscore,temp[4],round(sum(fpatient),2),col="black",font=2,pos=2)
		} # thiscore smaller than minscore but larger than min(cumsumfpatient)
		else if(sum(fpatient)<minscore & sum(fpatient)>min(cumsumfpatient)) {
			text(sum(fpatient),temp[4],round(sum(fpatient),2),col=riskcolor,font=2,pos=4)
		} 
		else if(sum(fpatient)<=min(cumsumfpatient)) {
			text(min(0,cumsumfpatient),temp[4],round(sum(fpatient),2),col="black",font=2,pos=4)
		}
		
		# add value of the maximal score in this data set
		text(min(0,cumsumfpatient),temp[3],"max. score",col="black",font=3,pos=4)
		if(maxscore>=0){
			text(maxscore,temp[3],round(maxscore,2),col="black",font=2,pos=2)
			
		} else {

			text(min(0,cumsumfpatient),temp[3],substitute(bold(phantom("max. score      ")*a),list(a=round(maxscore,2))),col="black",font=2,pos=4)		
		}
		
		# add value of the risk for this patient
		if(maxscore>=0){
			if (sum(fpatient)<maxscore & sum(fpatient)>minscore) {
				#text((thisrisk-minrisk)/(maxrisk-minrisk)*(maxscore-minscore)+minscore,temp[2],round(thisrisk,2),col=riskcolor,font=2,pos=4)
				text(thisrisk/maxrisk*maxscore,temp[2],round(thisrisk,2),col=riskcolor,font=2,pos=4)
			} else if (sum(fpatient)>=maxscore) {
				text(maxscore,temp[2],round(thisrisk,2),col="black",font=2,pos=2)
			} # thiscore smaller than minscore but larger than min(cumsumfpatient)
			else if(sum(fpatient)<=minscore & sum(fpatient)>min(cumsumfpatient)) {
				text(thisrisk/maxrisk*maxscore,temp[2],round(thisrisk,2),col=riskcolor,font=2,pos=4)
			} else if (sum(fpatient)<=min(cumsumfpatient)) {
				text(min(0,cumsumfpatient),temp[2],round(thisrisk,2),col="black",font=2,pos=4)
				
			}
		}
		else{
			if (sum(fpatient)<maxscore & sum(fpatient)>minscore) {
				#text((thisrisk-minrisk)/(maxrisk-minrisk)*(maxscore-minscore)+minscore,temp[2],round(thisrisk,2),col=riskcolor,font=2,pos=4)
				text(	min(0,cumsumfpatient)+(thisrisk)/maxrisk*abs(maxscore-min(0,cumsumfpatient)),temp[2],round(thisrisk,2),col=riskcolor,font=2,pos=4)
			} else if (sum(fpatient)>=maxscore) {
				text(maxscore,temp[2],round(thisrisk,2),col="black",font=2,pos=2)
			} # thiscore smaller than minscore but larger than min(cumsumfpatient)
			else if(sum(fpatient)<=minscore & sum(fpatient)>min(cumsumfpatient)) {
				text(thisrisk/maxrisk*maxscore,temp[2],round(thisrisk,2),col=riskcolor,font=2,pos=4)
			} else if (sum(fpatient)<=min(cumsumfpatient)) {
				text(min(0,cumsumfpatient),temp[2],round(thisrisk,2),col="black",font=2,pos=4)
				
			}
			
		}
		
		
		# add value of the maximal risk in this data set
		text(min(0,cumsumfpatient),temp[1],"max. risk",col="black",font=3,pos=4)
		if(maxscore>=0){
			text(maxscore,temp[1],round(maxrisk,2),col="black",font=2,pos=2)
			
		} else {
			text(min(0,cumsumfpatient),temp[1],substitute(bold(phantom("max. risk      ")*a),list(a=round(maxrisk,2))),col="black",font=2,pos=4)		
			
		}
		
		
	} else {
		# plot grey bars
		temp=barplot(c(min(cumsumfpatient), min(cumsumfpatient),plotgrey),horiz=TRUE,
				xlim=c(min(0,cumsumfpatient),max(maxscore)+0.5), 
				width=rep(10,length(fpatient)+2),axes=FALSE,space=0, border=NA, names.arg=c(rep("",length(fpatient)+2)))
		
		# plot grey bars and edges for risk bars
		barplot(c( maxscore, min(cumsumfpatient),plotgrey2),
				horiz=TRUE,xlim=c(min(0,cumsumfpatient),max(maxscore)+0.5), 	
				width=rep(10,length(fpatient)-1),axes=FALSE, 
				col=c("grey",riskcolor,rep("grey",length(fpatient))),add=TRUE,space=0, border=NA, names.arg=c(rep("",length(fpatient)+2)))
		
		
		# plot black bars and risk bars
		barplot(c( maxscore, sum(fpatient),plotblack),
				horiz=TRUE,xlim=c(min(0,cumsumfpatient),max(maxscore)+0.5), 	
				width=rep(10,length(fpatient)+1),axes=FALSE, 
				col=c("grey",riskcolor,rep("black",length(fpatient))),add=TRUE,space=0, border=NA, names.arg=c(rep("",length(fpatient)+2)))
		
		barplot(c(maxscore, sum(fpatient),plotblack2),
				horiz=TRUE,xlim=c(min(0,cumsumfpatient),max(maxscore)+0.5), 	
				width=rep(10,length(fpatient)+1),axes=FALSE, 
				col=c("grey",riskcolor,rep("black",length(fpatient))),add=TRUE,space=0, border=NA, names.arg=c(rep("",length(fpatient)+2)))
		
		
		# plot white bars
		barplot(c(0,0,plotwhite),
				horiz=TRUE,xlim=c(min(0,cumsumfpatient),max(maxscore)+0.5), 	width=rep(10,length(fpatient)+1),axes=FALSE, 
				col=c(rep("white",length(fpatient))),add=TRUE,space=0, border=NA, names.arg=c(rep("",length(fpatient)+2)))
		
		# chunk score bar for this patient
		if (sum(fpatient)<0) {
			barplot(c(0, sum(fpatient),numeric(length=length(plotwhite))),
					horiz=TRUE,xlim=c(min(0,cumsumfpatient),max(maxscore)+0.5), 	width=rep(10,length(fpatient)+1),axes=FALSE, 
					col=c("grey","white",rep("white",length(fpatient))),add=TRUE,space=0, border=NA, names.arg=c(rep("",length(fpatient)+2)))
		}
		
		# add zero line for contributions
		lines(c(0,0),c(temp[2],temp[length(temp)])+diff(temp)[1]/2,lty=2)
		
		
		
		axis(side=2, at=c(temp), labels=c("","Score",names2[xorder[thisseq]]), las=2,lty="blank",font=2,cex.axis=1.5)
		
		# add x-axis on top of the graph
		xrange=c(min(0,cumsumfpatient),max(maxscore)+0.5)
		xrange[1]=ceiling(xrange[1])
		xrange[2]=floor(xrange[2])
		axis(side=3,at=seq(xrange[1],xrange[2],1))
		
		# add line for y-axis
		lines(c(min(0,cumsumfpatient),min(0,cumsumfpatient)),c(temp[1]-diff(temp)[1]/2,temp[length(temp)]+diff(temp)[1]/2),lty=1)
		
		for (i in seq(along = seq(1,length(fpatient)))) {
			color="dodgerblue"
			if (xorder[i]<=d) {
				# add predictor values or levels for main effects
				if (x$vartypes[xorder[i]]=="cont") {
					# predictor level/value
					text(min(0,cumsumfpatient),temp[thisseq[i]+2],round(obs[xorder[i]],2),col=color,pos=4, font=4,srt=20)
				} else {
					# predictor level/value
					if ("glm"%in%names(x)) {
						if (class(x$x[,i])=="factor") {
							text(min(0,cumsumfpatient),temp[thisseq[i]+2],x$levelnames[[2]][[xorder[i]]][as.integer(obs[xorder[i]])],col=color,pos=4, font=4,srt=20)
						} else {
							text(min(0,cumsumfpatient),temp[thisseq[i]+2],obs[xorder[i]],col=color,pos=4, font=4,srt=20)
						}
					} else {
						text(min(0,cumsumfpatient),temp[thisseq[i]+2],x$levelnames[[1]][[xorder[i]]][[match(unlist(obs[xorder[i]]),x$levelnames[[2]][[xorder[i]]])]],col=color,pos=4, font=4,srt=20)
						
					}
				}
			}
						
			# add value of fpatient = contribution of each predictor to the score
			if (cumsumfpatient[i]>maxscore) {
				text(maxscore,temp[thisseq[i]+2]-3,round(fpatient[xorder[i]],2),col="black",pos=2, font=2)
				
			} else if (cumsumfpatient[i]<min(0,cumsumfpatient)) {
				text(min(0,cumsumfpatient),temp[thisseq[i]+2]-3,round(fpatient[xorder[i]],2),col="grey",pos=4, font=2)
			} else {
				text(cumsumfpatient[i],temp[thisseq[i]+2]-3,round(fpatient[xorder[i]],2),col="grey",pos=4, font=2)
				
			}
			
			
			# add line representing the water fall
			lines(c(cumsumfpatient[i],cumsumfpatient[i]),c(temp[thisseq[i]+2],temp[thisseq[i]+1])+diff(temp)[1]/2)
			lines(c(cumsumfpatient[i],cumsumfpatient[i+1]),c(temp[thisseq[i]+1],temp[thisseq[i]+1])+diff(temp)[1]/2)
		}
		
		# add value of the score for this patient
		if (sum(fpatient)<0.9*maxscore & sum(fpatient)>=min(cumsumfpatient)) {
			text(sum(fpatient),temp[2],round(sum(fpatient),2),col=riskcolor,font=2,pos=4)
		} else if(sum(fpatient)>=0.9*maxscore){
			text(maxscore,temp[2],round(sum(fpatient),2),col="black",font=2,pos=2)
		} else if(sum(fpatient)<min(cumsumfpatient)){
			text(min(0,cumsumfpatient),temp[2],round(sum(fpatient),2),col="black",font=2,pos=2)
		}
		
		
		# add value of the maximal score in this data set
		text(min(0,cumsumfpatient),temp[1],"max. score",col="black",font=3,pos=4)
		text(maxscore,temp[1],round(maxscore,2),col="black",font=2,pos=2)
		
		# add label
		mtext(paste("Score equals",risklabel),col="black",font=2,side=1,line=2,adj=0)
		
	}
	
	# add legend for the colored text added to the plot
	# blue indicates the value of the predictor
	# grey indicates the contribution to the score of the predictor 	
	x0=ceiling(par("usr")[1])
	unit=(par("usr")[2]-par("usr")[1])/7.5
	
	
	mtext(expression(bold(phantom("legend of reported figures: "))*"blue - predictor value"),col=color,font=2,side=1,line=0,adj=0)
	mtext(expression(bold("legend of reported figures: ") * phantom("blue - predictor value")),col="black",font=2,side=1,line=0,adj=0)
	mtext(expression(bold(phantom("legend of reported figures: "))*"gray - contribution to score" ),col="grey",font=2,side=1,line=1,adj=0)
		
	for (i in seq(1,length(thisseq),1)) {
		if(!is.nan(thisrisk)){
			axis(side=2, at=c(temp[i+4]), labels=names2[xorder[thisseq]][i], las=2,lty="blank",font=2,cex.axis=1.5,col.axis=yaxiscolor[thisseq][i])			
			
		} else {
			axis(side=2, at=c(temp[i+2]), labels=names2[xorder[thisseq]][i], las=2,lty="blank",font=2,cex.axis=1.5,col.axis=yaxiscolor[thisseq][i])			
			
		}
	}
	
	garbage <-dev.off()
	
}


