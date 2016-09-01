# Project: visualizationEXTRA
# 
# Author: vvanbell
###############################################################################

getfuncformSVM<-function(x, obs=c()){
	
	subkernels=vector("list",length=x$d+x$d*(x$d-1)/2)
	
	if (length(obs)>0) {
		xnew=scaleData(obs,x$center,x$scale)		
	} else {
		xnew=scaleData(x$x,x$center,x$scale)
	}
	### RBF kernel
	# main effects
	f=c()
	pos=1
	for (i in seq(1,dim(x$x)[2])) {
		if (x$kernelname=="rbfkernel") {
			km=kernelMatrix(x$kernel, x$xSV[,i],xnew[,i])
			
		} else if (x$kernelname=="polykernel") {
			degree=as.numeric(x$kernel@kpar)[1]
			scale=as.numeric(x$kernel@kpar)[2]
			offset=as.numeric(x$kernel@kpar)[3]
			km=kernelMatrix(x$kernel, as.matrix(x$xSV[,i]),as.matrix(xnew[,i]))-offset^degree
		}
		subkernels[[pos]]$km=km
		f=cbind(f,as.vector((x$coef%*%km)))
		pos=pos+1
	}
	
	# calculate interaction effect between first and second input
	for (i in seq(1,dim(x$x)[2]-1)) {
		for (j in seq(i+1,dim(x$x)[2])) {
			if (x$kernelname=="rbfkernel") {
				
				if (length(obs)>0 && dim(obs)[1]==1) {
					km=kernelMatrix(x$kernel, x$xSV[,c(i,j)],as.matrix(xnew[,c(i,j)],ncol=2))-subkernels[[i]]$km-subkernels[[j]]$km
				} else {
					km=kernelMatrix(x$kernel, x$xSV[,c(i,j)],as.matrix(xnew[,c(i,j)]))-subkernels[[i]]$km-subkernels[[j]]$km
				}
				subkernels[[pos]]$km=km
				pos=pos+1
				f=cbind(f,as.vector((x$coef%*%km)))
			}
			else if (x$kernelname=="polykernel") {
				if (length(obs)>0 && dim(obs)[1]==1) {
					km=kernelMatrix(x$kernel, x$xSV[,c(i,j)],as.matrix(xnew[,c(i,j)],ncol=2))-subkernels[[i]]$km-subkernels[[j]]$km-offset^degree
				} else {
					km=kernelMatrix(x$kernel, x$xSV[,c(i,j)],as.matrix(xnew[,c(i,j)]))-subkernels[[i]]$km-subkernels[[j]]$km-offset^degree
				}
				subkernels[[pos]]$km=km
				pos=pos+1
				f=cbind(f,as.vector((x$coef%*%km)))
			}
		}
	}
	
	return<-f
}


# get functional forms after adapting them to correct for frest
adaptfuncformSVM<-function(x, obs=c(),corrind,intercept,slope){
	
	x2=x
		
	x2$f[,corrind]=x2$f[,corrind]+intercept+slope*x2$f[,corrind]
	temp=intercept+slope*x2$f_allx[,corrind]
	x2$f_allx[,corrind]=x2$f_allx[,corrind]+temp
	
	frest=x2$lpmodel-rowSums(x2$f_allx)+x2$ksvm@b
	x2$frest=frest
	x2$mfrest=median(frest)
	# calculate difference term after substraction of the median of this term
	x2$frest2=x2$frest-x2$mfrest
	
	if("myptest"%in%names(x)){
		temptest=intercept+slope*x2$ftest_allx[,corrind]
		x2$ftest_allx[,corrind]=x2$ftest_allx[,corrind]+temptest
	}
	
	# adapt plotting areas
	if(corrind<=x$d){# correlated with main effect
		x2$maincont$fsteps[,corrind]=x2$maincont$fsteps[,corrind]+intercept+slope*x2$maincont$fsteps[,corrind]
	} else { # correlated with interaction effect
		x2$intcont[[corrind-x$d]]$fi=x2$intcont[[corrind-x$d]]$fi+intercept+slope*x2$intcont[[corrind-x$d]]$fi
	}
	
	return<-x2
}

# getff and adapt for frest highly correlated with other term
getfuncformSVM2<-function(x, obs=c()){
	
	f=getfuncformSVM(x,obs)
	f[,x$indmaxcorr]=f[,x$indmaxcorr]+x$intercept+x$slope*f[,x$indmaxcorr]
	
	return<-f
}

getriskSVM<-function(index,x,thisindex=c()) {
	if (length(x$ksvm@prob.model[[1]])==2) {
		
		if (length(thisindex)==0) {
			latentvar=seq(min(index),max(index),length=100)
		}
		else { # only one index to convert
			latentvar=thisindex
		}
		if (x$kernelname=="rbfkernel") {
			latentvar2=latentvar-x$ksvm@b+x$mfrest
			
		} else if (x$kernelname=="polykernel") {
			degree=as.numeric(x$kernel@kpar)[1]
			scale=as.numeric(x$kernel@kpar)[2]
			offset=as.numeric(x$kernel@kpar)[3]
			latentvar2=latentvar-x$ksvm@b+x$mfrest+sum(x$coef)*offset^degree
			
		}
		
		riskestimate=1-1./(exp(-(x$ksvm@prob.model[[1]]$B+x$ksvm@prob.model[[1]]$A*latentvar2))+1);
		result<-list(latentvar,riskestimate)
		
	} else {
		result<-list(NaN,NaN)
		
	}
}


# function for scaling
scaleData<-function(mydata,center,scale){
	scaleddata=(mydata-matrix(1,nrow=dim(mydata)[1],ncol=1)%*%t(center))/(matrix(1,nrow=dim(mydata)[1],ncol=1)%*%t(scale))
	return<-scaleddata
}


#' @export
#' @importFrom kernlab rbfdot polydot predict kernelMatrix fitted
colplot.ksvm<-function(x,filename, coloroptions=2,zerolevel="zero",
		risklabel="Predicted risk",xmin,xmax,adverse=FALSE,obs,q5,q95,time){
	
	x2<-ksvm2data(x,zerolevel,risklabel,adverse)
	x2$model=as.data.frame(cbind(matrix(NaN,nrow=x2$n,ncol=1),x2$x))
	names(x2$model[,-1])
	
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
		xmin=as.numeric(adaptObs(x,xmin))
	}
	if(hasArg(xmax)){
		xmax=as.numeric(adaptObs(x,xmax))
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


ksvm2data <-function(x,zerolevel,risklabel="Predicted risk",adverse){
	
	if (!class(x)%in%c("ksvm")) {
		stop("x is not of the correct class (ksvm).")
	}
	
	if (!class(x@kernelf)%in%c("rbfkernel","polykernel")) {
		stop("This approach is only implemented for kernels of the classes 'rbfkernel' and 'polykernel'.")
	}
	
	if (x@type!="C-svc") {
		stop("This approach is (currently) only implemented for C-svc.")
	}
	
	if (x@nclass!=2) {
		stop("This approach is not (yet) implemented for multiclass classification.")
	}
	
	x2=list()
	x2$ksvm=x
	
	x2$names=colnames(x@xmatrix[[1]])
	
	x2$scale=x@scaling$x.scale$`scaled:scale`
	x2$center=x@scaling$x.scale$`scaled:center`
	
	x2$xSV=x@xmatrix[[1]] # these are the scaled (!!) training data for support vectors only!!
	x2$d=length(x2$names)
	
	x2$n=dim(x2$x)[1]
	x2$d=dim(x2$x)[2]
	x2$x=x@xmatrix[[1]]*(matrix(1,nrow=x2$n,ncol=1)%*%t(x2$scale))+matrix(1,nrow=x2$n,ncol=1)%*%t(x2$center) # these are the UNscaled (!!) training data for support vectors only!!
	
	x2$coeffs=x@coef[[1]] # this equal y_i*alpha_i for all support vectors
	x2$getriskestimate=getriskSVM
	x2$getfuncform=getfuncformSVM
	x2$kernelname=class(x@kernelf)[1]
	if (x2$kernelname=="rbfkernel") {
		x2$kernel=rbfdot(sigma=as.numeric(x@kernelf@kpar))
	} else if (x2$kernelname=="polykernel") {
		x2$kernel=polydot(degree=as.numeric(x@kernelf@kpar)[1],scale=as.numeric(x@kernelf@kpar)[2],offset=as.numeric(x@kernelf@kpar)[3])
	}
	else {
		stop("This approach is not (yet) implemented for kernels other than the rbf kernel")
	}
	
	x2$zerolevel=zerolevel
	x2$risklabel=risklabel
	if (hasArg(adverse)) {
		x2$adverse=adverse
	}
	
	vartypes=rep("cont",x2$d)
	indcat=as.numeric(which(attr(x@terms,"dataClasses")=="factor")-1)
	indlogic=as.numeric(which(attr(x@terms,"dataClasses")=="logical")-1)
	vartypes[indcat]="cat"
	vartypes[indlogic]="cat"
	
	# Indicate levels for categorical variables and the corresponding predictor values
	levels=(rep(list(""),x2$d))
	levelvalues=(rep(list(""),x2$d))
	
	for (i in seq(1,x2$d,1)) {
		if(length(unique(x2$x[,i]))==2 & vartypes[i]=='cont'){
			vartypes[i]="cat"
			levels[[i]]=as.character(sort(unique(x2$x[,i])))
			levelvalues[[i]]=sort(unique(x2$x[,i]))
		}
	}
	
	levelnames<-list(levels, levelvalues)
		
	# indicate the order of the interactions # for RBF kernel there are always interactions!!
	x2$interactions=t(combn(seq(1,x2$d,1),2))
	
	x2$vartypes=vartypes
	x2$levelnames=levelnames
	
	f=x2$getfuncform(x2)
	lpmodel=predict(x2$ksvm,x2$x,type="decision")
	frest=lpmodel-rowSums(f)+x2$ksvm@b
	x2$frest=frest
	x2$mfrest=median(frest)
	x2$f=f
	
	return<-x2
}

#' @export
cchart.ksvm<-function(x,  obs, filename, zerolevel="zero",
		risklabel="Predicted risk",sorted=FALSE,time,xmin,xmax) {
	x2<-ksvm2data(x,zerolevel,risklabel)
	
	cchart(x2, obs=obs, filename,zerolevel,
			risklabel=x2$risklabel,sorted,time,xmin,xmax)
}

#' @export
ccchart.ksvm<-function(x, obs, filename, zerolevel='zero',
		risklabel="Predicted risk",riskcutoff=0.1,type="logistic",sorted=FALSE,time,xmin,xmax) {
	x2<-ksvm2data(x,zerolevel,risklabel)
	
	ccchart(x2, obs=obs, filename,zerolevel,
			risklabel=x2$risklabel,riskcutoff,type=type,sorted,time,xmin,xmax)
}



#' Performance plots for the approximation of an SVM model.
#' 
#' Generate performance plots for the approximation of an SVM model.
#' 
#' Different types of plots are possible.  When \code{type}="all", all the options are generated.  When \code{type}="lp", 
#' the latent variables of the approximation and the original SVM model are plotted against eachother.  When \code{type}="probs"
#' the estimated probabilities of the approximation and the SVM model are plotted against eachother.  When \code{type}="outcomes" a bubble 
#' plot indicating the agreement between the approximation and the SVM model is generated.  When \code{type}="contributions", the range of
#' the contributions within the approximation, the range of the rest term and the range of the latent variable of the SVM model are represented
#' by means of boxplots.  All of these are shifted to have a median equal to zero. When \code{type}="ROC", ROC curves for the approximation and 
#' the SVM model are plotted.  When mytestdata is non-empty, ROC curves for the test set are also provided.
#' 
#' @param mymodel Element of class \code{ksvm}.
#' @param mydata Data on which \code{mymodel} was trained on.
#' @param indy Column number of the outcome in \code{mydata}.
#' @param mytestdata Data on which to evaluate \code{mymodel}.  (Optional)
#' @param type Type of performance plot (c="all","lp","probs","outcomes","contributions",
#' 		 "ROC","corrplot").  See details for more information.
#' @param filename Name of the resulting graph.
#' @import ggplot2
#' @example /inst/examples/plotperfex.R
#' @author Vanya Van Belle
#' @references Van Belle V., Van Calster B., Suykens J.A.K., Van Huffel S. and Lisboa P., \emph{Explaining support vector machines: a color based nomogram}, Internal Report 16-27, ESAT-Stadius, KU Leuven (Leuven, Belgium), 2016
#' @export
plotperf<-function(mymodel,mydata,indy,mytestdata,type="all",filename){

	if(!all(type%in%c("all","lp","probs","outcomes","contributions","ROC","corrplot"))){
		stop("Undefined type of plot.")
	}
		
	
	if (!class(mymodel)%in%c("ksvm","list")) {
		stop("Mymodel is not of the correct class.")
	}
	
	if (class(mymodel)=="ksvm") {
	mymodel=preplotperf(mymodel,mydata,indy,mytestdata,zerolevel="zero",risklabel="Estimated risk",adverse=FALSE)
	} 
	# plot latent variables (i.e linear predictor) for SVM and approximation
	if(any(type%in%c("all","lp"))){
		png(ifelse(hasArg(filename),paste0(filename,"_lps.png"),"lps.png"))
		plot(mymodel$mylp,mymodel$lpmodel,xlab="latent variable of approximation",ylab="latent variable of SVM",font.axis=2,cex.lab=1.5) # difference between my lp and correct lp-b-median(frest)
		lines(sort(mymodel$mylp),sort(mymodel$mylp))
		garbage<-dev.off()
	}
	
	
	# plot estimated probabilities for SVM and approximation
	if(any(type%in%c("all","probs"))){
		png(ifelse(hasArg(filename),paste0(filename,"_probs.png"),"probs.png"))
		plot(mymodel$myp,mymodel$pmodel[,2],xlab="estimated probability of approximation",ylab="estimated probability of SVM",font.axis=2,cex.lab=1.5) # difference between my lp and correct lp-b-median(frest)
		lines(sort(mymodel$myp),sort(mymodel$myp))
		garbage<-dev.off()
	}
	
	# bubble plot showing (dis)agreement of SVM and approximation predicted outcomes
	if(any(type%in%c("all","outcomes"))){
		png(ifelse(hasArg(filename),paste0(filename,"_outcomes.png"),"outcomes.png"))
		symbols(x=c(0,1,0,1),y=c(0,0,1,1),circles=c(mymodel$predtab),xlab="outcome by model",ylab="outcome by approximation",font.axis=2,cex.lab=1.5)
		text(0,0,mymodel$predtab[1,1])
		text(0,1,mymodel$predtab[1,2])
		text(1,0,mymodel$predtab[2,1])
		text(1,1,mymodel$predtab[2,2])
		garbage<-dev.off()
	}
	
	# plot the contributions of the approximation.  They are shifted such that all means are zero and boxplots align at the median.
	if(any(type%in%c("all","contributions"))){
		
		png(ifelse(hasArg(filename),paste0(filename,"_contributions2.png"),"contributions2.png"))
		n=max(nchar(mymodel$ffnames))
		oldpar=par()
		par(mar=c(4,n/2,2,2))
		boxplot(cbind(mymodel$fallmedian,mymodel$lpmodel-median(mymodel$lpmodel)),horizontal=TRUE,yaxt="n")
		axis(2,labels=c(mymodel$ffnames,"lpmodel"),at=seq(1,dim(mymodel$fallmedian)[2]+1,1),las=2,font=2,cex.lab=1.5)
		axis(1,font=2,cex.lab=1.5)
		mtext("value of shifted contribution",side=1,line=2.5,font=2,cex=1.5)
		garbage<-dev.off()
	}
	
	# ROC plot for SVM and approximation
	if(any(type%in%c("all","ROC"))){
		png(ifelse(hasArg(filename),paste0(filename,"_ROC.png"),"ROC.png"))
		par(mar=c(4,4,2,2))
		plot(mymodel$perfSVMtr,font.axis=2,xlab="",ylab="")
		plot(mymodel$myperfSVMtr,add=TRUE,lwd=2,xlab="",ylab="")
		if(hasArg("mytestdata")){
			plot(mymodel$perfSVMte,add=TRUE,font.axis=2,xlab="",ylab="",col="red")
			plot(mymodel$myperfSVMte,add=TRUE,lwd=2,xlab="",ylab="",col="red")
			legend("bottomright",legend=c("SVM train","approx. train","SVM test","approx. test"),col=c("black","black","red","red"),lwd=c(1,2,1,2))
		} else {
			legend("bottomright",legend=c("SVM train","approx. train"),col=c("black","black"),lwd=c(1,2))
		}
		mtext("false positive rate",side=1,line=2.5,font=2,cex=1.5)
		mtext("true positive rate",side=2,line=2.5,font=2,cex=1.5)
		garbage<-dev.off()
	}
	
	# plot correlation between terms in the approximation and the rest term
	if(any(type%in%c("all","corrplot"))){
		value=NaN
		frest=NaN
		png(ifelse(hasArg(filename),paste0(filename,"_corrplot.png"),"corrplot.png"))
		d=ggplot(as.data.frame(mymodel$tempdata), aes(value, frest)) +
				geom_point() + theme(aspect.ratio = 1) + facet_wrap(~ var)+
				xlab("contribution")+
				ylab("rest term")+
				theme(axis.title = element_text(face="bold", size=15),axis.text = element_text(face="bold", size=10))
		plot(d)
		garbage<-dev.off()
		remove(value)
		remove(frest)
	}
	
	# plot the functional forms of the main contributions in the approximation
	if(any(type%in%c("all","ff"))){
		png(ifelse(hasArg(filename),paste0(filename,"_ff.png"),"ff.png"))
		d=dim(mydata)[2]-1
		tempdata=mydata[,-which(names(mydata)=="y")]
		par(mfrow=c(ceiling(sqrt(d)),ceiling(d/sqrt(d))))
		for (i in seq(1,d,1)) {
			plot(tempdata[,i],mymodel$f_allx[,i],xlab=names(tempdata)[i],ylab="contribution")
		}
		garbage<-dev.off()
	}
	
	# plot conversion form latent var to change, for SVM and approximation
	if(any(type%in%c("all","links"))){
		png(ifelse(hasArg(filename),paste0(filename,"_links.png"),"links.png"))
		plot(mymodel$mylp,mymodel$myp,xlab="latent variable",ylab="estimated chance",font.axis=2,cex.lab=1.5)
		# link function of the model, corrected for constants
		points(mymodel$lpmodel,mymodel$pmodel[,2],col='red')
		grid()
		legend("bottomleft",legend=c("approx.","SVM"),col=c("black","red"),pch=c(1,1))
		garbage<-dev.off()
	}
}




#' Preprocess a \code{ksvm} object
#' 
#' Performs necessary preprocessing of a \code{ksvm} object when the plots should be generated based on all training data and not only on the support vectors.
#' 
#'  Depending on the value of \code{zerolevel}, the visualized contributions are slightly different.  
#' 	If \code{zerolevel}="zero", the contribution for variable \eqn{x^p} is \eqn{\beta_pf_p(x^p)}, with \eqn{\beta_p} the model
#' 	coefficient corresponding to this predictor and \eqn{f_p(x^p)} a (possible) transformation of \eqn{x^p}.  If \code{zerolevel} is "min", "median" or "mean", a value equal to 
#' 	the minimum, median and mean of the contribution \eqn{\beta_pf_p(x^p)} in the training data, respectively, is substracted from the contribution.  See the references for more information.
#' 
#' @inheritParams colplot
#' @inheritParams plotperf
#' @param model Object of class \code{ksvm}
#' @importFrom ROCR prediction performance
#' @return List object
#' @author Vanya Van Belle
#' @seealso \code{\link{colplot}}, \code{\link{cchart}}, \code{\link{ccchart}}
#' @references Van Belle V., Van Calster B., Suykens J.A.K., Van Huffel S. and Lisboa P., \emph{Explaining support vector machines: a color based nomogram}, Internal Report 16-27, ESAT-Stadius, KU Leuven (Leuven, Belgium), 2016
#' @export
preplotperf<-function(model,mydata,indy,mytestdata,zerolevel="zero",risklabel="Estimated risk",adverse=FALSE){
	#library("ROCR")
	
	if(names(mydata)[indy]!="y"){
		names(mydata)[indy]="y"
	}
	
	if (!"done"%in%names(model)) {
		#####################
		# SVM dependent
		tempmodel=ksvm2data(model,zerolevel=zerolevel,risklabel=risklabel,adverse=adverse)
		# adapt x to include all training data, not only the support vectors
		tempmodel$x=as.matrix(mydata[,-indy]) 
		tempmodel$n=dim(tempmodel$x)[1] # adapt for frest
		# calculate latent var of SVM model
		tempmodel$lpmodel=predict(tempmodel$ksvm,tempmodel$x,type="decision")
		# calculate the estimated probability of the SVM model
		tempmodel$pmodel=predict(model,mydata[,-indy],type="probabilities")
		
		if(hasArg("mytestdata")){
			xtest=as.matrix(mytestdata[,-indy])
			
			tempmodel$lpmodeltest=predict(tempmodel$ksvm,xtest,type="decision")
			tempmodel$pmodeltest=predict(model,xtest,type="probabilities")	
		}
		
		# calculate predicted outcome by SVM model on training data
		if(all(predict(model,mydata,type="response")==as.numeric(tempmodel$lpmodel>0))){
			tempmodel$ypred=as.numeric(tempmodel$lpmodel>0)
			if(hasArg("mytestdata")){
				tempmodel$ypredtest=as.numeric(tempmodel$lpmodeltest>0)			
				# calculate accuracy of the SVM model on the training data
				tempmodel$acctestSVM=round(sum(tempmodel$ypredtest==mytestdata$y)/dim(mytestdata)[1],2)
			}
			
		} else {
			tempmodel$ypred=as.numeric(tempmodel$lpmodel<0)		
			if(hasArg("mytestdata")){
				tempmodel$ypredtest=as.numeric(tempmodel$lpmodeltest<0)	
				# calculate accuracy of the SVM model on the training data
				tempmodel$acctestSVM=round(sum(tempmodel$ypredtest==mytestdata$y)/dim(mytestdata)[1],2)
			}
		}
		
		# calculate accuracy of the SVM model on the training data
		tempmodel$acctrainSVM=round(sum(tempmodel$ypred==mydata$y)/dim(mydata)[1],2)
		
		# roc curve for SVM model
		tempmodel$predSVMtr <- prediction(tempmodel$lpmodel, mydata$y)
		tempmodel$perfSVMtr <- performance(tempmodel$predSVMtr, measure = "tpr", x.measure = "fpr") 
		
		# roc curves for test set
		if(hasArg("mytestdata")){
			# roc curve for SVM model
			tempmodel$predSVMte <- prediction(tempmodel$lpmodeltest, mytestdata$y)
			tempmodel$perfSVMte <- performance(tempmodel$predSVMte, measure = "tpr", x.measure = "fpr") 
		}
		
		##########################
		# approx dependent
		f=tempmodel$getfuncform(tempmodel)
		tempmodel$f_allx=f
		# calculate difference term
		frest=tempmodel$lpmodel-rowSums(f)+tempmodel$ksvm@b
		tempmodel$frest=frest
		tempmodel$mfrest=median(frest)
		# calculate difference term after substraction of the median of this term
		tempmodel$frest2=tempmodel$frest-tempmodel$mfrest
		
		if(hasArg("mytestdata")){
			ftest=tempmodel$getfuncform(tempmodel,xtest)
			tempmodel$ftest_allx=ftest
		}
	} else {
		tempmodel=model
	}
	
	
	
	##########################
	# approx dependent

	# calculate the estimated probability of the approximation
	tempmodel$myp=getriskSVM(rowSums(tempmodel$f_allx),tempmodel,rowSums(tempmodel$f_allx))[[2]] # my calculated risk
	# calculate the latent var of the approximation (take the constant and the median of the rest term into account)
	tempmodel$mylp=rowSums(tempmodel$f_allx)-tempmodel$ksvm@b+tempmodel$mfrest # -> scores as calculated from the plot + constant + median of frest
	
	if(hasArg("mytestdata")){
		tempmodel$mylptest=rowSums(tempmodel$ftest_allx)-tempmodel$ksvm@b+tempmodel$mfrest
		tempmodel$myptest=getriskSVM(rowSums(tempmodel$f_allx),tempmodel,rowSums(tempmodel$ftest_allx))[[2]] # my calculated risk
	}
	
	# calculate predicted outcome by SVM model on training data
	if(all(tempmodel$ypred==as.numeric(tempmodel$lpmodel>0))){
		# calculate predicted outcome by approximation on training data
		tempmodel$thisypred=as.numeric(tempmodel$mylp>0)
		if(hasArg("mytestdata")){
			# calculate predicted outcome by approximation on training data
			tempmodel$thisypredtest=as.numeric(tempmodel$mylptest>0)
			tempmodel$myaccwrtSVMtest=sum(tempmodel$ypredtest==tempmodel$thisypredtest)/length(tempmodel$ypredtest)
			# calculate accuracy of the approximation on the training data
			tempmodel$acctestAPPROX=round(sum(tempmodel$thisypredtest==mytestdata$y)/dim(mytestdata)[1],2) 
		}
		
	} else {
		# calculate predicted outcome by approximation on training data
		tempmodel$thisypred=as.numeric(tempmodel$mylp<0)	
		if(hasArg("mytestdata")){
			# calculate predicted outcome by approximation on training data
			tempmodel$thisypredtest=as.numeric(tempmodel$mylptest<0)
			tempmodel$myaccwrtSVMtest=sum(tempmodel$ypredtest==tempmodel$thisypredtest)/length(tempmodel$ypredtest)
			# calculate accuracy of the approximation on the training data
			tempmodel$acctestAPPROX=round(sum(tempmodel$thisypredtest==mytestdata$y)/dim(mytestdata)[1],2) 
		}
	}
	
	# cross table of predicted outcomes for SVM model and approximation
	tempmodel$predtab=table(as.factor(c(0,1,tempmodel$ypred))[-c(1,2)],as.factor(c(0,1,tempmodel$thisypred))[-c(1,2)])
	
	# calculate all contributions and substract the median of all: this results in boxplots that are aligned at zero=median
	tempmodel$fall=cbind(tempmodel$f_allx,tempmodel$frest2)#-matrix(1,nrow=tempmodel$n,ncol=1)%*%fmin
	tempnames=names(mydata[,-indy]) #c(1:tempmodel$d)
	ffnames=c(tempnames,paste0(tempnames[tempmodel$interactions[,1]],",",
					tempnames[tempmodel$interactions[,2]]),"rest")
	colnames(tempmodel$fall)=ffnames
	cnames <- rep("f",length(ffnames))
	legend_expressions <- 
			sapply(1:length(ffnames), function(i) {
						as.expression(substitute(A (B), 
										list(A = as.name(cnames[i]), B = as.name(ffnames[i]))))
					})
	tempmodel$ffnames=legend_expressions
	tempmodel$fallzero=tempmodel$fall-matrix(1,nrow=dim(tempmodel$fall)[1],ncol=1)%*%apply(tempmodel$fall,2,min)
	tempmodel$fallmedian=tempmodel$fall-matrix(1,nrow=dim(tempmodel$fall)[1],ncol=1)%*%apply(tempmodel$fall,2,median)
	
	# calculate accuracy of the approximation on the training data
	tempmodel$acctrainAPPROX=round(sum(tempmodel$thisypred==mydata$y)/dim(mydata)[1],2) 
	# calculate the accuracy of the approximation w.r.t. the SVM model
	tempmodel$myaccwrtSVMtrain=round(sum(tempmodel$ypred==tempmodel$thisypred)/length(tempmodel$ypred),2)
	
	# roc curve for approximation
	tempmodel$mypredSVMtr <- prediction(tempmodel$mylp, mydata$y)
	tempmodel$myperfSVMtr <- performance(tempmodel$mypredSVMtr, measure = "tpr", x.measure = "fpr") 
	
	# roc curves for test set
	if(hasArg("mytestdata")){
		# roc curve for approximation
		tempmodel$mypredSVMte <- prediction(tempmodel$mylptest, mytestdata$y)
		tempmodel$myperfSVMte <- performance(tempmodel$mypredSVMte, measure = "tpr", x.measure = "fpr") 
	}
	
	# prepare to plot correlations between terms and frest
	tempdata=data.frame(value=1,frest=1,var="")
	for (i in seq(1,dim(tempmodel$fall)[2]-1)) {
		tempdata=rbind(tempdata,data.frame(value=tempmodel$fall[,i],frest=tempmodel$fall[,dim(tempmodel$fall)[2]],var=matrix(rep(ffnames[i],dim(tempmodel$fall)[1]),ncol=1)))
	}
	tempmodel$tempdata=tempdata[-1,]
	
	
	tempmodel
}
