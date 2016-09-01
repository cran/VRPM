getRiskGLM<-function(index,x,thisindex=c()){
	beta0=x$coeffs[1];
	
	if (length(thisindex)==0) {
		latentvar=seq(min(index),max(index),length=100)
	}
	else { # only one index to convert
		latentvar=thisindex
	}
	
	riskestimate=1./(exp(-(beta0+latentvar))+1);
	result<-list(latentvar,riskestimate)
}

getfuncformGLM<-function(x,obs=c()){
	
	if(length(obs)>0){
		
		ffterm=predict(x$glm,newdata=obs,type="terms") 
		
		if ("assign"%in%names(x)) {
			ffterm2=matrix(0,nrow=1,ncol=max(x$assign))
			
			for (i in seq(1,max(x$assign),1)) {
				ffterm2[i]=sum(ffterm[x$assign==i])
			}
			ffterm=ffterm2
		}
		
	} else {
		
		if ("mfp"%in%names(x)) {
			newdata=as.data.frame(x$x)
			ffterm=predict(x$glm,newdata=newdata,type="terms") 
			
			if ("assign"%in%names(x)) {
				ffterm2=matrix(0,nrow=dim(ffterm)[1],ncol=max(x$assign))
				
				for (i in seq(1,max(x$assign),1)) {
					if(sum(x$assign==i)>1){
						ffterm2[,i]=apply(ffterm[,x$assign==i],1,sum)
					} else {
						ffterm2[,i]=ffterm[,x$assign==i]
					}
				}
				ffterm=ffterm2
			}
			
		} else{
			newdata=as.data.frame(x$x)
			names(newdata)=attr(x$glm$terms,"term.labels")[which(attr(x$glm$terms,"order")==1)]
			ffterm=predict(x$glm,newdata=newdata,type="terms")  
		}
		
		
		
	}
	# check wheter there are interaction effects
	indint=which(attr(x$glm$terms,"order")==2)
	if(length(indint)>0){ 
		# set correct dimensions for ff
		if (length(obs)>0) {
			ff=matrix(0,nrow=dim(obs)[1],ncol=x$d+x$d2)
		} else {
			ff=matrix(0,nrow=dim(x$x)[1],ncol=x$d+x$d2)
		}
		# find main effects
		indmain=which(attr(x$glm$terms,"order")==1)
		# find position of interaction effects
		inteffects=matrix(0,ncol=2,nrow=length(indint))
		indfinal=matrix(0,nrow=x$d+length(indint),ncol=1)
		indfinal[1:x$d]=indmain
		for (i in seq(1,length(indint),1)) {
			inteffects[i,]=which(attr(x$glm$terms,"factors")[-1,indint[i]]==1)
			indfinal[x$d+i]=x$d+which(rowSums(abs(x$interactions-matrix(1,ncol=1,nrow=dim(x$interactions)[1])%*%inteffects[i,]))==0)
		}
		ff[,indfinal]=ffterm+matrix(1,nrow=dim(ffterm)[1],ncol=1)%*%x$glmconst
		
	} else { # no interactions
		ff=ffterm+matrix(1,nrow=dim(ffterm)[1],ncol=1)%*%x$glmconst
		
	}
	return(ff)
}

#' @importFrom methods hasArg 
#' @importFrom stats model.frame model.matrix terms
#' @importFrom utils read.table capture.output tail combn 
coxph2data<-function(x, zerolevel,risklabel="Predicted survival",adverse,time){
	
	
	x2=list()
	if ("mfp"%in%class(x)) {
		x2$mfp=x
		x2$x=x$X
		correctnames=unique(substr(colnames(x$x),1,nchar(colnames(x$x))-2))
		# adapt correctnames for logicals
		
		oldnames=colnames(x$X)
		xorder=match(correctnames,oldnames)
		newnames=oldnames[xorder]
		x2$names=newnames
		x2$x=x$X[,xorder]
		colnames(x2$x)=x2$names
		
		dataclasses=attr(x$fit$terms,"dataClasses")[-1]
		names(dataclasses)=c()
		
		indfactor=which(attr(x$fit$terms,"dataClasses")=="factor")
		factornames=names(indfactor)
		
		indfactor=indfactor-1
		
		newdata2=x2$x
		
		indtodelete=c()
		toadd=matrix(data=NaN,nrow=dim(x2$x)[1],ncol=length(indfactor))
		addindex=1
		namestoadd=c()
		#newnames=c()
		if (length(indfactor)>0) {
			for (i in seq(1,length(indfactor),1)) {

				indcurrentfactor=grep(factornames[i],x2$names,fixed=TRUE)
				indtodelete=c(indtodelete,indcurrentfactor)
				levels=strsplit(x2$names[indcurrentfactor],factornames[i],fixed=TRUE)
				indreflevel=seq(1,dim(x2$x)[1],1)
				levels2=c()
				for (j in seq(1,length(indcurrentfactor),1)) {
					levels2=c(levels2,levels[[j]][2])
					toadd[x2$x[,indcurrentfactor[j]]==1,addindex]=levels[[j]][2]
					indcurrentlevel=which(x2$x[,indcurrentfactor[j]]==1)
					indreflevel=indreflevel[-which(indreflevel%in%indcurrentlevel)]
				}
				indthisreflevel=which(names(x$fit$xlevels)==factornames[i])
				reflevel=x$fit$xlevels[indthisreflevel]
				names(reflevel)=c()
				reflevel=reflevel[[1]][-which(reflevel[[1]]%in%levels2)]
				toadd[indreflevel,addindex]=reflevel
				namestoadd=c(namestoadd,factornames[i])
				addindex=addindex+1
			}
			toadd=as.data.frame(toadd)	
			
			newdata2=cbind(newdata2[,-indtodelete],as.data.frame(toadd))
			currentnames=c(x2$names[-indtodelete],factornames)
		} else {
			currentnames=x2$names
		}
		
		# adapt currentnames for logical variables
		fitnames=names(attr(x$fit$terms,"dataClasses"))[-1]
		deletefitnames=which(sapply(strsplit(colnames(x$x), ".",fixed=TRUE), "[", 2)=="2")
		if (length(deletefitnames)>0) {
			fitnames=fitnames[-deletefitnames]
			dataclasses=dataclasses[-deletefitnames]
		}
		if (sum(dataclasses=="factor")>0) {
			indlogicals=which(dataclasses[-which(dataclasses=="factor")]=="logical")
		} else{
			indlogicals=which(dataclasses=="logical")
		}
		
		for (i in seq(along = indlogicals)) {
			tempind=which(pmatch(fitnames,currentnames[indlogicals[i]])==1)
			currentnames[indlogicals[i]]=fitnames[tempind]
		}
		xorder=numeric(length(currentnames))
		for (i in seq(1,length(currentnames),1)) {
			xorder[grep(currentnames[i],fitnames)]=i
		}		
		
		if (length(indfactor)>0) {
			newdataclasses=c(dataclasses[-which(dataclasses=="factor")],rep("factor",length(indfactor)))
			
		} else {
			newdataclasses=dataclasses
			
		}		
		# adapt classes
		newdata2=as.data.frame(newdata2)
		names(newdata2)[which(newdataclasses=="logical")]=fitnames[which(newdataclasses=="logical")]
		
		for (i in seq(1,dim(newdata2)[2]-length(indfactor))) {
			attributes(newdata2[,i])$class <-newdataclasses[i]
		}
		colnames(newdata2)=currentnames
		
		x2$x=newdata2[,xorder]
		
		x2$names=names(x2$x)
		
		indlogical=which(newdataclasses[xorder]=="logical")
		
		for (i in seq(along = indlogicals)) {
			x2$x[,indlogical[i]]=as.logical(x2$x[,indlogical[i]])
		}
		
		# make assign vector to indicate which ff-terms belong to which variable
		assignnames=names(x$fit$assign)
		assign=numeric(length(assignnames))
		for (i in seq(1,length(x2$names),1)) {
			assign[grep(x2$names[i],assignnames)]=i
		}
		x2$assign=assign
		x=x$fit
		mm=model.frame(x)
		mm=mm[,!duplicated(assign)]
		names(mm)[-1]=x2$names
		x2$model=mm
	} else{
		x2$x=x$model[,-1]
		x2$names=names(x2$x)
		x2$model=x$model
	}
	x2$y=x$y
	x2$n=dim(x2$x)[1]
	x2$d=dim(x2$x)[2]
	x2$coeffs=x$coefficients
	x2$getriskestimate=getRiskCPH
	x2$getfuncform=getfuncformCPH
	x2$glm=x
	
	x2$zerolevel=zerolevel
	x2$risklabel=risklabel
	if (hasArg(adverse)) {
		x2$adverse=adverse
		
	}
	
	if(!hasArg(time)){
		# if no time is indicated for the estimated survival, take the median survival (survival time at which 50% of the patients has relapse)
		s=survival::survfit(x)
		# test whether baseline survival at this time is 0.5
		x2$time=read.table(textConnection(capture.output(s)),skip=2,header=TRUE)$median
		if (!is.finite(x2$time)) { # if the median survival time can not be calculated, take the last event time
			x2$time=tail(s$time[s$n.event>0],1)
		}
	}else{
		x2$time=time
	}
	x2$risklabel=paste0(risklabel,"(t=",round(x2$time,2),")")
	
	# Indicate the type for each covariate
	if("assign"%in%names(x2)){
		unik <- !duplicated(x2$assign)  ## logical vector of unique values
		vartypes=rep("cont",x2$d)
		vartypes0=attr(x$terms,"dataClasses")[-1]
		names(vartypes0)=c()
		vartypes0=vartypes0[unik]
		indcat=which(vartypes0=="factor")
		indlogic=which(vartypes0=="logical")
		vartypes[indcat]="cat"
		vartypes[indlogic]="cat"
		
	}else{
		vartypes=rep("cont",x2$d)
		indcat=as.numeric(which(attr(x$terms,"dataClasses")=="factor")-1)
		indlogic=as.numeric(which(attr(x$terms,"dataClasses")=="logical")-1)
		vartypes[indcat]="cat"
		vartypes[indlogic]="cat"
	}
	
	# Indicate levels for categorical variables and the corresponding predictor values
	levels=(rep(list(""),x2$d))
	levelvalues=(rep(list(""),x2$d))
	if (length(indcat)>0) {
		for (i in seq(1,length(indcat),1)) {
			levels[[indcat[i]]]=(x$xlevels[[i]])
			levelvalues[[indcat[i]]]=(x$xlevels[[i]])
		}
	}
	if (length(indlogic)>0) {
		for (i in seq(1,length(indlogic),1)) {
			levels[[indlogic[i]]]=c("FALSE","TRUE")
			levelvalues[[indlogic[i]]]=c(FALSE,TRUE)
		}
	}
	
	for (i in seq(1,x2$d,1)) {
		if(length(unique(x2$x[,i]))==2 & vartypes[i]=='cont'){
			vartypes[i]="cat"
			levels[[i]]=as.character(sort(unique(x2$x[,i])))
			levelvalues[[i]]=(unique(x2$x[,i]))
		}
	}
	
	levelnames<-list(levels, levelvalues)
	
	
	# get constants to be added to result of predict(...,type="terms") to obtain functional forms: 
	# mean covariate vector multiplied with coefficients
	mm=model.matrix(x)
	tt <- terms(x)
	avx=colMeans(mm)
	aa <- attr(mm, "assign")
	ll <- attr(tt, "term.labels")
	aaa <- factor(aa, labels = ll)
	asgn <- split(order(aa), aaa)
	
	nterms <- length(asgn)
	constants=numeric(length=nterms)
	for (i in seq(1,nterms,1)) {
		constants[i]=avx[asgn[[i]]]%*%x$coefficients[asgn[[i]]]
	}
	x2$glmconst=numeric(x2$d)
	if ("assign"%in%names(x2)) {
		for (i in seq(1,max(x2$assign),1)) {
			x2$glmconst[i]=sum(constants[x2$assign==i])
		}
	}else{
		x2$glmconst=constants
	}
	
	# check whether there are interactions
	indint=which(attr(x$terms,"order")==2)
	posint=indint
	if(length(indint)>0){ 
		# indicate the order of the interactions
		x2$interactions=t(combn(seq(1,x2$d,1),2))
		x2$d2=dim(x2$interactions)[1]
		# make sure indint indicates the row of interactions corresponding to non-zero interactions
		for (i in c(1:length(indint))) {
			vars=which(attr(x$terms,"factors")[,indint[i]]==1)-1
			names(vars)=c()
			row.is.a.match <- apply(x2$interactions, 1, identical, vars) 
			match.idx <- which(row.is.a.match)
			
			posint[i]=match.idx
		}
	}
	
	x2$vartypes=vartypes
	x2$levelnames=levelnames
	x2$posint=posint
	
	return<-x2
}

getfuncformCPH<-function(x,obs=c()){
	
	if(length(obs)>0){
		
		ffterm=predict(x$glm,newdata=obs,type="terms",reference="sample") 
		if ("assign"%in%names(x)) {
			ffterm2=matrix(0,nrow=1,ncol=max(x$assign))
			
			for (i in seq(1,max(x$assign),1)) {
				ffterm2[i]=sum(ffterm[x$assign==i])
			}
			ffterm=ffterm2
		}
		
	} else {
		if ("mfp"%in%names(x)) {
			newdata=as.data.frame(x$x)
			ffterm=predict(x$glm,newdata=newdata,type="terms",reference="sample") 
			
			if ("assign"%in%names(x)) {
				ffterm2=matrix(0,nrow=dim(ffterm)[1],ncol=max(x$assign))
				
				for (i in seq(1,max(x$assign),1)) {
					if(sum(x$assign==i)>1){
						ffterm2[,i]=apply(ffterm[,x$assign==i],1,sum)
					} else {
						ffterm2[,i]=ffterm[,x$assign==i]
					}
				}
				ffterm=ffterm2
			}
			
		} else{
			newdata=as.data.frame(x$x)
			names(newdata)=attr(x$glm$terms,"term.labels")[which(attr(x$glm$terms,"order")==1)]
			ffterm=predict(x$glm,newdata=newdata,type="terms",reference="sample") 
		}
		
		
	}
	# check wheter there are interaction effects
	indint=which(attr(x$glm$terms,"order")==2)
	if(length(indint)>0){ 
		# set correct dimensions for ff
		if (length(obs)>0) {
			ff=matrix(0,nrow=dim(obs)[1],ncol=x$d+x$d2)
		} else {
			ff=matrix(0,nrow=dim(x$x)[1],ncol=x$d+x$d2)
		}
		# find main effects
		indmain=which(attr(x$glm$terms,"order")==1)
		# find position of interaction effects
		inteffects=matrix(0,ncol=2,nrow=length(indint))
		indfinal=matrix(0,nrow=x$d+length(indint),ncol=1)
		indfinal[1:x$d]=indmain
		for (i in seq(1,length(indint),1)) {
			inteffects[i,]=which(attr(x$glm$terms,"factors")[-1,indint[i]]==1)
			indfinal[x$d+i]=x$d+which(rowSums(abs(x$interactions-matrix(1,ncol=1,nrow=dim(x$interactions)[1])%*%inteffects[i,]))==0)
		}
		ff[,indfinal]=ffterm+matrix(1,nrow=dim(ffterm)[1],ncol=1)%*%x$glmconst
		
	} else { # no interactions
		ff=ffterm+matrix(1,nrow=dim(ffterm)[1],ncol=1)%*%x$glmconst
		
	}
	return(ff)
}

getRiskCPH<-function(index,x,thisindex=c()){
	
	s0=survival::survfit(x$glm)
	times=which(s0$time<=x$time)
	s0t=s0$surv[tail(times,1)] # baseline survival at specified time
	
	if (length(thisindex)==0) {
		latentvar=seq(min(index),max(index),length=100)
	}
	else { # only one index to convert
		latentvar=thisindex
	}
	sallt=s0t^exp(latentvar-sum(x$glmconst))
	result<-list(latentvar,sallt)
}

glm2data<-function(x,zerolevel,	risklabel="Predicted risk",adverse){
	
	if (x$family$link!="logit" ) {
		stop("colplot() not implemented for this case")
	}
	x2=list()
	
	
	if ("mfp"%in%class(x)) {
		x2$mfp=x
		x2$x=x$X
		correctnames=unique(substr(colnames(x$x),1,nchar(colnames(x$x))-2))
		# adapt correctnames for logicals
		
		oldnames=colnames(x$X)
		xorder=match(correctnames,oldnames)
		newnames=oldnames[xorder[-1]]
		x2$names=newnames
		x2$x=x$X[,xorder[-1]]
		colnames(x2$x)=x2$names
		
		dataclasses=attr(x$fit$terms,"dataClasses")[-1]
		names(dataclasses)=c()
		
		indfactor=which(attr(x$fit$terms,"dataClasses")=="factor")
		factornames=names(indfactor)
		
		indfactor=indfactor-1
		
		newdata2=x2$x
		
		indtodelete=c()
		toadd=matrix(data=NaN,nrow=dim(x2$x)[1],ncol=length(indfactor))
		addindex=1
		namestoadd=c()
		if (length(indfactor)>0) {
			for (i in seq(1,length(indfactor),1)) {

				indcurrentfactor=grep(factornames[i],x2$names,fixed=TRUE)
				indtodelete=c(indtodelete,indcurrentfactor)
				levels=strsplit(x2$names[indcurrentfactor],factornames[i],fixed=TRUE)
				indreflevel=seq(1,dim(x2$x)[1],1)
				levels2=c()
				for (j in seq(1,length(indcurrentfactor),1)) {
					levels2=c(levels2,levels[[j]][2])
					toadd[x2$x[,indcurrentfactor[j]]==1,addindex]=levels[[j]][2]
					indcurrentlevel=which(x2$x[,indcurrentfactor[j]]==1)
					indreflevel=indreflevel[-which(indreflevel%in%indcurrentlevel)]
				}
				indthisreflevel=which(names(x$fit$xlevels)==factornames[i])
				reflevel=x$fit$xlevels[indthisreflevel]
				names(reflevel)=c()
				alllevels=reflevel
				
				reflevel=reflevel[[1]][-which(reflevel[[1]]%in%levels2)]
				toadd[indreflevel,addindex]=reflevel
				namestoadd=c(namestoadd,factornames[i])
				addindex=addindex+1
			}	
			toadd=as.data.frame(toadd)				
			
			newdata2=cbind(newdata2[,-indtodelete],as.data.frame(toadd))
			currentnames=c(x2$names[-indtodelete],factornames)
		} else {
			currentnames=x2$names
		}
		
		
		# adapt currentnames for logical variables
		fitnames=names(attr(x$fit$terms,"dataClasses"))[-1]
		deletefitnames=which(sapply(strsplit(colnames(x$x)[-1], ".",fixed=TRUE), "[", 2)=="2")
		if (length(deletefitnames)>0) {
			fitnames=fitnames[-deletefitnames]
			dataclasses=dataclasses[-deletefitnames]
		}
		if (sum(dataclasses=="factor")>0) {
			indlogicals=which(dataclasses[-which(dataclasses=="factor")]=="logical")
		} else{
			indlogicals=which(dataclasses=="logical")
		}
		
		for (i in seq(along = indlogicals)) {
			tempind=which(pmatch(fitnames,currentnames[indlogicals[i]])==1)
			currentnames[indlogicals[i]]=fitnames[tempind]
		}
		xorder=numeric(length(currentnames))
		for (i in seq(1,length(currentnames),1)) {
			xorder[grep(currentnames[i],fitnames)]=i
		}		
		
		if (length(indfactor)>0) {
			newdataclasses=c(dataclasses[-which(dataclasses=="factor")],rep("factor",length(indfactor)))
			
		} else {
			newdataclasses=dataclasses
			
		}		
		# adapt classes
		newdata2=as.data.frame(newdata2)
		names(newdata2)[which(newdataclasses=="logical")]=fitnames[which(newdataclasses=="logical")]
		
		for (i in seq(1,dim(newdata2)[2]-length(indfactor))) {
			attributes(newdata2[,i])$class <-newdataclasses[i]
		}
		colnames(newdata2)=currentnames
		
		x2$x=newdata2[,xorder]
		
		x2$names=names(x2$x)
		
		indlogical=which(newdataclasses[xorder]=="logical")
		
		for (i in seq(along = indlogicals)) {
			x2$x[,indlogical[i]]=as.logical(x2$x[,indlogical[i]])
		}
		
		# make assign vector to indicate which ff-terms belong to which variable
		assignnames=attr(terms(x$fit),"term.labels")
		assign=numeric(length(assignnames))
		for (i in seq(1,length(x2$names),1)) {
			assign[grep(x2$names[i],assignnames)]=i
		}
		x2$assign=assign
		x=x$fit
		mm=model.frame(x)
		mm=mm[,!duplicated(assign)]
		names(mm)[-1]=x2$names
		x2$model=mm
	} else{
		x2$x=x$model[,-1]
		x2$names=names(x2$x)
		x2$model=x$model
	}
	
	
	x2$y=x$y
	x2$n=dim(x2$x)[1]
	x2$d=dim(x2$x)[2]
	x2$coeffs=x$coefficients
	x2$getriskestimate=getRiskGLM
	x2$getfuncform=getfuncformGLM
	x2$glm=x
	x2$zerolevel=zerolevel
	x2$risklabel=risklabel
	if (hasArg(adverse)) {
		x2$adverse=adverse
		
	}
	
	
	# Indicate the type for each covariate
	if("assign"%in%names(x2)){
		unik <- !duplicated(x2$assign)  ## logical vector of unique values
		vartypes=rep("cont",x2$d)
		vartypes0=attr(x$terms,"dataClasses")[-1]
		names(vartypes0)=c()
		vartypes0=vartypes0[unik]
		indcat=which(vartypes0=="factor")
		indlogic=which(vartypes0=="logical")
		vartypes[indcat]="cat"
		vartypes[indlogic]="cat"
		
	}else{
		vartypes=rep("cont",x2$d)
		indcat=as.numeric(which(attr(x$terms,"dataClasses")=="factor")-1)
		indlogic=as.numeric(which(attr(x$terms,"dataClasses")=="logical")-1)
		vartypes[indcat]="cat"
		vartypes[indlogic]="cat"
	}
		
		
	
	# Indicate levels for categorical variables and the corresponding predictor values
	levels=(rep(list(""),x2$d))
	levelvalues=(rep(list(""),x2$d))
	if (sum(indcat!=0)) {
		for (i in seq(1,length(indcat),1)) {
			levels[[indcat[i]]]=(x$xlevels[[i]])
			levelvalues[[indcat[i]]]=(x$xlevels[[i]])
		}
	}
	if (sum(indlogic!=0)) {
		for (i in seq(1,length(indlogic),1)) {
			levels[[indlogic[i]]]=c("FALSE","TRUE")
			levelvalues[[indlogic[i]]]=c(FALSE,TRUE)
		}
	}
	
	for (i in seq(1,x2$d,1)) {
		if(length(unique(x2$x[,i]))==2 & vartypes[i]=='cont'){
			vartypes[i]="cat"
			levels[[i]]=as.character(sort(unique(x2$x[,i])))
			levelvalues[[i]]=(unique(x2$x[,i]))
		}
	}
	
	levelnames<-list(levels, levelvalues)
	
	# get constants to be added to result of predict(...,type="terms") to obtain functional forms
	mm=model.matrix(x)
	tt <- terms(x)
	avx=colMeans(mm)
	aa <- attr(mm, "assign")
	ll <- attr(tt, "term.labels")
	hasintercept <- attr(tt, "intercept") > 0L
	if (hasintercept) ll <- c("(Intercept)", ll)
	aaa <- factor(aa, labels = ll)
	asgn <- split(order(aa), aaa)
	
	nterms <- length(asgn)-ifelse(hasintercept,1,0)
	constants=numeric(length=nterms)
	for (i in seq(1,nterms,1)) {
		if(hasintercept){
			constants[i]=avx[asgn[[i+1]]]%*%x$coefficients[asgn[[i+1]]]
		} else {
			constants[i]=avx[asgn[[i]]]%*%x$coefficients[asgn[[i]]]
		}
	}
	x2$glmconst=numeric(x2$d)
	if ("assign"%in%names(x2)) {
		for (i in seq(1,max(x2$assign),1)) {
			x2$glmconst[i]=sum(constants[x2$assign==i])
		}
	}else{
		x2$glmconst=constants
	}
	
	# check whether there are interactions
	indint=which(attr(x$terms,"order")==2)
	posint=indint
	if(length(indint)>0){ 
		# indicate the order of the interactions
		x2$interactions=t(combn(seq(1,x2$d,1),2))
		x2$d2=dim(x2$interactions)[1]
		# make sure indint indicates the row of interactions corresponding to non-zero interactions
		for (i in c(1:length(indint))) {
			vars=which(attr(x$terms,"factors")[,indint[i]]==1)-1
			names(vars)=c()
			row.is.a.match <- apply(x2$interactions, 1, identical, vars)  
			match.idx <- which(row.is.a.match)
			
			posint[i]=match.idx
		}
	}
	
	x2$vartypes=vartypes
	x2$levelnames=levelnames
	x2$posint=posint
	
	return<-x2
}

adaptObs<-function(x,obs){
	# make sure the levels of thispatient are the same as in the whole dataset
	obs=obs[is.finite(match(names(obs),names(x$model[,-1])))]
	obs=merge(obs,x$model[,-1],by=names(x$model[,-1]),all.x = TRUE, all.y=TRUE,sort=FALSE)
	obs=obs[1,]
	for (i in seq(1:dim(obs)[2])) {
		if (is.factor(x$model[,-1][,i])) {
			if(!obs[,i]%in%levels(x$model[,-1][,i])){
				stop("The levels of obs do not match of the levels of the training data.")
			}
			obs[,i]=factor(obs[,i],levels=levels(x$model[,-1][,i]))
		}
	}
	return<-obs
}

