

# Project: visualizationEXTRA
# 
# Author: vvanbell
###############################################################################

getallfitsMLR<-function(x,filename, coloroptions=2,zerolevel="zero",
		risklabel="Predicted risk",xmin,xmax,adverse=FALSE,obs,q5,q95,time){
	
	if (zerolevel%in%c("mean","median")) {
		stop("The colplot for multinomial logistic regression in combination with a zerolevel in c('mean','median') is not supported.")
	}
	
	
	# coeffs
	coeffs=coefficients(x)
	
	d=length(attr(x$terms,"term.labels"))
	
	outnames=colnames(fitted(x))
	# colplots for all but the reference level
	if (hasArg(filename)) {
		names1=paste0(filename,"_",outnames[-1])
		# risk calculation for all but reference level
		names3=paste0(filename,"p_",outnames)
	} else {
		names1=paste0("fit_",outnames[-1])
		# risk calculation for all but reference level
		names3=paste0("fitp_",outnames)
	}
	
	listnames <- c(names1,names3)
	allfits <- vector("list", length(listnames))
	names(allfits) <- c(listnames)
	
	x2=x$model[,-1]
	d=dim(x2)[2]
	n=dim(x2)[1]
	names=names(x$model[,-1])
	beta=coeffs[,-1]
	
	if (length(risklabel)==2*length(outnames)-1) {
		risklabels=risklabel
	}else{
		risklabels=c(paste0("lin.pred.",outnames[-1]),paste("Predicted risk on",outnames))
	}
	
	################################
	# create the colplots for all but reference level
	################################
	
	vartypes=rep("cont",d)
	indcat=as.numeric(which(attr(x$terms,"dataClasses")[-1]=="factor"))
	indlogic=as.numeric(which(attr(x$terms,"dataClasses")[-1]=="logical"))
	vartypes[indcat]="cat"
	vartypes[indlogic]="cat"
	
	# Indicate levels for categorical variables and the corresponding predictor values
	levels=(rep(list(""),d))
	levelvalues=(rep(list(""),d))
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
	
	for (i in seq(1,d,1)) {
		if(length(unique(x2[,i]))==2 & vartypes[i]=='cont'){
			vartypes[i]="cat"
			levels[[i]]=as.character(sort(unique(x2[,i])))
			levelvalues[[i]]=(unique(x2[,i]))
		}
	}
	
	levelnames<-list(levels, levelvalues)
	lp=matrix(0,nrow=n,ncol=dim(coeffs)[1])
	if(hasArg(obs)){
		lpobs=matrix(0,nrow=1,ncol=dim(coeffs)[1])
	} else {
		lpobs=c()
	}
	beta0=coeffs[,1]
	
	allfits=generateLP(x,coeffs,allfits,obs,names,getfuncform1=getfuncformMLR1,getrisk1=getriskMLR1,levelnames, vartypes, filename, 
			coloroptions, zerolevel,risklabels,xmin,xmax,adverse=FALSE,q5,q95)
	
	################################
	# generate plot for pref
	################################
	
	allfits=generatePref(x2,coeffs,allfits,obs,risklabel=risklabels)
	
	################################
	# generate plots for p's (other than ref)
	################################
	
	allfits=generatePother(x2,coeffs,allfits,obs,filename,coloroptions,zerolevel,risklabel=risklabels,adverse=FALSE,q5,q95,outcomelevels=outnames)
	
	return<-allfits
}


wingplot<-function(allfits,coeffs,ylabel){
	
	iref=dim(coeffs)[1]+1
	
	lp=allfits[[iref]]$x
	beta0=allfits[[iref]]$beta0
	lpobs=allfits[[iref]]$obs
	
	allpref=allfits[[dim(coeffs)[1]+1+1]]$pref
	
	pref=round(seq(0.05,0.95,by=0.15),2)
	ind1=which(pref<min(allpref))
	ind2=which(pref>max(allpref))
	if (length(ind1)>0) {
		pref=pref[-ind1]
	} 
	if (length(ind2)>0) {
		pref=pref[-ind2]
	} 
	
	plevel <- vector("list", dim(coeffs)[1])
	
	if(length(lpobs)>0){
		fprefobs=getfuncformMLR2(allfits[[dim(coeffs)[1]+1]],obs=lpobs)
		prefobs=getriskMLR2(index=sum(fprefobs),thisindex=sum(fprefobs))[[2]]
		fplobs=matrix(NaN,nrow=1,ncol=length(lpobs))
		pobs=matrix(NaN,nrow=1,ncol=length(lpobs))
		
		for (i in seq(1,length(lpobs),1)) {
			fplobs=getfuncformMLR3(allfits[[dim(coeffs)[1]+1]],obs=data.frame(x1=prefobs,x2=lpobs[i]))
			pobs[i]=getriskMLR3(index=sum(fplobs),data=allfits[[dim(coeffs)[1]+1+i]],thisindex=sum(fplobs))[[2]]
		}
	}
	
	for (i in seq(1,dim(coeffs)[1],1)) {
		png(paste0(names(allfits)[i+dim(coeffs)[1]+1],"_wing.png"))
		
		lplevel=seq(min(lp[,i]),min(5-beta0[i],max(lp[,i])),length=30)+beta0[i]
		lpmatrix=matrix(rep(lplevel,length(pref)),nrow=length(lplevel))
		prefmatrix=matrix(rep(pref,length(lplevel)),ncol=length(pref),byrow=TRUE)
		plevel[[i]]=prefmatrix*exp(lpmatrix)
		plot(lplevel-beta0[i],plevel[[i]][,1],ylim=c(-0.05,1),xlab=allfits[[1+dim(coeffs)[1]]]$names[i], ylab=ylabel[1+dim(coeffs)[1]+i],type="n")
		for (j in seq(1,dim(plevel[[i]])[2])) {
			lines(lplevel-beta0[i],plevel[[i]][,j])
			
		}
		j=which(pref==0.05)
		if(length(j)>0){
			lines(lplevel-beta0[i],plevel[[i]][,j],lwd=2)
			xtext=tail(which(plevel[[i]][,j]<1),n=1)
			text(min(0.9*diff(range(lplevel))+min(lplevel)-beta0[i],lplevel[xtext]-beta0[i]),max(-0.05,plevel[[i]][,j][xtext]-0.2),expression(paste(p[ref],"=0.05")),font=2)
		}
		
		j=which(pref==0.5)
		
		if(length(j)>0){
			lines(lplevel-beta0[i],plevel[[i]][,j],lwd=2)
			xtext=tail(which(plevel[[i]][,j]<1),n=1)
			text(min(0.9*diff(range(lplevel))+min(lplevel)-beta0[i],lplevel[xtext]-beta0[i]),plevel[[i]][,j][xtext],expression(paste(p[ref],"=0.5")),font=2)
		}
		
		j=which(pref==0.95)
		if(length(j)>0){
			lines(lplevel-beta0[i],plevel[[i]][,j],lwd=2)
			xtext=tail(which(plevel[[i]][,j]<1),n=1)
			text(min(0.9*diff(range(lplevel))+min(lplevel)-beta0[i],lplevel[xtext]-beta0[i]),min(1,plevel[[i]][,j][xtext]+0.2),expression(paste(p[ref],"=0.95")),font=2)
		}
		
		if (length(lpobs)>0) {
			plevelobs=exp(lplevel)*prefobs		
			xtext=tail(which(plevelobs<1),n=1)
			lines(lplevel-beta0[i],plevelobs,lwd=2,col="red")
			lines(c(lpobs[i],lpobs[i]),c(0,pobs[i]),col="red",lty=3)
			lines(c(min(lp[,i]),lpobs[i]),c(pobs[i],pobs[i]),col="red",lty=3)
			text(min(lp[,i])+0.2,pobs[i]+0.03,round(pobs[i],2),col="red")
			text(lpobs[i],-0.05,round(lpobs[i],2),col="red")
			text(min(0.9*diff(range(lplevel))+min(lplevel)-beta0[i],lplevel[xtext]-beta0[i]),min(1,plevelobs[xtext]),bquote(paste(p[ref],"=",.(round(prefobs,2)))),font=2,col="red")
			
		}
		
		garbage<-dev.off()
		
	}
}






# from predictors to linear predictor
getfuncformMLR1<-function(data, obs=c()) {
	beta=data$beta
	
	if (length(obs)==0) {
		
		f=data$x;
		f[]=0;
		
		pos=0
		for (i in seq(1,data$d,1)) {
			if (class(data$x[,i])=="factor") {
				alllevels=levels(data$xall[,i])
				for (j in seq(2,length(alllevels),1)) {
					thislevel=which(data$x[,i]==alllevels[j])
					f[thislevel,i]=beta[pos+1]
					pos=pos+1
				}
				
			} else if (class(data$x[,i])=="logical") {
				nonreflevel=gsub(names(data$x)[i],"",names(beta[pos+1]))
				f[data$x[,i]==nonreflevel,i]=beta[pos+1]
				pos=pos+1
			} else {
				f[,i]=data$x[,i]*beta[pos+1]
				pos=pos+1
			}
		}
		
	} else {
		f=obs;
		f[]=0;
		
		pos=0
		obs2=as.matrix(obs)
		colnames(obs2)=NULL
		for (i in seq(1,data$d,1)) {
			if (class(data$x[,i])=="factor") {
				alllevels=levels(data$xall[,i])
				thislevel=which(obs2[i]==alllevels)
				if (thislevel>1) {
					f[i]=beta[pos+thislevel-1]
				}
				pos=pos+length(alllevels)-1
				
			} else if (class(data$x[,i])=="logical") {
				nonreflevel=gsub(names(data$x)[i],"",names(beta[pos+1]))
				if (obs[i]==nonreflevel) {
					f[i]=beta[pos+1]
				}
				pos=pos+1
			} else {
				f[i]=obs[i]*beta[pos+1]
				pos=pos+1
			}
		}
		
	}
	
	result<-f
}
# from linear predictor to nothing
getriskMLR1<-function(index,data,thisindex=c()) {
	
	result<-list(NaN,NaN)
}
# from sum of lp's to exp(lp+beta0)
getfuncformMLR2<-function(data, obs=c()) {
	if (length(obs)==0) {
		f=exp(data$x+matrix(1,nrow=dim(data$x)[1],ncol=1)%*%data$beta0)
	} else {
		if (length(dim(obs))==0) {
			f=exp(obs+data$beta0)
		} else {
			f=exp(obs+matrix(1,nrow=dim(obs)[1],ncol=1)%*%data$beta0)
		}
	}
}
# from sum of lp's to pref
getriskMLR2<-function(index,data,thisindex=c()) {
	
	if (length(thisindex)==0) {
		latentvar=seq(max(min(index),0.05),min(19,max(index)),by=0.01)
	}
	else { # only one index to convert
		latentvar=thisindex
	}
	riskestimate=1/(1+latentvar)
	
	result<-list(latentvar,riskestimate)
}
# from pref and lp to ln(pref) and lp
getfuncformMLR3<-function(data, obs=c()) {
	if (length(obs)==0) {
		f=data$x;
		f[]=0;
		
		f[,1]=log(data$x[,1])
		f[,2]=data$x[,2]
	} else {
		f=obs;
		f[]=0;
		if (length(dim(obs))==0) {
			f[1]=log(obs[1])
			f[2]=obs[2]	
		} else{
			f[,1]=log(obs[,1])
			f[,2]=obs[,2]
		}
		
	}
	return<-f
}
# from sum of ln(pref) and lp to p
getriskMLR3<-function(index,data,thisindex=c()) {
	
	if (length(thisindex)==0) {
		latentvar=seq(min(index),max(index),by=0.05)	
	}
	else { # only one index to convert
		latentvar=thisindex
	}
	
	riskestimate=exp(latentvar+data$beta0)
	
	result<-list(latentvar,riskestimate)
}


patientsummary<-function(allfits,filename,pbase,outlevels,allp_data_range){
	
	nb=(length(allfits)-1)/2
	iref=nb+1
	
	if (hasArg(allp_data_range)) {
		allp_data=allp_data_range
	} else {
		allp_data=matrix(NaN,nrow=dim(allfits[[iref]]$xall)[1],ncol=nb+1)
		
		allp_data[,1]=allfits[[iref]]$getriskestimate(rowSums(getfuncformMLR2(allfits[[iref]])),allfits[[iref]],rowSums(getfuncformMLR2(allfits[[iref]],allfits[[iref]]$xall)))[[2]]
		
		for (i in seq(iref+1,iref+nb)) {
			temprisks=allfits[[i]]$getriskestimate(rowSums(getfuncformMLR3(allfits[[i]])),allfits[[i]],rowSums(getfuncformMLR3(allfits[[i]])))
			temprisks2=allfits[[i]]$getriskestimate(rowSums(getfuncformMLR3(allfits[[i]])),allfits[[i]],rowSums(getfuncformMLR3(allfits[[i]],allfits[[i]]$x)))
			allp_data[,i-iref+1]=allfits[[i]]$getriskestimate(rowSums(getfuncformMLR3(allfits[[i]])),allfits[[i]],rowSums(getfuncformMLR3(allfits[[i]],allfits[[i]]$xall)))[[2]]
		}
	}
	allp_obs=vector("numeric",nb+1)
			
	allp_obs[1]=allfits[[iref]]$getriskestimate(rowSums(getfuncformMLR2(allfits[[iref]])),allfits[[iref]],rowSums(getfuncformMLR2(allfits[[iref]],allfits[[iref]]$obs)))[[2]]
	
	for (i in seq(iref+1,iref+nb)) {
		temprisks=allfits[[i]]$getriskestimate(rowSums(getfuncformMLR3(allfits[[i]])),allfits[[i]],rowSums(getfuncformMLR3(allfits[[i]])))
		temprisks2=allfits[[i]]$getriskestimate(rowSums(getfuncformMLR3(allfits[[i]])),allfits[[i]],rowSums(getfuncformMLR3(allfits[[i]],allfits[[i]]$x)))
		allp_obs[i-iref+1]=allfits[[i]]$getriskestimate(rowSums(getfuncformMLR3(allfits[[i]])),allfits[[i]],rowSums(getfuncformMLR3(allfits[[i]],allfits[[i]]$obs)))[[2]]
	}
	
	# comparison with prevalance of outcome levels in the training data set
	
	newdata=rbind(pbase*100,allp_obs*100)
	rownames(newdata)=c("baseline","observation")
	colnames(newdata)=outlevels
	newdata=t(newdata)
	rr=round(allp_obs/pbase,2)
	png(filename=paste0(filename,"_patientsummary.png"))
	oldpar=par()
	layout(cbind(1,2), heights=c(1,1))
	if (length(pbase)==5) {
		barplot(newdata, width=2,ylab="Predicted risk (%)",col=c("green","yellow","orange","red","brown"))
		title(main="Comparison of predicted risks with baseline risks.",outer=TRUE,line=-2)
		par(mar=c(0, 0, 0, 0))
		plot.new()
		legend("left", fill=c("green","yellow","orange","red","brown"), legend=paste0(rownames(newdata)," (risk=",round(allp_obs,2)*100,"%, rel.risk=",rr,")"))
		suppressWarnings(par(oldpar))
		garbage<-dev.off()
	}else if (length(pbase)==3) {
					barplot(newdata, width=2,ylab="Predicted risk (%)",col=c("red","blue","green"))
					title(main="Comparison of predicted risks with baseline risks.",outer=TRUE,line=-2)
					par(mar=c(0, 0, 0, 0))
					plot.new()
					legend("left", fill=c("red","blue","green"), legend=paste0(rownames(newdata)," (risk=",round(allp_obs,2)*100,"%, rel.risk=",rr,")"))
					suppressWarnings(par(oldpar))
					garbage<-dev.off()
				}else{
		barplot(newdata, col=c(1:length(rownames(newdata)))+1, width=2,ylab="Predicted risk (%)")
		title(main="Comparison of predicted risks with baseline risks.",outer=TRUE,line=-2)
		par(mar=c(0, 0, 0, 0))
		plot.new()
		legend("left", fill=c(1:length(rownames(newdata)))+1, legend=paste0(rownames(newdata)," (risk=",round(allp_obs,2)*100,"%, rel.risk=",rr,")"))
		suppressWarnings(par(oldpar))
		garbage<-dev.off()
	}
	
	png(filename=paste0(filename,"_patientsummary2.png"))
	layout(rbind(1,2), heights=c(7,1))	
	oldmar=par()$mar
	par(mar=par()$mar+c(0,0,0,6))
	plot(pbase,-c(1:length(allp_obs))+0.1,pch=24,col="black",cex=2,xlim=c(0,1),ylim=c(-1+0.1,-length(allp_obs)-0.1),xlab="Predicted chance",yaxt="n",ylab="")
	points(allp_obs,-c(1:length(allp_obs))-0.1,pch=25,col="red",bg="red",cex=2,xlim=c(0,1))
	for (i in seq(1,length(allp_obs),1)) {
		lines(c(min(allp_data[,i]),max(allp_data[,i])),c(-i,-i),lwd=2)
	}
	points(allp_obs,-c(1:length(allp_obs))-0.1,pch=25,col="red",bg="red",cex=2,xlim=c(0,1))
	axis(4,at=-c(1:length(allp_obs)),labels=outlevels,las=2,main="asdf")
	mtext(side=2,line=1,"Outcome level")
	title(main="Predicted chances for this observation compared to \n prevalence in training data.")
	par(mar=c(0, 0, 0, 0))
	plot.new()
	legend("center",legend=c("prevalence in training data","predicted risks for this observation","range of predicted risks in training data"),lty=c(0,0,1),lwd=c(1,1,2),pch=c(24,25,NaN),col=c("black","red","black"),pt.bg=c("white","red","black"))
	par(mar=oldpar$mar)
	garbage<-dev.off()
	
	return=allp_obs	
}


#' @export
cchart.multinom<-function(x, obs, filename, zerolevel="zero",
		risklabel="Predicted risk",sorted=FALSE,time,xmin,xmax){
	
	
	message(paste("More than one figure will be created within the directory",getwd()))
	
	obs=adaptObs(x,obs)
	
	if(hasArg(xmin)){
		xmin=as.numeric(adaptObs(x,xmin))
	}
	if(hasArg(xmax)){
		xmax=as.numeric(adaptObs(x,xmax))
	}
	
	coeffs=coefficients(x)
	
	
	################################
	# create list of lists (containing all the information for colplot.default)
	################################
	
	allfits=getallfitsMLR(x,filename,zerolevel=zerolevel,
			risklabel=risklabel,obs=obs,xmin=xmin,xmax=xmax)
	
	# visualization of linear predictors for non-reference outcome levels
	for (i in seq(1,dim(coeffs)[1],1)) {
		cchart(allfits[[i]],obs=allfits[[i]]$obs,filename=paste0(names(allfits)[i],"_cchart"),zerolevel=zerolevel,
				risklabel=allfits[[i]]$risklabel,sorted=sorted,xmin=xmin,xmax=xmax)
	}
	
	# calculation of predicted chance for reference outcome level
	i=dim(coeffs)[1]+1
	
	cchart(allfits[[i]],obs=allfits[[i]]$obs,filename=paste0(names(allfits)[i],"_cchart"),zerolevel=zerolevel,
			risklabel=allfits[[i]]$risklabel,sorted=sorted,xmin=xmin,xmax=xmax)
	
	# calculation of predicted chance for non-reference outcome levels
	for (i in dim(coeffs)[1]+1+seq(1,dim(coeffs)[1],1)) {
		cchart(allfits[[i]],obs=allfits[[i]]$obs,filename=paste0(names(allfits)[i],"_cchart"),zerolevel=zerolevel,
				risklabel=allfits[[i]]$risklabel,sorted=sorted,xmin=xmin,xmax=xmax)
	}
}

#' @export
ccchart.multinom<-function(x, obs, filename, zerolevel="zero",
		risklabel="Estimated risk",riskcutoff=0.1,type="logistic",sorted=FALSE,time,xmin=xmin,xmax=xmax){
	
	message(paste("More than one figure will be created within the directory",getwd()))
	
	obs=adaptObs(x,obs)
	
	if(hasArg(xmin)){
		xmin=as.numeric(adaptObs(x,xmin))
	}
	if(hasArg(xmax)){
		xmax=as.numeric(adaptObs(x,xmax))
	}
	coeffs=coefficients(x)
	
	
	################################
	# create list of lists (containing all the information for colplot.default)
	################################
	
	allfits=getallfitsMLR(x,filename,zerolevel=zerolevel,
			risklabel=risklabel,obs=obs,xmin=xmin,xmax=xmax)
	
	# visualization of linear predictors for non-reference outcome levels
	for (i in seq(1,dim(coeffs)[1],1)) {
		ccchart(allfits[[i]],obs=allfits[[i]]$obs,filename=paste0(names(allfits)[i],"_ccchart"),zerolevel=zerolevel,
				risklabel=allfits[[i]]$risklabel,riskcutoff,type,sorted=sorted,xmin=xmin,xmax=xmax)
	}
	
	# calculation of predicted chance for reference outcome level
	i=dim(coeffs)[1]+1
	
	ccchart(allfits[[i]],obs=allfits[[i]]$obs,filename=paste0(names(allfits)[i],"_ccchart"),zerolevel=zerolevel,
			risklabel=allfits[[i]]$risklabel,riskcutoff,type,sorted=sorted,xmin=xmin,xmax=xmax)
	
	# calculation of predicted chance for non-reference outcome levels
	for (i in dim(coeffs)[1]+1+seq(1,dim(coeffs)[1],1)) {
		ccchart(allfits[[i]],obs=allfits[[i]]$obs,filename=paste0(names(allfits)[i],"_ccchart"),zerolevel=zerolevel,
				risklabel=allfits[[i]]$risklabel,riskcutoff,type,sorted=sorted,xmin=xmin,xmax=xmax)
	}
}


#' Summarize the risk prediction plots.
#' 
#' Create an HTML page that summarizes all the plots that have been created to visualize the risk prediction.  When a patient was given, a patient summary will be given as well.  This function is only applicable for \code{multinom} objects.
#' 
#' @param fit A \code{multinom} object or a character vector.  In the latter case the vector consists of the names of the outcome levels.
#' @param summaryfile Name of the resulting HTML file
#' @param filename The filename that was given to the \code{colplot}, \code{cchart} or \code{ccchart} commands.
#' @param title Title of the summary.
#' @example /inst/examples/HTMLsummaryex.R
#' @references Van Belle V., Van Huffel S., Timmerman D., Froyman W., Bourne T. and Van Calster B., \emph{A color based nomogram for Multinomial Logistic Regression}, Internal Report 16-28, ESAT-Stadius, KU Leuven (Leuven, Belgium), 2016
#' @importFrom utils Sweave 
#' @importFrom R2HTML RweaveHTML 
#' @author Vanya Van Belle
#' @export
HTMLsummary<-function(fit, summaryfile, filename,title){
	
	if ("character"%in%class(fit)) {
		outnames=fit
	} else {
		outnames=colnames(fitted(fit))
	}
		
	lpplots=paste0(filename,"_",outnames[-1],".png")
	prefplot=paste0(filename,"p_",outnames[1],".png")
	pallplot=paste0(filename,"p_",outnames[-1],".png")
	
	lpplotscc=paste0(filename,"_",outnames[-1],"_cchart.png")
	prefplotcc=paste0(filename,"p_",outnames[1],"_cchart.png")
	pallplotcc=paste0(filename,"p_",outnames[-1],"_cchart.png")
	
	lpplotsccc=paste0(filename,"_",outnames[-1],"_ccchart.png")
	prefplotccc=paste0(filename,"p_",outnames[1],"_ccchart.png")
	pallplotccc=paste0(filename,"p_",outnames[-1],"_ccchart.png")
	
	pallplotwing=paste0(filename,"p_",outnames[-1],"_wing.png")
	mytitle=title
	
	if(!all(file.exists(c(lpplotscc,prefplotcc,pallplotcc)))&!all(file.exists(c(lpplotsccc,prefplotccc,pallplotccc)))&!all(file.exists(c(lpplots,prefplot,pallplot)))){
		stop("Not all graphics are available for plotting.  Please check the entered filename and make sure you ran colplot() with the same filename.")
	}
	
	
	sink(paste0(summaryfile,".Rnw"))
	temp="
			<HTML>
			<Head>
			<style>
			img{
			height: auto; 
			width: auto; 
			max-width: 500px; 
			max-height: 500px;
			background:yellow; /* to show: same height */
			}
			h1{
			color: lightgrey;
			#font-size: 250%;
			background-color:black;
			}
			h2{
			color: black;
			#font-size: 100%;
			background-color:lightgrey;
			}
			h3{
			color: black;
			#font-size: 50%;
			background-color:white;
			
			}
			</style>
			
			<Title>
			Global summary
			</Title>
			</Head>
			<Body>
			
			"
	cat(temp)
	
# set title
	temp=paste0("<center><H1>
					",title,"
					</H1></center>
					
					")
	cat(temp)
	
	############
	# if colplot was called: plot global summary of model
	###########
	if(all(file.exists(c(lpplots,prefplot,pallplot)))){
		
		# set subtitle
		temp=paste0("<center><H2>
						","Global overview of the model","
						</H2></center>
						
						")
		cat(temp)
		
		# set charts for linear predictors
		temp="<center><H3>
				Calculation charts for the linear predictors.
				</H3></center>
				
				"
		cat(temp)
		
		temp=paste0("<img src=", lpplots, ">",collapse="")
		cat(paste0("<center>
								", temp,"</center>
								
								"))
		
		# set chart for risk on reference outcome level
		temp="<center><H3>
				Risk calculation chart for the risk on the reference outcome level.
				</H3></center>
				
				"
		cat(temp)
		temp=paste0("<center> 
						<img src=", prefplot, ">
						</center>
						
						")
		cat(temp)
		
		# set chart for risk on non-reference outcome levels
		temp="<center><H3>
				Risk calculation charts for the risks on non-reference outcome levels.
				</H3></center>
				
				"
		cat(temp)
		temp=paste0("<img src=", pallplot, ">",collapse="")
		cat(paste0("<center>
								", temp,"</center>
								
								"))
		
		
		# set chart for risk on non-reference outcome levels
		temp="<center><H2>
				Risk calculation charts for the risks on non-reference outcome levels: alternative.
				</H2></center>
				
				"
		cat(temp)
		temp=paste0("<img src=", pallplotwing, ">",collapse="")
		cat(paste0("<center>
								", temp,"</center>
								
								"))
	}

	###################
	# if cchart was called 
	###################
	
	if(all(file.exists(c(lpplotscc,prefplotcc,pallplotcc)))){
		
		# set subtitle
		temp=paste0("<center><H2>
						","Contribution charts for this patient","
						</H2></center>
						
						")
		cat(temp)
		
		# set charts for linear predictors
		temp="<center><H3>
				Contribution charts for the linear predictors.
				</H3></center>
				
				"
		cat(temp)
		
		temp=paste0("<img src=", lpplotscc, ">",collapse="")
		cat(paste0("<center>
								", temp,"</center>
								
								"))
		
		# set chart for risk on reference outcome level
		temp="<center><H3>
				Contribution chart for the risk on the reference outcome level.
				</H3></center>
				
				"
		cat(temp)
		temp=paste0("<center> 
						<img src=", prefplotcc, ">
						</center>
						
						")
		cat(temp)
		
		# set chart for risk on non-reference outcome levels
		temp="<center><H3>
				Contribution charts for the risks on non-reference outcome levels.
				</H3></center>
				
				"
		cat(temp)
		temp=paste0("<img src=", pallplotcc, ">",collapse="")
		cat(paste0("<center>
								", temp,"</center>
								
								"))
	
	}
	
	
	
	###################
	# if ccchart was called 
	###################
	
	if(all(file.exists(c(lpplotsccc,prefplotccc,pallplotccc)))){
		
		# set subtitle
		temp=paste0("<center><H2>
						","Cumulative contribution charts for this observation","
						</H2></center>
						
						")
		cat(temp)
		
		# set charts for linear predictors
		temp="<center><H3>
				Cumulative contribution charts for the linear predictors.
				</H3></center>
				
				"
		cat(temp)
		
		temp=paste0("<img src=", lpplotsccc, ">",collapse="")
		cat(paste0("<center>
								", temp,"</center>
								
								"))
		
		# set chart for risk on reference outcome level
		temp="<center><H3>
				Cumulative contribution chart for the risk on the reference outcome level.
				</H3></center>
				
				"
		cat(temp)
		temp=paste0("<center> 
						<img src=", prefplotccc, ">
						</center>
						
						")
		cat(temp)
		
		# set chart for risk on non-reference outcome levels
		temp="<center><H3>
				Cumulative contribution charts for the risks on non-reference outcome levels.
				</H3></center>
				
				"
		cat(temp)
		temp=paste0("<img src=", pallplotccc, ">",collapse="")
		cat(paste0("<center>
								", temp,"</center>
								
								"))
		
	}
	
	
	##################
	# patient summary
	##################
	
	if(file.exists(paste0(filename,"_patientsummary.png"))){
		# set subtitle
		temp=paste0("<center><H2>
						","Summary for this patient","
						</H2></center>
						
						")
		cat(temp)
		temp=paste0("<img src=", paste0(filename,c("_patientsummary.png","_patientsummary2.png")), ">",collapse="")
		cat(paste0("<center>
								", temp,"</center>
								
								"))
	}
	
	
	
	temp="</Body>
			</HTML>"
	cat(temp)
	
	sink()
	
	# library(R2HTML)
	Sweave(paste0(summaryfile,".Rnw"),driver=RweaveHTML)
}

#' @importFrom grDevices rgb 
generateLP<-function(x,coeffs,allfits,obs,names,getfuncform1,getrisk1,levelnames, vartypes, filename, 
		coloroptions=2, zerolevel="zero",risklabel,xmin,xmax,adverse=FALSE,q5,q95){
		
	if (zerolevel%in%c("mean","median")) {
		stop("The colplot for multinomial logistic regression in combination with a zerolevel in c('mean','median') is not supported.")
	}
	
	if("multinom"%in%class(x)){
		x2=x
		x=x2$model[,-1]
	}
	
	n=dim(x)[1]
	d=dim(x)[2]
	lp=matrix(0,nrow=n,ncol=dim(coeffs)[1])
	if(hasArg(obs)){
		lpobs=matrix(0,nrow=1,ncol=dim(coeffs)[1])
	} else {
		lpobs=c()
	}
	beta0=coeffs[,1]
	beta=coeffs[,-1]
	
# calculate fzeroALL (fzero for all outcome levels) # this is necessary to obtain the same color legend for each outcome level
	fzeroALL=c()
	allcolorsALL=c()
	if("interactions"%in%names(allfits[[1]])){
		allfmin=matrix(NaN,nrow=dim(coeffs)[1],ncol=d+dim(allfits[[1]]$interactions)[1])
	} else {
		allfmin=matrix(NaN,nrow=dim(coeffs)[1],ncol=d)
	}
	
#allranges=c(0,0)
	for (i in seq(1,dim(coeffs)[1],1)) {
		if(!hasArg(allfits)){
			allfits[[i]]=list()
		}
		allfits[[i]]$x=x
		allfits[[i]]$xall=x
		allfits[[i]]$d=d
		allfits[[i]]$n=n
		allfits[[i]]$names=names
		allfits[[i]]$getriskestimate=getrisk1
		allfits[[i]]$beta=beta[i,]
		allfits[[i]]$beta0=beta0[i]
		allfits[[i]]$getfuncform=getfuncform1
		allfits[[i]]$vartypes=vartypes
		allfits[[i]]$levelnames=levelnames
		
		# if only categorical x are present in the model, plot the model as a scoring system
		if (all(allfits[[i]]$vartypes=="cat")&exists("x2")) {
			# define the cutoff-values and points for main effects
			indfactors=which(attr(x2$terms,"dataClasses")=="factor")-1
			indfactors=indfactors[-1]
			indlogicals=which(attr(x2$terms,"dataClasses")=="logical")-1
			indother=c(1:allfits[[i]]$d)
			if(length(indfactors)+length(indlogicals)>0){
				indother=indother[-c(indfactors,indlogicals)]
			}
			cutoffs=list()
			points=list()
			alllevels=numeric(length(allfits[[i]]$vartypes))
			for (j in seq(1,length(allfits[[i]]$vartypes),1)) {
				alllevels[j]=length(grep(names[j],colnames(coeffs))) # indicates the number of coefficients for variable i
			}
			cumlevels=cumsum(alllevels)
			if (length(indfactors)>0) {
				for (j in seq(1,length(indfactors))) {
					cutoffs[[indfactors[j]]]=c(1:length(grep(names[indfactors[j]],colnames(coeffs))))
					points[[indfactors[j]]]=c(0,allfits[[i]]$beta[seq(ifelse(indfactors[j]==1,1,cumlevels[indfactors[j]-1]+1),cumlevels[indfactors[j]],1)])
				}
			}
			if (length(indlogicals)>0) {
				for (j in seq(1,length(indlogicals))) {
					cutoffs[[indlogicals[j]]]=c(1)
					points[[indlogicals[j]]]=c(0,allfits[[i]]$beta[seq(ifelse(indlogicals[j]==1,1,cumlevels[indlogicals[j]-1]+1),cumlevels[indlogicals[j]],1)])
				}
			}
			if (length(indother)>0) {
				for (j in seq(1,length(indother))) {
					cutoffs[[indother[j]]]=c(1)
					points[[indother[j]]]=c(0,allfits[[i]]$beta[seq(ifelse(indother[j]==1,1,cumlevels[indother[j]-1]+1),cumlevels[indother[j]],1)+1])
				}
			}
			
			# generate list of cutoffs and points for the interactions
			
			allfits[[i]]$points=points
			allfits[[i]]$cutoffs=cutoffs
			
		} 

		fzeroALL=rbind(fzeroALL,precolplot(allfits[[i]], filename, coloroptions,zerolevel,
						risklabel,xmin,xmax,adverse,obs,q5,q95)$frange)
		
	
		
		lp[,i]=apply(allfits[[i]]$getfuncform(allfits[[i]]),1,sum)
		if(hasArg(obs)){
			lpobs[,i]=apply(allfits[[i]]$getfuncform(allfits[[i]],obs=obs),1,sum)
		}
		
		allfits[[i]]=precolplot(allfits[[i]], filename=names(allfits)[i], coloroptions,zerolevel,
				risklabel=risklabel[i],xmin,xmax,adverse,obs,q5,q95)
		
		allfmin[i,]=allfits[[i]]$fmin
		
		if("points"%in%names(allfits[[i]])){
			allcolorsALL=c(allcolorsALL,allfits[[i]]$allcolors)
		}
		
	}
	
	if("points"%in%names(allfits[[1]])){
		allcolorsALL=sort(unique(allcolorsALL))
		printcolor=logical(length=length(allcolorsALL))
		
		if (min(allcolorsALL)<0 & max(allcolorsALL)>0) {
			switch(coloroptions,
					# coloroption=1
					{printcolor=(abs(allcolorsALL)>0.65*max(abs(allcolorsALL)))},
					# coloroption=2
					{printcolor=(allcolorsALL<mean(allcolorsALL))},
					# coloroption=3
					{printcolor=(abs(allcolorsALL)>0.65*max(abs(allcolorsALL)))}
			)
		}
		if (min(allcolorsALL)>=0 & max(allcolorsALL)>=0) {
			switch(coloroptions,
					# coloroption=1
					{printcolor=(allcolorsALL<0.25*max(allcolorsALL))|(allcolorsALL>0.75*max(allcolorsALL))},
					# coloroption=2
					{	printcolor=(allcolorsALL<mean(allcolorsALL))},
					# coloroption=3
					{printcolor=(allcolorsALL>mean(allcolorsALL))}
			)
		}
	}
	
	
# adapt the color map in case of diverging colormap
	if (coloroptions==3) {
		if ("points"%in%names(allfits[[1]])) { # score model
			cols1=c(fields::designer.colors(col=c(rgb(5,113,176,maxColorValue = 255),rgb(209,229,240,maxColorValue = 255)),n=sum(allcolorsALL<0)),
					rgb(247,247,247,maxColorValue = 255),
					fields::designer.colors(col=c(rgb(253,219,199,maxColorValue = 255),
									rgb(202,0,32,maxColorValue = 255)),n=sum(allcolorsALL>0)))
		} else {# not a score model
		cols1=c(fields::designer.colors(col=c(rgb(5,113,176,maxColorValue = 255),rgb(209,229,240,maxColorValue = 255)),n=round(abs(min(fzeroALL))/abs(max(fzeroALL)-min(fzeroALL))*100)),
				rgb(247,247,247,maxColorValue = 255),
				fields::designer.colors(col=c(rgb(253,219,199,maxColorValue = 255),
								rgb(202,0,32,maxColorValue = 255)),n=round(abs(max(fzeroALL))/abs(max(fzeroALL)-min(fzeroALL))*100)))
		}
		if (!zerolevel%in%c("mean","median")) {
			
			cols2=fields::designer.colors(col=c(rgb(5,113,176,maxColorValue = 255),rgb(247,247,247,maxColorValue = 255),
							rgb(202,0,32,maxColorValue = 255)))
			cols2b=fields::designer.colors(col=c(rgb(202,0,32,maxColorValue = 255),rgb(247,247,247,maxColorValue = 255),
							rgb(5,113,176,maxColorValue = 255))) # reversed scale
		}
	}
	
	for (i in seq(1,dim(coeffs)[1],1)) {
		
		allfits[[i]]$frange=range(fzeroALL)
		allfits[[i]]$fzeroALL=fzeroALL
		allfits[[i]]$multinom=TRUE
		if("points"%in%names(allfits[[1]])){
			allfits[[i]]$allcolors=allcolorsALL
			allfits[[i]]$printcolor=printcolor
		}
		
		allfits[[i]]$risklabel=risklabel[i]
		if(hasArg(obs)){
			allfits[[i]]$obs=obs
		}
		if (coloroptions==3) {
			allfits[[i]]$cols1=cols1
			allfits[[i]]$cols2=cols2
			allfits[[i]]$cols2b=cols2b
		}
		
		
	}
	return<-allfits
}

generatePref<-function(x,coeffs,allfits,obs,risklabel){
	################################
	# generate plot for pref
	################################
	n=dim(x)[1]
	d=dim(x)[2]
	
	beta0=coeffs[,1]
	beta=coeffs[,-1]
	
	lp=matrix(0,nrow=n,ncol=dim(coeffs)[1])
	if(hasArg(obs)){
		lpobs=matrix(0,nrow=1,ncol=dim(coeffs)[1])
	} else {
		lpobs=c()
	}
		
	if("interactions"%in%names(allfits[[1]])){
		allfmin=matrix(NaN,nrow=dim(coeffs)[1],ncol=d+dim(allfits[[1]]$interactions)[1])
	} else {
		allfmin=matrix(NaN,nrow=dim(coeffs)[1],ncol=d)
	}
	
	for (i in seq(1,dim(coeffs)[1],1)) {
		allfmin[i,]=allfits[[i]]$fmin
		lp[,i]=apply(allfits[[i]]$getfuncform(allfits[[i]]),1,sum)
		if(hasArg(obs)){
			lpobs[,i]=apply(allfits[[i]]$getfuncform(allfits[[i]],obs=obs),1,sum)
		}
	}
	
	sumfmin=apply(allfmin,1,sum)
	
	i=dim(coeffs)[1]+1
	iref=i
	vartypes2=rep("cont",dim(coeffs)[1])
	levels2=(rep(list(""),dim(coeffs)[1]))
	levelvalues2=(rep(list(""),dim(coeffs)[1]))
	levelnames2<-list(levels2, levelvalues2)
	
	
	# there is no use in plotting values for which lp+beta0>3 (this yields large scores and the resulting risk is a plateau)
# the corresponding color code = 20 -> restrict color range to 20
	lpthreshold=3
	lpbeta=lp+matrix(1,nrow=n,ncol=1)%*%beta0
	ind=c()
	for (k in seq(1,dim(lp)[2],1)) {
		ind=c(ind,which(lpbeta[,k]>3))
	}
	
	
	lptemp=lp
	
	allfits[[i]]$x=lptemp-matrix(1,nrow=dim(lptemp)[1],ncol=1)%*%sumfmin
	allfits[[i]]$xall=lp-matrix(1,nrow=dim(lp)[1],ncol=1)%*%sumfmin
	
	allfits[[i]]$d=dim(allfits[[i]]$x)[2]
	allfits[[i]]$n=dim(allfits[[i]]$x)[1]
	allfits[[i]]$beta0=beta0+sumfmin
	allfits[[i]]$names=paste0("lin.pred.",row.names(coeffs))
	allfits[[i]]$getfuncform=getfuncformMLR2
	allfits[[i]]$getriskestimate=getriskMLR2
	allfits[[i]]$vartypes=vartypes2
	allfits[[i]]$levelnames=levelnames2
	allfits[[i]]$risklabel=risklabel[i]
	allfits[[i]]$multinomclass=TRUE
	allfits[[i]]$colplotpref=TRUE
	
	if(hasArg(obs)){
		allfits[[i]]$obs=lpobs-sumfmin
	}
	return<-allfits
	
}

generatePother<-function(x,coeffs,allfits,obs,filename,coloroptions=2, zerolevel="zero",risklabel,adverse=FALSE,q5,q95,outcomelevels){
	################################
	# generate plots for p's (other than ref)
	################################
	n=dim(x)[1]
	d=dim(x)[2]
	
	beta0=coeffs[,1]
	beta=coeffs[,-1]
	
	lp=matrix(0,nrow=n,ncol=dim(coeffs)[1])
	if(hasArg(obs)){
		lpobs=matrix(0,nrow=1,ncol=dim(coeffs)[1])
	} else {
		lpobs=c()
	}
	
	if("interactions"%in%names(allfits[[1]])){
		allfmin=matrix(NaN,nrow=dim(coeffs)[1],ncol=d+dim(allfits[[1]]$interactions)[1])
	} else {
		allfmin=matrix(NaN,nrow=dim(coeffs)[1],ncol=d)
	}
	
	for (i in seq(1,dim(coeffs)[1],1)) {
		allfmin[i,]=allfits[[i]]$fmin
		lp[,i]=apply(allfits[[i]]$getfuncform(allfits[[i]]),1,sum)
		if(hasArg(obs)){
			lpobs[,i]=apply(allfits[[i]]$getfuncform(allfits[[i]],obs=obs),1,sum)
		}
	}
	
	sumfmin=apply(allfmin,1,sum)
	
	# there is no use in plotting values for which lp+beta0>3 (this yields large scores and the resulting risk is a plateau)
# the corresponding color code = 20 -> restrict color range to 20
	lpthreshold=3
	lpbeta=lp+matrix(1,nrow=n,ncol=1)%*%beta0
	ind=c()
	for (k in seq(1,dim(lp)[2],1)) {
		ind=c(ind,which(lpbeta[,k]>3))
	}
	
	lptemp=lp
	
	fzeroALL=c()
	
	vartypes3=rep("cont",2)
	levels3=(rep(list(""),2))
	levelvalues3=(rep(list(""),2))
	levelnames3<-list(levels3, levelvalues3)
	
	iref=dim(coeffs)[1]+1
	
	for (i in dim(coeffs)[1]+1+seq(1,dim(coeffs)[1],1)) {
		i2=i-dim(coeffs)[1]-1
		
		allfits[[i]]$vartypes=vartypes3
		allfits[[i]]$levelnames=levelnames3
		allfits[[i]]$risklabel=risklabel[i]
		allfits[[i]]$multinomclass=TRUE
		
		allfits[[i]]$pref=getriskMLR2(rowSums(getfuncformMLR2(allfits[[iref]])),allfits[[iref]],rowSums(getfuncformMLR2(allfits[[iref]])))[[2]]
		allfits[[i]]$thislp=lptemp[,i2]-sumfmin[i2]
		allfits[[i]]$x=cbind(allfits[[i]]$pref,allfits[[i]]$thislp)
		allfits[[i]]$xall=cbind(allfits[[iref]]$getriskestimate(rowSums(getfuncformMLR2(allfits[[iref]])),allfits[[iref]],rowSums(getfuncformMLR2(allfits[[iref]],allfits[[iref]]$xall)))[[2]],
				lp[,i2]-sumfmin[i2])
		if(hasArg(obs)){
			pref_obs=getriskMLR2(rowSums(getfuncformMLR2(allfits[[iref]])),allfits[[iref]],rowSums(getfuncformMLR2(allfits[[iref]],obs=lpobs-sumfmin)))[[2]]
			thislp_obs=lpobs[,i2]-sumfmin[i2]
			allfits[[i]]$obs=cbind(pref_obs,thislp_obs)
		} else {
			allfits[[i]]$obs=c()
		}
		
		allfits[[i]]$names=c(paste0("p.",outcomelevels[1]),paste0("lin.pred.",outcomelevels[i2+1]))
		allfits[[i]]$d=dim(allfits[[i]]$x)[2]
		allfits[[i]]$n=dim(allfits[[i]]$x)[1]
		allfits[[i]]$getfuncform=getfuncformMLR3
		allfits[[i]]$getriskestimate=getriskMLR3
		allfits[[i]]$beta0=beta0[i2]+sumfmin[i2]
		
		fzeroALL=rbind(fzeroALL,precolplot(allfits[[i]], filename, coloroptions,zerolevel,
						risklabel,,,adverse,obs=allfits[[i]]$obs,q5,q95)$fzero)
		
		allfits[[i]]=precolplot(allfits[[i]], filename=names(allfits)[i], coloroptions,zerolevel,
				risklabel=risklabel[i],,,adverse,obs=allfits[[i]]$obs,q5,q95)
	}
	
	# adapt the color map in case of diverging colormap
	if (coloroptions==3) {
		# not a score model
		cols1=c(fields::designer.colors(col=c(rgb(5,113,176,maxColorValue = 255),rgb(209,229,240,maxColorValue = 255)),n=round(abs(min(fzeroALL))/abs(max(fzeroALL)-min(fzeroALL))*100)),
				rgb(247,247,247,maxColorValue = 255),
				fields::designer.colors(col=c(rgb(253,219,199,maxColorValue = 255),
								rgb(202,0,32,maxColorValue = 255)),n=round(abs(max(fzeroALL))/abs(max(fzeroALL)-min(fzeroALL))*100)))
		
		if (!zerolevel%in%c("mean","median")) {
			
			cols2=fields::designer.colors(col=c(rgb(5,113,176,maxColorValue = 255),rgb(247,247,247,maxColorValue = 255),
							rgb(202,0,32,maxColorValue = 255)))
			cols2b=fields::designer.colors(col=c(rgb(202,0,32,maxColorValue = 255),rgb(247,247,247,maxColorValue = 255),
							rgb(5,113,176,maxColorValue = 255))) # reversed scale
		}
	}
	
	for (i in dim(coeffs)[1]+1+seq(1,dim(coeffs)[1],1)) {
		allfits[[i]]$frange=range(fzeroALL)
		allfits[[i]]$fzeroALL=fzeroALL
		allfits[[i]]$multinom=TRUE
		
		if (coloroptions==3) {
			allfits[[i]]$cols1=cols1
			allfits[[i]]$cols2=cols2
			allfits[[i]]$cols2b=cols2b
		}
		
	}
	
	return<-allfits
	
}
