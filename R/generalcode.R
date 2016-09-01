
check_data<-function(x,type) {
	
	# the following attributes are mandatory:
	if (!"d" %in% names(x)) {
		stop("The dimension of the xset is not defined (x$d)")
	}
	if (!"names" %in% names(x)) {
		stop("The variable names are not defined (x$names)")
	}
	if (!"n" %in% names(x)) {
		stop("The number of patients are not defined (x$n)")
	}
	if (!"x" %in% names(x)) {
		stop("The matrix of observations is not defined (x$x)")
	}
	if (!"levelnames" %in% names(x)) {
		stop("The values and names of categorical x are not defined (x$levelnames)")
	}
	if (!"vartypes" %in% names(x)) {
		stop("The variables types are not defined (please choose between 'cont' and 'cat') (x$vartypes)")
	}
	if (!"getfuncform" %in% names(x)) {
		stop("The function calculating the functional forms (= the contributions to the score for each input variable)
						is not defined (x$getfuncform)")
	}
	if (!"getriskestimate" %in% names(x)) {
		stop("The function calculating the risk estimate is not defined (x$getriskestimate)")
	}
	if (!type%in%c("cchart","ccchart")) {
		if (!"adverse" %in% names(x)) {
			stop("It is not indicated whether the score and the represented risk are in line. (x$adverse)")
		}
	}
	
	
	# check the correctness of the values of the attributes
	if (x$d!=dim(x$x)[2]) {
		stop("The given dimension x$d does not correspond to the dimension of the observation matrix x$x")
	}
	if (x$n!=dim(x$x)[1]) {
		stop("The given number of observations x$n does not correspond with the observation matrix x$x")
	}
	if (length(x$names)!=x$d) {
		stop("The length of the variable names does not correspond to the dimension of the x")
	}
	if (length(x$levelnames[[1]])!=x$d) {
		stop("The length of x$levelnames[[1]] does not correspond to the dimension of the x")
	}
	if (length(x$levelnames[[2]])!=x$d) {
		stop("The length of x$levelnames[[2]] does not correspond to the dimension of the x")
	}
	if (length(x$vartypes)!=x$d) {
		stop("The length of x$levelnames[[2]] does not correspond to the dimension of the x")
	}
		
	if (!type%in%c("cchart","ccchart")){
		if (!is.logical(x$adverse)) {
			stop("Adverse is not defined correctly.  It should be a logical.")
		}
		if (!x$coloroptions%in%c(1,2,3,4,5)) {
			stop("Something wrong with the chosen color options. Please adapt.")
		}
	}
		
	
	if(!x$zerolevel %in% c("zero","min","median","mean")){
		stop("The value of zerolevel is not in c('zero','min','median','mean').  Please adapt in order to continue.")
	}
	
	
	# additional checks
	
	if(length(x$levelnames[[1]])!=length(x$levelnames[[2]])){
		stop("The variable levelnames is not correct.  Please adapt in order to continue.")
	}
	if (dim(x$x)[2]!=length(x$levelnames[[1]])) {
		stop("The dimension of the given x set does not match with the length of levelnames.  Please adapt in order to continue.")
	}
	
	if ("q5"%in%names(x)) {
		if (length(x$q5)>0 & length(x$q5)!=x$d) {
			stop("The specified percentiles do not have the right number of predictors.  Please adapt in order to continue.")
		}
	}
	if ("q95"%in%names(x)) {
		if (length(x$q95)>0 & length(x$q95)!=x$d) {
			stop("The specified percentiles do not have the right number of predictors.  Please adapt in order to continue.")
		}
	}
	
	if (sum(is.element(x$vartypes,c("cat","cont")))!=x$d) {
		stop("The length of vartypes does not match the number of predictors.  Please adapt in order to continue.")
	}
	# return x object
	result<-x
}

convert_to_points<-function(x,cutoffs,points) {
	z=x;
	z[]=0;
	
	# some checks
	if(length(cutoffs)!=length(points)){
		stop("The length of cutoffs does not equal the length of points")
	}
	for (i in seq(along = length(cutoffs))) {
		if(length(cutoffs[[i]])!=length(points[[i]])-1){
			stop("The length of cutoffs[[i]] does not equal the length of points[[i]]-1")
		}
	}
	
	if (is.vector(x)) { # for one single input vector
		dimx=1
		
		for (i in seq(1:length(x))) {
			
			z[i]=z[i]+points[[i]][Hmisc::cut2(c(x[i],cutoffs[[i]]+1,cutoffs[[i]]-1),cuts=c(cutoffs[[i]],Inf))][1]
			
		}
		
	} else 
	{ # for a whole data set
		dimx=dim(x)[2]
		
		for (i in seq(along = c(1:dimx))) {
			
			temp=Hmisc::cut2(x[,i],cuts=c(cutoffs[[i]],Inf))
			z[,i]=z[,i]+points[[i]][temp]
			
		}
	}
	
	result=z
}



######################################
# Internal function: given cutoffs and points: allocate points to continuous values: interaction effects
# Input: 
#	x				data matrix
#	cutoffs			cutoff values at which the points change
#	points			points
#	
# Output:
#	z 				output vector
#######################################
convert_to_points_interactions<-function(x,icutoffs,ipoints, interactions) {
	# checks
	if (length(icutoffs)!=length(ipoints)) {
		stop("The cutoffs and points for the interaction effects are not correct.")
	}
	if (length(icutoffs)!=dim(interactions)[1]) {
		stop("The cutoffs do not correspond with the number of interactions.")
	}
	
	z=matrix(data=0,nrow=dim(x)[1],ncol=dim(interactions)[1]);
	
	if (is.vector(x)) { # for one single input vector
		dimint=dim(interactions)[1]
		
		for (i in seq(along = c(1:dimint))) {
			
			temp1=Hmisc::cut2(c(x[interactions[i,1]],icutoffs[[i]][[1]]+1,icutoffs[[i]][[1]]-1),cuts=c(icutoffs[[i]][[1]],Inf))[1] # row index
			temp2=Hmisc::cut2(c(x[interactions[i,2]],icutoffs[[i]][[2]]+1,icutoffs[[i]][[2]]-1),cuts=c(icutoffs[[i]][[2]],Inf))[1] # row index
			
			z[i]=z[i]+ipoints[[i]][temp1,temp2]
		}
		
	} else 
	{ # for a whole data set
		dimint=dim(interactions)[1]
		
		for (i in seq(along = c(1:dimint))) {
			
			temp1=Hmisc::cut2(x[,interactions[i,1]],cuts=c(icutoffs[[i]][[1]],Inf)) # row indices
			temp2=Hmisc::cut2(x[,interactions[i,2]],cuts=c(icutoffs[[i]][[2]],Inf)) # column indices
			
			ind = (as.numeric(temp2)-1)*dim(ipoints[[i]])[1] + as.numeric(temp1)
			
			z[,i]=z[,i]+ipoints[[i]][ind]
			
		}
	}
	
	result=z
}


meshgrid<-function(x,y) {
	m=length(x); n=length(y);
	X=matrix(rep(x,each=n),nrow=n);
	Y=matrix(rep(y,m),nrow=n)
	return=list(X,Y)
}

