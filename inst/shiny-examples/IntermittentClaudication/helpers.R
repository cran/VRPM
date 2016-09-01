# Project: visualizationU
# 
# Author: vvanbell
#
# Content: 	Function definitions for the Intermittent Claudication model
###############################################################################


######################################
# Generate data: Intermittent claudication  model
# Attention: 
#	The output (data) should be a list containing the following attributes:
#		d				dimension of the dataset
#		names			a vector of strings indicating the names of the input variables
#		n 				the number of observations in the dataset
#		x				a matrix of size nxd with the observations
#		levelnames 		a list containing two lists:  The first list is a list of names of the levels ("" is used for contiunuous variables), 
#						the second is a list of values of the corresponding levels ("" is used for contiunuous variables)
#		vartypes		a vector of strings indicating the type of the input variables ("cont" and "cat" are the only possibilities)
#		getfuncform		the function calculating the functional form for the input variables
#		getriskestimate	the function calculating the risk estimate from the score
#
# 	The following attributes can be specified but are not mandatory:
#		risklabel		text label to indicate which risk is modelled (default: "Estimated risk")
#		riskmax			the maximal risk to be presented (default: 1)
#		riskcutoff		the risk value at which the color indication changes (default: 0.5)
#		interactions	a matrix with 2 columns, each row contains the number of the first and second input involved in the interaction.  
#						If interactions are present the number of rows should equal d*(d-1)/2.
#						(default: no interactions: interactions=NaN)
#		q5				5th quantile of the data (default: NaN)
#		q95				95th quantile of the data (default: NaN)
#		minzero			If equal to 1 (default value) the minimal value of the functional forms for each predictor is set to 0. (default: 1)
# Output:
#	data 				list of the above attributes
#######################################
IC_data<-function() {
	names=c("sex","age","blood pressure","diabetes","cigarettes","cholesterol","CHD");
	
	d=length(names);
	n=400;
	
	# generate data
	library(stats)
	set.seed(45)
	x=matrix(1,n,d)
	x[,1]=round(runif(n));
	x[,2]=round(45+runif(n)*39);
	x[,3]=round(runif(n)*3);
	x[,4]=round(runif(n));
	x[,5]=round(runif(n)*32);
	x[,6]=150+round(runif(n)*150);
	x[,7]=round(runif(n));
	
	# Indicate levels for categorical variables and the corresponding predictor values
	levels=list(list('female','male'),'',list('normal','high', 'stage 1', 'stage 2'),list('no','yes'),'','',list('no','yes'))
	levelvalues=list(list(0,1),'',list(0,1,2,3),list(0,1),'','',list(0,1))
	levelnames<-list(levels, levelvalues)
	
	# Indicate the type for each covariate
	vartypes=c('cat','cont','cat','cat','cont','cont','cat')
	
	# define the function which converts to predictors into their functional form
	getfuncform<-funcform_IC_model
	
	# define the function which converts the score into a risk estimate
	getriskestimate<-getrisk_IC_model
	
	# define the label of the risk
	risklabel<-'Estimated risk on intermittent claudication'
	
	# define the maximal risk to be represented by this model
	riskmax=0.4
	
	# a high score represents a bad prognosis/diagnosis and will be represented in red
	highscore="bad"
	
	# indicate the predictor values for patient1
	patient1=c(1, 55, 1, 1, 6, 190, 0)
	
	# indicate the predictor values for patient2
	patient2=c(1, 80, 2, 1, 30, 289, 1)
	
	# calculate quantiles 5 and 95
	q5=apply(x,2,quantile,probs=0.05)
	q95=apply(x,2,quantile,probs=0.95)
	
	# the cutoff at which the risk changes color in the representation
	riskcutoff=0.25
	
	
	result<-list(names=names,d=d,n=n,x=x,levelnames=levelnames,vartypes=vartypes,getfuncform=getfuncform,
			getriskestimate=getriskestimate,risklabel=risklabel, riskmax=riskmax,highscore=highscore,
			patient1=patient1, patient2=patient2,riskcutoff=riskcutoff, q5=q5, q95=q95)
}


######################################
# Extract functional form for the intermittent claudication model from data:
# Input: 
#	data			data object
#	thispatient		a specific patient for whom to calculate the functional forms
#
# Output:
#	f 				value of the functional forms evaluated in the data x.  f should contain as many rows as data$x or 1 if thispatient is given.
#					f should be a data$n x data$d matrix in the case of no interactions.  
#					If interactions are present, f should have data$d*(data$d-1)/2 columns
#######################################
funcform_IC_model<-function(data, obs=c()) {
	
	
	if (length(obs)==0) {
		f=data$x;
		f[]=0;
		
		f[,1]=0.5033*data$x[,1];
		f[,2]=0.0372*data$x[,2];
		f[,3]=0.2621*(data$x[,3]==1)+0.4067*(data$x[,3]==2)+0.7977*(data$x[,3]==3);
		f[,4]=0.9503*data$x[,4];
		f[,5]=0.0314*data$x[,5];
		f[,6]=0.0048*data$x[,6];
		f[,7]=0.9939*data$x[,7];
		
	} else {
		f=obs;
		f[]=0;
		
		f[1]=0.5033*obs[1];
		f[2]=0.0372*obs[2];
		f[3]=0.2621*(obs[3]==1)+0.4067*(obs[3]==2)+0.7977*(obs[3]==3);
		f[4]=0.9503*obs[4];
		f[5]=0.0314*obs[5];
		f[6]=0.0048*obs[6];
		f[7]=0.9939*obs[7];
	}
	
	result<-f
}


######################################
# Extract risk estimate for the intermittent claudication model from data:
# Input: 
#	index			prognostic index for the whole dataset
#	data			data object
# 	thisindex		prognostic index for one specific patient
#	
# Output:
#	latentvar 		sequence of indexes within range(index).  If thisindex is provided, latentvar=thisindex
#	riskestimate	risk estimate corresponding to the values in latentvar
#######################################
getrisk_IC_model<-function(index,data,thisindex=c()) {
	
	beta0=-8.9152;
	
	if (length(thisindex)==0) {
		latentvar=seq(min(index),max(index),length=100)
	}
	else { # only one index to convert
		latentvar=thisindex
	}
	
	riskestimate=1./(exp(-(beta0+latentvar))+1);
	result<-list(latentvar,riskestimate)
}



