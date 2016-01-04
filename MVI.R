###MISSING VALUE IMPUTATION


args <- commandArgs(TRUE)
dataSource <- args[1]
MVImode <- args[2]
outputName <- args[3]

X <- read.csv(dataSource,header=T,row.names=1)

if(MVImode=="SV"){
	X[,-1][is.na(X[,-1])] <- min(X[,-1],na.rm=T)/2 ##SMV

}else if(MVImode=="MN"){
	meanrep <- function(mat) apply(mat,2,mean,na.rm=T) ###MEAN REP
	meanVec <- meanrep(X[,-1])
	for(i in 1:(ncol(X)-1))
		X[,i+1][is.na(X[,i+1])] <- meanVec[i]	

}else if(MVImode=="MD"){
	medianrep <- function(mat) apply(mat,2,median,na.rm=T)
	medianVec <- medianrep(X[,-1])
	for(i in 1:(ncol(X)-1))
		X[,i+1][is.na(X[,i+1])] <- medianVec[i]

}else if(MVImode=="KNN"){
	Class <- X[,1]
	classname <- colnames(X)[1]
	library(impute)
	obj <- impute.knn(as.matrix(X[,-1]))
	X <- as.data.frame(obj$data)
	X <- cbind(Class,X)
	colnames(X)[1] <- classname

}else if(MVImode=="BPCA"){
	library(pcaMethods)
	pcaOb <- pcaMethods::pca(X[,-1],method="bpca",scale="none")
	X[,-1] <- pcaOb@completeObs
	X[X<0] <- min(X[,-1][X[,-1]>0])	##GUARD AGAINST NEGATIVE VALUES

}else {
	stop("Error occurred. MVI was not selected")
}

write.csv(X,outputName)


