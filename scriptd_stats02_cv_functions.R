#!/usr/bin/env Rscript

compute.acc = function(y,yhat){
	#print("----------")
	#print(length(y))
	#print(length(yhat))
	#print("----------")
	
	y = as.numeric(y)
	yhat = as.numeric(yhat)

	#print(sum(y==yhat))
	#print(length(y))
	acc<-sum(y==yhat)/length(y)
	
	ylevel = unique(y)
	
	if (length(ylevel)==2){
		sensi<-sum(y==yhat & y==ylevel[2])/sum(y==ylevel[2])
		speci<-sum(y==yhat & y==ylevel[1])/sum(y==ylevel[1])
	}
	else if (max(yhat)==max(y)){
		sensi<-sum(y==yhat & y==ylevel)/sum(y==ylevel)
		speci<-NaN
	} else{
		speci<-sum(y==yhat & y==ylevel)/sum(y==ylevel)
		sensi<-NaN
	}
	temp<-c(acc, sensi, speci)
	return(temp)
}

remap.factor = function(f,min=0,max=1){
	f.remap = f
	f.remap[f==max(f)]=max
	f.remap[f==min(f)]=min
	return(f.remap)
}


library(rda)
rda.cv.fun = function(x, y, k){
	
	#sort data according to y so that we have ballanced value in each group:
	x = x[order(y),]
	y = sort(y)
	y = remap.factor(y)

	num.sample = nrow(x)
	cv.k = sample(rep(1:k, each = ceiling(num.sample/k)))[1:num.sample]
	
	test.result=data.frame(acc=rep(NA,k), sensi=rep(NA,k), speci=rep(NA,k))
	train.result=data.frame(acc=rep(NA,k), sensi=rep(NA,k), speci=rep(NA,k))
	
	for (i in 1:k){	
		x.train = t(x[cv.k!=i,])
		x.test = t(x[cv.k==i,])
		y.train = y[cv.k!=i]
		y.test = y[cv.k==i]
		
		print(dim(x.train))
		print(table(y.train))

		fit<-rda(x.train, y.train)
		cv.fit<-rda.cv(fit, x.train, y.train)
		print(cv.fit)
		
		fit2<-rda(x.train, y.train,alpha = cv.fit$alpha, delta=cv.fit$delta)
		y.pred<-predict(fit2, x=x.train ,y=y.train, xnew=x.test, alpha=0, delta=0.333)
		print("y.pred")
		print(cbind(y.pred, y.test))
		
		y.pred.train<-predict(fit2, x=x.train ,y=y.train, xnew=x.train, alpha=0, delta=0.333)
		
#		print("prediction of testing data:")
#		print(table(y.test, y.pred))
#		print("prediction of training data:")
#		print(table(y.train, y.pred.train))
		
		result.test = compute.acc(y.test, y.pred)
		result.train = compute.acc(y.train, y.pred.train)

		test.result[i,] = result.test
		train.result[i,] = result.train
	}
	return(list(test.result, train.result))
}


library(SIS)
scad.cv.fun = function(x, y, k){
	
	#sort data according to y so that we have ballanced value in each group:
	x = x[order(y),]
	y = sort(y)
	y = remap.factor(y)

	num.sample = nrow(x)
	cv.k = sample(rep(1:k, each = ceiling(num.sample/k)))[1:num.sample]
	
	test.result=data.frame(acc=rep(NA,k), sensi=rep(NA,k), speci=rep(NA,k))
	train.result=data.frame(acc=rep(NA,k), sensi=rep(NA,k), speci=rep(NA,k))
	
	for (i in 1:k){	
		x.train = x[cv.k!=i,]
		x.test = x[cv.k==i,]
		y.train = y[cv.k!=i]
		y.test = y[cv.k==i]
	
		#cv.fit = cv.ncvreg(x.train, y.train, seed=123, family="binomial")
		#cv.fit<-SIS(x.train, y.train, family="binomial", penalty="SCAD", tune="cv", nfolds=10, type.measure="deviance", seed=123)
		#cv.fit<-SIS(x.train, y.train, family="binomial", penalty="SCAD", tune="cv", nfolds=10, type.measure="deviance", seed=123)
		cv.fit<-SIS(x.train, y.train, family="binomial", penalty="MCP", tune="cv", nfolds=10, type.measure="deviance", seed=123)
			
		y.prob = predict(cv.fit, x.test, type="response")
		print("y.prob")
		y.pred = rep(NA, length(y.prob))
		y.pred[y.prob>=.5] = 1
		y.pred[y.prob<.5] = 0
		print(cbind(y.prob, y.pred, y.test))
		
		y.prob.train = predict(cv.fit, x.train, type="response")
		y.pred.train = rep(NA, length(y.prob.train))
		y.pred.train[y.prob.train>=.5] = 1
		y.pred.train[y.prob.train<.5] = 0
		
#		print("prediction of testing data:")
#		print(table(y.test, y.pred))
#		print("prediction of training data:")
#		print(table(y.train, y.pred.train))
		
		result.test = compute.acc(y.test, y.pred)
		result.train = compute.acc(y.train, y.pred.train)

		test.result[i,] = result.test
		train.result[i,] = result.train
	}
	return(list(test.result, train.result))
}


