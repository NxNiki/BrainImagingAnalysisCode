#!/usr/bin/env Rscript

rm(list=ls())
graphics.off()

source("scriptd_stats01_read_features.R")

#brain.feature = scale(cbind(fsl.vbm, alff, reho, label.fa, tract.fa, tract.md))
#brain.feature = scale(cbind(label.md.pca, tract.md.pca,falff.pca)) 
brain.feature = scale(cbind(fsl.vbm))

df.all = cbind(subject.info[,-1], brain.feature)

compute.acc = function(y,yhat){
	acc<-length(which(y==yhat))/length(y)
	sensi<-length(which(y==yhat & y==1))/length(which(y==1))
	speci<-length(which(y==yhat & y==0))/length(which(y==0))
	temp<-c(acc, sensi, speci)
	return(temp)
}

svm.cv.fun = function(x, y, k, cost.seq){
	
	#sort data according to y so that we have ballanced value in each group:
	x = x[order(y),]
	y = sort(y)

	num.sample = nrow(x)
	cv.k = sample(rep(1:k, each = ceiling(num.sample/k)))[1:num.sample]
	
	test.result=data.frame(acc=rep(NA,k), sensi=rep(NA,k), speci=rep(NA,k))
	train.result=data.frame(acc=rep(NA,k), sensi=rep(NA,k), speci=rep(NA,k))
	
	for (i in 1:k){	
		x.train = x[cv.k!=i,]
		x.test = x[cv.k==i,]

		dat.train = data.frame(x=x.train, y=y[cv.k!=i])
		dat.test = data.frame(x=x.test, y=y[cv.k==i])
	
		tune.svm = tune(svm, y~., data = dat.train, kernel = "linear", ranges = list(cost=cost.seq))
		print(summary(tune.svm$best.model))
		
		svm.fit = svm(y~., dat.train, kernel = "linear", cost = tune.svm$best.parameters$cost)
		y.pred = predict(svm.fit, dat.test)
		y.pred.train = predict(svm.fit, dat.train)
		
		print("prediction of testing data:")
		print(table(dat.test$y, y.pred))
		print("prediction of training data:")
		print(table(dat.train$y, y.pred.train))
		
		result.test = compute.acc(dat.test$y, y.pred)
		result.train = compute.acc(dat.train$y, y.pred.train)

		test.result[i,] = result.test
		train.result[i,] = result.train
	}
	return(list(test.result, train.result))
}
# svm binomial regression:

cost.seq = 10^seq(2, -3, length = 20)
k=5
error = data.frame(cv = c(1:k))
train.error = data.frame(cv = c(1:k))
library(e1071)

## ---------------------select data for loc 0 and 1 :---------------------

df.hc.trauma = df.all[df.all$group_ind==0|df.all$group_ind==1,]
print("dimension for loc dataset")
print(dim(df.hc.trauma))
print(table(df.hc.trauma$group_ind))

x = model.matrix(df.hc.trauma$group_ind~., df.hc.trauma)
y = as.factor(df.hc.trauma$group_ind)

set.seed(333)

svm.cv.result = svm.cv.fun(x, y, k, cost.seq)
result = svm.cv.result[[1]]
train.result = svm.cv.result[[2]]

print(result)
print(colMeans(result))
print(train.result)
print(colMeans(train.result))
