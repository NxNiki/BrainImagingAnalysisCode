#!/usr/bin/env Rscript

# svm without t test to select features.
# Xin Niu Apr.25.2017


rm(list=ls())
graphics.off()

source("scriptd_stats01_read_feature.R")
#source("scriptd_stats01_read_pca_features.R")

brain.feature = scale(cbind(fsl.vbm, alff, reho, label.fa, tract.fa, tract.md))
#brain.feature = scale(cbind(label.md.pca, tract.md.pca,falff.pca)) 
#brain.feature = as.data.frame(scale(cbind(pc1)))
#brain.feature = cbind(pc1)

df.all = cbind(subject.info[,-1], brain.feature)

compute.acc = function(y,yhat){
	acc<-length(which(y==yhat))/length(y)

	ylevel = unique(y)
	sensi<-length(which(y==yhat & y==ylevel[2]))/length(which(y==ylevel[2]))
	speci<-length(which(y==yhat & y==ylevel[1]))/length(which(y==ylevel[1]))
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
		print(summary(tune.svm))
		print(summary(tune.svm$best.model))
		
		y.pred = predict(tune.svm$best.model, dat.test)
		y.pred.train = predict(tune.svm$best.model, dat.train)
		
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

## ---------------------select data for ptsd 0 and 1 :---------------------

df.subset = df.all[df.all$ptsd==0|df.all$ptsd==1,]
print("dimension for subset dataset")
print(dim(df.subset))
print(table(df.subset$ptsd))

x = model.matrix(df.subset$ptsd~., df.subset)
y = as.factor(df.subset$ptsd)

set.seed(333)

svm.cv.result = svm.cv.fun(x, y, k, cost.seq)
result = svm.cv.result[[1]]
train.result = svm.cv.result[[2]]

print(result)
print(colMeans(result))
print(train.result)
print(colMeans(train.result))

## ---------------------select data for ptsd 1 and 2 :---------------------

df.subset = df.all[df.all$ptsd==1|df.all$ptsd==2,]
print("dimension for subset dataset")
print(dim(df.subset))
print(table(df.subset$ptsd))

x = model.matrix(df.subset$ptsd~., df.subset)
y = as.factor(df.subset$ptsd)

set.seed(111)

svm.cv.result = svm.cv.fun(x, y, k, cost.seq)
result = svm.cv.result[[1]]
train.result = svm.cv.result[[2]]

print(result)
print(colMeans(result))
print(train.result)
print(colMeans(train.result))

## ---------------------select data for ptsd 0 and 2 :---------------------

df.subset = df.all[df.all$ptsd==0|df.all$ptsd==2,]
print("dimension for subset dataset")
print(dim(df.subset))
print(table(df.subset$ptsd))

x = model.matrix(df.subset$ptsd~., df.subset)
y = as.factor(df.subset$ptsd)

set.seed(111)

svm.cv.result = svm.cv.fun(x, y, k, cost.seq)
result = svm.cv.result[[1]]
train.result = svm.cv.result[[2]]

print(result)
print(colMeans(result))
print(train.result)
print(colMeans(train.result))
