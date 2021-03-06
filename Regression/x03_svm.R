#!/usr/bin/env Rscript

rm(list=ls())
graphics.off()

source("x02_read_features.R")

#brain.feature = scale(cbind(fsl.vbm, alff, reho, label.fa, label.md, tract.fa, tract.md))
#brain.feature = scale(cbind(fsl.vbm.pca, alff.pca, reho.pca, label.fa.pca, label.md.pca, tract.fa.pca, tract.md.pca))
#brain.feature = scale(cbind(label.md.pca, tract.md.pca,falff.pca)) 
brain.feature = scale(label.md)

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
test.result = data.frame(cv = c(1:k))
train.result = data.frame(cv = c(1:k))
library(e1071)

## ---------------------select data for hc and trauma:---------------------

df.hc.trauma = df.all[df.all$ptsd==0|df.all$ptsd==1,]
print("dimension for hc and trauma dataset")
print(dim(df.hc.trauma))
print(table(df.hc.trauma$ptsd))

x = model.matrix(df.hc.trauma$ptsd~., df.hc.trauma)
y = as.factor(df.hc.trauma$ptsd)
set.seed(111)

svm.cv.result = svm.cv.fun(x, y, k, cost.seq)
sapply(1:ncol(svm.cv.result[[1]]), function(i) test.result[,paste0('hc.trauma','.',names(svm.cv.result[[1]])[i])] <<- svm.cv.result[[1]][,i]) 
sapply(1:ncol(svm.cv.result[[2]]), function(i) train.result[,paste0('hc.trauma','.',names(svm.cv.result[[2]])[i])] <<- svm.cv.result[[2]][,i]) 
#test.result$hc.trauma = svm.cv.result[[1]]
#train.result$hc.trauma = svm.cv.result[[2]]

##--------------------- select data for hc and ptsd: -------------------------------------- 

df.hc.ptsd = df.all[df.all$ptsd==0|df.all$ptsd==2,]
print("dimension for hc and ptsd dataset")
print(dim(df.hc.ptsd))
print(table(df.hc.ptsd$ptsd))

x = model.matrix(df.hc.ptsd$ptsd~., df.hc.ptsd)
y = as.factor(df.hc.ptsd$ptsd)
set.seed(123)

svm.cv.result = svm.cv.fun(x, y, k, cost.seq)
sapply(1:ncol(svm.cv.result[[1]]), function(i) test.result[,paste0('hc.ptsd','.',names(svm.cv.result[[1]])[i])] <<- svm.cv.result[[1]][,i]) 
sapply(1:ncol(svm.cv.result[[2]]), function(i) train.result[,paste0('hc.ptsd','.',names(svm.cv.result[[2]])[i])] <<- svm.cv.result[[2]][,i]) 
#test.result$hc.ptsd = svm.cv.result[[1]]
#train.result$hc.ptsd = svm.cv.result[[2]]

##--------------------- select data for trauma and ptsd:---------------------

df.trauma.ptsd = df.all[df.all$ptsd==1|df.all$ptsd==2,]
print("dimension for hc and trauma data set")
print(dim(df.trauma.ptsd))
print(table(df.trauma.ptsd$ptsd))

x = model.matrix(df.trauma.ptsd$ptsd~., df.trauma.ptsd)
y = as.factor(df.trauma.ptsd$ptsd)
set.seed(333)

svm.cv.result = svm.cv.fun(x, y, k, cost.seq)
sapply(1:ncol(svm.cv.result[[1]]), function(i) test.result[,paste0('trauma.ptsd','.',names(svm.cv.result[[1]])[i])] <<- svm.cv.result[[1]][,i]) 
sapply(1:ncol(svm.cv.result[[1]]), function(i) train.result[,paste0('trauma.ptsd','.',names(svm.cv.result[[2]])[i])] <<- svm.cv.result[[2]][,i]) 
#test.result$ptsd.trauma = svm.cv.result[[1]]
#train.result$ptsd.truama = svm.cv.result[[2]]

# ---------------------select data for ptsd and others:---------------------
#df.ptsd.other = df.all
#df.ptsd.other$ptsd = df.all$ptsd==2
#print(table(df.ptsd.other$ptsd))
#
#x = model.matrix(df.ptsd.other$ptsd~., df.ptsd.other)
#y = as.factor(df.ptsd.other$ptsd)
#dat = data.frame(x = x, y = y)
#
#set.seed(1)
#test.result = svm.cv.fun(dat, k, cost.seq)
#print(test.result)
#

print(test.result)
print(colMeans(test.result))
print(train.result)
print(colMeans(train.result))
