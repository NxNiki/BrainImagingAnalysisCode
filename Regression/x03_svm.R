#!/usr/bin/env Rscript

rm(list=ls())
graphics.off()

source("x02_read_features.R")

#brain.feature = scale(cbind(fsl.vbm, alff, reho, label.fa, label.md, tract.fa, tract.md))
#brain.feature = scale(cbind(fsl.vbm.pca, alff.pca, reho.pca, label.fa.pca, label.md.pca, tract.fa.pca, tract.md.pca))
#brain.feature = scale(cbind(label.md.pca, tract.md.pca,falff.pca)) 
brain.feature = scale(cbind(label.md.pca, tract.md.pca))

df.all = cbind(subject.info[,-1], brain.feature)

svm.cv.fun = function(dat, k, cost.seq){
	
	#sort data according to y so that we have ballanced value in each group:
	dat = dat[order(dat$y),]

	num.sample = nrow(dat)
	cv.k = sample(rep(1:k, each = ceiling(num.sample/k)))[1:num.sample]
	test.error=rep(NA, k)
	train.error=rep(NA, k)
	for (i in 1:k){	
		dat.train = dat[cv.k!=i,]
		dat.test = dat[cv.k==i,]
	
		tune.svm = tune(svm, y~., data = dat.train, kernel = "linear", ranges = list(cost=cost.seq))
		print(summary(tune.svm$best.model))
		
		svm.fit = svm(y~., dat.train, kernel = "linear", cost = tune.svm$best.parameters$cost)
		y.pred = predict(svm.fit, dat.test)
		y.pred.train = predict(svm.fit, dat.train)
		
		print(table(dat.test$y, y.pred))
		print("prediction of training data:")
		print(table(dat.train$y, y.pred.train))
		
		test.error[i] = mean(dat.test$y != y.pred)
		train.error[i] = mean(dat.train$y != y.pred.train)
	}
	return(list(test.error, train.error))
}
# svm binomial regression:

cost.seq = 10^seq(2, -3, length = 20)
k=5
error = data.frame(cv = c(1:k))
train.error = data.frame(cv = c(1:k))
library(e1071)

## ---------------------select data for hc and trauma:---------------------

df.hc.trauma = df.all[df.all$ptsd==0|df.all$ptsd==1,]
print("dimension for hc and trauma dataset")
print(dim(df.hc.trauma))
print(table(df.hc.trauma$ptsd))

x = model.matrix(df.hc.trauma$ptsd~., df.hc.trauma)
y = as.factor(df.hc.trauma$ptsd)
dat = data.frame(x = x, y = y)

set.seed(111)

svm.cv.result = svm.cv.fun(dat, k, cost.seq)
print(svm.cv.result)
error$hc.trauma = svm.cv.result[[1]]
train.error$hc.trauma = svm.cv.result[[2]]

##--------------------- select data for hc and ptsd: -------------------------------------- 

df.hc.ptsd = df.all[df.all$ptsd==0|df.all$ptsd==2,]
print("dimension for hc and ptsd dataset")
print(dim(df.hc.ptsd))
print(table(df.hc.ptsd$ptsd))

x = model.matrix(df.hc.ptsd$ptsd~., df.hc.ptsd)
y = as.factor(df.hc.ptsd$ptsd)
dat = data.frame(x = x, y = y)

set.seed(123)
svm.cv.result = svm.cv.fun(dat, k, cost.seq)
error$hc.ptsd = svm.cv.result[[1]]
train.error$hc.ptsd = svm.cv.result[[2]]

##--------------------- select data for trauma and ptsd:---------------------

df.trauma.ptsd = df.all[df.all$ptsd==1|df.all$ptsd==2,]
print("dimension for hc and trauma data set")
print(dim(df.trauma.ptsd))
print(table(df.trauma.ptsd$ptsd))

x = model.matrix(df.trauma.ptsd$ptsd~., df.trauma.ptsd)
y = as.factor(df.trauma.ptsd$ptsd)
dat = data.frame(x = x, y = y)

set.seed(333)
svm.cv.result = svm.cv.fun(dat, k, cost.seq)
error$ptsd.trauma = svm.cv.result[[1]]
train.error$ptsd.truama = svm.cv.result[[2]]

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
#error = svm.cv.fun(dat, k, cost.seq)
#print(error)
#

print(error)
print(colMeans(error))
print(train.error)
print(colMeans(train.error))
