#!/usr/bin/env Rscript

rm(list=ls())
graphics.off()

source("x02_read_features.R")

#brain.feature = scale(cbind(fsl.vbm, alff, reho, label.fa, label.md, tract.fa, tract.md))
#brain.feature = cbind(fsl.vbm.pca[, 1:35], reho.pca[,1:35])
brain.feature = scale(fsl.vbm.pca[,1:50]) 

df.all = cbind(subject.info[,-1], brain.feature)

svm.cv.fun = function(dat, k, cost.seq){
	
	#sort data according to y so that we have ballanced value in each group:
	dat = dat[order(dat$y),]

	num.sample = nrow(dat)
	cv.k = sample(rep(1:k, each = ceiling(num.sample/k)))[1:num.sample]
	test.error=0
	for (i in 1:k){	
		dat.train = dat[cv.k!=i,]
		dat.test = dat[cv.k==i,]
	
		tune.svm = tune(svm, y~., data = dat.train, kernel = "linear", ranges = list(cost=cost.seq))
		print(summary(tune.svm$best.model))
		
		svm.fit = svm(y~., dat.train, kernel = "linear", cost = tune.svm$best.parameters$cost)
		y.pred = predict(svm.fit, dat.test)
		
		print(table(dat.test$y, y.pred))
		
		error.i = mean(dat.test$y != y.pred)
		print(error.i)
		test.error= test.error*(i-1)/i + error.i/i
	}
	return(test.error)
}
# svm binomial regression:

cost.seq = 10^seq(2, -3, length = 20)
k=5

library(e1071)

## ---------------------select data for hc and trauma:---------------------

df.ptsd.hc = df.all[df.all$ptsd==0|df.all$ptsd==1,]
print("dimension for hc and trauma dataset")
print(dim(df.ptsd.hc))
print(table(df.ptsd.hc$ptsd))
x = model.matrix(df.ptsd.hc$ptsd~., df.ptsd.hc)
y = as.factor(df.ptsd.hc$ptsd)
dat = data.frame(x = x, y = y)

set.seed(1)
error = svm.cv.fun(dat, k, cost.seq)

print("average test error for hc and trauma:") 
print(error) 

##--------------------- select data for hc and ptsd: -------------------------------------- 

df.ptsd.trauma = df.all[df.all$ptsd==0|df.all$ptsd==2,]
print("dimension for hc and ptsd dataset")
print(dim(df.ptsd.trauma))
print(table(df.ptsd.trauma$ptsd))

x = model.matrix(df.ptsd.trauma$ptsd~., df.ptsd.trauma)
y = as.factor(df.ptsd.trauma$ptsd)
dat = data.frame(x = x, y = y)

set.seed(123)
error = svm.cv.fun(dat, k, cost.seq)

print("average test error for hc and ptsd:")
print(error)

##--------------------- select data for ptsd and trauma:---------------------
#
#df.hc.trauma = df.all[df.all$ptsd==1|df.all$ptsd==2,]
#print("dimension for hc and trauma data set")
#print(dim(df.hc.trauma))
#
#x = model.matrix(df.hc.trauma$ptsd~., df.hc.trauma)
#y = as.factor(df.hc.trauma$ptsd)
#dat = data.frame(x = x, y = y)
#
#set.seed(3)
#error = svm.cv.fun(dat, k, cost.seq)
#print(error)
#
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
