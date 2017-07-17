#!/usr/bin/env Rscript 
rm(list=ls())
graphics.off()

source("scriptd_stats01_read_feature_fc.R")
#brain.feature = cbind(fc)
#report.name = "fc"

source("scriptd_stats01_read_feature.R")
brain.feature = scale(cbind(fsl.vbm, alff, reho, label.fa, label.md, tract.fa, tract.md))
report.name = "svmtall_vbm_rs_dti_"
#brain.feature = scale(cbind(label.md, tract.md))
#report.name = "md"

# paramters for this script:
p.thresh = .001
cost.seq = 10^seq(2, -3, length = 20)
k=5
report.rows = 3

select.feature = function(feature.in, factor, p){
	
	print(dim(feature.in))
	print(table(factor))
	
	p.value = rep(NA, ncol(feature.in))
	for (i.feature in 1:ncol(feature.in)) {
		test.result = t.test(feature.in[,i.feature] ~ factor, var.equal=TRUE)
		p.value[i.feature] = test.result$p.value
	}
	#test.result = lapply(feature.in, function(x) t.test(x ~ factor, var.equal = TRUE))
	#p.value = test.result$p.value;
	
	print("pvalues:")
	print(p.value)
	feature.out = feature.in[, p.value<p]
	return(feature.out)
}

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
error = data.frame(cv = c(1:k))
train.error = data.frame(cv = c(1:k))
library(e1071)

report = data.frame(acc=rep(NA, report.rows),sensi=rep(NA,report.rows),speci=rep(NA,report.rows))

## ---------------------select data for hc and ptsd :---------------------
idx = subject.info$ptsd==0|subject.info$ptsd==1
# select fc features by runing t test:
brain.feature.subset = select.feature(brain.feature[idx,], subject.info$ptsd[idx], p.thresh)
df.subset = cbind(subject.info[idx,-1], brain.feature.subset)

print("dimension for subset dataset")
print(dim(df.subset))
print(table(df.subset$ptsd))
print(brain.feature.subset[1,])

x = model.matrix(df.subset$ptsd~., df.subset)
y = as.factor(df.subset$ptsd)

set.seed(333)

svm.cv.result = svm.cv.fun(x, y, k, cost.seq)
result = svm.cv.result[[1]]
train.result = svm.cv.result[[2]]

print(result)
print(train.result)

report[1,]=colMeans(result)

## ---------------------select data for ptsd and trauma :---------------------

idx = subject.info$ptsd==1|subject.info$ptsd==2
# select fc features by runing t test:
brain.feature.subset = select.feature(brain.feature[idx,], subject.info$ptsd[idx], p.thresh)

df.subset = cbind(subject.info[idx,-1], brain.feature.subset)

print("dimension for subset dataset")
print(dim(df.subset))
print(table(df.subset$ptsd))
print(brain.feature.subset)
x = model.matrix(df.subset$ptsd~., df.subset)
y = as.factor(df.subset$ptsd)

set.seed(111)

svm.cv.result = svm.cv.fun(x, y, k, cost.seq)
result = svm.cv.result[[1]]
train.result = svm.cv.result[[2]]

print(result)
print(train.result)

report[2,]=colMeans(result)
## ---------------------select data for hc and trauma :---------------------

idx = subject.info$ptsd==0|subject.info$ptsd==2
# select fc features by runing t test:
brain.feature.subset = select.feature(brain.feature[idx,], subject.info$ptsd[idx], p.thresh)
df.subset = cbind(subject.info[idx,-1], brain.feature.subset)

print("dimension for subset dataset")
print(dim(df.subset))
print(table(df.subset$ptsd))
print(brain.feature.subset)

x = model.matrix(df.subset$ptsd~., df.subset)
y = as.factor(df.subset$ptsd)

set.seed(111)

svm.cv.result = svm.cv.fun(x, y, k, cost.seq)
result = svm.cv.result[[1]]
train.result = svm.cv.result[[2]]

print(result)
print(train.result)

report[3,]=colMeans(result)
row.names(report) = c("hc vs ptsd", "ptsd vs trauma", "hc vs trauma")

filename = paste("result_ptsd_svm_", report.name, "_p", toString(p.thresh), ".csv", sep = "")
write.table(report, filename, sep = ",", row.names = T)

print(report)
