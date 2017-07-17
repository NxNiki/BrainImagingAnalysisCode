#!/usr/bin/env Rscript 
rm(list=ls())
graphics.off()

#source("scriptd_stats01_read_feature_fc.R")
#brain.feature = cbind(fc)
#report.name = "fc"

source("scriptd_stats01_read_feature.R")

#scale.fun = function(x){((x)-min(x))/(max(x)-min(x))}
#brain.feature = apply(cbind(fsl.vbm, alff, reho, label.fa, label.md, tract.fa, tract.md), MARGIN = 2, FUN = scale.fun)
brain.feature = cbind(fsl.vbm, falff, alff, reho, label.fa, label.md, tract.fa, tract.md)
report.name = "vbm_rs_dti"
#brain.feature = scale(cbind(label.md, tract.md))
#report.name = "md"

# paramters for this script:
p.thresh = 10^-50
cost.seq = 10^seq(2, -3, length = 20)
gamma.seq = seq(0.01, 10, length = 20)
k=5
report.rows = 3

select.feature = function(feature.in, factor, p){
	print("dimension of features:")
	print(dim(feature.in))
	print(length(factor))
	
	factor.lvl = unique(factor)
	p.value1 = rep(NA, ncol(feature.in))
	p.value2 = rep(NA, ncol(feature.in))
	for (i.feature in 1:ncol(feature.in)) {
		test.result1 = t.test(feature.in[factor==factor.lvl[1],i.feature], mu=0)
		test.result2 = t.test(feature.in[factor==factor.lvl[2],i.feature], mu=0)
		p.value1[i.feature] = test.result1$p.value
		p.value2[i.feature] = test.result2$p.value
	}
	#test.result = lapply(feature.in, function(x) t.test(x ~ factor, var.equal = TRUE))
	#p.value = test.result$p.value;
	
	feature.idx = union(which(p.value1<p), which(p.value2<p))
	#print("selected features:")
	#print(feature.idx)
	
	print("number of selected features:")
	print(length(feature.idx))

	return(feature.idx)

}

compute.acc = function(y,yhat){
	
	#print("----------")
	#print(y)
	#print(yhat)
	#print("----------")
	
	acc<-sum(y==yhat)/length(y)
	ylevel = unique(y)
	print(ylevel)
	sensi<-sum(y==yhat & y==ylevel[2])/sum(y==ylevel[2])
	speci<-sum(y==yhat & y==ylevel[1])/sum(y==ylevel[1])
	temp<-c(acc, sensi, speci)
	return(temp)
}

svm.cv.fun = function(subject.info, brain.feature){
	
	#sort data according to y so that we have ballanced value in each group:
	idx.order.factor = order(subject.info$ptsd)
	subject.info = subject.info[idx.order.factor,]
	brain.feature = brain.feature[idx.order.factor,]

	num.sample = nrow(brain.feature)
	cv.k = sample(rep(1:k, each = ceiling(num.sample/k)))[1:num.sample]
	
	test.result=data.frame(acc=rep(NA,k), sensi=rep(NA,k), speci=rep(NA,k))
	train.result=data.frame(acc=rep(NA,k), sensi=rep(NA,k), speci=rep(NA,k))
	
	for (i in 1:k){	
		
		brain.feature.train = brain.feature[cv.k!=i,]
		brain.feature.test = brain.feature[cv.k==i,]
		subject.info.train = subject.info[cv.k!=i,]
		subject.info.test = subject.info[cv.k==i,]

		# select features bfactor runing t test:
		feature.idx = select.feature(brain.feature.train, subject.info.train$ptsd, p.thresh)
		
		feature.train = cbind(subject.info.train, y=as.data.frame(scale(brain.feature.train[, feature.idx])))
		feature.test = cbind(subject.info.test, y=as.data.frame(scale(brain.feature.test[, feature.idx])))
		
		svm.x.train = model.matrix(feature.train$ptsd~., feature.train)
		svm.y.train = as.factor(feature.train$ptsd)
		
		svm.x.test = model.matrix(feature.test$ptsd~., feature.test)
		svm.y.test = as.factor(feature.test$ptsd)
		
		svm.dat.train = data.frame(x=svm.x.train, y=svm.y.train)
		svm.dat.test = data.frame(x=svm.x.test, y=svm.y.test)

		tune.svm = tune(svm, y~., data = svm.dat.train, kernel = "linear", ranges = list(cost=cost.seq))
		#tune.svm = tune(svm, y~., data = svm.dat.train, kernel = "radial", ranges = list(cost=cost.seq,gamma=gamma.seq))
		#print(summary(tune.svm))
		#print(summary(tune.svm$best.model))
		
		svm.pred = predict(tune.svm$best.model, svm.dat.test)
		svm.pred.train = predict(tune.svm$best.model, svm.dat.train)
		
		print("prediction of testing data:")
		print(table(svm.dat.test$y, svm.pred))
		print("prediction of training data:")
		print(table(svm.dat.train$y, svm.pred.train))
		
		result.test = compute.acc(svm.dat.test$y, svm.pred)
		result.train = compute.acc(svm.dat.train$y, svm.pred.train)

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
report.sd = data.frame(acc=rep(NA, report.rows),sensi=rep(NA,report.rows),speci=rep(NA,report.rows))

# ---------------------select data for hc and trauma :---------------------

idx = subject.info$ptsd==0|subject.info$ptsd==1
set.seed(333)
svm.cv.result = svm.cv.fun(subject.info[idx,-1], brain.feature[idx,])
result = svm.cv.result[[1]]
train.result = svm.cv.result[[2]]

print(result)
print(train.result)

report[1,]=colMeans(result)
report.sd[1,]=apply(result,2,sd)

# ---------------------select data for ptsd and trauma :---------------------

idx = subject.info$ptsd==1|subject.info$ptsd==2
set.seed(111)

svm.cv.result = svm.cv.fun(subject.info[idx,-1], brain.feature[idx,])
result = svm.cv.result[[1]]
train.result = svm.cv.result[[2]]

print(result)
print(train.result)

report[2,]=colMeans(result)
report.sd[2,]=apply(result,2,sd)

# ---------------------select data for hc and ptsd :---------------------

idx = subject.info$ptsd==0|subject.info$ptsd==2
set.seed(222)

svm.cv.result = svm.cv.fun(subject.info[idx,-1], brain.feature[idx,])
result = svm.cv.result[[1]]
train.result = svm.cv.result[[2]]

print(result)
print(train.result)

report[3,]=colMeans(result)
report.sd[3,]=apply(result,2,sd)
# -------------------- save results : -------------------------------------
row.names(report) = c("hc vs trauma", "ptsd vs trauma", "hc vs ptsd")

filename = paste("result_ptsd_svm_t0_", report.name, "_k", toString(k), "_p", toString(p.thresh), ".csv", sep = "")
write.table(report, filename, sep = ",", row.names = T)
filename = paste("result_ptsd_svm_t0_sd_", report.name, "_k", toString(k), "_p", toString(p.thresh), ".csv", sep = "")
write.table(report.sd, filename, sep = ",", row.names = T)

print(report)
print(report.sd)
