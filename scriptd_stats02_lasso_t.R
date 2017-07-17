#!/usr/bin/env Rscript 
rm(list=ls())
graphics.off()

#source("scriptd_stats01_read_feature_fc.R")
#brain.feature = cbind(fc)
#report.name = "fc"

source("scriptd_stats01_read_feature.R")
brain.feature = scale(cbind(spm.vbm, alff, reho, label.fa, label.md, tract.fa, tract.md))
report.name = "multimodal_lasso_t"
#brain.feature = scale(cbind(spm.vbm))
#report.name = "spm.vbm"

# paramters for this script:
p.thresh = .01
cost.seq = 10^seq(2, -3, length = 20)
report.rows = 3
k=7

select.feature = function(feature.in, factor, p){
	
	p.value = rep(NA, ncol(feature.in))
	for (i.feature in 1:ncol(feature.in)) {
		test.result = t.test(feature.in[,i.feature] ~ factor, var.equal=TRUE)
		p.value[i.feature] = test.result$p.value
	}
	#test.result = lapply(feature.in, function(x) t.test(x ~ factor, var.equal = TRUE))
	#p.value = test.result$p.value;
	
	feature.idx = which(p.value<p)
	print("selected features:")
	print(feature.idx)
	return(feature.idx)

}

compute.acc = function(y,yhat){
	#print("----------")
	#print(length(y))
	#print(length(yhat))
	#print("----------")
	
	y = as.numeric(y)
	yhat = as.numeric(yhat)

	print(sum(y==yhat))
	print(length(y))
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

cv.fun = function(subject.info, brain.feature, k, cost.seq, p.thresh){
	
	#sort data according to y so that we have ballanced value in each group:
	idx.order.factor = order(subject.info$ptsd)
	subject.info = subject.info[idx.order.factor,]
	brain.feature = brain.feature[idx.order.factor,]

	num.sample = nrow(brain.feature)
	cv.k = sample(rep(1:k, each = ceiling(num.sample/k)))[1:num.sample]
	print(sort(cv.k))	
	test.result=data.frame(acc=rep(NA,k), sensi=rep(NA,k), speci=rep(NA,k))
	train.result=data.frame(acc=rep(NA,k), sensi=rep(NA,k), speci=rep(NA,k))

	feature.idx.all = vector()
	for (i in 1:k){	
		
		brain.feature.train = brain.feature[cv.k!=i,]
		brain.feature.test = brain.feature[which(cv.k==i),,drop = F]
		subject.info.train = subject.info[cv.k!=i,]
		subject.info.test = subject.info[which(cv.k==i),,drop = F]
		
		# select features bfactor runing t test:
		feature.idx = select.feature(brain.feature.train, subject.info.train$ptsd, p.thresh)
		feature.train = cbind(subject.info.train, y=brain.feature.train[, feature.idx, drop=F])
		feature.test = cbind(subject.info.test, y=brain.feature.test[, feature.idx, drop=F])
		
		feature.idx.all = c(feature.idx.all, feature.idx)
		# remove the 1st column of all ones:
		x.train = model.matrix(feature.train$ptsd~., feature.train)[,-1]
		y.train = as.factor(feature.train$ptsd)

		x.test = model.matrix(feature.test$ptsd~., feature.test)
		y.test = as.factor(feature.test$ptsd)
		
		cv.out = cv.glmnet(x.train, y.train, alpha=1)
		print(cv.out)
		print(names(cv.out))
		
		svm.pred = predict(cv.out$best.model, svm.dat.test)
		svm.pred.train = predict(cv.out$best.model, svm.dat.train)
		
		print("prediction of testing data:")
		print(table(svm.dat.test$y, svm.pred))
		print("prediction of training data:")
		print(table(svm.dat.train$y, svm.pred.train))
		
		result.test = compute.acc(svm.dat.test$y, svm.pred)
		result.train = compute.acc(svm.dat.train$y, svm.pred.train)

		test.result[i,] = result.test
		train.result[i,] = result.train
	}
	print("feature.idx---------------------")
	frequent.feature=as.numeric(names(sort(table(feature.idx.all),decreasing=TRUE)[1:3]))
	print(frequent.feature)
	print(brain.feature[1, frequent.feature])
	print("feature.idx---------------------")
	return(list(test.result, train.result))
}


# glmnet regression:
library(glmnet)

report = data.frame(acc=rep(NA, report.rows),sensi=rep(NA,report.rows),speci=rep(NA,report.rows))
report.sd = data.frame(acc=rep(NA, report.rows),sensi=rep(NA,report.rows),speci=rep(NA,report.rows))

# ---------------------select data for hc and ptsd :---------------------

idx = subject.info$ptsd==0|subject.info$ptsd==1
set.seed(333)

#error = data.frame(cv = c(1:k))
#train.error = data.frame(cv = c(1:k))

svm.cv.result = cv.fun(subject.info[idx,-1], brain.feature[idx,], k, cost.seq, p.thresh)
result = svm.cv.result[[1]]
train.result = svm.cv.result[[2]]

print(result)
print(train.result)

report[1,]=colMeans(result, na.rm=T)
report.sd[1,]=apply(result,2,function(x) sd(na.omit(x)))

# ---------------------select data for ptsd and trauma :---------------------

idx = subject.info$ptsd==1|subject.info$ptsd==2
set.seed(111)

svm.cv.result = cv.fun(subject.info[idx,-1], brain.feature[idx,], k, cost.seq, p.thresh)
result = svm.cv.result[[1]]
train.result = svm.cv.result[[2]]

print(result)
print(train.result)

report[2,]=colMeans(result, na.rm=T)
report.sd[2,]=apply(result,2,function(x) sd(na.omit(x)))

# ---------------------select data for hc and trauma :---------------------

idx = subject.info$ptsd==0|subject.info$ptsd==2
set.seed(222)

svm.cv.result = cv.fun(subject.info[idx,-1], brain.feature[idx,], k, cost.seq, p.thresh)
result = svm.cv.result[[1]]
train.result = svm.cv.result[[2]]

print(result)
print(train.result)

report[3,]=colMeans(result, na.rm=T)
report.sd[3,]=apply(result,2,function(x) sd(na.omit(x)))

# -------------------- save results : -------------------------------------

row.names(report) = c("hc vs trauma", "ptsd vs trauma", "hc vs ptsd")

filename = paste("result", report.name, "_k", toString(k), "_p", toString(p.thresh), ".csv", sep = "")
write.table(report, filename, sep = ",", row.names = T)
filename = paste("result_", report.name, "_k", toString(k), "_p", toString(p.thresh), ".csv", sep = "")
write.table(report.sd, filename, sep = ",", row.names = T)

print(report)
print(report.sd)
