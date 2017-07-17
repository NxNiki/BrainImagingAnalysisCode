#!/usr/bin/env Rscript 
rm(list=ls())
graphics.off()

source("scriptd_stats01_read_feature_fc.R")
brain.feature = cbind(fc)
report.name = "fc"

#source("scriptd_stats01_read_feature.R")
#brain.feature = scale(cbind(spm.vbm, alff, reho, label.fa, label.md, tract.fa, tract.md))
#brain.feature = scale(cbind(spm.vbm))
#report.name = "multimodal_svmt"

# paramters for this script:
p.thresh = .001
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
	#print("selected features:")
	#print(feature.idx)
	return(feature.idx)

}

################################################
# This function gives the weights of the hiperplane
################################################
svm.weights<-function(model){
w=0
  if(model$nclasses==2){
       w=t(model$coefs)%*%model$SV
  }else{    #when we deal with OVO svm classification
      ## compute start-index
      start <- c(1, cumsum(model$nSV)+1)
      start <- start[-length(start)]

      calcw <- function (i,j) {
        ## ranges for class i and j:
        ri <- start[i] : (start[i] + model$nSV[i] - 1)
        rj <- start[j] : (start[j] + model$nSV[j] - 1)

      ## coefs for (i,j):
        coef1 <- model$coefs[ri, j-1]
        coef2 <- model$coefs[rj, i]
        ## return w values:
        w=t(coef1)%*%model$SV[ri,]+t(coef2)%*%model$SV[rj,]
        return(w)
      }

      W=NULL
      for (i in 1 : (model$nclasses - 1)){
        for (j in (i + 1) : model$nclasses){
          wi=calcw(i,j)
          W=rbind(W,wi)
        }
      }
      w=W
  }
  return(w)
}


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

svm.cv.fun = function(subject.info, brain.feature, k, cost.seq, p.thresh){
	
	#sort data according to y so that we have ballanced value in each group:
	idx.order.factor = order(subject.info$ptsd)
	subject.info = subject.info[idx.order.factor,]
	brain.feature = brain.feature[idx.order.factor,]

	num.sample = nrow(brain.feature)
	cv.k = sample(rep(1:k, each = ceiling(num.sample/k)))[1:num.sample]
	
	test.result=data.frame(acc=rep(NA,k), sensi=rep(NA,k), speci=rep(NA,k))
	train.result=data.frame(acc=rep(NA,k), sensi=rep(NA,k), speci=rep(NA,k))

	feature.idx.all = vector()
	#feature.weights = data.frame(matrix(NA, ncol=ncol(brain.feature), nrow=k))
	#colnames(feature.weights) = brain.feature[0,]
	#print(ncol(feature.weights))
	
	for (i in 1:k){	
		
		brain.feature.train = brain.feature[cv.k!=i,]
		brain.feature.test = brain.feature[which(cv.k==i),,drop = F]
		subject.info.train = subject.info[cv.k!=i,]
		subject.info.test = subject.info[which(cv.k==i),,drop = F]
		
		# select features bfactor runing t test:
		feature.idx = select.feature(brain.feature.train, subject.info.train$ptsd, p.thresh)
		feature.train = cbind(subject.info.train, brain.feature.train[, feature.idx, drop=F])
		feature.test = cbind(subject.info.test, brain.feature.test[, feature.idx, drop=F])
		
		feature.idx.all = c(feature.idx.all, feature.idx)
		tune.svm = tune(svm, ptsd ~., data = feature.train, kernel = "linear", ranges = list(cost=cost.seq))
		
		#print(summary(tune.svm))
		#print(tune.svm$best.parameters$cost)
		#print(tune.svm$best.modal$coefs)		
		#print(svm.weights(tune.svm$best.modal))		
		
		svm.mod = svm(ptsd~., data = feature.train, kernel = "linear", cost = tune.svm$best.parameters$cost)
		
		weights.i = data.frame(svm.weights(svm.mod))
		#print(weights.i)
		colnames(weights.i) = colnames(subset(feature.train, select = -ptsd))
		
		if (i==1){
			feature.weights = weights.i
		}else{
			feature.weights = rbind.fill(feature.weights, weights.i)
		}

		#svm.pred = predict(tune.svm$best.model, svm.dat.test)
		svm.pred = predict(tune.svm$best.model, subset(feature.test, select=-ptsd))
		#svm.pred.train = predict(tune.svm$best.model, svm.dat.train)
		svm.pred.train = predict(tune.svm$best.model, subset(feature.train, select=-ptsd))
		
		print("prediction of testing data:")
		#print(cbind(feature.test$ptsd, svm.pred))
		print(table(feature.test$ptsd, svm.pred))
		print("prediction of training data:")
		print(table(feature.train$ptsd, svm.pred.train))
		
		result.test = compute.acc(feature.test$ptsd, svm.pred)
		result.train = compute.acc(feature.train$ptsd, svm.pred.train)

		test.result[i,] = result.test
		train.result[i,] = result.train
	}
	#print("feature.idx---------------------")
	#print(sort(table(feature.idx.all),decreasing=T))
	#frequent.feature=as.numeric(names(sort(table(feature.idx.all),decreasing=TRUE)[1:3]))
	#print(frequent.feature)
	#print(brain.feature[1, frequent.feature])
	#print("feature.idx---------------------")
	return(list(test.result, train.result, feature.weights))
}


# svm binomial regression:
library(e1071)
library(plyr)

report = data.frame(acc=rep(NA, report.rows),sensi=rep(NA,report.rows),speci=rep(NA,report.rows))
report.sd = data.frame(acc=rep(NA, report.rows),sensi=rep(NA,report.rows),speci=rep(NA,report.rows))

subject.info$ptsd = as.factor(subject.info$ptsd)

# ---------------------select data for hc and trauma :---------------------

idx = subject.info$ptsd==0|subject.info$ptsd==1
set.seed(333)

#error = data.frame(cv = c(1:k))
#train.error = data.frame(cv = c(1:k))

svm.cv.result = svm.cv.fun(subject.info[idx,-1], brain.feature[idx,], k, cost.seq, p.thresh)
result = svm.cv.result[[1]]
train.result = svm.cv.result[[2]]
feature.weights = svm.cv.result[[3]]
feature.weights.mean = as.data.frame.list(colMeans(feature.weights, na.rm=T))

print(result)
print(train.result)
print(feature.weights)

filename = paste("result_featureweights_hc_trauma", report.name, "_k", toString(k), "_p", toString(p.thresh), ".csv", sep = "")
write.table(rbind(feature.weights, feature.weights.mean), filename, sep = ",", row.names = F)

report[1,]=colMeans(result, na.rm=T)
report.sd[1,]=apply(result,2,function(x) sd(na.omit(x)))

# ---------------------select data for ptsd and trauma :---------------------

idx = subject.info$ptsd==1|subject.info$ptsd==2
set.seed(111)

svm.cv.result = svm.cv.fun(subject.info[idx,-1], brain.feature[idx,], k, cost.seq, p.thresh)
result = svm.cv.result[[1]]
train.result = svm.cv.result[[2]]
feature.weights = svm.cv.result[[3]]
feature.weights.mean = as.data.frame.list(colMeans(feature.weights, na.rm=T))

print(result)
print(train.result)
print(feature.weights)

filename = paste("result_featureweights_ptsd_trauma", report.name, "_k", toString(k), "_p", toString(p.thresh), ".csv", sep = "")
write.table(rbind(feature.weights, feature.weights.mean), filename, sep = ",", row.names = F)

report[2,]=colMeans(result, na.rm=T)
report.sd[2,]=apply(result,2,function(x) sd(na.omit(x)))

# ---------------------select data for hc and ptsd :---------------------

idx = subject.info$ptsd==0|subject.info$ptsd==2
set.seed(222)

svm.cv.result = svm.cv.fun(subject.info[idx,-1], brain.feature[idx,], k, cost.seq, p.thresh)
result = svm.cv.result[[1]]
train.result = svm.cv.result[[2]]
feature.weights = svm.cv.result[[3]]
feature.weights.mean = as.data.frame.list(colMeans(feature.weights, na.rm=T))

print(result)
print(train.result)
print(feature.weights)

filename = paste("result_featureweights_hc_ptsd", report.name, "_k", toString(k), "_p", toString(p.thresh), ".csv", sep = "")
write.table(rbind(feature.weights, feature.weights.mean), filename, sep = ",", row.names = F)

report[3,]=colMeans(result, na.rm=T)
report.sd[3,]=apply(result,2,function(x) sd(na.omit(x)))

# -------------------- save results : -------------------------------------

row.names(report) = c("hc vs trauma", "ptsd vs trauma", "hc vs ptsd")

filename = paste("result_", report.name, "_k", toString(k), "_p", toString(p.thresh), ".csv", sep = "")
write.table(report, filename, sep = ",", row.names = T)
filename = paste("result_sd", report.name, "_k", toString(k), "_p", toString(p.thresh), ".csv", sep = "")
write.table(report.sd, filename, sep = ",", row.names = T)

print(report)
print(report.sd)
