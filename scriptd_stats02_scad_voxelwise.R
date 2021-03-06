#!/usr/bin/env Rscript

# scad to select features.
# Xin Niu May.4.2017


rm(list=ls())
graphics.off()

#cost.seq = 10^seq(2, -3, length = 20)
k=5
error = data.frame(cv = c(1:k))
train.error = data.frame(cv = c(1:k))
report.rows = 3
report.name = "scad_spmvbm_rs_dti"

source("scriptd_stats01_read_voxelwise_feature.R")

#brain.feature = scale(cbind(fsl.vbm, alff, reho, label.fa, tract.fa, tract.md))
brain.feature = scale(cbind(fsl.vbm))
df.all = cbind(subject.info[,-1], brain.feature)

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

scad.cv.fun = function(x, y, k, cost.seq){
	
	#sort data according to y so that we have ballanced value in each group:
	x = x[order(y),]
	y = sort(y)
	y[y==min(y)]=0
	y[y==max(y)]=1

	num.sample = nrow(x)
	cv.k = sample(rep(1:k, each = ceiling(num.sample/k)))[1:num.sample]
	
	test.result=data.frame(acc=rep(NA,k), sensi=rep(NA,k), speci=rep(NA,k))
	train.result=data.frame(acc=rep(NA,k), sensi=rep(NA,k), speci=rep(NA,k))
	
	for (i in 1:k){	
		x.train = x[cv.k!=i,]
		x.test = x[cv.k==i,]
		y.train = y[cv.k!=i]
		y.test = y[cv.k==i]
	
		cv.fit = cv.ncvreg(x.train, y.train, seed=123, family="binomial")
		#print(head(cv.fit))
		
		y.pred = predict(cv.fit, x.test)
		print(y.pred)
		y.pred = as.numeric(y.pred>.5)
		print(y.pred)
		y.pred.train = predict(cv.fit, x.train)
		print(y.pred.train)
		y.train = as.numeric(y.train>.5)
		
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

# binomial regression:


library(ncvreg)

report = data.frame(group = c("hc vs trauma", "ptsd vs trauma", "hc vs ptsd"), acc=rep(NA, report.rows),sensi=rep(NA,report.rows),speci=rep(NA,report.rows))
report.sd = data.frame(group = c("hc vs trauma", "ptsd vs trauma", "hc vs ptsd"), acc=rep(NA, report.rows),sensi=rep(NA,report.rows),speci=rep(NA,report.rows))

## ---------------------select data for ptsd 0 and 1 :---------------------

df.subset = df.all[df.all$ptsd==0|df.all$ptsd==1,]
print("dimension for subset dataset")
print(dim(df.subset))
print(table(df.subset$ptsd))

x = model.matrix(df.subset$ptsd~., df.subset)
y = df.subset$ptsd
print(dim(x))
set.seed(333)

cv.result = scad.cv.fun(x[,-1], y, k, cost.seq)
result = cv.result[[1]]
train.result = cv.result[[2]]

print(result)
print(train.result)

### ---------------------select data for ptsd 1 and 2 :---------------------
#
#df.subset = df.all[df.all$ptsd==1|df.all$ptsd==2,]
#print("dimension for subset dataset")
#print(dim(df.subset))
#print(table(df.subset$ptsd))
#
#x = model.matrix(df.subset$ptsd~., df.subset)
#y = df.subset$ptsd
#print(dim(x))
#set.seed(111)
#
#cv.result = scad.cv.fun(x[,-1], y, k, cost.seq)
#result = cv.result[[1]]
#train.result = cv.result[[2]]
#
#print(result)
#print(train.result)
#
### ---------------------select data for ptsd 0 and 2 :---------------------
#
#df.subset = df.all[df.all$ptsd==0|df.all$ptsd==2,]
#print("dimension for subset dataset")
#print(dim(df.subset))
#print(table(df.subset$ptsd))
#
#x = model.matrix(df.subset$ptsd~., df.subset)
#y = df.subset$ptsd
#
#set.seed(111)
#
#cv.result = scad.cv.fun(x[,-1], y, k, cost.seq)
#result = cv.result[[1]]
#train.result = cv.result[[2]]
#
#print(result)
#print(train.result)
#
## -------------------- save results : -------------------------------------
#
#filename = paste("result_", report.name, "_k", toString(k), ".csv", sep = "")
#write.table(report, filename, sep = ",", row.names = T)
#filename = paste("result_sd_", report.name, "_k", toString(k), ".csv", sep = "")
#write.table(report.sd, filename, sep = ",", row.names = T)
#
#print(report)
#print(report.sd)
