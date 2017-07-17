#!/usr/bin/env Rscript

# svmrfe.
# Xin Niu Apr.27.2017


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

svmrfeFeatureRankingForMulticlass333 = function(x,y){
    n = ncol(x)
    
    survivingFeaturesIndexes = seq(1:n)
    featureRankedList = vector(length=n)
    rankedFeatureIndex = n
    N=floor(log(n)/log(2))
    kk=N
  
     
      while(length(survivingFeaturesIndexes)>0){

        #train the support vector machine
        svmModel = svm(x[, survivingFeaturesIndexes], y, cost = 10, cachesize=500,  scale=F, type="C-classification", kernel="linear" )
        
        #compute the weight vector
        multiclassWeights = svm.weights(svmModel)
        
        #compute ranking criteria
        multiclassWeights = multiclassWeights * multiclassWeights
        rankingCriteria = 0
        for(i in 1:ncol(multiclassWeights))rankingCriteria[i] = mean(multiclassWeights[,i])
        
        #rank the features
        (ranking = sort(rankingCriteria, index.return = TRUE)$ix)

      
        if (kk==N){
             red_step=n-2^kk } else { 
             red_step=2^kk
            }
        kk=kk-1

        testind=rankedFeatureIndex-red_step
        if (testind < 0) red_step=rankedFeatureIndex

        #update feature ranked list
        rankedFeatureIndexsss=rankedFeatureIndex-red_step+1
        (featureRankedList[rankedFeatureIndexsss:rankedFeatureIndex] = survivingFeaturesIndexes[ranking[1:red_step]])
        rankedFeatureIndex = rankedFeatureIndex - red_step
        
        #eliminate the feature with smallest ranking criterion
        (survivingFeaturesIndexes = survivingFeaturesIndexes[-ranking[1:red_step]])
        cat(length(survivingFeaturesIndexes),"\n")
       

    }
    
    return (featureRankedList)

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
		y.train = y[cv.k!=i]
		x.test = x[cv.k==i,]
		y.test = y[cv.k==i]

		feature.idx = svmrfeFeatureRankingForMulticlass333 = function(x.train,y.train)
		print("---")
		print(feature.idx)	
		print("---")
		dat.train = data.frame(x=x.train[,feature.idx], y=y[cv.k!=i])
		dat.test = data.frame(x=x.test[,feature.idx], y=y[cv.k==i])
	
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
report.rows = 3
error = data.frame(cv = c(1:k))
train.error = data.frame(cv = c(1:k))
library(e1071)

report = data.frame(group=rep(NA, report.rows), acc=rep(NA, report.rows), sensi=rep(NA,report.rows), speci=rep(NA,report.rows))
report.sd = data.frame(group=rep(NA, report.rows), acc=rep(NA, report.rows), sensi=rep(NA,report.rows), speci=rep(NA,report.rows))

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

report[1,]=colMeans(result)
report.sd[1,]=apply(result,2,sd)

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

report[2,]=colMeans(result)
report.sd[2,]=apply(result,2,sd)

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

report[3,]=colMeans(result)
report.sd[3,]=apply(result,2,sd)

# -------------------- save results : -------------------------------------

report$group = c("hc vs ptsd", "ptsd vs trauma", "hc vs trauma")

filename = paste("result_ptsd_svmrfe_", report.name, "_k", toString(k), "_p", toString(p.thresh), ".csv", sep = "")
write.table(report, filename, sep = ",", row.names = F)

filename = paste("result_ptsd_svmrfe_sd_", report.name, "_k", toString(k), "_p", toString(p.thresh), ".csv", sep = "")
write.table(report.sd, filename, sep = ",", row.names = F)

print(report)
print(report.sd)
