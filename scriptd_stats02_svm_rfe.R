#!/usr/bin/env Rscript 
rm(list=ls())
graphics.off()

#source("scriptd_stats01_read_feature_fc.R")
#brain.feature = cbind(fc)
#report.name = "fc"

source("scriptd_stats01_read_feature.R")
#brain.feature = scale(cbind(fsl.vbm, alff, falff, reho, label.fa, label.md, tract.fa, tract.md))
brain.feature = scale(alff)
report.name = "svmrfe_alff"
#brain.feature = scale(cbind(label.md, tract.md))
#report.name = "md"

# paramters for this script:
cost.seq = 10^seq(2, -3, length = 20)
report.rows = 3
num.features.list = (c(2:5)*10);
k = 10 

################################################
# Feature Ranking with Average Multiclass SVM-RFE
################################################


svmrfeFeatureRankingForMulticlass333 = function(x,y){
    n = ncol(x)
    
    survivingFeaturesIndexes = seq(1:n)
    featureRankedList = vector(length=n)
    rankedFeatureIndex = n
    N=floor(log(n)/log(2))
    kk=N
  
     
      while(length(survivingFeaturesIndexes)>0   ){

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
            #Zoe 	
            red_step=2^kk
            #red_step=1
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

svm.cv.fun = function(subject.info, brain.feature, k, cost.seq, num.features){
	
	#sort data according to y so that we have ballanced value in each group:
	idx.order.factor = order(subject.info$ptsd)
	subject.info = subject.info[idx.order.factor,]
	brain.feature = brain.feature[idx.order.factor,]

	num.sample = nrow(brain.feature)
	cv.k = sample(rep(1:k, each = ceiling(num.sample/k)))[1:num.sample]
	#print(sort(cv.k))	
	test.result=data.frame(acc=rep(NA,k), sensi=rep(NA,k), speci=rep(NA,k))
	train.result=data.frame(acc=rep(NA,k), sensi=rep(NA,k), speci=rep(NA,k))
	
	for (i in 1:k){	
		brain.feature.train = brain.feature[cv.k!=i,]
		brain.feature.test = brain.feature[which(cv.k==i),,drop = F]
		subject.info.train = subject.info[cv.k!=i,]
		subject.info.test = subject.info[which(cv.k==i),,drop = F]
	
		brain.feature.train.mat = data.matrix(brain.feature.train)
		factor = subject.info.train$ptsd
		#factor[factor = min(factor)] = 0
		#factor[factor = max(factor)] = 1
		feature.rank = svmrfeFeatureRankingForMulticlass333(brain.feature.train.mat,factor)
		
		# select features bfactor runing t test:
		feature.idx = feature.rank[1:num.features]
		feature.train = cbind(subject.info.train, y=brain.feature.train[, feature.idx])
		feature.test = cbind(subject.info.test, y=brain.feature.test[, feature.idx, drop=F])
		
		svm.x.train = model.matrix(feature.train$ptsd~., feature.train)
		svm.y.train = as.factor(feature.train$ptsd)

		svm.x.test = model.matrix(feature.test$ptsd~., feature.test)
		svm.y.test = as.factor(feature.test$ptsd)
		
		svm.dat.train = data.frame(x=svm.x.train, y=svm.y.train)
		svm.dat.test = data.frame(x=svm.x.test, y=svm.y.test)

		tune.svm = tune(svm, y~., data = svm.dat.train, kernel = "linear", ranges = list(cost=cost.seq))
		print(summary(tune.svm))
		print(summary(tune.svm$best.model))
		
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
library(e1071)
length.num.features = length(num.features.list);
report.rows = report.rows*length.num.features;

report = data.frame(group = c("hc vs trauma", "ptsd vs trauma", "hc vs ptsd"), acc=rep(NA, report.rows),sensi=rep(NA,report.rows),speci=rep(NA,report.rows))
report.sd = data.frame(group = c("hc vs trauma", "ptsd vs trauma", "hc vs ptsd"), acc=rep(NA, report.rows),sensi=rep(NA,report.rows),speci=rep(NA,report.rows))

for (i.num.feature in 1:length.num.features){
num.features = num.features.list[i.num.feature]
row.idx = (i.num.feature-1)*3
# ---------------------select data for hc and ptsd :---------------------
idx = subject.info$ptsd==0|subject.info$ptsd==1
set.seed(333)

#error = data.frame(cv = c(1:k))
#train.error = data.frame(cv = c(1:k))

svm.cv.result = svm.cv.fun(subject.info[idx,-1], brain.feature[idx,], k, cost.seq, num.features)
result = svm.cv.result[[1]]
train.result = svm.cv.result[[2]]

print(result)
print(train.result)

report[row.idx+1,2:4]=colMeans(result, na.rm=T)
report.sd[row.idx+11,2:4]=apply(result,2,function(x) sd(na.omit(x)))

# ---------------------select data for ptsd and trauma :---------------------

idx = subject.info$ptsd==1|subject.info$ptsd==2
set.seed(111)

svm.cv.result = svm.cv.fun(subject.info[idx,-1], brain.feature[idx,], k, cost.seq, num.features)
result = svm.cv.result[[1]]
train.result = svm.cv.result[[2]]

print(result)
print(train.result)

report[row.idx+2,2:4]=colMeans(result, na.rm=T)
report.sd[row.idx+2,2:4]=apply(result,2,function(x) sd(na.omit(x)))

# ---------------------select data for hc and trauma :---------------------

idx = subject.info$ptsd==0|subject.info$ptsd==2
set.seed(222)

svm.cv.result = svm.cv.fun(subject.info[idx,-1], brain.feature[idx,], k, cost.seq, num.features)
result = svm.cv.result[[1]]
train.result = svm.cv.result[[2]]

print(result)
print(train.result)

report[row.idx+3,2:4]=colMeans(result, na.rm=T)
report.sd[row.idx+3,2:4]=apply(result,2,function(x) sd(na.omit(x)))

}

# -------------------- save results : -------------------------------------

filename = paste("result_", report.name, "_k", toString(k), "_p", toString(num.features.list), ".csv", sep = "")
write.table(report, filename, sep = ",", row.names = T)
filename = paste("result_sd_", report.name, "_k", toString(k), "_p", toString(num.features.list), ".csv", sep = "")
write.table(report.sd, filename, sep = ",", row.names = T)

print(report)
print(report.sd)
