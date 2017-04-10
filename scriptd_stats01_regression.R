#!/usr/bin/env Rscript

rm(list=ls())
graphics.off()

# read all txt and.txt files:
#file.list = list.files(pattern = "outc01*")
#file.list
#data.list = lapply(file.list, FUN = read.table, header = TRUE)
#
#rest.vbm.features = do.call("cbind", data.list)
#dim(rest.vbm.features)

# read txt files:
subject.info = read.table("outb01_ptsd_hc_info2.txt", header=TRUE)

subject.delete = 601167369441
delete.idx = subject.info$SUBJID!=subject.delete

subject.info = subject.info[,-1]
subject.info$Sex = as.numeric(subject.info$Sex)
subject.info$age = scale(subject.info$age)
print(head(subject.info))

fsl.vbm = read.table("outc01_fslvbm_features_thr_30.txt", header=TRUE)
colnames(fsl.vbm)=paste("fsl.vbm", colnames(fsl.vbm), sep = "_")

spm.vbm = read.table("outc01_spmvbm_features.txt", header=TRUE)
colnames(spm.vbm)=paste("spm.vbm", colnames(spm.vbm), sep = "_")

alff = read.table("outc01_alff_features_thr_30.txt", header=TRUE)
colnames(alff)=paste("alff", colnames(alff), sep = "_")

falff = read.table("outc01_Rest_fALFF_Feature.txt", header=TRUE)
colnames(falff)=paste("falff", colnames(falff), sep = "_")

reho = read.table("outc01_reho_features_thr_30.txt", header=TRUE)
colnames(reho)=paste("reho", colnames(reho), sep = "_")

dti.label.fa = read.table("WMlabelResults_FA.txt", header=TRUE)
colnames(dti.label.fa)=paste("dti.label.fa", colnames(dti.label.fa), sep = "_")

dti.label.md = read.table("WMlabelResults_MD.txt", header=TRUE)

colnames(dti.label.md)=paste("dti.label.md", colnames(dti.label.md), sep = "_")

dti.tract.fa = read.table("WMtractResults_FA.txt", header=TRUE)
colnames(dti.tract.fa)=paste("dti.tract.fa", colnames(dti.tract.fa), sep = "_")

dti.tract.md = read.table("WMtractResults_MD.txt", header=TRUE)
colnames(dti.tract.md)=paste("dti.tract.md", colnames(dti.tract.md), sep = "_")

brain.feature = scale(cbind(fsl.vbm, alff, reho, dti.label.fa, dti.label.md, dti.tract.fa, dti.tract.md))
brain.feature = scale(cbind(fsl.vbm))


# -------------------------- PCA: ---------------------------

pca = princomp(brain.feature)
print(summary(pca))
brain.feature = pca$score[,cbind(1:50)]

# -------------


df.all = cbind(subject.info, brain.feature)
df.all = df.all[-delete.idx,]
#dim(df.all)
#print(head(df.all))

res<-function(y,yhat){
	acc<-length(which(y==yhat))/length(y)
	sensi<-length(which(y==yhat & y==1))/length(which(y==1))
	speci<-length(which(y==yhat & y==0))/length(which(y==0))
	temp<-c(acc, sensi, speci)
	return(temp)
}

lasso.cv.fun = function(x, y, k, lambda.seq, fmly){
	
	#sort data according to y so that we have ballanced value in each group:
	sort.y = sort(y, index.return = T)
	y = sort.y$x
	x = x[sort.y$ix,]
	
	num.sample = length(y)
	cv.k = sample(rep(1:k, each = ceiling(num.sample/k)))[1:num.sample]
	#print(cbind(y,cv.k))
	test.error=0
	for (i in 1:k){	
		x.train = x[cv.k!=i,]
		y.train = y[cv.k!=i]
	
		x.test = x[cv.k==i,]
		y.test = y[cv.k==i]
	
		#lasso.mod = cv.glmnet(x.train, y.train, alpha = 1, lambda = lambda.seq, family=fmly, standardize = F)
		lasso.mod = glmnet(x.train, y.train, alpha = 1, lambda = lambda.seq, family=fmly, standardize = F)
		y.pred = predict(lasso.mod, x.test, s=lasso.mod$lambda.min, type = "class")
		#print("y pred and y test:")
		#print(y.pred)
		#print(y.test)
		
		#y.pred = predict(lasso.mod, x.test, s=lasso.mod$lambda.min, type = "response")
		#print(min(y.pred))
		#print(max(y.pred))
		#print(unique(y.test))
		#
		beta = coef(lasso.mod, s="lambda.1se")
		print(length(which(beta!=0)))
		
		#y.pred = y.pred>=.5
		#y.test = y.test>=mean(y.test)
		#print("predcted y:")
		#print(data.frame(y.pred))
		#print("actual y:")
		#print(y.test)
		error.i = mean(y.pred!=y.test)
		test.error= test.error*(i-1)/i + error.i/i
		
		print(table(y.test, y.pred))
	}
	return(test.error)
}

# lasso binomial regression:

lambda.seq = 10^seq(2, -3, length = 100)
k=5
library(glmnet)

# ---------------------select data for ptsd and hc:---------------------
df.ptsd.hc = df.all[df.all$ptsd==0|df.all$ptsd==1,]
print("dimension for ptsd and hc dataset")
print(dim(df.ptsd.hc))
x = model.matrix(df.ptsd.hc$ptsd~., df.ptsd.hc)
y = df.ptsd.hc$ptsd

lasso.mod = glmnet(x, y, alpha = 1, lambda = lambda.seq,  family = "binomial")
dev.new()
plot(lasso.mod, xvar = "lambda")

cv.lasso = cv.glmnet(x, y, alpha=1, nfolds = k , lambda = lambda.seq, family = "binomial", type.measure = "class")
dev.new()
plot(cv.lasso)
best.lambda = cv.lasso$lambda.min

# k-fold CV:
set.seed(2)
error.ptsd.hc=lasso.cv.fun(x, y, k, best.lambda, "binomial")
print("test error for ptsd and hc:")
print(error.ptsd.hc)

#--------------------- select data for ptsd and trauma: --------------------------------------
df.ptsd.trauma = df.all[df.all$ptsd==0|df.all$ptsd==2,]
print("dimension for ptsd and trauma dataset")
print(dim(df.ptsd.trauma))
x = model.matrix(df.ptsd.trauma$ptsd~., df.ptsd.trauma)
y = df.ptsd.trauma$ptsd

lasso.mod = glmnet(x, y, alpha = 1, lambda = lambda.seq,  family = "binomial")
dev.new()
plot(lasso.mod, xvar = "lambda")

cv.lasso = cv.glmnet(x, y, alpha=1, nfolds = k , lambda = lambda.seq, family = "binomial", type.measure = "class")
dev.new()
plot(cv.lasso)


best.lambda = cv.lasso$lambda.min

print("k-fold CV: for pstd and trauma")
set.seed(3)
error.ptsd.trauma = lasso.cv.fun(x, y, k, best.lambda, "binomial")
print("test error for ptsd and trauma:")
print(error.ptsd.trauma)

#--------------------- select data for hc and trauma:---------------------
df.hc.trauma = df.all[df.all$ptsd==1|df.all$ptsd==2,]
print("dimension for hc and trauma data set")
print(dim(df.hc.trauma))

x = model.matrix(df.hc.trauma$ptsd~., df.hc.trauma)
y = df.hc.trauma$ptsd

lasso.mod = glmnet(x, y, alpha = 1, lambda = lambda.seq,  family = "binomial")
dev.new()
plot(lasso.mod, xvar = "lambda")

cv.lasso = cv.glmnet(x, y, alpha=1, nfolds = k , lambda = lambda.seq, family = "binomial", type.measure = "class")
dev.new()
plot(cv.lasso)
best.lambda = cv.lasso$lambda.min

# k-fold CV:
set.seed(4)
error.hc.trauma = lasso.cv.fun(x, y, k, lambda.seq, "binomial")
print("test error for hc and trauma:")
print(error.hc.trauma)
