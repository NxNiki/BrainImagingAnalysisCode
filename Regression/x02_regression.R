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
head(subject.info)

fsl.vbm = read.table("outc01_fslvbm_features.txt", header=TRUE)
colnames(fsl.vbm)=paste("fsl.vbm", colnames(fsl.vbm), sep = "_")

spm.vbm = read.table("outc01_spmvbm_features.txt", header=TRUE)
colnames(spm.vbm)=paste("spm.vbm", colnames(spm.vbm), sep = "_")

alff = read.table("outc01_Rest_ALFF_Feature.txt", header=TRUE)
colnames(alff)=paste("alff", colnames(alff), sep = "_")

falff = read.table("outc01_Rest_fALFF_Feature.txt", header=TRUE)
colnames(falff)=paste("falff", colnames(falff), sep = "_")

reho = read.table("outc01_Rest_ReHo_Feature.txt", header=TRUE)
colnames(reho)=paste("reho", colnames(reho), sep = "_")

dti.label.fa = read.table("WMlabelResults_FA.txt", header=TRUE)
colnames(dti.label.fa)=paste("dti.label.fa", colnames(dti.label.fa), sep = "_")

dti.label.md = read.table("WMlabelResults_MD.txt", header=TRUE)

colnames(dti.label.md)=paste("dti.label.md", colnames(dti.label.md), sep = "_")

dti.tract.fa = read.table("WMtractResults_FA.txt", header=TRUE)
colnames(dti.tract.fa)=paste("dti.tract.fa", colnames(dti.tract.fa), sep = "_")

dti.tract.md = read.table("WMtractResults_MD.txt", header=TRUE)
colnames(dti.tract.md)=paste("dti.tract.md", colnames(dti.tract.md), sep = "_")

df.all = cbind(subject.info, fsl.vbm, alff, falff, reho, dti.label.fa, dti.label.md, dti.tract.fa, dti.tract.md)
dim(df.all)
head(df.all)

# lasso multinomial regression:
x = model.matrix(df.all$ptsd~., df.all)
y = df.all$ptsd

library(glmnet)
lambda.seq = 10^seq(10, -10, length = 100)

lasso.mod = glmnet(x, y, alpha = 1, lambda = lambda.seq,  family = "multinomial")
dev.new()
plot(lasso.mod, xvar = "lambda")

cv.lasso = cv.glmnet(x, y, alpha=1, nfolds = 10 , type.measure = "class")
dev.new()
plot(cv.lasso)

# 10-fold CV:
k=10
num.sample = length(y)
set.seed(1)
cv.k = sample(rep(1:k, each = ceiling(num.sample/k)))[1:num.sample]

error=0
for (i in 1:10){	
	x.train = x[cv.k!=i,]
	y.train = y[cv.k!=i]

	x.test = x[cv.k==i,]
	y.test = y[cv.k==i]

	lasso.mod = glmnet(x.train, y.train, alpha = 1, lambda = lambda.seq, family="multinomial", )
	y.pred = predict(lasso.mod, x.test, s=lasso.mod$lambda.min, type = "class")
	#y.pred.mse = predict(lasso.mod, x.test, s=lasso.mod$lambda.min)
	error.i = mean(y.pred!=y.test)
	error.i = min(colMeans(y.pred!=y.test))
	error = error*(i-1)/i + error.i/i
}
print("test error for multinomial lasso:")
print(error)

# lasso binomial regression:
# ---------------------select data for ptsd and hc:---------------------
df.ptsd.hc = df.all[df.all$ptsd==0|df.all$ptsd==1,-1]
print("dimension for ptsd and hc dataset")
print(dim(df.ptsd.hc))
x = model.matrix(df.ptsd.hc$ptsd~., df.ptsd.hc)
y = df.ptsd.hc$ptsd

library(glmnet)
lambda.seq = 10^seq(10, -10, length = 100)

lasso.mod = glmnet(x, y, alpha = 1, lambda = lambda.seq,  family = "binomial")
dev.new()
plot(lasso.mod, xvar = "lambda")

cv.lasso = cv.glmnet(x, y, alpha=1, nfolds = 10 , type.measure = "class")
dev.new()
plot(cv.lasso)
best.lambda = cv.lasso$lambda.min

# 10-fold CV:
k=10
num.sample = length(y)
set.seed(1)
cv.k = sample(rep(1:k, each = ceiling(num.sample/k)))[1:num.sample]

error.ptsd.hc=0
for (i in 1:10){	
	x.train = x[cv.k!=i,]
	y.train = y[cv.k!=i]

	x.test = x[cv.k==i,]
	y.test = y[cv.k==i]

	lasso.mod = glmnet(x.train, y.train, alpha = 1, lambda = lambda.seq, family="binomial", )
	y.pred = predict(lasso.mod, x.test, s=best.lambda, type = "class")
	error.i = mean(y.pred!=y.test)
	error.ptsd.hc = error.ptsd.hc*(i-1)/i + error.i/i
}
print("test error for ptsd and hc:")
print(error.ptsd.hc)

#--------------------- select data for ptsd and trauma: --------------------------------------
df.ptsd.trauma = df.all[df.all$ptsd==0|df.all$ptsd==2,-1]
print("dimension for ptsd and trauma dataset")
print(dim(df.ptsd.trauma))
x = model.matrix(df.ptsd.trauma$ptsd~., df.ptsd.trauma)
y = df.ptsd.trauma$ptsd

library(glmnet)
lambda.seq = 10^seq(10, -10, length = 100)

lasso.mod = glmnet(x, y, alpha = 1, lambda = lambda.seq,  family = "binomial")
dev.new()
plot(lasso.mod, xvar = "lambda")

cv.lasso = cv.glmnet(x, y, alpha=1, nfolds = 10 , type.measure = "class")
dev.new()
plot(cv.lasso)
best.lambda = cv.lasso$lambda.min

# 10-fold CV:
k=10
num.sample = length(y)
cv.k = sample(rep(1:k, each = ceiling(num.sample/k)))[1:num.sample]

error.ptsd.trauma=0
for (i in 1:10){	
	x.train = x[cv.k!=i,]
	y.train = y[cv.k!=i]

	x.test = x[cv.k==i,]
	y.test = y[cv.k==i]

	lasso.mod = glmnet(x.train, y.train, alpha = 1, lambda = lambda.seq, family="binomial", )
	y.pred = predict(lasso.mod, x.test, s=best.lambda, type = "class")
	error.i = mean(y.pred!=y.test)
	error.ptsd.trauma = error.ptsd.trauma*(i-1)/i + error.i/i
}
print("test error for ptsd and trauma:")
print(error.ptsd.trauma)

#--------------------- select data for hc and trauma:---------------------
df.hc.trauma = df.all[df.all$ptsd==1|df.all$ptsd==2,-1]
print("dimension for hc and trauma data set")
print(dim(df.hc.trauma))

x = model.matrix(df.hc.trauma$ptsd~., df.hc.trauma)
y = df.hc.trauma$ptsd

library(glmnet)
lambda.seq = 10^seq(10, -10, length = 100)

lasso.mod = glmnet(x, y, alpha = 1, lambda = lambda.seq,  family = "binomial")
dev.new()
plot(lasso.mod, xvar = "lambda")

cv.lasso = cv.glmnet(x, y, alpha=1, nfolds = 10 , type.measure = "class")
dev.new()
plot(cv.lasso)
best.lambda = cv.lasso$lambda.min

# 10-fold CV:
k=10
num.sample = length(y)
set.seed(1)
cv.k = sample(rep(1:k, each = ceiling(num.sample/k)))[1:num.sample]

error.hc.trauma=0
for (i in 1:10){	
	x.train = x[cv.k!=i,]
	y.train = y[cv.k!=i]

	x.test = x[cv.k==i,]
	y.test = y[cv.k==i]

	lasso.mod = glmnet(x.train, y.train, alpha = 1, lambda = lambda.seq, family="binomial", )
	y.pred = predict(lasso.mod, x.test, s=best.lambda, type = "class")
	error.i = mean(y.pred!=y.test)
	error.hc.trauma= error.hc.trauma*(i-1)/i + error.i/i
}
print("test error for hc and trauma:")
print(error.hc.trauma)
