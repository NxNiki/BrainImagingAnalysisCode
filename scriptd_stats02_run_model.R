#!/usr/bin/env Rscript

#  to select features.
# Xin Niu May.4.2017


rm(list=ls())
graphics.off()

#cost.seq = 10^seq(2, -3, length = 20)
k=7
report.name = "rda"

library(reshape2)
source("scriptd_stats01_read_feature.R")
source("scriptd_stats01_read_feature_fc.R")
source("scriptd_stats02_cv_functions.R")

multimodal.feature = scale(cbind(spm.vbm, alff, reho, label.fa, tract.fa, tract.md))
feature.name = c("spm.vbm", "alff", "reho", "label.fa", "tract.fa", "tract.md", "fc", "multimodal.feature")
feature.list = list(spm.vbm, alff, reho, label.fa, tract.fa, tract.md, fc, multimodal.feature)
#feature.list = list(spm.vbm, alff)
report.rows = length(feature.list)

report = data.frame(group = feature.name, Hc_Trauma.acc=rep(NA, report.rows), Hc_Trauma.sensi=rep(NA, report.rows), Hc_Trauma.speci=rep(NA, report.rows),
	Hc_Ptsd.acc=rep(NA,report.rows), Hc_Ptsd.sensi=rep(NA,report.rows), Hc_Ptsd.speci=rep(NA,report.rows),
	Trauma_Ptsd.acc=rep(NA,report.rows), Trauma_Ptsd.sensi=rep(NA,report.rows), Trauma_Ptsd.sensi=rep(NA,report.rows))

report.sd = data.frame(group = feature.name, Hc_Trauma.acc=rep(NA, report.rows), Hc_Trauma.sensi=rep(NA, report.rows), Hc_Trauma.speci=rep(NA, report.rows),
	Hc_Ptsd.acc=rep(NA,report.rows), Hc_Ptsd.sensi=rep(NA,report.rows), Hc_Ptsd.speci=rep(NA,report.rows),
	Trauma_Ptsd.acc=rep(NA,report.rows), Trauma_Ptsd.sensi=rep(NA,report.rows), Trauma_Ptsd.sensi=rep(NA,report.rows))

for (i.feature in 1:length(feature.list)) {
	
	brain.feature = scale(feature.list[[i.feature]])
	df.all = cbind(subject.info[,-1], brain.feature)

	# ---------------------select data for ptsd 0 and 1 :---------------------
	
	df.subset = df.all[df.all$ptsd==0|df.all$ptsd==1,]
	print("dimension for subset dataset")
	print(dim(df.subset))
	print(table(df.subset$ptsd))
	
	x = model.matrix(df.subset$ptsd~., df.subset)
	y = df.subset$ptsd
	print(dim(x))
	set.seed(333)
	
	cv.result = rda.cv.fun(x[,-1], y, k)
	result = cv.result[[1]]
	train.result = cv.result[[2]]
	
	print(result)
	print(train.result)
	
	report[i.feature,2:4] = colMeans(result,na.rm=T)
	report.sd[i.feature,2:4] = apply(result, 2, function(x)sd(na.omit(x)))

	## ---------------------select data for ptsd 1 and 2 :---------------------
	
	df.subset = df.all[df.all$ptsd==1|df.all$ptsd==2,]
	print("dimension for subset dataset")
	print(dim(df.subset))
	print(table(df.subset$ptsd))
	
	x = model.matrix(df.subset$ptsd~., df.subset)
	y = df.subset$ptsd
	set.seed(111)
	
	cv.result = rda.cv.fun(x[,-1], y, k) 
	result = cv.result[[1]]
	train.result = cv.result[[2]]
	
	print(result)
	print(train.result)
	
	report[i.feature,5:7] = colMeans(result,na.rm=T)
	report.sd[i.feature,5:7] = apply(result, 2, function(x)sd(na.omit(x)))

	## ---------------------select data for ptsd 0 and 2 :---------------------
	
	df.subset = df.all[df.all$ptsd==0|df.all$ptsd==2,]
	print("dimension for subset dataset")
	print(dim(df.subset))
	print(table(df.subset$ptsd))
	
	x = model.matrix(df.subset$ptsd~., df.subset)
	y = df.subset$ptsd
	
	set.seed(222)
	
	cv.result = rda.cv.fun(x[,-1], y, k)
	result = cv.result[[1]]
	train.result = cv.result[[2]]
	
	print(result)
	print(train.result)
	
	report[i.feature,8:10] = colMeans(result,na.rm=T)
	report.sd[i.feature,8:10] = apply(result, 2, function(x)sd(na.omit(x)))
	
}

# -------------------- save results : -------------------------------------
print(report)
print(report.sd)

filename = paste("result_", report.name, "_k", toString(k), Sys.time(), ".csv", sep = "")
write.table(report, filename, sep = ",", row.names = F)
filename = paste("result_sd_", report.name, "_k", toString(k), Sys.time(), ".csv", sep = "")
write.table(report.sd, filename, sep = ",", row.names = F)

