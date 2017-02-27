data_info<-read.table(file="outb01_ptsd_hc_info2.txt",  header=T, sep=" ")
head(data_info)
dim(data_info)
#165 by 4
table(data_info$ptsd)
#> table(data$ptsd)
#   0    1     2 
#  60   35   70 

#install.packages("xlsx")
library(xlsx)
MRI<-read.xlsx(file="outc01_SPM_GM_Feature.xlsx", sheetIndex=1,  header=T)
head(MRI)
dim(MRI)
#165 by 48
str(MRI)


data_c<-cbind(data_info, MRI)
dim(data_c)
#write.xlsx(data_c, file="combined_data.xlsx")
#165 by 52
data_c$Sex<-as.numeric(data_c$Sex)
data_c$ptsd<-as.numeric(data_c$ptsd)
res<-sort(data_c$ptsd, decreasing=F, index.return=T)
data <- data_c[ res$ix , ]


ind0<-c(1:45)
ind1<-c(61:87)
ind2<-c(96:148)

ind<-c(1:95)

res<-function(y,yhat){
	acc<-length(which(y==yhat))/length(y)
	sensi<-length(which(y==yhat & y==1))/length(which(y==1))
	speci<-length(which(y==yhat & y==0))/length(which(y==0))
	temp<-c(acc, sensi, speci)
	return(temp)
}

my_lasso<-function(data, ind, ind0, ind1){
data_temp<-data[ind, ]
ind_tr<-c(ind0, ind1)
data_tr<-data_temp[ind_tr, ]
data_tst<-data_temp[-ind_tr, ]
ind_p<-c(2,3,5:52)
data_trx<-data_tr[, ind_p]
data_trx<-data.matrix(data_trx)
data_try<-data_tr[, 4]
data_tstx<-data_tst[, ind_p]
data_tstx<-data.matrix(data_tstx)
data_tsty<-data_tst[, 4]
table(data_try)
#### LASSO , should I standardize ????  #####
library(glmnet)
set.seed(123)
fitls1<-cv.glmnet(x=data_trx,y=data_try, type.measure="class", nfolds=5,alpha=1,family="binomial", standardize = F)
betas<-coef(fitls1, s="lambda.min")
betas
yprob1 = predict(fitls1,s=fitls1$lambda.min,newx=data_tstx,type="response")
yhat1<-yprob1
yhat1[yprob1>=0.5]<-1
yhat1[yprob1<0.5]<-0

return(list(betas=betas, yhat=yhat1, y=data_tsty))
}
### PTSD0 versus PTSD1 ####
temp<-my_lasso(data, ind, ind0, ind1)
### PTSD0 versus PTSD2 ####
ind<-c(1:60, 96:165)
ind0<-c(1:45)
ind1<-c(61:113)
temp<-my_lasso(data, ind, ind0, ind1)


p=length(which(temp$betas!=0))
p
table(temp$yhat, temp$y)
res(temp$yhat, temp$y)
cbind(temp$yhat, temp$y)
