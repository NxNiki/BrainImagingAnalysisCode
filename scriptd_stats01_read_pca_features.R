#!/usr/bin/env Rscript

rm(list=ls())

# library to read xls files
#library(gdata)

# read subject information files:
subject.info = read.table("outb01_subject_info.txt", header=TRUE)
subject.info$Sex = as.numeric(subject.info$Sex)
subject.info$age = scale(subject.info$age)

# delete subjects with severe head motion or other problems:
subject.delete = read.table("subject_delete.txt", header=F)
print(subject.delete)
delete.idx = subject.info$SUBJID%in%subject.delete$V1
#delete.idx = rep(FALSE, length(delete.idx))

subject.info = subject.info[!delete.idx,]
write.table(subject.info, "outd01_ptsd_subject_info.txt", row.names=F)

print(head(subject.info))
print(dim(subject.info))

# -------------------------- read pca features ---------------------

pc1 = read.table("outc02_pca_feature1.txt", header=TRUE)
colnames(pc1)=paste("pc1", colnames(pc1), sep = "_")

pc2 = read.table("outc02_pca_feature2.txt", header=TRUE)
colnames(pc2)=paste("pc2", colnames(pc2), sep = "_")

pc3 = read.table("outc02_pca_feature3.txt", header=TRUE)
colnames(pc3)=paste("pc3", colnames(pc3), sep = "_")

pc.loading = read.table("outc02_pca_loading.txt", header=TRUE)
