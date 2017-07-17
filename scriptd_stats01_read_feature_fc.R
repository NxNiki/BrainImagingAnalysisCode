#!/usr/bin/env Rscript

graphics.off()

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
write.table(subject.info, "outd01_subject_info.txt", row.names=F)

print(head(subject.info))
print(dim(subject.info))
culmulative.variance = .95

# ------------------------------- fc --------------------------------

fc = read.csv(file="outc01_fc.csv", sep = ",")
colnames(fc)=paste("fc", colnames(fc), sep = "_")
fc = fc[!delete.idx,-1]
write.table(fc, "outd01_fc_features.txt", row.names=F)



