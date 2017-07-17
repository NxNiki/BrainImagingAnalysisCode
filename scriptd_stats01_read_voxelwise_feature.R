#!/usr/bin/env Rscript

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
write.table(subject.info, "outd01_subject_info.txt", row.names=F)

print(head(subject.info))
print(dim(subject.info))
print(table(subject.info$ptsd))
print(table(subject.info[subject.info$ptsd==0,]$Sex))
print(table(subject.info[subject.info$ptsd==1,]$Sex))
print(table(subject.info[subject.info$ptsd==2,]$Sex))
# -------------------------- fsl gray matter volume ---------------------

fsl.vbm = read.table("outc01_fslvbm_voxelwise_feature.txt", header=F, sep=",")
print(head(fsl.vbm))
colnames(fsl.vbm)=paste("fsl.vbm", colnames(fsl.vbm), sep = "_")
fsl.vbm = fsl.vbm[!delete.idx,]
print("dimension of fsl.vbm")
print(dim(fsl.vbm))

write.table(fsl.vbm, "outd01_fslvbm_voxelwise_feature.txt", row.names=F)

## ------------------------- spm gray matter volume ----------------------
#
#spm.vbm = read.table("outc01_spmvbm_voxelwise_feature.txt", header=TRUE)
#colnames(spm.vbm)=paste("spm.vbm", colnames(spm.vbm), sep = "_")
#spm.vbm = spm.vbm[!delete.idx,]
#write.table(spm.vbm, "outd01_spmvbm_voxelwise_feature.txt", row.names=F)
#
## ------------------------------- alff ---------------------------------
#
#alff = read.table("outc01_alff_voxelwise_feature.txt", header=TRUE)
#colnames(alff)=paste("alff", colnames(alff), sep = "_")
#alff = alff[!delete.idx,]
#write.table(alff, "outd01_alff_voxelwise_feature.txt", row.names=F)
#
## ------------------------------- falff --------------------------------
#
#falff = read.table("outc01_falff_voxelwise_feature.txt", header=TRUE)
#colnames(falff)=paste("falff", colnames(falff), sep = "_")
#falff = falff[!delete.idx,]
#write.table(falff, "outd01_falff_voxelwise_feature.txt", row.names=F)
#
## --------------------------------- reho ----------------------------------
#
#reho = read.table("outc01_reho_voxelwise_feature.txt", header=TRUE)
#colnames(reho)=paste("reho", colnames(reho), sep = "_")
#reho = reho[!delete.idx,]
#write.table(reho, "outd01_reho_voxelwise_feature.txt", row.names=F)
#
#write.table(tract.md, "outd01_tract.md_voxelwise_feature.txt", row.names=F)
#
#
#
