#!/usr/bin/env Rscript

rm(list=ls())
graphics.off()

# read subject information files:
subject.info = read.table("outb01_ptsd_hc_info2.txt", header=TRUE)

subject.info$Sex = as.numeric(subject.info$Sex)
subject.info$age = scale(subject.info$age)

# delete subjects with severe head motion or other problems:
subject.delete = read.table("subject_delete.txt", header=F)
print(subject.delete)
delete.idx = subject.info$SUBJID%in%subject.delete$V1
#delete.idx = rep(FALSE, length(delete.idx))

subject.info = subject.info[!delete.idx,]

print(head(subject.info))
print(dim(subject.info))

# -------------------------- fsl gray matter volume ---------------------

fsl.vbm = read.table("outc01_fslvbm_features_thr_30.txt", header=TRUE)
colnames(fsl.vbm)=paste("fsl.vbm", colnames(fsl.vbm), sep = "_")

fsl.vbm = fsl.vbm[!delete.idx,]
pca = princomp(fsl.vbm)
print(summary(pca))
fsl.vbm.pca = pca$score
colnames(fsl.vbm.pca)=paste("fsl.vbm", colnames(fsl.vbm.pca), sep = "_")

# ------------------------- spm gray matter volume ----------------------

spm.vbm = read.table("outc01_spmvbm_features_thr_30.txt", header=TRUE)
colnames(spm.vbm)=paste("spm.vbm", colnames(spm.vbm), sep = "_")

spm.vbm = spm.vbm[!delete.idx,]
pca = princomp(spm.vbm)
print(summary(pca))
spm.vbm.pca = pca$score
colnames(spm.vbm.pca)=paste("spm.vbm", colnames(spm.vbm.pca), sep = "_")

# ------------------------------- alff ---------------------------------

alff = read.table("outc01_alff_features_thr_30.txt", header=TRUE)
colnames(alff)=paste("alff", colnames(alff), sep = "_")

alff = alff[!delete.idx,]
pca = princomp(alff)
print(summary(pca))
alff.pca = pca$score
colnames(alff.pca)=paste("alff", colnames(alff.pca), sep = "_")

# ------------------------------- falff --------------------------------

falff = read.table("outc01_falff_features_thr_30.txt", header=TRUE)
colnames(falff)=paste("falff", colnames(falff), sep = "_")

falff = falff[!delete.idx,]
pca = princomp(falff)
print(summary(pca))
falff.pca = pca$score
colnames(falff.pca)=paste("falff", colnames(falff.pca), sep = "_")

# --------------------------------- reho ----------------------------------

reho = read.table("outc01_reho_features_thr_30.txt", header=TRUE)
colnames(reho)=paste("reho", colnames(reho), sep = "_")

reho = reho[!delete.idx,]
pca = princomp(reho)
print(summary(pca))
reho.pca = pca$score
colnames(reho.pca)=paste("reho", colnames(reho.pca), sep = "_")

# ------------------------------- label.fa --------------------------------

label.fa = read.table("WMlabelResults_FA.txt", header=TRUE)
colnames(label.fa)=paste("label.fa", colnames(label.fa), sep = "_")

label.fa = label.fa[!delete.idx,]
pca = princomp(label.fa)
print(summary(pca))
label.fa.pca = pca$score
colnames(label.fa.pca)=paste("label.fa", colnames(label.fa.pca), sep = "_")

# ------------------------------- label.md --------------------------------

label.md = read.table("WMlabelResults_MD.txt", header=TRUE)
colnames(label.md)=paste("label.md", colnames(label.md), sep = "_")

label.md = label.md[!delete.idx,]
pca = princomp(label.md)
print(summary(pca))
label.md.pca = pca$score
colnames(label.md.pca)=paste("label.md", colnames(label.md.pca), sep = "_")

# ------------------------------- tract.fa --------------------------------

tract.fa = read.table("WMtractResults_FA.txt", header=TRUE)
colnames(tract.fa)=paste("tract.fa", colnames(tract.fa), sep = "_")

tract.fa = tract.fa[!delete.idx,]
pca = princomp(tract.fa)
print(summary(pca))
tract.fa.pca = pca$score
colnames(tract.fa.pca)=paste("tract.fa", colnames(tract.fa.pca), sep = "_")

# ------------------------------- tract.md --------------------------------

tract.md = read.table("WMtractResults_MD.txt", header=TRUE)
colnames(tract.md)=paste("tract.md", colnames(tract.md), sep = "_")

tract.md = tract.md[!delete.idx,]
pca = princomp(tract.md)
print(summary(pca))
tract.md.pca = pca$score
colnames(tract.md.pca)=paste("tract.md", colnames(tract.md.pca), sep = "_")



