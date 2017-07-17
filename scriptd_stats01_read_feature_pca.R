#!/usr/bin/env Rscript

rm(list=ls())
graphics.off()

# library to read xls files
#library(gdata)

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
write.table(subject.info, "outd01_subject_info.txt", row.names=F)

print(head(subject.info))
print(dim(subject.info))
culmulative.variance = .95

# -------------------------- fsl gray matter volume ---------------------

fsl.vbm = read.table("outc01_fslvbm_features_thr_30.txt", header=TRUE)
colnames(fsl.vbm)=paste("fsl.vbm", colnames(fsl.vbm), sep = "_")

fsl.vbm = fsl.vbm[!delete.idx,]
pca = princomp(fsl.vbm)
cul.var = cumsum(pca$sdev^2/sum(pca$sdev^2))
num.comp = sum(cul.var<culmulative.variance)+1
fsl.vbm.pca = pca$score[,1:num.comp]
colnames(fsl.vbm.pca)=paste("fsl.vbm", colnames(fsl.vbm.pca), sep = "_")
write.table(fsl.vbm, "outd01_fslvbm_features.txt", row.names=F)
dim(fsl.vbm.pca)
# ------------------------- spm gray matter volume ----------------------

spm.vbm = read.table("outc01_spmvbm_features_thr_30.txt", header=TRUE)
colnames(spm.vbm)=paste("spm.vbm", colnames(spm.vbm), sep = "_")

spm.vbm = spm.vbm[!delete.idx,]
pca = princomp(spm.vbm)
cul.var = cumsum(pca$sdev^2/sum(pca$sdev^2))
num.comp = sum(cul.var<culmulative.variance)+1
spm.vbm.pca = pca$score[,1:num.comp]
colnames(spm.vbm.pca)=paste("spm.vbm", colnames(spm.vbm.pca), sep = "_")
write.table(smp.vbm, "outd01_spmvbm_features.txt", row.names=F)

# ------------------------------- alff ---------------------------------

alff = read.table("outc01_alff_features_thr_30.txt", header=TRUE)
colnames(alff)=paste("alff", colnames(alff), sep = "_")

alff = alff[!delete.idx,]
pca = princomp(alff)
cul.var = cumsum(pca$sdev^2/sum(pca$sdev^2))
num.comp = sum(cul.var<culmulative.variance)+1
alff.pca = pca$score[,1:num.comp]
colnames(alff.pca)=paste("alff", colnames(alff.pca), sep = "_")
write.table(alff, "outd01_alff_features.txt", row.names=F)

# ------------------------------- falff --------------------------------

falff = read.table("outc01_falff_features_thr_30.txt", header=TRUE)
colnames(falff)=paste("falff", colnames(falff), sep = "_")

falff = falff[!delete.idx,]
pca = princomp(falff)
cul.var = cumsum(pca$sdev^2/sum(pca$sdev^2))
num.comp = sum(cul.var<culmulative.variance)+1
falff.pca = pca$score[,1:num.comp]
colnames(falff.pca)=paste("falff", colnames(falff.pca), sep = "_")
write.table(falff, "outd01_falff_features.txt", row.names=F)

# --------------------------------- reho ----------------------------------

reho = read.table("outc01_reho_features_thr_30.txt", header=TRUE)
colnames(reho)=paste("reho", colnames(reho), sep = "_")

reho = reho[!delete.idx,]
pca = princomp(reho)
cul.var = cumsum(pca$sdev^2/sum(pca$sdev^2))
num.comp = sum(cul.var<culmulative.variance)+1
reho.pca = pca$score[,1:num.comp]
colnames(reho.pca)=paste("reho", colnames(reho.pca), sep = "_")
write.table(reho, "outd01_reho_features.txt", row.names=F)

# ------------------------------- label.fa --------------------------------

#label.fa = read.xls("Result04_Panda/AllAtlasResults/WMlabelResults_FA.xls")
label.fa = read.csv(file="Result04_Panda/AllAtlasResults/WMlabelResults_FA.csv", sep=",")
colnames(label.fa)=paste("label.fa", colnames(label.fa), sep = "_")

label.fa = label.fa[!delete.idx,-1]
pca = princomp(label.fa)
cul.var = cumsum(pca$sdev^2/sum(pca$sdev^2))
num.comp = sum(cul.var<culmulative.variance)+1
label.fa.pca = pca$score[,1:num.comp]
colnames(label.fa.pca)=paste("label.fa", colnames(label.fa.pca), sep = "_")
write.table(label.fa, "outd01_label.fa_features.txt", row.names=F)

# ------------------------------- label.md --------------------------------
#print("read label.md")
##label.md = read.xlsx(file="Result04_Panda/AllAtlasResults/WMlabelResults_MD.xls", sheetIndex=1, header=TRUE)
#label.md = read.csv(flie="Result04_Panda/AllAtlasResults/WMlabelResults_MD.csv", sep=",")
#print("read label.md finished")
#colnames(label.md)=paste("label.md", colnames(label.md), sep = "_")
#
#label.md = label.md[!delete.idx,-1]
#pca = princomp(label.md)
#cul.var = cumsum(pca$sdev^2/sum(pca$sdev^2))
#num.comp = sum(cul.var<culmulative.variance)+1
#label.md.pca = pca$score[,1:num.comp]
#colnames(label.md.pca)=paste("label.md", colnames(label.md.pca), sep = "_")
#write.table(label.md, "outd01_label.md_features.txt")

# ------------------------------- tract.fa --------------------------------

#tract.fa = read.xlsx(file="Result04_Panda/AllAtlasResults/WMtractResults_FA.xls", sheetIndex=1, header=TRUE)
tract.fa = read.csv(file="Result04_Panda/AllAtlasResults/WMtractResults_FA.csv", sep = ",")
colnames(tract.fa)=paste("tract.fa", colnames(tract.fa), sep = "_")

tract.fa = tract.fa[!delete.idx,-1]
pca = princomp(tract.fa)
cul.var = cumsum(pca$sdev^2/sum(pca$sdev^2))
num.comp = sum(cul.var<culmulative.variance)+1
tract.fa.pca = pca$score[,1:num.comp]
colnames(tract.fa.pca)=paste("tract.fa", colnames(tract.fa.pca), sep = "_")
write.table(tract.fa, "outd01_tract.fa_features.txt", row.names=F)

# ------------------------------- tract.md --------------------------------

#tract.md = read.xlsx(file="Result04_Panda/AllAtlasResults/WMtractResults_MD.xls", sheetIndex=1, header=TRUE)
tract.md = read.csv(file="Result04_Panda/AllAtlasResults/WMtractResults_MD.csv", sep = ",")
colnames(tract.md)=paste("tract.md", colnames(tract.md), sep = "_")

tract.md = tract.md[!delete.idx,-1]
pca = princomp(tract.md)
cul.var = cumsum(pca$sdev^2/sum(pca$sdev^2))
num.comp = sum(cul.var<culmulative.variance)+1
tract.md.pca = pca$score[,1:num.comp]
colnames(tract.md.pca)=paste("tract.md", colnames(tract.md.pca), sep = "_")
write.table(tract.md, "outd01_tract.md_features.txt", row.names=F)



