#!/usr/bin/env Rscript

rm(list=ls())
graphics.off()

# library to read xls files
#library(gdata)

# read subject information files:
subject.info = read.table("outb01_loc_info.txt", header=TRUE)

subject.info$Sex = as.numeric(subject.info$Sex)
subject.info$age = scale(subject.info$age)

# delete subjects with severe head motion or other problems:
subject.delete = read.table("subject_delete.txt", header=F)
print(subject.delete)
delete.idx = subject.info$SUBJID%in%subject.delete$V1
#delete.idx = rep(FALSE, length(delete.idx))

subject.info = subject.info[!delete.idx,]
write.table(subject.info, "outd01_loc_subject_info.txt", row.names=F)

print(head(subject.info))
print(dim(subject.info))
culmulative.variance = .95

# -------------------------- fsl gray matter volume ---------------------

fsl.vbm = read.table("outc01_fslvbm_features_thr_30.txt", header=TRUE)
colnames(fsl.vbm)=paste("fsl.vbm", colnames(fsl.vbm), sep = "_")

fsl.vbm = fsl.vbm[!delete.idx,]
write.table(fsl.vbm, "outd01_loc_fslvbm_features.txt", row.names=F)
# ------------------------- spm gray matter volume ----------------------

spm.vbm = read.table("outc01_spmvbm_features_thr_30.txt", header=TRUE)
colnames(spm.vbm)=paste("spm.vbm", colnames(spm.vbm), sep = "_")

spm.vbm = spm.vbm[!delete.idx,]
write.table(spm.vbm, "outd01_loc_spmvbm_features.txt", row.names=F)

# ------------------------------- alff ---------------------------------

alff = read.table("outc01_alff_features_thr_30.txt", header=TRUE)
colnames(alff)=paste("alff", colnames(alff), sep = "_")

alff = alff[!delete.idx,]
write.table(alff, "outd01_loc_alff_features.txt", row.names=F)

# ------------------------------- falff --------------------------------

falff = read.table("outc01_falff_features_thr_30.txt", header=TRUE)
colnames(falff)=paste("falff", colnames(falff), sep = "_")

falff = falff[!delete.idx,]
write.table(falff, "outd01_loc_falff_features.txt", row.names=F)

# --------------------------------- reho ----------------------------------

reho = read.table("outc01_reho_features_thr_30.txt", header=TRUE)
colnames(reho)=paste("reho", colnames(reho), sep = "_")

reho = reho[!delete.idx,]
write.table(reho, "outd01_loc_reho_features.txt", row.names=F)

# ------------------------------- label.fa --------------------------------

#label.fa = read.xls("Result04_Panda/AllAtlasResults/WMlabelResults_FA.xls")
label.fa = read.csv(file="Result04_Panda/AllAtlasResults/WMlabelResults_FA.csv", sep=",")
colnames(label.fa)=paste("label.fa", colnames(label.fa), sep = "_")

label.fa = label.fa[!delete.idx,-1]
write.table(label.fa, "outd01_loc_label.fa_features.txt", row.names=F)

# ------------------------------- label.md --------------------------------
#label.md = read.xlsx(file="Result04_Panda/AllAtlasResults/WMlabelResults_MD.xls", sheetIndex=1, header=TRUE)
#label.md = read.csv(flie="Result04_Panda/AllAtlasResults/WMlabelResults_MD.csv", sep=",")
label.md=read.table("Result04_Panda/AllAtlasResults/WMlabelResults_MD.csv", header=T,sep=",")
colnames(label.md)=paste("label.md", colnames(label.md), sep = "_")

label.md = label.md[!delete.idx,-1]
write.table(label.md, "outd01_loc_label.md_features.txt", row.names=F)

# ------------------------------- tract.fa --------------------------------

#tract.fa = read.xlsx(file="Result04_Panda/AllAtlasResults/WMtractResults_FA.xls", sheetIndex=1, header=TRUE)
tract.fa = read.csv(file="Result04_Panda/AllAtlasResults/WMtractResults_FA.csv", sep = ",")
colnames(tract.fa)=paste("tract.fa", colnames(tract.fa), sep = "_")

tract.fa = tract.fa[!delete.idx,-1]
write.table(tract.fa, "outd01_loc_tract.fa_features.txt", row.names=F)

# ------------------------------- tract.md --------------------------------

#tract.md = read.xlsx(file="Result04_Panda/AllAtlasResults/WMtractResults_MD.xls", sheetIndex=1, header=TRUE)
tract.md = read.csv(file="Result04_Panda/AllAtlasResults/WMtractResults_MD.csv", sep = ",")
colnames(tract.md)=paste("tract.md", colnames(tract.md), sep = "_")

tract.md = tract.md[!delete.idx,-1]
write.table(tract.md, "outd01_loc_tract.md_features.txt", row.names=F)

# ------------------------------- fc --------------------------------------

fc = read.csv(file="outc01_loc_fc.csv", sep = ",")
colnames(fc)=paste("fc", colnames(fc), sep = "_")

fc = fc[!delete.idx,]
write.table(fc, "outd01_loc_fc_features.txt", row.names=F)

