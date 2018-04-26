#!/usr/bin/env Rscript

# library to read xls files
#library(gdata)

# read subject information files:
subject.info = read.table("outd01_subject_info.txt", header=TRUE)

# delete subjects with severe head motion or other problems:
subject.delete = read.table("subject_delete.txt", header=F)
print(subject.delete)
delete.idx = subject.info$SUBJID%in%subject.delete$V1
#delete.idx = rep(FALSE, length(delete.idx))


#subject.info$age = scale(subject.info$age)
subject.info$Sex = as.factor(subject.info$Sex)

print(head(subject.info))
print(dim(subject.info))
print(table(subject.info$ptsd))
print(table(subject.info[subject.info$ptsd==0,]$Sex))
print(table(subject.info[subject.info$ptsd==1,]$Sex))
print(table(subject.info[subject.info$ptsd==2,]$Sex))

# -------------------------- fsl gray matter volume ---------------------

fsl.vbm = read.table("outd01_fslvbm_features.txt", header=TRUE)

# ------------------------- spm gray matter volume ----------------------

spm.vbm = read.table("outd01_spmvbm_features.txt", header=TRUE)
spm.vbm.aal = read.table("outd01_spm.vbm.aal_features.txt", header=TRUE)
spm.vbm.bn246 = read.table("outd01_spm.vbm.bn246_features.txt", header=TRUE, )

# ------------------------------- alff ---------------------------------

alff = read.table("outd01_alff_features.txt", header=TRUE)
alff.aal = read.table("outd01_alff.aal_features.txt", header=TRUE, )
alff.bn246 = read.table("outd01_alff.bn246_features.txt", header=TRUE, )

# ------------------------------- falff --------------------------------

falff = read.table("outd01_falff_features.txt", header=TRUE)
falff.aal = read.table("outd01_falff.aal_features.txt", header=TRUE, )
falff.bn246 = read.table("outd01_falff.bn246_features.txt", header=TRUE, )

# --------------------------------- reho ----------------------------------

reho = read.table("outd01_reho_features.txt", header=TRUE)
reho.aal = read.table("outd01_reho.aal_features.txt", header=TRUE, )
reho.bn246 = read.table("outd01_reho.bn246_features.txt", header=TRUE, )

# ------------------------------- label.fa --------------------------------

label.fa = read.table("outd01_label.fa_features.txt", header = TRUE)

# ------------------------------- label.md --------------------------------
label.md = read.table("outd01_label.md_features.txt", header = TRUE)

# ------------------------------- tract.fa --------------------------------

tract.fa = read.table("outd01_tract.fa_features.txt", header = TRUE)

# ------------------------------- tract.md --------------------------------

tract.md = read.table("outd01_tract.md_features.txt", header = TRUE)



