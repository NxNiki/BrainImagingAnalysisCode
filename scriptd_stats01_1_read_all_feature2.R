#!/usr/bin/env Rscript

# library to read xls files
#library(gdata)

# read subject information files:
subject.info = read.table("outd01_all_subject_info.txt", header=TRUE)

print(head(subject.info))
print(dim(subject.info))
print(table(subject.info$ptsd))
print(table(subject.info[subject.info$ptsd==0,]$Sex))
print(table(subject.info[subject.info$ptsd==1,]$Sex))
print(table(subject.info[subject.info$ptsd==2,]$Sex))

# -------------------------- cat12 gray matter volume ---------------------

cat.vbm.hammers = read.table("ROI_catROI_hammers_Vgm.csv", header=TRUE, sep = ",")[,-1]

# -------------------------- cat12 gray matter volume ---------------------

cat.vbm.neuromorph = read.table("ROI_catROI_neuromorphometrics_Vgm.csv", header=TRUE, sep = ",")[,-1]

# -------------------------- fsl gray matter volume ---------------------

fsl.vbm = read.table("outd01_all_fslvbm_features.txt", header=TRUE)

# ------------------------- spm gray matter volume ----------------------

spm.vbm = read.table("outd01_all_spmvbm_features.txt", header=TRUE)
spm.vbm.aal = read.table("outd01_all_spm.vbm.aal_features.txt", header=TRUE)
spm.vbm.bn246 = read.table("outd01_all_spm.vbm.bn246_features.txt", header=TRUE, )

# ------------------------------- alff ---------------------------------

alff = read.table("outd01_all_alff_features.txt", header=TRUE)
alff.aal = read.table("outd01_all_alff.aal_features.txt", header=TRUE, )
alff.bn246 = read.table("outd01_all_alff.bn246_features.txt", header=TRUE, )

# ------------------------------- falff --------------------------------

falff = read.table("outd01_all_falff_features.txt", header=TRUE)
falff.aal = read.table("outd01_all_falff.aal_features.txt", header=TRUE, )
falff.bn246 = read.table("outd01_all_falff.bn246_features.txt", header=TRUE, )

# --------------------------------- reho ----------------------------------

reho = read.table("outd01_all_reho_features.txt", header=TRUE)
reho.aal = read.table("outd01_all_reho.aal_features.txt", header=TRUE, )
reho.bn246 = read.table("outd01_all_reho.bn246_features.txt", header=TRUE, )

# ------------------------------- label.fa --------------------------------

label.fa = read.table("outd01_all_label.fa_features.txt", header = TRUE)

# ------------------------------- label.md --------------------------------
label.md = read.table("outd01_all_label.md_features.txt", header = TRUE)

# ------------------------------- tract.fa --------------------------------

tract.fa = read.table("outd01_all_tract.fa_features.txt", header = TRUE)

# ------------------------------- tract.md --------------------------------

tract.md = read.table("outd01_all_tract.md_features.txt", header = TRUE)

