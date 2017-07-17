#!/usr/bin/env Rscript

# run pca for each subject on the atlase by modality matrix
# modality: vbm, falff, alff, reho


# delete subjects with severe head motion or other problems:
subject.info = read.table("outb01_ptsd_hc_info2.txt", header=TRUE)
subject.delete = read.table("subject_delete.txt", header=F)
delete.idx = subject.info$SUBJID%in%subject.delete$V1

# read original features
# -------------------------- fsl gray matter volume ---------------------

fsl.vbm = read.table("outc01_fslvbm_features_thr_30.txt", header=TRUE)
brain.regions = colnames(fsl.vbm)
fsl.vbm = as.data.frame(scale(fsl.vbm[!delete.idx,]))

# ------------------------- spm gray matter volume ----------------------

spm.vbm = read.table("outc01_spmvbm_features_thr_30.txt", header=TRUE)
spm.vbm = as.data.frame(scale(spm.vbm[!delete.idx,]))

# ------------------------------- alff ---------------------------------

alff = read.table("outc01_alff_features_thr_30.txt", header=TRUE)
alff = as.data.frame(scale(alff[!delete.idx,]))

# ------------------------------- falff --------------------------------

falff = read.table("outc01_falff_features_thr_30.txt", header=TRUE)
falff = as.data.frame(scale(falff[!delete.idx,]))

# --------------------------------- reho ----------------------------------

reho = read.table("outc01_reho_features_thr_30.txt", header=TRUE)
reho = as.data.frame(scale(reho[!delete.idx,]))

# run PCA for each subject:

num.subjects = nrow(fsl.vbm)
num.pca.comp = 5
pca.loading = data.frame(matrix(ncol=num.pca.comp, nrow=num.subjects))
pca.feature = array(,c(num.subjects, length(brain.regions), num.pca.comp))
dimnames(pca.feature)[[2]]=brain.regions

for (i.sub in 1:num.subjects){
	feature = cbind(t(fsl.vbm[i.sub,]), t(spm.vbm[i.sub,]), t(alff[i.sub,]), t(falff[i.sub,]), t(reho[i.sub,]))
	pca = princomp(feature)
	pca.loading[i.sub,] = pca$loading
	
	for (i.pca in 1:num.pca.comp){
		pca.feature[i.sub,,i.pca] = t(pca$score[,i.pca])
	}
}

write.table(pca.loading, 'outc02_pca_loading.txt', row.names=F)

for (i.pca in 1:num.pca.comp){
	write.table(pca.feature[,,i.pca], paste('outc02_pca_feature', toString(i.pca), '.txt', sep=""), row.names=F)
}
