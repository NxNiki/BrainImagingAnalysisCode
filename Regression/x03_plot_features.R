#!/usr/bin/env Rscript

rm(list=ls())
graphics.off()

source("x02_read_features.R")

output.dir = "outx03_plot_features"
if (!file.exists(output.dir)){
	dir.create(output.dir)
}

brain.feature = scale(cbind(fsl.vbm, spm.vbm, alff, falff, reho, label.fa, label.md, tract.fa, tract.md))

ptsd.idx = subject.info$ptsd==2
trauma.idx = subject.info$ptsd==1
hc.idx = subject.info$ptsd==0

num.features = dim(brain.feature)[2]

for (i in 1:num.features){
	plot.name = colnames(brain.feature)[i]
	pdf(paste(plot.name, ".pdf", sep = ""))
	plot(subject.info$ptsd, brain.feature[,i], ylab = plot.name)
	file.rename(from=paste(plot.name, ".pdf", sep = ""), to = paste(output.dir, "/", plot.name, ".pdf", sep = ""))
	dev.off()	
}

