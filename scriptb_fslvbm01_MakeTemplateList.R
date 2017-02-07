#!/usr/bin/env Rscript

img.info=read.csv("PTSD_HC_Final2.csv")
img.info=img.info[!duplicated(img.info$SUBJID),]

img.complete=read.table("outa02_subject_t1_rest_dti2.txt", header=F)

#img.info.complete=merge(img.info, img.complete, by.x="SUBJID", by.y="V1")
complete.rows=(img.info$SUBJID %in% img.complete$V1)
img.info.complete=img.info[complete.rows,]

img.ptsd=img.info.complete[img.info.complete$ptsd==1,]
img.hc=img.info.complete[img.info.complete$ptsd==0,]
img.ptsd2=img.info.complete[img.info.complete$ptsd==2,]

num.img.ptsd=nrow(img.ptsd)
num.img.ptsd2=nrow(img.ptsd2)
num.img.hc=nrow(img.hc)

# decide the number of subject for template: we select minium of hc and ptsd as required by fslvbm
num.each.group=min(num.img.ptsd, num.img.hc, num.img.ptsd2)

# randomly select subjects as templates
template.ptsd=img.ptsd[sample(num.img.ptsd, num.each.group),"SUBJID"]
template.ptsd2=img.ptsd2[sample(num.img.ptsd2, num.each.group),"SUBJID"]
template.hc=img.hc[sample(num.img.hc, num.each.group),"SUBJID"]

template=sort(c(template.ptsd, template.hc, template.ptsd2))

write.table(template, file="template_list", col.names=F, row.names=F)
write.table(img.info.complete, file="outb01_ptsd_hc_info2.txt", col.names=T, row.names=F)

# adding .nii.gz to subject ids so that they match the brain imaging file names:
system("sed -i 's/$/.nii.gz/' template_list")

# move the file to required directory:
#file.rename(from="template_list", to="Reslut03_Nifti_T1/template_list")
system("mv template_list Result03_Nifti_T1_2")
