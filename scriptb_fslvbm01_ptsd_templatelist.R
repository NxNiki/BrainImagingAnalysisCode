#!/usr/bin/env Rscript

img.info=read.csv("PNC_LOC_SubInfo.csv")
# sort the rows according to subject id:
img.info=img.info[order(img.info$SUBJID),]

img.info=img.info[!duplicated(img.info$SUBJID),]

# subjects with compelete brain images (t1, rest, dti)
img.complete=read.table("outa02_subject_t1_rest_dti.txt", header=F)

#img.info.complete=merge(img.info, img.complete, by.x="SUBJID", by.y="V1")
complete.rows=(img.info$SUBJID %in% img.complete$V1)
img.info.complete=img.info[complete.rows,]

img.loc1=img.info.complete[img.info.complete$group_ind==1,]
img.loc0=img.info.complete[img.info.complete$group_ind==0,]

num.img.loc0=nrow(img.loc0)
num.img.loc1=nrow(img.loc1)

# decide the number of subject for template: we select minium of hc and loc as required by fslvbm
num.each.group=min(num.img.loc0, num.img.loc1)

# randomly select subjects as templates
template.loc0=img.loc0[sample(num.img.loc0, num.each.group),"SUBJID"]
template.loc1=img.loc1[sample(num.img.loc1, num.each.group),"SUBJID"]

template=sort(c(template.loc0, template.loc1))

write.table(template, file="template_list", col.names=F, row.names=F)
write.table(img.info.complete, file="outb01_loc_info.txt", col.names=T, row.names=F)

# adding .nii.gz to subject ids so that they match the brain imaging file names:
system("sed -i 's/$/.nii.gz/' template_list")

# move the file to required directory:
#file.rename(from="template_list", to="Reslut03_Nifti_T1/template_list")
system("mv template_list Result03_Nifti_T1")
