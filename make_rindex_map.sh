#!/bin/sh
# run bash **.sh rather than sh **.sh for this script or error may be enounterd


#atlas_file="aal/aal2.nii.gz"
#roi_idx=(104 106 109 111 35 36 51 88)

#roi_value=(.4 .2 .6 .8 1 .8 .4 .8)
#out_img_name="cindexmap_elastic_net"

#roi_value=(.2 .8 .8 .6 1 .6 1 1)
#out_img_name="cindexmap_Relastic_net"
 
#roi_value=(0 .6 .4 .6 1 .8 0 .6)
#out_img_name="cindexmap_LASSO"

#roi_value=(0 .2 0 .6 1 1 1 1)
#out_img_name="cindexmap_RLASSO"

atlas_file="JHU_ICBM/rICBM_DTI_81_WMPM_FMRIB58.nii.gz"
#roi_idx=(16 15 26)

#roi_value=(1 .8 .6)
#out_img_name="cindexmap_elastic_net_md_label"

#roi_value=(1 .8 0)
#out_img_name="cindexmap_Relastic_net_md_label"

#roi_value=(1 .8 0.4)
#out_img_name="cindexmap_LASSO_md_label"

#roi_value=(1 .8 1)
#out_img_name="cindexmap_RLASSO_md_label"

#roi_idx=(14)

#roi_value=(1)
#out_img_name="cindexmap_elastic_net_fa_label"

#roi_value=(1)
#out_img_name="cindexmap_Relastic_net_fa_label"

#roi_value=(1)
#out_img_name="cindexmap_LASSO_fa_label"

roi_value=(1)
out_img_name="cindexmap_RLASSO_fa_label"

fslmaths $atlas_file -mul 0 $out_img_name
fslmaths $atlas_file -mul 0 $out_img_name

idx=0
for roi in ${roi_idx[@]}; do
	echo $roi
	echo $idx
	echo ${roi_value[$idx]}

	fslmaths $atlas_file -thr $roi -uthr $roi -bin temp.nii.gz
	fslmaths temp.nii.gz -mul ${roi_value[$idx]} -add $out_img_name $out_img_name
	idx=$(expr $idx + 1)
done

rm temp.nii.gz


