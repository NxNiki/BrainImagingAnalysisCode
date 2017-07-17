#!/bin/sh
# run bash **.sh rather than sh **.sh for this script or error may be enounterd

atlas_wm_dir="Atlases_WM"

############################## md map:
roi_wm=( Uncinate_fasciculus_R Corticospinal_tract_L)
weight=( 0.313 0.337 )

out_img_name="weight_img_hc_ptsd_md"

fslmaths $atlas_wm_dir/${roi_wm[0]}.nii.gz -mul 0 $out_img_name

idx=0
for roi in ${roi_wm[@]}; do
	echo $roi
	echo $idx
	echo ${weight[$idx]}
	fslmaths $atlas_wm_dir/$roi.nii.gz -thr 30 -bin -mul ${weight[$idx]} -add $out_img_name $out_img_name
	idx=$(expr $idx + 1)
done

#fslmaths $atlas_wm_dir/${roi_left_neg[0]}.nii.gz -mul 0 ${out_img_name}_neg
#idx=0
#for roi in ${roi_right_neg[@]}; do
#	echo $roi
#	echo $idx
#	echo ${weight_right_neg[$idx]}
#	fslmaths $atlas_wm_dir/$roi.nii.gz -roi 0 45 0 -1 0 -1 0 -1 -thr 30 -bin -mul ${weight_right_neg[$idx]} -add ${out_img_name}_neg ${out_img_name}_neg
#	idx=$(expr $idx + 1)
#done
