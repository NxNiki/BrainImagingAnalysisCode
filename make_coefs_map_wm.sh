#!/bin/sh
# run bash **.sh rather than sh **.sh for this script or error may be enounterd


#atlas_file="JHU_ICBM/rICBM_DTI_81_WMPM_FMRIB58.nii.gz"
atlas_file="JHU_ICBM/JHU_ICBM_tracts_maxprob_thr25_1mm.nii.gz"

for ((i=1;i<=4;i++))
do
#file_name="multidimensional_age_prediction/out03_coefs_fa_label_cluster_km4"$i".csv"
#out_img_name="brain_map_fa_label_km4_"$i

file_name="multidimensional_age_prediction/out03_coefs_fa_tract_cluster_km4"$i".csv"
out_img_name="brain_map_fa_tract_km4_"$i

fslmaths $atlas_file -mul 0 ${out_img_name}_sex
fslmaths $atlas_file -mul 0 ${out_img_name}_intercept
fslmaths $atlas_file -mul 0 ${out_img_name}_age
fslmaths $atlas_file -mul 0 ${out_img_name}_age2
fslmaths $atlas_file -mul 0 ${out_img_name}_sexage
fslmaths $atlas_file -mul 0 ${out_img_name}_sexage2

tr -d '\15\32' < $file_name | sed 1d | while IFS=',' read idx intercept age age2 sex sexage sexage2; do
	echo $idx
	echo $intercept
	echo $age
	echo $sex
	echo $age2
	echo $sexage
	echo $sexage2

	fslmaths $atlas_file -thr $idx -uthr $idx -bin temp.nii.gz

	fslmaths temp.nii.gz -mul $intercept -add ${out_img_name}_intercept ${out_img_name}_intercept
	fslmaths temp.nii.gz -mul $sex -add ${out_img_name}_sex ${out_img_name}_sex
	fslmaths temp.nii.gz -mul $age -add ${out_img_name}_age ${out_img_name}_age
	fslmaths temp.nii.gz -mul $age2 -add ${out_img_name}_age2 ${out_img_name}_age2
	fslmaths temp.nii.gz -mul $sexage -add ${out_img_name}_sexage ${out_img_name}_sexage
	fslmaths temp.nii.gz -mul $sexage2 -add ${out_img_name}_sexage2 ${out_img_name}_sexage2
done

rm temp.nii.gz

done
