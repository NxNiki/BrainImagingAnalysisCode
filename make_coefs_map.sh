#!/bin/sh
# run bash **.sh rather than sh **.sh for this script or error may be enounterd


atlas_dir="aal"

file_name="multidimensional_age_prediction/out03_coefs_gmv_cluster_km44.csv"
out_img_name="brain_map_gmv_km4_4"

fslmaths $atlas_dir/aal2.nii.gz -mul 0 ${out_img_name}_sex
fslmaths $atlas_dir/aal2.nii.gz -mul 0 ${out_img_name}_intercept
fslmaths $atlas_dir/aal2.nii.gz -mul 0 ${out_img_name}_age
fslmaths $atlas_dir/aal2.nii.gz -mul 0 ${out_img_name}_age2
fslmaths $atlas_dir/aal2.nii.gz -mul 0 ${out_img_name}_sexage
fslmaths $atlas_dir/aal2.nii.gz -mul 0 ${out_img_name}_sexage2

tr -d '\15\32' < $file_name | sed 1d | while IFS=',' read idx intercept age age2 sex sexage sexage2; do
	echo $idx
	echo $intercept
	echo $age
	echo $sex
	echo $age2
	echo $sexage
	echo $sexage2

	fslmaths $atlas_dir/aal2.nii.gz -thr $idx -uthr $idx -bin temp.nii.gz

	fslmaths temp.nii.gz -mul $intercept -add ${out_img_name}_intercept ${out_img_name}_intercept
	fslmaths temp.nii.gz -mul $sex -add ${out_img_name}_sex ${out_img_name}_sex
	fslmaths temp.nii.gz -mul $age -add ${out_img_name}_age ${out_img_name}_age
	fslmaths temp.nii.gz -mul $age2 -add ${out_img_name}_age2 ${out_img_name}_age2
	fslmaths temp.nii.gz -mul $sexage -add ${out_img_name}_sexage ${out_img_name}_sexage
	fslmaths temp.nii.gz -mul $sexage2 -add ${out_img_name}_sexage2 ${out_img_name}_sexage2
done

rm temp.nii.gz


