#!/bin/sh
# run bash **.sh rather than sh **.sh for this script or error may be enounterd


atlas="aparcaseg.nii.gz"

#file_name="MRI_top.csv"
#out_img_name="brain_map_feature_importance_mri"

file_name="PET_top.csv"
out_img_name="brain_map_feature_importance_pet_surface"

fslmaths $atlas -mul 0 ${out_img_name}

tr -d '\15\32' < $file_name | sed 1d | while IFS=',' read idx value label; do
	echo $idx
	echo $value
	echo $label

	fslmaths $atlas -thr $idx -uthr $idx -bin temp.nii.gz
	fslmaths temp.nii.gz -mul $value -add ${out_img_name} ${out_img_name}
done

rm temp.nii.gz


