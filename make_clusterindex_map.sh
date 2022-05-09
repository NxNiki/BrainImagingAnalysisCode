#!/bin/sh
# run bash **.sh rather than sh **.sh for this script or error may be enounterd


#atlas_file="aal/aal2.nii.gz"
#file_name="multidimensional_age_prediction_notract/out03_coefs_gmv_cluster_gmmEEE4_all.csv"
#out_img_name="brain_map_gmv_gmm4_all"


atlas_file="JHU_ICBM/rICBM_DTI_81_WMPM_FMRIB58.nii.gz"
file_name="multidimensional_age_prediction_notract/out03_coefs_fa_label_cluster_gmmEEE4_all.csv"
out_img_name="brain_map_fa_label_gmm4_all"

#atlas_file="JHU_ICBM/JHU_ICBM_tracts_maxprob_thr25_1mm.nii.gz"
#file_name="multidimensional_age_prediction/out03_coefs_fa_tract_cluster_km4_all.csv"
#out_img_name="brain_map_fa_tract_km4_all"

fslmaths $atlas_file -mul 0 ${out_img_name}

tr -d '\15\32' < $file_name | sed 1d | while IFS=',' read idx cluster; do
	echo $idx
	echo $cluster

	fslmaths $atlas_file -thr $idx -uthr $idx -bin temp.nii.gz
	fslmaths temp.nii.gz -mul $cluster -add ${out_img_name} ${out_img_name}
done

rm temp.nii.gz


