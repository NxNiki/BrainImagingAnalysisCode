#!/bin/sh
# run bash **.sh rather than sh **.sh for this script or error may be enounterd

atlas_dir="Atlases"
atlas_sub_dir="Atlases_subcor"

############################## spm.vbm map:
#roi_left=( Inferior_Frontal_Gyrus_pars_triangularis Superior_Frontal_Gyrus )
#weight_left=( 0.212 0.449 )
#roi_right=( Occipital_Fusiform_Gyrus Parietal_Operculum_Cortex Frontal_Medial_Cortex)
#weight_right=(0.531 0.234 0.327)
#roi_subcor=(Left_Amygdala Right_Amygdala)
#weight_subcor=(0.132 0.082)			
#
#roi_right_neg=(Precuneous_Cortex )
#weight_right_neg=(0.455)
#
#out_img_name="weight_img_hc_ptsd_spmvbm"

############################### alff map:
#roi_left_neg=(Frontal_Pole)
#weight_left_neg=(0.543)
#
#out_img_name="weight_img_hc_ptsd_alff"

############################### reho map:
roi_right=(Inferior_Temporal_Gyrus_posterior_division Middle_Temporal_Gyrus_posterior_division)
weight_right=(0.917 0.973)
out_img_name="weight_img_hc_ptsd_reho"

fslmaths $atlas_dir/${roi_right[0]}.nii.gz -mul 0 $out_img_name

#idx=0
#for roi in ${roi_left[@]}; do
#	echo $roi
#	echo $idx
#	echo ${weight_left[$idx]}
#	fslmaths $atlas_dir/$roi.nii.gz -roi 46 45 0 -1 0 -1 0 -1 -thr 30 -bin -mul ${weight_left[$idx]} -add $out_img_name $out_img_name
#	idx=$(expr $idx + 1)
#done
#
idx=0
for roi in ${roi_right[@]}; do
	echo $roi
	echo $idx
	echo ${weight_right[$idx]}
	fslmaths $atlas_dir/$roi.nii.gz -roi 0 45 0 -1 0 -1 0 -1 -thr 30 -bin -mul ${weight_right[$idx]} -add $out_img_name $out_img_name
	idx=$(expr $idx + 1)
done
#
#idx=0
#for roi in ${roi_subcor[@]}; do
#	echo $roi
#	echo $idx
#	echo ${weight_subcor[$idx]}
#	fslmaths $atlas_sub_dir/$roi.nii.gz -roi 46 45 0 -1 0 -1 0 -1 -thr 30 -bin -mul ${weight_left[$idx]} -add $out_img_name $out_img_name
#	idx=$(expr $idx + 1)
#done
#fslmaths $atlas_dir/${roi_left_neg[0]}.nii.gz -mul 0 ${out_img_name}_neg
#idx=0
#for roi in ${roi_left_neg[@]}; do
#	echo $roi
#	echo $idx
#	echo ${weight_left_neg[$idx]}
#	fslmaths $atlas_dir/$roi.nii.gz -roi 46 45 0 -1 0 -1 0 -1 -thr 30 -bin -mul ${weight_left_neg[$idx]} -add ${out_img_name}_neg ${out_img_name}_neg
#	idx=$(expr $idx + 1)
#done

#idx=0
#for roi in ${roi_right_neg[@]}; do
#	echo $roi
#	echo $idx
#	echo ${weight_right_neg[$idx]}
#	fslmaths $atlas_dir/$roi.nii.gz -roi 0 45 0 -1 0 -1 0 -1 -thr 30 -bin -mul ${weight_right_neg[$idx]} -add ${out_img_name}_neg ${out_img_name}_neg
#	idx=$(expr $idx + 1)
#done
