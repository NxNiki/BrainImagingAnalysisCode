#!bin/sh
set -e

fun_make_4d_image(){
	
	img_3d_dir=$1
	prefix=$2
	img_4d_name=$3
	DoFlirt=$4

	# make 4D image if images are 3d in specifid directory:
	if [ "$DoFlirt" = true ];then
		echo flirt on 3d iamges in $img_3d_dir:
		for img_3d in $img_3d_dir/${prefix}*.nii; do
			image_name=${img_3d%%.nii}
			image_name=${image_name##*/}
			# change the resolution of atlas so that fslmaths works:
			flirt -in $img_3d -ref $FSLDIR/data/standard/avg152T1_brain -out tempc01_flirt_$image_name
		done	
		# create 4D image if necessary:
		fslmerge -t ${img_4d_name} `imglob tempc01_flirt_*.nii.gz`
		rm tempc01_flirt_*.nii.gz
	else
		echo create 4d images in $img_3d_dir
		# create 4D image if necessary:
		fslmerge -t ${img_4d_name} `imglob $img_3d_dir/${prefix}*.nii`
	fi

}

fun_extract_feature(){
	
	echo extract_feature: $img_4d_name
	img_4d_name=$1
	out_file_name=$2
	mask_thresh=$3
	
	##### Step01: compute mean for each atlase ##########
	for atlas in Atlases/*.nii.gz
	do
		atlas_name=${atlas%%.nii.gz}
		atlas_name=${atlas_name##*/}
		echo $atlas
		
		# make left hemisphere mask:
		fslmaths $atlas -roi 46 45 0 -1 0 -1 0 -1 -thr $mask_thresh tempc01_l_mask_$atlas_name
		fslmaths ${img_4d_name} -mul tempc01_l_mask_$atlas_name tempc01_l_${atlas_name}.nii.gz
		fslstats -t tempc01_l_${atlas_name}.nii.gz -M > tempc01_l_${atlas_name}.txt
		
		# make right hemisphere mask:
		fslmaths $atlas -roi 0 45 0 -1 0 -1 0 -1 -thr $mask_thresh tempc01_r_mask_$atlas_name
		fslmaths ${img_4d_name} -mul tempc01_r_mask_$atlas_name tempc01_r_${atlas_name}.nii.gz
		fslstats -t tempc01_r_${atlas_name}.nii.gz -M > tempc01_r_${atlas_name}.txt
	done
	
	for atlas in Atlases_subcor/*.nii.gz
	do
		atlas_name=${atlas%%.nii.gz}
		atlas_name=${atlas_name##*/}
		echo $atlas
		
		fslmaths $atlas -thr $mask_thresh tempc01_mask_$atlas_name
		fslmaths ${img_4d_name} -mul tempc01_mask_$atlas_name tempc01_${atlas_name}.nii.gz
		fslstats -t tempc01_${atlas_name}.nii.gz -M > tempc01_${atlas_name}.txt
		
	done
	
	
	##### Step02: combine data for all atlases ##########
	
	ls tempc01_*.txt | tr "\n" "\t"| sed 's/tempc01_//g'| sed 's/.txt//g' > ${out_file_name}_thr_${mask_thresh}.txt
	# add Feed Line, echo automaticlly add \n at the end of line, so we just add an empty charactor:
	echo "" >> ${out_file_name}_thr_${mask_thresh}.txt
	paste -d "\t" tempc01_*.txt >> ${out_file_name}_thr_${mask_thresh}.txt
	rm tempc01*

}


mask_thresh=30

# --------------parameters for fsl vbm:--------------

#img_4d="Result03_Nifti_T1/stats/GM_mod_merg_s3.nii.gz"
#out_file_name="outc01_fslvbm_features"
#
#fun_extract_feature $img_4d $out_file_name $mask_thresh

# --------------parameters for spm vbm:--------------

img_3d_dir="Result03_Nifti_T1/spmvbm"
prefix="sm0wrp1"
img_4d_name="outc01_spmvbm_s8mm"
out_file_name="outc01_spmvbm_features"
DoFlirt=true

fun_make_4d_image $img_3d_dir $prefix $img_4d_name $DoFlirt
fun_extract_feature $img_4d_name $out_file_name $mask_thresh

## --------------parameters for reho:--------------
#
#img_3d_dir="Result04_DPASF/ResultsS/ReHo_FunImgARCWF"
#prefix="szReHoMap_sub"
#img_4d_name="outc01_reho4d"
#out_file_name="outc01_reho_features"
#DoFlirt=false
#
#fun_make_4d_image $img_3d_dir $prefix $img_4d_name $DoFlirt
#fun_extract_feature $img_4d_name $out_file_name $mask_thresh
#
## --------------parameters for alff:--------------
#
#img_3d_dir="Result04_DPASF/ResultsS/ALFF_FunImgARCW"
#prefix="szALFFMap_sub"
#img_4d_name="outc01_alff4d"
#out_file_name="outc01_alff_features"
#DoFlirt=false
#
#fun_make_4d_image $img_3d_dir $prefix $img_4d_name $DoFlirt
#fun_extract_feature $img_4d_name $out_file_name $mask_thresh
#
## --------------parameters for falff:--------------
#
#img_3d_dir="Result04_DPASF/ResultsS/fALFF_FunImgARCW"
#prefix="szfALFFMap_sub"
#img_4d_name="outc01_falff4d"
#out_file_name="outc01_falff_features"
#DoFlirt=false
#
#fun_make_4d_image $img_3d_dir $prefix $img_4d_name $DoFlirt
#fun_extract_feature $img_4d_name $out_file_name $mask_thresh
#
## ---------------------------------------------------
#
