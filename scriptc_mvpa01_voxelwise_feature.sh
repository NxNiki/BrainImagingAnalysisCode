#!bin/sh
set -e
##### Step01: compute mean for each atlase ##########

# --------------parameters for fsl vbm:--------------

img="Result03_Nifti_T1_2/stats/GM_mod_merg_s3.nii.gz"
mask="Result03_Nifti_T1_2/stats/GM_mask.nii.gz"
out_file_name="outc01_fslvbm_voxel_features"
Is4DImg=true
DoFlirt=false

# --------------parameters for spm vbm:--------------

#img="Result03_Nifti_T1_2/stats/GM_SPM_Dartel_s.nii.gz"
#out_file_name="outc01_spmvbm_features"
#Make4DImg=false
#DoFlirt=false

# --------------parameters for reho:--------------

#img_dir="Result04_DPASF/ResultsS/ReHo_FunImgARCWF"
#out_file_name="outc01_reho_features"
#Make4DImg=true
#DoFlirt=false
#prefix="szReHoMap_sub"

# --------------parameters for alff:--------------

#img_dir="Result04_DPASF/ResultsS/ALFF_FunImgARCW"
#out_file_name="outc01_alff_features"
#Make4DImg=true
#DoFlirt=false
#prefix="szALFFMap_sub"

# --------------parameters for falff:--------------

#img_dir="Result04_DPASF/ResultsS/fALFF_FunImgARCW"
#out_file_name="outc01_falff_features"
#Make4DImg=true
#DoFlirt=false
#prefix="szfALFFMap_sub"

# ---------------------------------------------------

if [ "$Is4DImg" = true ]; then

	fslmaths $img -mul $mask temp_4d_mask.nii.gz

else if [ "$DoFlirt" = true ];then
		echo flirt on 3d iamges:
		for img_3d in $img_dir/${prefix}*.nii; do
			image_name=${img_3d%%.nii}
			image_name=${image_name##*/}
			# change the resolution of atlas so that fslmaths works:
			flirt -in $img_3d -ref $FSLDIR/data/standard/avg152T1_brain -out temp_flirt_$image_name
		done	
		# create 4D image if necessary:
		fslmerge -t temp_4D `imglob temp_flirt_*.nii.gz`
		rm temp_flirt_*.nii.gz
	fi
fi


fsl2ascii temp_4d_mask.nii.gz tempc01_ascii_

mkdir -p Result_c01_ascii
mv tempc01_ascii* Result_c01_ascii
#rm temp_4D.nii.gz
rm temp_4d_mask.nii.gz

##### Step02: combine data for all atlases ##########

cd Result_c01_ascii
for file in tempc01_ascii*; do
	cat $file | tr -d "\n" | sed 's/\ 0\ /\ /g'>> ${out_file_name}.txt
	echo "\n" >> ${out_file_name}.txt
done

## add Feed Line, echo automaticlly add \n at the end of line, so we just add an empty charactor:
#echo "" >> ${out_file_name}_thr_${mask_thresh}.txt
#
#paste -d "\t" tempc01_*.txt >> ${out_file_name}_thr_${mask_thresh}.txt
#
mv ${out_file_name}.txt ../
rm tempc01*
cd ..


