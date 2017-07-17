#!bin/sh
set -e
##### Step01: compute mean for each atlase ##########

# --------------parameters for fsl vbm:--------------

#img="Result03_Nifti_T1_2/stats/GM_mod_merg_s3.nii.gz"
#out_file_name="outc01_fslvbm_features"
#Make4DImg=false
#DoFlirt=false

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

img_dir="Result04_DPASF/ResultsS/fALFF_FunImgARCW"
out_file_name="outc01_falff_features"
Make4DImg=true
DoFlirt=false
prefix="szfALFFMap_sub"

# ---------------------------------------------------

#atlases="Atlases/"
mask_thresh=30

if [ "$Make4DImg" = true ]; then
	# make 4D image if images are 3d in specifid directory:
	if [ "$DoFlirt" = true ];then
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
	else
		echo create 4d images:
		# create 4D image if necessary:
		echo $img_dir
		fslmerge -t temp_4D `imglob $img_dir/${prefix}*.nii`
	fi
else
	cp -l $img temp_4D.nii.gz
fi

# compute mean for each atlas:
for atlas in Atlases/*.nii.gz
do
	atlas_name=${atlas%%.nii.gz}
	atlas_name=${atlas_name##*/}
	echo $atlas
	echo $atlas_name
	
	# make left hemisphere mask:
	fslmaths $atlas -roi 46 45 0 -1 0 -1 0 -1 -thr $mask_thresh tempc01_l_mask_$atlas_name
	fslmaths temp_4D -mul tempc01_l_mask_$atlas_name tempc01_l_${atlas_name}.nii.gz
	fslstats -t tempc01_l_${atlas_name}.nii.gz -M > tempc01_l_${atlas_name}.txt
	
	# make right hemisphere mask:
	fslmaths $atlas -roi 0 45 0 -1 0 -1 0 -1 -thr $mask_thresh tempc01_r_mask_$atlas_name
	fslmaths temp_4D -mul tempc01_r_mask_$atlas_name tempc01_r_${atlas_name}.nii.gz
	fslstats -t tempc01_r_${atlas_name}.nii.gz -M > tempc01_r_${atlas_name}.txt
done

for atlas in Atlases_subcor/*.nii.gz
do
	atlas_name=${atlas%%.nii.gz}
	atlas_name=${atlas_name##*/}
	echo $atlas
	echo $atlas_name
	
	fslmaths $atlas -thr $mask_thresh tempc01_mask_$atlas_name
	fslmaths temp_4D -mul tempc01_mask_$atlas_name tempc01_${atlas_name}.nii.gz
	fslstats -t tempc01_${atlas_name}.nii.gz -M > tempc01_${atlas_name}.txt
	
done

mkdir -p Result_c01_GM_Atlases
mv tempc01* Result_c01_GM_Atlases
rm temp_4D.nii.gz

##### Step02: combine data for all atlases ##########

cd Result_c01_GM_Atlases

ls tempc01_*.txt | tr "\n" "\t"| sed 's/tempc01_//g'| sed 's/.txt//g' > ${out_file_name}_thr_${mask_thresh}.txt

# add Feed Line, echo automaticlly add \n at the end of line, so we just add an empty charactor:
echo "" >> ${out_file_name}_thr_${mask_thresh}.txt

paste -d "\t" tempc01_*.txt >> ${out_file_name}_thr_${mask_thresh}.txt

cp ${out_file_name}_thr_${mask_thresh}.txt ../RegressionAnalysis
mv ${out_file_name}_thr_${mask_thresh}.txt ../
rm tempc01*
cd ..


