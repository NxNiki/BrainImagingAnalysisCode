#!bin/sh

set -e

in_dir="Result02_Dicom_T1_REST_DTI"
base_dir=$(pwd)


################## T1 Rest and DTI #######################
#out_dir="Result03_Nifti_T1_Rest_DTI"
#mkdir -p $out_dir
#
#cd $in_dir
#ls|cat>$base_dir/temp_list
#cd $base_dir
#
#mkdir -p $out_dir/
#while IFS= read line
#do
#	echo processing file for subject: $line
#	mkdir $out_dir/$line
#	dcm2nii -o $out_dir/$line -f y $in_dir/$line/T1
#	dcm2nii -o $out_dir/$line -f y $in_dir/$line/Rest
#	dcm2nii -o $out_dir/$line -f y $in_dir/$line/DTI
#done<temp_list

######################### T1 Image: ######################
out_dir="Result03_Nifti_T1"
mkdir -p $out_dir

cd $in_dir
ls|cat>$base_dir/temp_list
cd $base_dir

mkdir -p $out_dir/
while IFS= read line
do
	echo processing file for subject: $line
	dcm2nii -o $out_dir/ -f y $in_dir/$line/T1
	mv $out_dir/*T1*.nii.gz $out_dir/$line.nii.gz
done<temp_list
rm temp_list


################### Resting state Image: ################
#out_dir="Result03_Nifti_Rest"
#mkdir -p $out_dir
#
#cd $in_dir
#ls|cat>$base_dir/temp_list
#cd $base_dir
#
#mkdir -p $out_dir/
#while IFS= read line
#do
#	echo processing file for subject: $line
#	dcm2nii -o $out_dir/ -f y $in_dir/$line/Rest
#	mv $out_dir/*001.nii.gz $out_dir/Rest_$line.nii.gz
#done<temp_list
#rm temp_list

################### DTI Image: ################
#out_dir="Result03_Nifti_DTI"
#mkdir -p $out_dir
#
#cd $in_dir
#ls|cat>$base_dir/temp_list
#cd $base_dir
#
#mkdir -p $out_dir/
#while IFS= read line
#do
#	echo processing file for subject: $line
#	dcm2nii -o $out_dir/ -f y $in_dir/$line/DTI
#	mv $out_dir/*DTI*.nii.gz $out_dir/$line.nii.gz
#	mv $out_dir/*DTI*.bval $out_dir/$line.bval
#	mv $out_dir/*DTI*.bvec $out_dir/$line.bvec
#done<temp_list
#rm temp_list
