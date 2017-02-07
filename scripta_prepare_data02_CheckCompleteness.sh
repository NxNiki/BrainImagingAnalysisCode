#!/bin/sh
input_dir="Result01_Dicom2"
output_dir="Result02_Dicom_T1_REST_DTI2"

mkdir -p $output_dir

ls -d $input_dir/*/T1* | tr "/" "\t" > outa02_subject_t1.txt
wc -l outa02_subject_t1.txt
ls -d $input_dir/*/FMRI_BOLD_rest | tr "/" "\t" > outa02_subject_rest.txt
wc -l outa02_subject_rest.txt
ls -d $input_dir/*/DTI_36dir | tr "/" "\t" > outa02_subject_dti.txt
wc -l outa02_subject_dti.txt

join -1 2 -2 2 outa02_subject_t1.txt outa02_subject_rest.txt > temp_t1_rest.txt
join -1 1 -2 2 temp_t1_rest.txt outa02_subject_dti.txt | awk '{print $1}' > outa02_subject_t1_rest_dti2.txt

rm temp*

while read sub
do
	echo $sub
	mkdir $output_dir/$sub
	mkdir $output_dir/$sub/T1/
	cp -l -r $input_dir/$sub/T1*/Dicoms/ $output_dir/$sub/T1/
	
	mkdir $output_dir/$sub/Rest/
	cp -l -r $input_dir/$sub/FMRI_BOLD_rest/Dicoms/ $output_dir/$sub/Rest/
	
	mkdir $output_dir/$sub/DTI/
	cp -l -r $input_dir/$sub/DTI_36dir/Dicoms/ $output_dir/$sub/DTI/
done<outa02_subject_t1_rest_dti2.txt

