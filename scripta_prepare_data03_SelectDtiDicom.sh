#!/bin/sh

mkdir -p Result03_Dti_Dicom

cd Result02_Dicom_T1_REST_DTI2

for dir in *; do

echo $dir

mkdir -p ../Result03_Dti_Dicom/$dir
mkdir -p ../Result03_Dti_Dicom/$dir/DTI_1

cp -l $dir/DTI/Dicoms/*.dcm ../Result03_Dti_Dicom/$dir/DTI_1/

done
