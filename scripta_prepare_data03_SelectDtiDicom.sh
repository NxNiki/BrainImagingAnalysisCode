#!/bin/sh

mkdir -p Result03_Dti_Dicom

cd Result02_Dicom_T1_REST_DTI

i=1
for dir in *; do

echo $i $dir

#mkdir -p ../Result03_Dti_Dicom/$dir
#mkdir -p ../Result03_Dti_Dicom/$dir/DTI_1
mkdir -p ../Result03_Dti_Dicom/$dir/DTI_2

#cp -l $dir/DTI/Dicoms/*.dcm ../Result03_Dti_Dicom/$dir/DTI_1/
cp -l $dir/DTI2/Dicoms/*.dcm ../Result03_Dti_Dicom/$dir/DTI_2/
i=$(($i + 1))
done
