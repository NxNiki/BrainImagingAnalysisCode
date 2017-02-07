#!/bin/sh

mkdir -p Result04_DPASF
mkdir -p Result04_DPASF/FunRaw
mkdir -p Result04_DPASF/T1Raw

cd Result02_Dicom_T1_REST_DTI2

for dir in *; do

echo $dir

mkdir -p ../Result04_DPASF/FunRaw/sub_$dir
cp -l $dir/Rest/Dicoms/*.dcm ../Result04_DPASF/FunRaw/sub_$dir/

mkdir -p ../Result04_DPASF/T1Raw/sub_$dir
cp -l $dir/T1/Dicoms/*.dcm ../Result04_DPASF/T1Raw/sub_$dir/
done
