#!/bin/sh
set -e

mkdir -p Result04_DPASF
mkdir -p Result04_DPASF/FunRaw
mkdir -p Result04_DPASF/T1Raw

cd Result02_Dicom_T1_REST_DTI

i=1
for dir in *; do

echo $i $dir

mkdir -p ../Result04_DPASF/FunRaw/sub_$dir
cp -l $dir/Rest/Dicoms/*.dcm ../Result04_DPASF/FunRaw/sub_$dir/

mkdir -p ../Result04_DPASF/T1Raw/sub_$dir
cp -l $dir/T1/Dicoms/*.dcm ../Result04_DPASF/T1Raw/sub_$dir/

i=$(($i+1))
done
