#!bin/sh

set -e
# change write permission to make tar work:
sudo chmod o+w .

subject_list_file="PNC_LOC_SubInfo.csv"
raw_img_dir="/home/xin/BrainImaging2016/DATA_RawImg/52758/NeurodevelopmentalGenomics/"
out_dir="Result01_Dicom"

base_dir=$(pwd)

mkdir -p $out_dir

cd $raw_img_dir
#subject_list=$(sed '1d' $subject_list_file|awk -F "," '{print $1}')
sed '1d' $base_dir/$subject_list_file|awk -F "," '{print $1}'|cat > temp_list

while IFS= read line
do
	tar -xzvf $line*.tar.gz -C $base_dir/$out_dir
	echo extracing file for subject: $line
done<temp_list
rm temp_list
