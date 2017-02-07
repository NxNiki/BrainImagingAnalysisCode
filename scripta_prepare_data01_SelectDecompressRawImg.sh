#!bin/sh

subject_list_file="/home/xin/BrainImaging2016/BehaviorData/PTSD_HC_Final2.csv"
raw_img_dir="/home/xin/BrainImaging2016/DATA_RawImg/52758/NeurodevelopmentalGenomics/"
out_dir="Result01_Dicom2"

base_dir=$(pwd)

mkdir -p $out_dir

cd $raw_img_dir
#subject_list=$(sed '1d' $subject_list_file|awk -F "," '{print $1}')
sed '1d' $subject_list_file|awk -F "," '{print $1}'|cat > temp_list

while IFS= read line
do
	tar -xzvf $line*.tar.gz -C $base_dir/$out_dir
	echo extracing file for subject: $line
done<temp_list
#rm temp_list
