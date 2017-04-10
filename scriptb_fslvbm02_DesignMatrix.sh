#!/bin/sh

awk '{print $5==1}' outb01_loc_info.txt | sed 1d > temp_loc1.txt

awk '{print $5==0}' outb01_loc_info.txt | sed 1d > temp_loc0.txt

awk '{print $4}' outb01_loc_info.txt | sed 1d > temp_age.txt

awk '{print $3}' outb01_loc_info.txt | sed 1d > temp_bmi.txt

awk '{print $2}' outb01_loc_info.txt | sed 1d | sed 's/"//g' > temp_gender.txt
awk '{print $1=="M"}'  temp_gender.txt > temp_male.txt
awk '{print $1=="F"}'  temp_gender.txt > temp_female.txt

# global gray matter volumes:
#awk '{print $1}' spm_raw_volumes.txt > temp_global_gm_volumes.txt

# add gender as covariates:
paste -d '\t' temp_loc0.txt temp_loc1.txt temp_male.txt temp_female.txt temp_bmi.txt temp_age.txt > design.txt

Text2Vest design.txt design.mat

echo make contrast file:

echo 1 -1 0 0 0 0 > contrasts.txt
echo -1 1 0 0 0 0 >> contrasts.txt

Text2Vest contrasts.txt design.con

# without gender as covariates:
paste -d '\t' temp_loc0.txt temp_loc1.txt temp_bmi.txt temp_age.txt > design2.txt

Text2Vest design2.txt design2.mat

echo make contrast2 file:

echo 1 -1 0 0 > contrasts2.txt
echo -1 1 0 0 >> contrasts2.txt

Text2Vest contrasts2.txt design2.con

## without gender as covariates:
## add global gray matter volumnes as covariates:
#paste -d '\t' temp_loc0.txt temp_loc1.txt temp_age.txt temp_global_gm_volumes.txt > design3.txt
#
#Text2Vest design3.txt design3.mat
#
#echo make contrast3 file:
#
#echo 1 -1 0 0 0 > contrasts3.txt
#echo -1 1 0 0 0 >> contrasts3.txt
#
#Text2Vest contrasts3.txt design3.con

# remove temporary files and move desgin files:
rm temp*.txt
mv design*.* Result03_Nifti_T1
mv contrasts*.txt Result03_Nifti_T1
