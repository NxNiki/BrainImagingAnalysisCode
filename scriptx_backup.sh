#!/bin/sh
#set -e
current_dir=$(pwd)
backup_dir="/home/xin/BrainImagingAnalysisCode"

cp script*.* $backup_dir
cp RegressionAnalysis x*.* $backup_dir/RegressionAnalysis

cd $backup_dir
git commit -m "update scripts"
git push origin master
cd $current_dir
