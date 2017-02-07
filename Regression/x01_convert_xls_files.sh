#!/bin/sh

# failed to find commands to onvert .xls file to .csv file, we may do this with libreOffice Calc

for file in *.csv; do
    file_name=${file%%.csv}
	tr "," "\t" <$file >$file_name.txt
done
