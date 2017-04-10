% select upper triangular part of the matrix and save in txt files
file_dir='Result04_DPASF/Results/ROISignals_FunImgARCWF';

file_names = dir([file_dir, filesep, 'ROICorrelation_FisherZ_sub_*.mat']);

file_names = cat(1, {file_names(:).name});
num_files = length(file_names);

out_file_name = 'outc01_loc_fc.csv';

for i=1:num_files
    load([file_dir, filesep, file_names{i}], 'ROICorrelation_FisherZ')

    uptri = triu(ROICorrelation_FisherZ,1);
    line = uptri(uptri~=0)';
    if i==1
        dlmwrite(out_file_name,1:length(line));
    end
    dlmwrite(out_file_name,line,'-append')
    
end
