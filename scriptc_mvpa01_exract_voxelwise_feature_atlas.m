% extract voxel-wise features for each brain region:
clear;

%% fslvbm:
% nii_img = 'Result03_Nifti_T1_2/stats/GM_mod_merg_s3.nii';
% nii_mask = 'Result03_Nifti_T1_2/stats/GM_mask.nii';
% out_file_name = 'outc01_fslvbm_voxelwise_feature.txt';

%% spmvbm:
nii_img = 'Result03_Nifti_T1_2/stats/spm_gm_smooth.nii';
nii_mask = 'Result03_Nifti_T1_2/stats/GM_mask.nii';
out_file_name = 'outc01_spmvbm_voxelwise_feature';

%%
atlases = dir('Atlases/*.nii');
num_atlases = length(atlases);
img_struc = load_nii(nii_img);

for i_atlas = 1:num_atlases
    atlas = [atlases(i_atlas).folder, filesep, atlases(i_atlas).name];
    mask_struc = load_nii(atlas);
    
    img_left = bsxfun(@times, double(img_struc.img(1:45,:,:,:)), double(mask_struc.img(1:45,:,:,:)));
    siz = size(img_left);
    img_mat = reshape(img_left, prod(siz(1:3)), siz(4))';
    img_mat_nonezero = img_mat(:,any(img_mat~=0,1));
    csvwrite([out_file_name, '_l_', atlases(i_atlas).name, '.txt'], img_mat_nonezero);
    
    img_right = bsxfun(@times, double(img_struc.img(46:end,:,:,:)), double(mask_struc.img(46:end,:,:,:)));
    siz = size(img_right);
    img_mat = reshape(img_right, prod(siz(1:3)), siz(4))';
    img_mat_nonezero = img_mat(:,any(img_mat~=0,1));
    csvwrite([out_file_name, '_r_', atlases(i_atlas).name, '.txt'], img_mat_nonezero);
end