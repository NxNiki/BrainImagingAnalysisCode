% extract voxel-wise features
clear;

%% fslvbm:
% nii_img = 'Result03_Nifti_T1_2/stats/GM_mod_merg_s3.nii';
% nii_mask = 'Result03_Nifti_T1_2/stats/GM_mask.nii';
% out_file_name = 'outc01_fslvbm_voxelwise_feature.txt';

%% spmvbm:
nii_img = 'Result03_Nifti_T1_2/stats/spm_gm_smooth.nii';
nii_mask = 'Result03_Nifti_T1_2/stats/GM_mask.nii';
out_file_name = 'outc01_spmvbm_voxelwise_feature.txt';

%%

img_struc = load_nii(nii_img);
mask_struc = load_nii(nii_mask);
img = bsxfun(@times, double(img_struc.img), double(mask_struc.img));
siz = size(img);
img_mat = reshape(img, prod(siz(1:3)), siz(4))';

img_mat_nonezero = img_mat(:,any(img_mat~=0,1));

csvwrite(out_file_name, img_mat_nonezero);
