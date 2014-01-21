addpath('/data/common/FieldtripCurrent/fieldtrip-20120105/');
addpath('/data/projects/TransferEntropy/code/BazarControlled/TRENTOOL2.0/');
fieldtripdefs

path2data = '/data/projects/TransferEntropy/test_TRENTOOL20_large/group/';

fieldtripdefs


f_count=0;
for ii = 1:2
    for jj = 1:15
        f_count=f_count+1;
        filename=strcat(path2data,'vp_',num2str(jj),'_group',num2str(ii),'_prepared_TE_output.mat');
        FilesCell{f_count} = filename;
        clear filename
    end
end

 


%   cfg.design      = matrix containing a row with subject number and a row
%                     with independent variable representing the order of
%                     the data input.
%                       example:
%                       datasets:    1 2 3 4 5 1 2 3 4 5
%                       conditions:  1 1 1 1 1 2 2 2 2 2
%   cfg.uval        = row in cfg.design which contains the dataset number
%                     (in the example: 1)
%   cfg.ival        = row in cfg.design which contains the independent
%                     variable (in the example: 2)

cfg.design = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15;1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2];
cfg.uvar=1;
cfg.ivar=2;
cfg.permstatstype  = 'depsamplesT';
cfg.tail        = 2;
cfg.fileidout   = strcat('results_4_paper_',num2str(length(FilesCell)/2),'vp_',cfg.permstatstype,'_',num2str(cfg.tail),'tailed');

TEgroup_conditionstats(cfg,FilesCell);

