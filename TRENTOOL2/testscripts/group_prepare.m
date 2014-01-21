addpath('/data/common/FieldtripCurrent/fieldtrip-20120105/');
addpath('/data/projects/TransferEntropy/code/BazarControlled/TRENTOOL2.0/');
fieldtripdefs

path2data = '/data/projects/TransferEntropy/test_TRENTOOL20_large/group/';


f_count=0;
for ii = 1:2
    for jj = 1:15
        f_count=f_count+1;
        filename=strcat(path2data,'vp_',num2str(jj),'_group',num2str(ii),'_prepared.mat');
        FilesCell{f_count} = filename;
        clear filename
    end
end

cfg = [];

cfg.shifttesttype = 'TEshift>TE';


TEgroup_prepare(cfg,FilesCell);
