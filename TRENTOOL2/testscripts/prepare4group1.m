addpath('/data/common/FieldtripCurrent/fieldtrip-20120105/');
addpath('/data/projects/TransferEntropy/code/BazarControlled/TRENTOOL2.0/');
fieldtripdefs

path2data = '/data/projects/TransferEntropy/code/group_test_4_paper/';
outputpath = '/data/projects/TransferEntropy/test_TRENTOOL20_large/group/';

f_count=0;
for ii = 1:1
    for jj = 1:15
        f_count=f_count+1;
        filename=strcat('vp_',num2str(jj),'_group',num2str(ii),'.mat');
        FilesCell{f_count} = filename;
        clear filename
    end
end


cfg = [];

cfg.channel = {'F3','F4','T7','T8'};
cfg.Path2TSTOOL = '/data/common/OpenTSTOOL_v1-2';
cfg.toi = [0 2.999];
cfg.optimizemethod = 'cao';
cfg.caodim = 1:8;
cfg.predicttime_u = 21;
cfg.actthrvalue = 120;
cfg.minnrtrials = 30;


for ii = 1:length(FilesCell)
    
    load([path2data,FilesCell{ii}]);
    
    Data = TEprepare(cfg,Data);
    
    savename = strcat(outputpath,FilesCell{ii}(1:end-4),'_prepared.mat');
    save(savename,'Data');
    clear Data
end


