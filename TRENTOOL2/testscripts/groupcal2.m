addpath('/data/common/FieldtripCurrent/fieldtrip-20120105/');
addpath('/data/projects/TransferEntropy/code/BazarControlled/TRENTOOL2.0/');
fieldtripdefs

path2data = '/data/projects/TransferEntropy/test_TRENTOOL20_large/group/';

fieldtripdefs

for vp=4:6;
    for group=1:2
        filename=strcat(path2data,'vp_',num2str(vp),'_group',num2str(group),'_prepared_for_TEgroup_calculate.mat');
        TEgroup_calculate(filename);
    end
end