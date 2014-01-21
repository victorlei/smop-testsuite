

path ='/data/projects/TransferEntropy/test_TRENTOOL20_large/group/';
outputpath = '/data/projects/TransferEntropy/test_TRENTOOL20_large/condtest/';

filename = [path,'vp_1_group1_prepared.mat'];
load(filename);
Data1=Data;
clear Data

filename = [path,'vp_1_group2_prepared.mat'];
load(filename);
Data2=Data;
clear Data



cfg = [];

cfg.fileidout   = [outputpath,'testdata_v1ofgroup'];

TEconditionstatssingle(cfg,Data1,Data2);
