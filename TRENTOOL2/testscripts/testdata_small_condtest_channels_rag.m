addpath('/data/common/FieldtripCurrent/fieldtrip-20120105/');

addpath('/data/projects/TransferEntropy/code/BazarControlled/TRENTOOL2.0/');
fieldtripdefs


% cao
% ot = 'cao';
% path2data = '/data/projects/TransferEntropy/AR_simulations/SimData/Reference4TRENTOOL2/';
% outputpath = '/data/projects/TransferEntropy/test_TRENTOOL20_large/cao/';
% ragwitz
ot = 'ragwitz';
path2data = '/data/projects/TransferEntropy/AR_simulations/SimData/Reference4TRENTOOL2/';
outputpath = '/data/projects/TransferEntropy/test_TRENTOOL20_large/condtest/';


try
    mkdir(outputpath)
end

datafiles = {'testdata_small.mat','testdata_small.mat'};


for cccc = 1:2
for ii = 2
    loadname=[path2data,datafiles{cccc}];
    load(loadname);
    
    cfg=[];
        
    cfg.channel = Data.label;
    
    cfg.Path2TSTOOL = '/data/common/OpenTSTOOL_v1-2';
    cfg.toi = [.001 3.0];
    
    cfg.predicttime_u = 21;
    
    
    cfg.TEcalctype = 'VW';
    
    cfg.actthrvalue=120;
    cfg.minnrtrials=30;
    
    if strcmp(ot, 'ragwitz')
        cfg.optimizemethod = 'ragwitz';
        cfg.ragdim  = 1:6;
        cfg.ragtaurange  = [.1 2];
        cfg.ragtausteps  = 5;
        cfg.flagNei = 'Mass';
        cfg.sizeNei = 4;
        cfg.repPred = 150;
    else
        cfg.optimizemethod = 'cao';
        cfg.caodim = 1:10;
        cfg.caokth_neighbors = 3;
    end
    
    DataOut=TEprepare(cfg,Data);
    DataOut.testchannelindices = index(1:ii);
    filename = [outputpath,'testdata_small_condtest',num2str(cccc),'_NrCh',num2str(ii),'_',ot,'_',cfg.TEcalctype,'.mat'];
    save(filename, 'DataOut')
    TEcalctype = cfg.TEcalctype;
    clear data Data filename cfg loadname
  
    
end
end



filename1 = [outputpath,'testdata_small_condtest1_NrCh',num2str(ii),'_',ot,'_',cfg.TEcalctype,'.mat'];
load(filename1);
Data1=DataOut;
clear Data

filename2 = [outputpath,'testdata_small_condtest2_NrCh',num2str(ii),'_',ot,'_',cfg.TEcalctype,'.mat'];
load(filename2);
Data2=DataOut;
clear Data

cfg = [];

cfg.fileidout   = [outputpath,'testdata_small_condtestResult_NrCh',num2str(ii),'_',ot,'_',cfg.TEcalctype];

TEconditionsatssingle(cfg,Data1,Data2);


