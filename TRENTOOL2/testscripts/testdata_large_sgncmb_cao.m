addpath('/data/common/FieldtripCurrent/fieldtrip-20120105/');

addpath('/data/projects/TransferEntropy/code/BazarControlled/TRENTOOL2.0/');
fieldtripdefs

% cao
ot = 'cao';
path2data = '/data/projects/TransferEntropy/AR_simulations/SimData/Reference4TRENTOOL2/';
outputpath = '/data/projects/TransferEntropy/test_TRENTOOL20_large/cao/';
% ragwitz
% ot = 'ragwitz';
% path2data = '/data/projects/TransferEntropy/AR_simulations/SimData/Reference4TRENTOOL2_raw_VW/';
% outputpath = '/data/projects/TransferEntropy/test_TRENTOOL20_large/ragwitz/';

datafile = 'testdata_large.mat';


for ii = 2:3
    loadname=[path2data,datafile];
    load(loadname);
    
    cfg=[];
  
    
    switch ii
        case 1
            id1=1:2:19;
            id2=2:2:20;
            for sc = 1:10
            cfg.sgncmb{sc,1} = Data.label{id1(sc)};
            cfg.sgncmb{sc,2} = Data.label{id2(sc)};
            end
        

        case  2
            id1=[1:2:19,19:-2:1];
            id2=[2:2:20,20:-2:2];
            for sc = 1:20
            cfg.sgncmb{sc,1} = Data.label{id1(sc)};
            cfg.sgncmb{sc,2} = Data.label{id2(sc)};
            end
            
        case 3
            id1=[1:2:19,19:-2:1,1:2:19];
            id2=[2:2:20,20:-2:2,2:2:20];
            for sc = 1:30
            cfg.sgncmb{sc,1} = Data.label{id1(sc)};
            cfg.sgncmb{sc,2} = Data.label{id2(sc)};
            end
            
    end


    
    
    
    
    
    cfg.Path2TSTOOL = '/data/common/OpenTSTOOL_v1-2';
    cfg.toi = [.001 3.0];
    
    cfg.predicttime_u = 21;
    
    
    cfg.TEcalctype = 'VW';
    
    cfg.actthrvalue=120;
    cfg.minnrtrials=30;
    
    if strcmp(ot, 'ragwitz')
        cfg.optimizemethod = 'ragwitz';
        cfg.ragdim  = 1:10;
        cfg.ragtaurange  = [.1 2];
        cfg.ragtausteps  = 10;
        cfg.flagNei = 'Mass';
        cfg.sizeNei = 4;
        cfg.repPred = 150;
    else
        cfg.optimizemethod = 'cao';
        cfg.caodim = 1:10;
        cfg.caokth_neighbors = 3;
    end
    
    
    DataOut=TEprepare(cfg,Data);
    DataOut.testchannelindices{1} = id1;
    DataOut.testchannelindices{2} = id2;
    filename = [outputpath,'testdata_large_NrSgncmb',num2str(ii*10),'_',ot,'_',cfg.TEcalctype,'.mat'];
    save(filename, 'DataOut')
    TEcalctype = cfg.TEcalctype;
    clear data Data filename cfg loadname
    
    cfg = [];
    cfg.optdimusage = 'maxdim';
    cfg.surrogatetype = 'trialshuffling';
    cfg.shifttesttype='TEshift>TE';
    cfg.fileidout = strcat(outputpath,'testdata_large_NrSgncmb',num2str(ii*10),'_',ot,'_',TEcalctype,'_',cfg.shifttesttype,'_',cfg.optdimusage,'_');
    TEsurrogatestats(cfg,DataOut)
    
end