cd('/data/projects/TransferEntropy/AR_simulations/SimData/Reference4TRENTOOL2')

origfiles= dir('*raw_data.mat');
prepfiles= dir('*TE_output.mat');

f = 1;

for ii = 1
    load(origfiles(ii).name)
    load(prepfiles(ii).name)
    
    cfg = TEresult.TEprepare.cfg;
    cfg.caotau = cfg.tau;
    rmfield(cfg, 'tau');
    
    dataprep = TEprepare(cfg,data);
    
    clear cfg;
    cfg = TEresult.cfg;
    
    cfg.optdimusage = 'maxdim';
    
    searchpattern = 'TRENTOOL2/';
    d = strfind(cfg.fileidout,searchpattern);
    newstring = strrep(cfg.fileidout, cfg.fileidout(1:d+(length(searchpattern)-1)), [cfg.fileidout(1:74),'TRENTOOL20_tested/',cfg.optdimusage]);
        
    cfg.fileidout = newstring;
    
    
    TEsurrogatestats(cfg,dataprep);
    
    
end

    