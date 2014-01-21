function TEgroup_prepare(cfg,FilesCell)


% TEGROUP_PREPARE:
%
%
% !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
% !!!    The function TEprepare has to be run on all datasets first!    !!!
% !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
%
% You can call this function directly as follows:
%         TEgroup_prepare(cfg, FilesCell)
%
%
% * REFERENCE INFORMATION
%
%   - transfer entropy
%     - The concept of TE appears in Schreiber's article,
%       "Measuring Information Transfer", Phys. Rev. Lett. 85, 461 - 464
%       (2000).
%     - For the estimation of probability densities needed for the TE
%       computation, the function implements the Kraskov-Stoegbauer-
%       Grassberger estimator described in Kraskov et al. "Estimating
%       mutual information", Phys. Rev. E 69 (6) 066138, (2004).
%
%   - permutation test
%       - Maris & Oostenveld (2007). Nonparametric statistical testing of
%         EEG- and MEG-data. J. of Neuroscience Methods, 164, 177-190.
%
%
% * DEPENDENCIES
%     - Package TSTOOL is used at nearest neighbors searches
%       required for the KSG estimator. (Gnu Public License)
%       http://www.dpi.physik.uni-goettingen.de/tstool/
%     - The following Matlab toolboxes:
%         - statistic toolbox
%     - The functions
%         - transferentropy
%         - TEcmc
%         - TEperm
%         - TEvalues
%         - TEwait
%
%
%
%
% * INPUT PARAMETERS
%
% FilesCell = Cell including the names of all files for the group analyses
%
% AND
%
%   cfg: The configuration MUST contain:
%
%   cfg.dim         = Number of embedding dimensions. If not specified, the
%                     maximum of the optimal dimensions found in TEprepare
%                     will be used, which is the recommended option!
%   cfg.tau         = embedding delay in units of act (x*act). If not
%                     specified (recommended option), the tau is used as
%                     followed:
%                     In case of optimizemethod in TEprepare:
%                           'ragwitz' = optimal tau found via ragwitz
%                                       critrion
%                           'cao'     = cfg.tau given by user in TEprepare
%
%   cfg.shifttest   = perform shift test to identify instantaneous mixing
%                     between the signal pairs. Values: 'yes' or 'no'
%                     (default = 'yes')
%                     This shift test is important for EEG and MEG data,
%                     because linear mixing is always present in the data.
%                     In case of instantaneous mixing transfer entropy
%                     should not be calculated for the affected
%                     channelpairs with the corresponding parameter sets,
%                     because it could result in false positive results.
%                     Hence the TE values for these cases will be set to
%                     NaN and the corresponding p-values of the permutation
%                     test to 1. Alpha is set to 0.05.
%   cfg.shifttesttype = The shift test can be calculated for the direction
%                     TE value of original data > TE values of shifted data
%                     (value = 'TE>TEshift') or for the other direction
%                     (value = 'TEshift>TE'). In this case the alpha is
%                     set to 0.1. (default = 'TE>TEshift')
%   cfg.shifttype     = Shifting the data 'onesample' or the length of the
%                      'predicttime' (default = 'predicttime')
%
%
% OUTPUT PARAMETERS
%
%
%
%
%
%
%
% This program is free software; you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation;
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY;
%
% Version 1.0 by Michael Lindner, Raul Vicente, Michael Wibral
% Frankfurt 2010
%

%% Remember the working directory
working_directory1 = pwd;

% load Data
DataCell={};
for ll = 1:length(FilesCell)
    varinfile = who('-file',FilesCell{ll});
    load(FilesCell{ll});
    x = strcat('DataCell{ll}=',varinfile{1},';');
    eval(x)
    y=strcat( ['clear ',varinfile{1} ]);
    eval(y)
    clear x y varinfile
end
clear ll

nrdata = length(DataCell);

if nrdata ~= length(FilesCell)
    error('TRENTOOL ERROR: unequal number of loaded Data and entries in FileCell')
end





%% check data
% -------------------------------------------------------------------------
fprintf('\nCheck data and config');


% check parameters of the input data for equality
TEgroupprepare = TEpreparegroup(DataCell);

if isfield(DataCell{1}, 'outputtype')
    if strcmp(DataCell{1}.outputtype, '3DAsEmbed') || strcmp(DataCell{1}.outputtype, '3DAsEmbed')
        error('TRENTOOL ERROR: group analysis of fMRI Data (3D as Embed) can not be performed  !')
    end
end

if strcmp(DataCell{1}.TEprepare.cfg.optimizemethod, 'ragwitz')
    cfg.tau = DataCell{1}.TEprepare.opttau;
elseif strcmp(DataCell{1}.TEprepare.cfg.optimizemethod, 'cao') 
    cfg.tau = DataCell{1}.TEprepare.cfg.caotau;
end


if ~isfield(cfg, 'dim'), cfg.dim=TEgroupprepare.groupoptdim;  end;
if cfg.dim < TEgroupprepare.groupoptdim
    fprintf('\nTRENTOOL WARNING: specified embedding dimension (cfg.dim) is smaller then the optimal dimension from TEconditiongroup_prepare.')
elseif cfg.dim > TEgroupprepare.groupoptdim
    fprintf('\nTRENTOOL WARNING: specified embedding dimension (cfg.dim) is bigger then the optimal dimension from TEconditiongroup_prepare.')
end



if ~isfield(cfg, 'shifttest'),  cfg.shifttest = 'yes'; end;
if strcmp(cfg.shifttest , 'yes') == 0 && strcmp(cfg.shifttest , 'no') == 0
    error('\nTRENTOOL ERROR: wrong cfg.shifttest - use ''yes'' or ''no'', see help!');
end
if strcmp(cfg.shifttest , 'yes')
    if ~isfield(cfg, 'shifttype'),    cfg.shifttype = 'predicttime'; end;
    if ~isfield(cfg, 'shifttesttype'),  cfg.shifttesttype = 'TE>TEshift'; end;
    if strcmp(cfg.shifttesttype , 'TE>TEshift') == 0 && strcmp(cfg.shifttesttype , 'TEshift>TE') == 0
        error('\nTRENTOOL ERROR: wrong cfg.shifttesttype - use ''TE>TEshift'' or ''TEshift>TE'', see help!');
    end
end



% compare new cfg and cfg from TEprepare if equal entries exist
% -------------------------------------------------------------------------


if size(cfg,1) > 0
    currentcfgfields = fieldnames(cfg);
    for checkloop = 1:nrdata
        doublefields = 0;
        cfgTEprepare = DataCell{1}.TEprepare.cfg;
        cfgfields = fieldnames(cfgTEprepare);
        
        for ii = 1:size(cfgfields,1);
            for jj = 1:size(currentcfgfields,1);
                if strcmp(cfgfields{ii},currentcfgfields{jj}) 
                    if strcmp(cfgfields{ii}, 'dim') == 0 && strcmp(cfgfields{ii}, 'tau') == 0
                        doublefields = doublefields + 1;
                    end
                end
            end
        end
        
        
        if doublefields  > 0
            fprintf('\n')
            error('\nTRENTOOL ERROR: Illegal attempt to overwrite entry generated by or used for TEprepare! Change cfg or rerun TEprepare. (see help)')
        end
        
        nr1 = size(cfgfields,1);
        for ii = 1:nr1
            eval(strcat('cfg.',cfgfields{ii},' = getfield(cfgTEprepare, {1}, cfgfields{ii});'));
        end
    end
    clear doublefields cfgfields currentcfgfields ii jj checkloop
end



% get max number of trials over all data
datatrials = zeros(nrdata,size(DataCell{1}.TEprepare.nrtrials,1),size(DataCell{1}.TEprepare.nrtrials,2));
for checkloop = 1:nrdata
    datatrials(checkloop,:,:)= DataCell{checkloop}.TEprepare.nrtrials;
    maxnrtrials = max(max(datatrials));
end









%% create code
% -------------------------------------------------------------------------
try
    RandStream.setDefaultStream(RandStream('mt19937ar','seed',sum(100*clock)));
end;
numpart = round(47+rand(1,10)*10);
charpart = char(round(65+rand(1,10)*26));
code = [numpart(1:5),charpart(1:5),numpart(6:10),charpart(6:10)];


TEgroupprepare.code = code;

TEgroupprepare.files = FilesCell;

TEgroupprepare.maxnrtrials = maxnrtrials;

TEgroupprepare.cfg = cfg;


TEgroupprepare.cfg.dim(1:size(TEgroupprepare.groupoptdimmat{1,1},1)) = TEgroupprepare.cfg.dim; 
TEgroupprepare.cfg.tau(1:size(TEgroupprepare.groupoptdimmat{1,1},1)) = TEgroupprepare.cfg.dim; 





fprintf(' - ok');


%% save data
% -------------------------------------------------------------------------
fprintf('\nSave data');

for ll = 1:length(FilesCell)
    varinfile = who('-file',FilesCell{ll});
    load(FilesCell{ll});
    
    adddata = strcat(varinfile{1},'.TEgroupprepare = TEgroupprepare ;');
    eval(adddata);
    adddata2 = strcat(varinfile{1},'.TEgroupprepare.assigndata = ',num2str(ll),';');
    eval(adddata2);
    savedata = strcat(['save(''',FilesCell{ll}(1:end-4),'_for_TEgroup_calculate.mat'',  ''', varinfile{1},''' )']);
    eval(savedata)
    deldata = strcat(['clear ',varinfile{1}]);
    eval(deldata)
    
    clear x varinfile
    
end

fprintf(' - done');

%% Returning to the working directory
cd(working_directory1)


return;



