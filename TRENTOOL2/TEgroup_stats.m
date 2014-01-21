function TEgroup_stats(cfg,FilesCell)

% TEGROUP_CONDTIONSTATS: 
%
%
% !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
% !!!         The functions TEgroup_prepare and TEgroup_calculate       !!!
% !!!               has to be run on all datasets first!                !!!
% !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
%
% You can call this function directly as follows:
%         TEgroup_conditionstats(cfg, FilesCell)
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
%   cfg.alpha       = significance level for statistical shift test,
%                     permutation test and correction for multiple
%                     comparison (default = 0.05)
%   cfg.numpermutation = nr of permutations in permutation test
%                     (default = 190100)
%   cfg.permstatstype  = 'mean' to use the distribution of the mean
%                     differences and 'depsamplesT' or
%                     'indepsamplesT' for distribution of the
%                     t-values. (default = 'mean')
%   cfg.tail        = '1' tail or '2' tailed test of significance (for the
%                     permutation tests) (default = 2)
%   cfg.correctm    = correction method used for correction of the multiple
%                     comparison problem - False discovery rate 'FDR' or
%                     Bonferroni correction 'BONF' (default = 'FDR')
%   cfg.fileidout   = string for the first part of the output filename.
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

%% load Data
% -------------------------------------------------------------------------
Data={};

cfg.nrdatasets = length(FilesCell);

% create empty cells
TEprepare{cfg.nrdatasets} = [];
TEgroupprepare{cfg.nrdatasets} = [];
TEmat{cfg.nrdatasets} = [];
MImat{cfg.nrdatasets} = [];
nrchannels = nan(1,cfg.nrdatasets);
assigndata = nan(1,cfg.nrdatasets);
channelcombisize = nan(1,cfg.nrdatasets);

for ll = 1:cfg.nrdatasets
    varinfile = who('-file',FilesCell{ll});
    load(FilesCell{ll});
    adddata = strcat('Data{ll}=',varinfile{1},';');
    eval(adddata)
    clear adddata varinfile
    
    % check data for TEprepare/TEproupprepare structure
    
    if ~isfield(Data{ll}, 'TEprepare')
        error(strcat('TRENTOOL ERROR: .TEprepare is missing in dataset',num2str(ll)));  
    end
    if ~isfield(Data{ll}, 'TEgroupprepare')
        error(strcat('TRENTOOL ERROR: .TEgroupprepare is missing in dataset',num2str(ll)));  
    end
    
        
    TEprepare{ll}=Data{ll}.TEprepare;
    TEgroupprepare{ll}=Data{ll}.TEgroupprepare;
    nrchannels(ll) = TEgroupprepare{ll}.nrchannels;
    assigndata(ll) = TEgroupprepare{ll}.assigndata;
    channelcombisize(ll)= size(Data{ll}.TEprepare.channelcombi,1) ;
        
    % check code 
    if TEgroupprepare{1}.code ~= TEgroupprepare{ll}.code
        error('TRENTOOL ERROR: Only datasets, which were prepared together with TEgroup_prepare can be analysed together! Code is not identical')
    end
    
    % create cell TE values
    TEmat{ll} = Data{ll}.TEmat;
    MImat{ll} = Data{ll}.MImat;
    
        
end
clear ll 

nrdata = length(Data);

if nrdata ~= cfg.nrdatasets
    error('TRENTOOL ERROR: unequal number of loaded Data and entries in FileCell')
end

% check assigndata
assigndata = sort(assigndata);
check =1:length(cfg.design);
if isequal(assigndata,check) == 0
    error('TRENTOOL ERROR: Files prepared with TEgroup_prepare and files to analyse differ')
end
if assigndata(length(assigndata)) ~= nrdata
    error('TRENTOOL ERROR: Number of files prepared with TEgroup_prepare and number files to analyse differ')
end

% check number of channels
if min(nrchannels) ~= max(nrchannels)
    error('TRENTOOL ERROR: Number of channels prepared with TEgroup_prepare are unequal over datasets')
end

% check number of channelcombis
if min(channelcombisize) ~= max(channelcombisize)
    error('TRENTOOL ERROR: Number of channelcombinations prepared with TEgroup_prepare are unequal over datasets')
end



% check first FilesCell










%% check input cfg
% -------------------------------------------------------------------------
if ~isfield(cfg, 'alpha'),          cfg.alpha = 0.05;           end;
if ~isfield(cfg, 'correctm'),       cfg.correctm = 'FDR';       end;
if ~isfield(cfg, 'tail'),           cfg.tail = 2;               end;

if ~isfield(cfg, 'design'),
    error('\nTRENTOOL ERROR: cfg.design must be defined, see help!');
end;
if ~isfield(cfg, 'ivar'),
    error('\nTRENTOOL ERROR: cfg.ivar must be defined, see help!');
end;
if ~isfield(cfg, 'uvar'),
    error('\nTRENTOOL ERROR: cfg.uvar must be defined, see help!');
end;
if size(cfg.design,2) ~= cfg.nrdatasets
    error('\nTRENTOOL ERROR: cfg.design must have the same number of columns than number of datasets exist, see help!');
end

if ~isfield(cfg, 'permstatstype'),  cfg.permstatstype = 'mean'; end;
if strcmp(cfg.permstatstype , 'mean') == 0 && strcmp(cfg.permstatstype , 'indepsamplesT') == 0 && strcmp(cfg.permstatstype , 'depsamplesT') == 0
    error('\nTRENTOOL ERROR: wrong cfg.permstatstype - use ''mean'' ''depsamplesT'' or ''indepsamplesT'', see help!');
end

if ~isfield(cfg, 'fileidout'),
    error('\nTRENTOOL ERROR: cfg.fileidout must be defined, see help!');
end;

% compare new cfg and cfg from TEprepare if equal fields exist
% -------------------------------------------------------------------------

doublefields = 0;
cfgTEprepare = TEprepare{1}.cfg;
if isfield(cfg, 'Path2TSTOOL') && isfield(cfgTEprepare, 'Path2TSTOOL')
    cfgTEprepare = rmfield(cfgTEprepare, 'Path2TSTOOL');
end
if isfield(cfgTEprepare, 'feedback')
    cfgTEprepare = rmfield(cfgTEprepare, 'feedback');
end
cfgfields = fieldnames(cfgTEprepare);
cfgfields2 = fieldnames(cfg);

for ii = 1:size(cfgfields,1);
    for jj = 1:size(cfgfields2,1);
        if strcmp(cfgfields{ii},cfgfields2{jj})
            doublefields = doublefields + 1;
        end
    end
end


if doublefields  > 0
    fprintf('\n')
    error('\nTRENTOOL ERROR: Illegal attempt to overwrite entry generated by or used for TEprepare! Change cfg or rerun TEprepare. (see help)')
end


% add structures and values of data.TEprepare.cfg to cfg
names1 = fieldnames(TEprepare{1}.cfg);
nr1 = size(names1,1);
for ii = 1:nr1
    eval(strcat('cfg.',names1{ii},' = getfield(TEprepare{1}.cfg, {1}, names1{ii});'))
end

clear ii nr1 names1

% add structures and values of data.TEgroupprepare.cfg to cfg
names1 = fieldnames(TEgroupprepare{1}.cfg);
nr1 = size(names1,1);
for ii = 1:nr1
    eval(strcat('cfg.',names1{ii},' = getfield(TEgroupprepare{1}.cfg, {1}, names1{ii});'))
end
clear ii nr1 names1


% add structures and values of data.TEgroupprepare.cfg to cfg
names1 = fieldnames(TEgroupprepare{1});
nr1 = size(names1,1);
for ii = 1:nr1
    eval(strcat('cfg.',names1{ii},' = getfield(TEgroupprepare{1}, {1}, names1{ii});'))
end
clear ii nr1 names1




%% check nr of permutations
% -------------------------------------------------------------------------
fprintf('\n\nChecking number of permutations\n');


% cfg.permtest.channelcombi = channelcombi;
% cfg.permtest.channelcombilabel = data.TEprepare.channelcombilabel ;


nr2cmc=min(channelcombisize)*size(cfg.predicttime_u,2);

if ~isfield(cfg, 'numpermutation'),
    cfg.numpermutation = 190100; % for p<0.01 with a possible bonferroni correcetion of 100
elseif cfg.numpermutation < ceil(1/cfg.alpha)
    error(strcat('\nTRENTOOL ERROR: cfg.numpermutation too small - Nr of permutations must be at least :',num2str(numpermutation),' !'));
else
    if cfg.maxtrials>31
        if cfg.numpermutation > 2^31
            error(strcat('\nTRENTOOL ERROR: cfg.numpermutation too huge - Nr of permutations must be at least :',num2str(numpermutation),' !'));
        end
    else
        if cfg.numpermutation > 2^cfg.maxtrials
            error(strcat('\nTRENTOOL ERROR: cfg.numpermutation too huge - Nr of permutations must be at least :',num2str(numpermutation),' !'));
        end
    end
    if cfg.numpermutation < ceil(1/(cfg.alpha/nr2cmc))
       fprintf('\n#######################################################################################\n# WARNING: Nr of permutations not sufficient for correction for multiple comparisons! #\n#######################################################################################\n'); 
    end
end

fprintf(' - ok\n');



%% Define conditions
% -------------------------------------------------------------------------
fprintf('\nDefine conditions:\n');

conds=squeeze(cfg.design(cfg.ivar,:));
condtype = unique(conds);
nrconds=length(condtype);
if nrconds ~=2
    fprintf(strcat(['TRENTOOL ERROR: You defined ',num2str(nrconds),' conditions.']))
    error('\nTRENTOOL ERROR: You have to define two conditions in cfg.design, see help!');
end

units=squeeze(cfg.design(cfg.uvar,:));
unittype = unique(units);
nrunits=length(unittype);

condindex1 = find(conds == condtype(1));
condindex2 = find(conds == condtype(2));

fprintf(strcat(['Nr of different datasets: ',num2str(nrunits),'\n']))
fprintf(strcat(['Condition 1 -> Datasets: ' num2str(condindex1),'\n' ]))
fprintf(strcat(['Condition 2 -> Datasets: ' num2str(condindex2),'\n' ]))


%% create empty matrices
% -------------------------------------------------------------------------
TEresultmean.TEmat = zeros(cfg.nrchannels,cfg.nrdatasets);
TEresultmean.MImat = zeros(cfg.nrchannels,cfg.nrdatasets);
%TEresult{cfg.nrdatasets} = [];


%% Mean data over trials per subject
% -------------------------------------------------------------------------
for subject = 1:cfg.nrdatasets
    TEresultmean.TEmat(:,:,subject) = squeeze(mean(TEmat{subject}, 2));
    TEresultmean.MImat(:,:,subject) = squeeze(mean(MImat{subject}, 2));
end


%% Split TEresultmean matrices for permutation tests
% -------------------------------------------------------------------------
fprintf('Prepare condition matrices');

TEresult1.TEmat = TEresultmean.TEmat(:,:,condindex1);
TEresult1.MImat = TEresultmean.MImat(:,:,condindex1);

TEresult2.TEmat = TEresultmean.TEmat(:,:,condindex2);
TEresult2.MImat = TEresultmean.MImat(:,:,condindex2);



%% permutation tests
% -------------------------------------------------------------------------

TEpermtestgroup = TEperm(cfg,TEresult1,TEresult2);

TEpermtestgroup.dimord = 'chanpair_u_value';
TEpermtestgroup.cfg = cfg;
TEpermtestgroup.sgncmb = Data{1}.sgncmb;
TEpermtestgroup.numpermutation = cfg.numpermutation;
TEpermtestgroup.nrdatasets = cfg.nrdatasets;
TEpermtestgroup.TEprepare = TEprepare{1};
TEpermtestgroup.TEgroupprepare = Data{1}.TEgroupprepare;

fprintf('\nCalculation ready\n')


%% save results
% -------------------------------------------------------------------------
fprintf('\nSaving ...')
fprintf('\nResults of TE')
savename1 = strcat(cfg.fileidout,'_time',num2str(cfg.toi(1)),'-',num2str(cfg.toi(2)),'ms_TE_output.mat');
save(savename1, 'TEresultmean','TEresult1','TEresult2','-v7.3');
fprintf(' - ok');
fprintf('\nResults of permutation test')
save(strcat(cfg.fileidout,'_time',num2str(cfg.toi(1)),'-',num2str(cfg.toi(2)),'ms_TEpermtestgroup_output.mat'), 'TEpermtestgroup','-v7.3');
fprintf(' - ok');


%% Returning to the working directory
cd(working_directory1)


end
