function TEpermtest=TEsurrogatestats(cfg,data)

% TESURROGATESTATS: This function calculates the transfer entropy values
% and performs a permutation test on two transfer entropy data sets (one
% original and one surrogate).
%
% !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
% !!!      The function TEprepare has to be run on the data first!      !!!
% !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
%
% You can call this function directly as follows:
%         TEsurrogatestats(cfg, data)
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
%         - signal processing toolbox
%         - statistic toolbox
%     - The functions
%         - TEprepare
%         - transferentropy
%         - TEactdetect
%         - TEchannelselect
%         - TEcmc
%         - TEperm
%         - TEtrialselect
%         - TEvalues
%         - TEwait
%
% * INPUT PARAMETERS
%
%   data            = Fieldtrip raw data structure - it MUST contain:
%       .trials     = cell array (1xnr of trials) containing the data for
%                     each trial
%       .time       = cell (1xnr of trials) containing the time indices for
%                     each trial
%       .label      = cell (1xnr of channels), containing the labels of 
%                     channels included in the data
%       .fsample    = value of sampling rate (in Hertz)
%       .TEprepare  = structure added by TEprepare 
%
% AND
%
%   cfg: The configuration MUST contain:
%
%  cfg.optdimusage  = 'maxdim' to use maximum of optimal dimensions over
%                      all channels for all channels, or 'indivdim' to use
%                      the individual optimal dimension for each channel.
%                      In case of using ragwitz criterion also the optimal
%                      embedding delay tau per channelcombi is used.
%
%   cfg.dim         = Value(s) for embedding dimension. In case of using
%                     cfg.optdimusage = 'maxdim' this has to be a scalar
%                     value. In case of cfg.optdimusage = 'indivdim' this 
%                     has to be a vector of the size (channelcombi x 1). 
%                     If not specified, the optimal dimension(s) found in 
%                     TEprepare will be used, which is the recommended 
%                     option!
%   cfg.tau         = embedding delay in units of act (x*act). If not
%                     specified (recommended option), the tau is used as
%                     followed:
%                     Depending optimizemethod in TEprepare:
%                           'ragwitz' = optimal tau found via ragwitz 
%                                       critrion
%                           'cao'     = cfg.tau given by user in TEprepare
%                     If not specified, the optimal embedding delay found  
%                     in TEprepare will be used, which is the recommended 
%                     option!
%   cfg.alpha       = significance level for statisatical permutation test 
%                     and correction for multiple comparison 
%                     (default = 0.05)
%   cfg.surrogatetype = 'trialshuffling','trialreverse','blockresampling',
%                     'blockreverse1','blockreverse2', or 'blockreverse3'
%                     surrogate data for trial(n) will be created as
%                     following:
%                     trialshuffling:   trial(n+1)
%                     trialreverse:     reverse of trial(n)
%                     blockresampling:  cuts trial(n) at random point and
%                                       resamples the trial
%                     blockreverse1:    reverse after blockresampling
%                     blockreverse2:    reverse first block after
%                                       blockresampling
%                     blockreverse3:    reverse second block after
%                                       blockresampling
%                     swapnaighbors:    pair odd trials with the higher
%                                       neighbor and 3even with the lower
%                                       neighbor
%
%                       examples:
%                       original trial:     1 2 3 4 5 6
%                       trialshufling:      2 3 4 5 6 1
%                       trialreverse:       6 5 4 3 2 1
%                       blockresampling:    4 5 6 1 2 3
%                       blockreverse1:      3 2 1 6 5 4
%                       blockreverse2:      6 5 4 1 2 3
%                       blockreverse3:      4 5 6 3 2 1
%                       swapneighbors:      2 1 4 3 6 5
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
%                     test to 1.
%   cfg.shifttesttype = The shift test can be calculated for the direction
%                     TE value of original data > TE values of shifted data
%                     (value = 'TE>TEshift') or for the other direction
%                     (value = 'TEshift>TE'). In this case the alpha is
%                     set to 0.1 . (default = 'TE>TEshift')
%   cfg.shifttype     = Shifting the data 'onesample' or the length of the 
%                     'predicttime' (default = 'predicttime')
%   cfg.numpermutation = nr of permutations in permutation test
%                     (default = 190100)
%   cfg.permstatstype  = 'mean' to use the distribution of the mean
%                     differences and 'depsamplesT' or
%                     'indepsamplesT' for distribution of the
%                     t-values. (default = 'indepsamplesT')
%   cfg.tail        = 1 tail or 2 tailed test of significance (for the
%                     permutation tests) (default in TEsurrogatestats= 1)
%   cfg.correctm    = correction method used for correction of the multiple
%                     comparison problem - False discovery rate 'FDR' or
%                     Bonferroni correction 'BONF' (default = 'FDR')
%   cfg.fileidout   = string for the first part of the output filename.
%
%
%
% * OUTPUT PARAMETERS
%
%
%  TEpermtest
%            .TEpermvalues  = matrix with size:
%                             (channelpair,value)
%                           The last dimension "value" includes:
%                           1 - p_values of the statistic within the
%                               distribution given by the permutations
%                           2 - 1 (0), if the statistics is significant at
%                               the prescribed alpha level (or not)
%                           3 - 1 (0), if the statistics is significant
%                               after correction for multiple comparisons
%                               (or not)
%                           4 - 1 (0), mean difference or tvalue of mean
%                               difference depending on cfg.permstatstype
%                           5 - 1 (0), if instantaneous mixing (volume
%                               conduction) exists (or not)
%            .dimord        = dimensions of TEpermvalues
%            .cfg           = configuration file used to calculate TE and
%                             permtest
%            .sgncmb        = labels of channel combinations (source ->
%                             target)
%            .numpermutation = number of permutations
%            .ACT           = structure including
%                .act       = ACT matrix (channelcombi x 2 x trial)
%            .nr2cmc        = number of tests to correct for multiple
%                             comparisons
%            .TEprepare     = results of the function TEprepare from the
%                             data
%
% AND 
%
%  TEresult             = Output structure of the function tranferentropy
%          .TEmat       = resultmatrix including transfer entropy(TE)
%                         values (channelpairs x u x trial)
%          .MImat       = resultmatrix including mutual information (MI)
%                         values (channelpairs x u x trial)
%          .dimord      = 'channelpair_u_trial'; the dimensions of TEmat 
%                         and MImat
%          .cfg         = configuration file used to calculate TE
%          .trials      = trial numbers selected from raw dataset
%          .act         = ACT matrix (channelcombi x 2 x trial)
%          .sgncmb      = labels of channel combinations (source -> target)
%          .TEprepare   = results of the function TEprepare from the
%                         data
%   if instantaneous mixing is found in the data, then another field will 
%   be added:
%          .instantaneousmixing = matrix (channel x u) which indicates were
%          the instantaneous mixings were found (1) or not (0).%
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
% Frankfurt 2009
%

% CHANGELOG
% 2012-02-09 MW: chnaged default of cfg.tail to 1 because in
% TEsurrogatestats we're alwazs testing whther the true TE is bigger than
% the surrogate TE.

%% Remember the working directory
working_directory1 = pwd;

%% check data
% -------------------------------------------------------------------------
fprintf('\nCheck data and config');

% check if TEprepare was performed
if ~isfield(data, 'TEprepare'),
    fprintf('\n')
    error('TRENTOOL ERROR: The function TEprepare must be performed on the data, see help!');
end;

% check data using checkdata from Fieldtrip
[data] = ft_checkdata(data, 'datatype','raw');

% check the data structure
if ~isfield(data, 'trial'),
    fprintf('\n')
    error('TRENTOOL ERROR: data must be in ''.trial''-structure, see help!');
end;
if ~isfield(data, 'time'),
    fprintf('\n')
    error('TRENTOOL ERROR: data contains no ''.time''-structure, see help!');
end;
if ~isfield(data, 'label'),
    fprintf('\n')
    error('TRENTOOL ERROR: data contains no ''.label''-structure, see help!');
end;
if ~isfield(data, 'fsample'),
    fprintf('\n')
    error('TRENTOOL ERROR: data contains no ''.fsample''-structure, see help!');
end;
if size(data.time,1)>size(data.time,2)
    data.time=data.time';
end

% compare new cfg and cfg from TEprepare if equal fields exist
% -------------------------------------------------------------------------

doublefields = 0;
cfgTEprepare = data.TEprepare.cfg;
if isfield(cfg, 'Path2TSTOOL') && isfield(cfgTEprepare, 'Path2TSTOOL')
    cfgTEprepare = rmfield(cfgTEprepare, 'Path2TSTOOL');
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

clear cfgTEprepare

if doublefields  > 0
    fprintf('\n')
    error('TRENTOOL ERROR: Illegal attempt to overwrite entry generated by or used for TEprepare! Change cfg or rerun TEprepare. (see help)')
end


% add structures and values of data.TEprepare.cfg to cfg
names1 = fieldnames(data.TEprepare.cfg);
nr1 = size(names1,1);
for ii = 1:nr1
    eval(strcat('cfg.',names1{ii},' = getfield(data.TEprepare.cfg, {1}, names1{ii});'))
end


% check configuration and set defaults
% -------------------------------------------------------------------------

% if not defined set defaults
if ~isfield(cfg, 'alpha'),          cfg.alpha = 0.05;           end;
if ~isfield(cfg, 'correctm'),       cfg.correctm = 'FDR';       end;
if ~isfield(cfg, 'tail'),           cfg.tail = 1;               end;

if ~isfield(cfg, 'permstatstype'),  cfg.permstatstype = 'mean'; end;
if strcmp(cfg.permstatstype , 'mean') == 0 && strcmp(cfg.permstatstype , 'indepsamplesT') == 0 && strcmp(cfg.permstatstype , 'depsamplesT') == 0
    fprintf('\n')
    error('TRENTOOL ERROR: wrong cfg.permstatstype - use ''mean'' ''depsamplesT'' or ''indepsamplesT'', see help!');
end

if ~isfield(cfg, 'shifttest'),  cfg.shifttest = 'yes'; end;
if strcmp(cfg.shifttest , 'yes') == 0 && strcmp(cfg.shifttest , 'no') == 0
    fprintf('\n')
    error('TRENTOOL ERROR: wrong cfg.shifttest - use ''yes'' or ''no'', see help!');
end
if strcmp(cfg.shifttest , 'yes') 
    if ~isfield(cfg, 'shifttype'),    cfg.shifttype = 'predicttime'; end;
    if ~isfield(cfg, 'shifttesttype'),  cfg.shifttesttype = 'TE>TEshift'; end;
    if strcmp(cfg.shifttesttype , 'TE>TEshift') == 0 && strcmp(cfg.shifttesttype , 'TEshift>TE') == 0
        fprintf('\n')
        error('TRENTOOL ERROR: wrong cfg.shifttesttype - use ''TE>TEshift'' or ''TEshift>TE'', see help!');
    end
end
    
if ~isfield(cfg, 'fileidout'),
    fprintf('\n')
    error('TRENTOOL ERROR: cfg.fileidout must be defined, see help!');
end;

% check optimizemethod
if ~isfield(cfg, 'optdimusage'),  
    fprintf('\n')
    error('TRENTOOL ERROR: cfg.optdimusage is not defined, see help!')
else
    if strcmp(cfg.optdimusage, 'maxdim') == 0 && strcmp(cfg.optdimusage, 'indivdim') == 0 
        fprintf('\n')
        error(['TRENTOOL ERROR: ',cfg.optdimusage,' is a wrong input for cfg.optdimusage , see help!'])
    end
end;

% check dim 
if ~isfield(cfg, 'dim')
    if strcmp(cfg.optdimusage, 'indivdim')
        cfg.dim = data.TEprepare.optdimmat;
%         cfg.optdimusage = cfg.optdimusage;
    else
        cfg.dim(1:size(data.TEprepare.optdimmat,1),1) = data.TEprepare.optdim;
%         cfg.optdimusage = cfg.optdimusage;
    end
else
    if strcmp(cfg.optdimusage, 'indivdim')
        if size(cfg.dim,1) ~= size(data.TEprepare.channelcombi,1)
            fprintf('\n')
            error('TRENTOOL ERROR: cfg.dim has to be in that size: (channelcombi x 1), see help!')
        elseif size(cfg.dim,2)>1
            fprintf('\n')
            error('TRENTOOL ERROR: cfg.dim has to be in that size: (channelcombi x 1), see help!')
        end
    else
        if size(cfg.dim,1)>1 && size(cfg.dim,2)>1
            fprintf('\n')
            error('TRENTOOL ERROR: cfg.dim must include a scalar, see help!');
        end
        if cfg.dim < data.TEprepare.optdim
            fprintf('\n')
            fprintf('TRENTOOL WARNING: specified embedding dimension (cfg.dim) is smaller then the optimal dimension from TEprepare.')
        elseif cfg.dim > data.TEprepare.optdim
            fprintf('\n')
            fprintf('TRENTOOL WARNING: specified embedding dimension (cfg.dim) is bigger then the optimal dimension from TEprepare.')
        end
    end
end;


% check tau
if ~isfield(cfg, 'tau')
    if strcmp(data.TEprepare.cfg.optimizemethod, 'ragwitz') 
        if strcmp(cfg.optdimusage, 'indivdim')
            cfg.tau = data.TEprepare.opttaumat;
        else
            cfg.tau(1:size(data.TEprepare.channelcombi,1)) = data.TEprepare.opttau;
        end
    elseif strcmp(data.TEprepare.cfg.optimizemethod, 'cao') 
        cfg.tau(1:size(data.TEprepare.channelcombi,1)) = data.TEprepare.cfg.caotau;
    end
    
else
    if strcmp(cfg.optdimusage, 'indivdim') && strcmp(data.TEprepare.cfg.optimizemethod, 'ragwitz') 
        if size(cfg.tau,1) ~= size(data.TEprepare.channelcombi,1)
            fprintf('\n')
            error('TRENTOOL ERROR: cfg.tau has to be in that size: (channelconmbi x 1), see help!')
        elseif size(cfg.tau,2)>1
            fprintf('\n')
            error('TRENTOOL ERROR: cfg.tau has to be in that size: (channelconmbi x 1), see help!')
        end
    else
        if size(cfg.tau,1)>1 && size(cfg.tau,2)>1
            fprintf('\n')
            error('TRENTOOL ERROR: cfg.tau must include a scalar, see help!');
        end
    end
    
end
    
    
% check TE parameter
if isempty(cfg.predicttime_u), error('TRENTOOL ERROR: specify cfg.predicttime_u, see help!');  end;

if ~isfield(cfg, 'kth_neighbors'),  cfg.kth_neighbors = 4;  end;

if ~isfield(cfg, 'TheilerT'),       cfg.TheilerT = 'ACT';   end;
if ~strcmp(cfg.TheilerT, 'ACT');
    if size(cfg.TheilerT,1)>1 || size(cfg.TheilerT,2)>1
        fprintf('\n')
        error('TRENTOOL ERROR: cfg.TheilerT must include a scalar, see help!');
    end
end


% check the format of input vectors
if size(cfg.toi,1)>size(cfg.toi,2)
    cfg.toi=cfg.toi';
elseif size(cfg.predicttime_u,1)>size(cfg.predicttime_u,2)
    cfg.predicttime_u=cfg.predicttime_u';
elseif size(cfg.kth_neighbors,1)>1 || size(cfg.kth_neighbors,2)>1
    fprintf('\n')
    error('TRENTOOL ERROR: cfg.dim must include a scalar, see help!');
end




fprintf(' - ok');





%% get channels, ACT and trials from the cfg.TEprepare
% ------------------------------------------------------------------------

cfg.permtest.channelcombi = data.TEprepare.channelcombi;
cfg.permtest.channelcombilabel = data.TEprepare.channelcombilabel ;
cfg.permtest.ACT=data.TEprepare.ACT;

% select trials
trials=data.TEprepare.trials;
nrtrials=data.TEprepare.nrtrials;
cfg.permtest.trials=trials;
cfg.permtest.nrtrials=nrtrials;


%% check nr of permutations
% -------------------------------------------------------------------------
fprintf('\n\nChecking number of permutations');

nr2cmc=size(data.TEprepare.channelcombilabel,1)*size(cfg.predicttime_u,2);

if ~isfield(cfg, 'numpermutation'),
    cfg.numpermutation = 190100; % for p<0.01 with a possible bonferroni correcetion of 100
elseif cfg.numpermutation < ceil(1/cfg.alpha)
    fprintf('\n')
    error('TRENTOOL ERROR: cfg.numpermutation too small!');
else
    if nrtrials>31
        if cfg.numpermutation > 2^31
            fprintf('\n')
            error('TRENTOOL ERROR: cfg.numpermutation too huge!');
        end
    else
        if cfg.numpermutation > 2^min(nrtrials)
            fprintf('\n')
            error('TRENTOOL ERROR: cfg.numpermutation too huge!');
        end
    end
    if cfg.numpermutation < ceil(1/(cfg.alpha/nr2cmc))
       fprintf('\n#######################################################################################\n# WARNING: Nr of permutations not sufficient for correction for multiple comparisons! #\n#######################################################################################\n'); 
    end
end

fprintf(' - ok\n');


%% start calculating TE
% -------------------------------------------------------------------------

cfg.calctime = 'yes';

% for unshuffled data
% ----------------------
fprintf('\nStart calculating transfer entropy for unshuffled data');
cfg.shuffle = 'no';
[TEresult] = transferentropy(cfg,data);
TEresult.TEprepare = data.TEprepare;

% %$ML
% save(strcat(cfg.fileidout,'_TEresultorig'), 'TEresult','-v7.3');

cfg.calctime = 'no';

% for shifted data
% ----------------------
% TEshift is created inside transferentropy.m as a reduced version of
% TEresult without certain fields. TEshift is never written to disk/file
% to avoid later confusion. Please save TEshift yourself if necessary.
if strcmp(cfg.shifttest, 'yes')
    fprintf('\nStart calculating transfer entropy for shifted data');
    cfg.shuffle = 'no';
    [TEshift] = transferentropy(cfg,data,'shifttest');

%     %$ML
%     save(strcat(cfg.fileidout,'_TEshift'), 'TEshift','-v7.3');

    
    % permutation test for shift test
    fprintf('\nStart permutation tests for shift test');
    permstatstype = cfg.permstatstype;
    cfg.permstatstype = 'indepsamplesT';
    tailtype = cfg.tail;
    cfg.tail = 1;
    if strcmp(cfg.shifttesttype, 'TE>TEshift')
        alpha = cfg.alpha;
        cfg.alpha = 0.05;
        TEpermshift = TEperm(cfg,TEresult,TEshift);
        cfg.alpha = alpha;
    elseif strcmp(cfg.shifttesttype, 'TEshift>TE')
        alpha = cfg.alpha;
        cfg.alpha = 0.1;
        TEpermshift = TEperm(cfg,TEshift,TEresult);
        cfg.alpha = alpha;
    end
    cfg.permstatstype = permstatstype;
    cfg.tail=tailtype;
    
    
    
%     %$ML
%     save(strcat(cfg.fileidout,'_TEpermshift'), 'TEpermshift','-v7.3');

    
    % analyze shift test
    fprintf('\nanalyze shift test\n');
    
    % MW: check if there are NaNs in TEresult from errors in
    % transferentropy
    NaNidx=find(isnan(TEresult.TEmat));
    if ~isempty(NaNidx)
        disp('Found NaN in TEresult.TEmat! Aborting')
        return
    end
    
    
    if strcmp(cfg.shifttesttype, 'TE>TEshift')
        indexinstmix = find(TEpermshift.TEpermvalues(:,2)==0);
        if size(indexinstmix,1) == 0
            fprintf('No instantaneous mixing found!\n')
        else
            fprintf(strcat(num2str(size(indexinstmix,1)),' instantaneous mixings found by strict shifttest!\nFor these cases TEvalues of all trials are set to NaN!\n'))
            mask=repmat((TEpermshift.TEpermvalues(:,2)-1)*-1, [1 1 size(TEresult.TEmat,2)]);
            TEresult.TEmat(mask==1) = NaN;
            TEresult.MImat(mask==1) = NaN;
            clear mask;
            TEresult.instantaneousmixing = (TEpermshift.TEpermvalues(:,2)-1)*-1;
        end
    elseif strcmp(cfg.shifttesttype, 'TEshift>TE')
        indexinstmix = find(TEpermshift.TEpermvalues(:,2)==1);
        if size(indexinstmix,1) == 0
            fprintf('No instantaneous mixing found!\n')
        else
            fprintf(strcat(num2str(size(indexinstmix,1)),' instantaneous mixings found by non-strict shifttest!\nFor these cases TEvalues of all trials are set to NaN!\n'))
            mask=repmat(TEpermshift.TEpermvalues(:,2), [1 1 size(TEresult.TEmat,2)]);
            TEresult.TEmat(mask==1) = NaN;
            TEresult.MImat(mask==1) = NaN;
            clear mask;
            TEresult.instantaneousmixing = TEpermshift.TEpermvalues(:,2);
        end
    end
    
    clear TEpermshift
end

% for shuffled data
% ----------------------
% TEshuffle is created inside transferentropy.m as a reduced version of
% TEresult without certain fields. TEshuffle is never written to disk/file
% to avoid later confusion. Please save TEshuffle yourself if necessary.
fprintf('\nStart calculating transfer entropy for shuffled data');
cfg.shuffle = 'yes';
[TEshuffle] = transferentropy(cfg,data);

cfg = rmfield(cfg, 'shuffle');
cfg = rmfield(cfg, 'calctime');

%     %$ML
%    save(strcat(cfg.fileidout,'_TEshuffle'), 'TEshuffle','-v7.3');



%% permutation tests
% -------------------------------------------------------------------------
fprintf('\nStart permutation tests');

%TEpermtest=[];

TEpermtest = TEperm(cfg,TEresult,TEshuffle);

TEpermtest.dimord = 'chanpair_value';
TEpermtest.cfg = cfg;
TEpermtest.ACT.actvalue = data.TEprepare.ACT;
TEpermtest.sgncmb = TEresult.sgncmb;
TEpermtest.numpermutation = cfg.numpermutation;
TEpermtest.TEprepare = data.TEprepare;
TEpermtest.nr2cmc = nr2cmc;
fprintf('\nCalculation ready\n')


%% save results
% -------------------------------------------------------------------------
fprintf('\nSaving ...')
fprintf('\nResults of TE')
save(strcat(cfg.fileidout,'_time',num2str(cfg.toi(1)),'-',num2str(cfg.toi(2)),'s_TE_output.mat'), 'TEresult','-v7.3');
fprintf(' - ok');
fprintf('\nResults of permutation test')
save(strcat(cfg.fileidout,'_time',num2str(cfg.toi(1)),'-',num2str(cfg.toi(2)),'s_TEpermtest_output.mat'), 'TEpermtest','-v7.3');
fprintf(' - ok');


%% Returning to the working directory
cd(working_directory1)


fprintf('\n\nThank you for using this transfer entropy tool!\n')

return;

