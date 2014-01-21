function DataOut=TEprepare(varargin)

% TEPREPARE this function checks the input data and parameter for
% completeness and correctness. Further, it optimizes the embedding
% parameters and adds a substructure to the data, which is nesseccary for
% the further functions.
% TEPREPARE has to be performed on all datasets first!!!
%
% You can call this function directly as follows:
%         dataout=TEprepare(cfg, data)
%
% * DEPENDENCIES
%     - Package TSTOOL is used at nearest neighbors searches
%       required for the KSG estimator. (Gnu Public License)
%       http://www.dpi.physik.uni-goettingen.de/tstool/
%     - The following Matlab toolboxes:
%         - signal processing toolbox
%         - statistic toolbox
%     - The functions
%         - TEactdetect
%         - TEchannelselect
%         - TEtrialselect
%         - TEwait
%         - TEprepareview
%
%
% * INPUT PARAMETERS
%
%   data           = Fieldtrip raw data structure - it MUST contain:
%       .trial    = cell array (nr of channels x nr of samples) containing
%                    the data for each trial
%       .time      = cell (1xnr of samples) containing the time indices for
%                    each trial (in seconds)
%       .label     = cell (1xnr of channels), containing the labels
%                    (strings) of channels included in the data
%       .fsample   = value of sampling rate (in Hertz)
%   in case of fMRI data obtained from the function TEnifti2TRENTOOL_3D
%   additionally:
%       .datatype  = 'fMRI'
%       .outputtype= '3DAsTrial', '3DAsEmbed' or 'SingleVoxel' - see help
%                     of the function TEnifti2TRENTOOL_3D
%
% AND
%
%   cfg: The configuration MUST contain:
%
%  cfg.sgncmb      = list of channelpairs
%                    cell array (Nx(source, target))
%  or
%  cfg.channel     = list of channels - testing will be done all-by-all
%
%  and
%
%  cfg.Path2TSTOOL = Path to the folder including the TSTOOL package
%  cfg.toi         = the time range of interest (vector 1 x 2) in seconds
%                    e.g. (time_from, time_to) (units: seconds)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   WARNING:
%   The span of time needed for embedding is: (max(dim)-1)*max(tau)
%   The prediction time starts after this embedding time. Hence the span of
%   time defined in cfg.toi must be a good deal longer than the embedding
%   time, leastwise a multiple of the prediction time (nrk).
%
%       |<  embedding time  >|< prediction time ...
%   ----|--------------------|-----------------------------------|-->
%       |<                       cfg.toi                        >|
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  cfg.TEcalctype =   'V' : self-prediction of the target signal and cross-
%                     prediction are both made from states in source and 
%                     target that precede the target state to be predicted
%                     by cfg.predicttime_u.
%                     'VW' : the self-prediction time for the target is tau
%                     and cross-predictions are made from source states
%                     that precede the target state to be predicted by
%                     cfg.predicttime_u.
%                     (to solve the problem of decreasing self-prediction
%                     accuracy for large prediction times)
%                     (default = 'VW')
%  cfg.predicttime_u = time ahead for the advance prediction (scalar, in
%                      ms)
%
%
%  cfg.optimizemethod = Method to optimize parameters: 'ragwitz' or 'cao'
%                       
%
%  if you choose 'ragwitz':
%       cfg.ragdim  = range of embedding dimensions to scan vector
%                     (1xnumdim)
%       cfg.ragtaurange  = vector (1x2) of min and max embedding delays (in
%                          multiples of the autocorrelation decay time)
%       cfg.ragtausteps  = number of equidistant steps in ragtaurange
%                          (min 5) (default = 10)
%       cfg.flagNei = 'Range' or 'Mass' type of neighbor search
%       cfg.sizeNei = Radius or mass for the neighbor search according to
%                     flagNeighborhood
%       cfg.repPred = repPred represents the number of points for which the
%                     prediction is performed (it has to be smaller than
%                     length(timeSeries)-(dimEmb-1)*tauEmb-u)
%
%  if you choose 'cao' (recommended for fMRI Data):
%       cfg.caodim      = range of embedding dimension to scan with
%                       stepwidth 1 (vector 1 x numdim)
%                       (default = [1,2,3,4,5,6,7,8,9,10])
%       cfg.caokth_neighbors = number of neighbors for fixed mass search
%                    for cao (controls balance of bias/statistical errors)
%                    (default = 4)
%       cfg.caotau      = embedding delay in units of ACT (x*ACT)
%                       (default = 1.5)
%
%   cfg.kth_neighbors = number of neighbors for fixed mass search (controls
%                     balance of bias/statistical errors) (default = 4)
%   cfg.TheilerT    = number of temporal neighbors excluded to avoid serial
%                     correlations (Theiler correction) (default = ACT)
%
%
%  cfg.trialselect = ACT threshholding of trials - 'ACT' ,'range' or 'no'
%                    (default = 'ACT'; for fMRI default = 'no')
%      if you chose 'ACT' (or nothing):
%      cfg.actthrvalue = max threshold for ACT; min threshold
%      cfg.minnrtrials = minimum Nr of trials with ACT < actthrest used to
%                    calculate transfer entropy
%      if you chose 'range':
%      cfg.trial_from  = Inferior limit for the trials to be considered
%      cfg.trial_to    = Superior limit for the trials to be considered
%  cfg.maxlag      = the range of lags for computing the auto correlation
%                    time: from -MAXLAG to MAXLAG (default = 1000)
%
% in case of fMRI data:
%  cfg.embedding_delay_unit = 'ACT' or 'Volumes' (default = 'ACT')
%
%
% * OUTPUT PARAMETERS
%
%   DATA    = The output of this function is the data from the input with
%           the added structure TEprepare.  THE DATA IS NOT SAVED BY THIS
%           FUNCTION. You have to do it by yourself (this is the FieldTrip
%           convention)!
%   .TEprepare
%       .channelcombi       = matrix (nx2) with indices of channels in n
%                             user-defined channel combinations
%       .channelcombilabel  = cell with the channel labels of the
%                             channel pairs
%       .ACT                = matrix (channelcombi x 2 x trial) with the
%                             values of the auto correlation decay times of
%                             the channelcombinations (unit: samples)
%       .trials             = cell {channelcombi x 2} with indices of
%                             the used trials
%       .nrtrials           = matrix with the number of the used trials per
%                             channel combination (channelcombi x 2)
%       .nrreferencepoints  = matrix (channelcombi x trial) with the number
%                             of reference points for the cao calculation
%       .cao                = Structure containing two matrices
%                             (trial x channel x caodim) with the
%                             values E1 and E2 of the cao function
%       .optdimmattrial     = matrix (channelcombi x trial) with optimal
%                             embedding dimension for each trial
%       .optdimmat          = vector (channelcombi) with optimal
%                             embedding dimension over trials
%       .optdim             = max of the optdimmat which should be used as
%                             embedding dimension in the further steps
%       .timeindices        = vector of timeindices in samples (from 
%                             cfg.toi) (1 x 2)
%       .u_in_samples       = points ahead for the advance prediction (from
%                             cfg.predictionstime_u) in samples
%       .cfg                = cfg from the input
%       .maxact             = maximum autocorrelation decay time of the
%                             targetchannels
%   in case of Ragwitz criterion additionally:
%       .opttaumat          = vector (channelcombi) with optimal
%                             embedding delays tau for each channel
%                             combination over trials
%       .opttau             = max of the opttaumat which should be used as
%                             embedding delay in the further steps
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
% Version 2.0 by Michael Lindner, Raul Vicente, Michael Wibral
% Bonn 2011
%

% CHANGELOG:
% 2011-12-28: ML changed the internal cells and matrices (datacell, datamat,
% ACT, trials, nrtrials) to a common indexing scheme (channelcombi x ??), the
% computation and storing of individual embedding parameters for each
% channel-pair is now possible and the options for the fMRI data
% analysis were added.
% 2011-0809: MW - changed allowed values of TEcalctype to V for Vicente
% style estimator and 'VW' for Wibral-Vicente style estimator. Made sure
% cfg.TEcalctype is passed on down to the level of transferentropy.m
%
% 2011-08-01: MW - tried to check and follow the bookeeping of channel
% indices - replaced channel with uc1(channel where necessary)
%


%% Remember the working directory
working_directory = pwd;


%% check data
% -------------------------------------------------------------------------
fprintf('\nCheck data and config\n');

if isfield(varargin{1},'toi') && isstruct(varargin{1}) && isstruct(varargin{2}) && isfield(varargin{2},'trial')
    cfg =  varargin{1};
    data = varargin{2};
else
    error('\nTRENTOOL ERROR: incorrect input values, see help!');
end


% check data using checkdata from Fieldtrip
[data] = ft_checkdata(data, 'datatype','raw');

% check whether time axes and trials have the same number of entries
if iscell(data.time) % one time axis per trial
    for tt=1:size(data.trial,2) % for each trial
        if ~( size(data.time{tt},2) == size(data.trial{tt},2) )
            disp('in trial Nr: ')
            disp(num2str(tt))
            fprintf('\n')
            error('TRENTOOL ERROR! incorrect number of samples in time axis or trial detected')
        end
    end
else % time is a single vector
    for tt=    size(data.trial,2)    % for each trial
        if ~( length(data.time) == size(data.trial{tt},2) )
            disp('in trial Nr: ')
            disp(num2str(tt))
            error('TRENTOOL ERROR! incorrect number of samples in time axis or trial detected')
        end
    end
end

% check the data structure
if ~isfield(data, 'trial'),
    fprintf('\n')
    error('TRENTOOL ERROR: data must be in ''.trial''-field, see help!');
end;
if ~isfield(data, 'time'),
    fprintf('\n')
    error('TRENTOOL ERROR: data contains no ''.time''-field, see help!');
end;
if ~isfield(data, 'label'),
    fprintf('\n')
    error('TRENTOOL ERROR: data contains no ''.label''-field, see help!');
end;
if ~isfield(data, 'fsample'),
    fprintf('\n')
    error('TRENTOOL ERROR: data contains no ''.fsample''-field, see help!');
end;

% for use of fMRI Data from the function TEnifti2TRENTOOL_3D
if isfield(data, 'datatype'),
    TEprepare.datatype = data.datatype;
    cfg.datatype = data.datatype;
    if ~isfield(data, 'outputtype')
        fprintf('\n')
        error('TRENTOOL ERROR: data contains no ''.outputdata''-field, see help!');
    else
        cfg.fmridatatype = data.outputtype;
    end
    if ~isfield(cfg, 'trialselect'),    cfg.trialselect = 'no';    end;
    if ~isfield(cfg, 'TheilerT'),       cfg.TheilerT = 4;           end;
    if ~isfield(cfg, 'embedding_delay_unit'),       cfg.embedding_delay_unit = 'ACT';           end;


    if strcmp(cfg.embedding_delay_unit,'Volumes')==0 && strcmp(cfg.embedding_delay_unit,'ACT')==0
        fprintf('\n')
        error('TRENTOOL ERROR: wrong input for cfg.embedding_delay_unit - see help!')
    end

    if strcmp(cfg.fmridatatype, '3DAsEmbed')
        if isfield(cfg, 'optimizemethod')
            fprintf('\n')
            error('TRENTOOL ERROR. No optimization can be used for the 3DAsEmbed data!')
        end
    end

    if strcmp(cfg.embedding_delay_unit,'Volumes')
        TEprepare.change_act = 1;

%         if strcmp(cfg.optimizemethod, 'cao') && ~isfield(cfg, 'tau')
%             cfg.caotau = 1;
%         else
        if strcmp(cfg.optimizemethod, 'cao') && isfield(cfg, 'tau')
            if mod(cfg.caotau,1)~=0
                fprintf('\n')
                error('TRENTOOL ERROR: in case of fMRI Data and cfg.embedding_delay_unit is not ''ACT'' cfg.caotau must be an integer number.')
            end
        elseif strcmp(cfg.optimizemethod, 'ragwitz')
            fprintf('\n')
            error('TRENTOOL ERROR: in case of fMRI Data and the use of ragwitz criterion cfg.embedding_delay_unit must be ''ACT''.')
        
%         elseif strcmp(cfg.fmridatatype, '3DAsEmbed')
%             if ~isfield(cfg, 'tau'),              cfg.caotau = 1;               end;
        end;
        
    end

end;


if size(data.time,1)>size(data.time,2)
    data.time=data.time';
end


% check configuration and set defaults
if ~isfield(cfg, 'trialselect'),      cfg.trialselect = 'ACT';         end;
if ~isfield(cfg, 'maxlag'),           cfg.maxlag = 1000;               end;
if ~isfield(cfg, 'TEcalctype'),       cfg.TEcalctype = 'VW';           end;


% check optimizemethod 
if ~isfield(cfg, 'optimizemethod'),  cfg.optimizemethod = 'ragwitz';  end;

if strcmp(cfg.optimizemethod, 'ragwitz')

    % check input for ragwitz

    if ~isfield(cfg, 'ragtausteps'), cfg.ragtausteps = 10;  end
    if isfield(cfg, 'ragtausteps')
        if cfg.ragtausteps < 5
            fprintf('\n')
            error('TRENTOOL ERROR: cfg.ragtausteps must be 5 or higher, see help!')
        end
    end

    if ~isfield(cfg, 'ragtaurange'),
        fprintf('\n')
        error('TRENTOOL ERROR: specify cfg.ragtaurange, see help!')
    else

        if cfg.ragtaurange(1) > cfg.ragtaurange(2)
            fprintf('\n')
            error('TRENTOOL ERROR: cfg.ragtaurange must be a vector with increasing values!')
        end

        if size(cfg.ragtaurange,1)>size(cfg.ragtaurange,2)
            cfg.ragtaurange=cfg.ragtaurange';
        end

        if size(cfg.ragtaurange,2) > 2 || size(cfg.ragtaurange,2) < 2
            fprintf('\n')
            error('TRENTOOL ERROR: cfg.ragtaurange has too many values, see help!')
        end

        % create cfg.ragtau vector
        % cfg.ragtau = cfg.ragtaurange(1):(cfg.ragtaurange(2)-cfg.ragtaurange(1))/(cfg.ragtausteps):cfg.ragtaurange(2);
        cfg.ragtau = unique(linspace(cfg.ragtaurange(1),cfg.ragtaurange(2),cfg.ragtausteps));
    end

    if ~isfield(cfg, 'ragdim'),
        cfg.ragdim = 1:10;
    else
        ragdimsort=sort(cfg.ragdim);
        if ragdimsort ~= cfg.ragdim
            fprintf('\n')
            error('TRENTOOL ERROR: cfg.ragdim must be a vector with increasing values!')
        end

        if size(cfg.ragdim,1)>size(cfg.ragdim,2)
            cfg.ragdim=cfg.ragdim';
        end
    end;

elseif strcmp(cfg.optimizemethod, 'cao')

    if ~isfield(cfg, 'caokth_neighbors'), cfg.caokth_neighbors = 4;        end;
    if ~isfield(cfg, 'kth_neighbors'),  cfg.kth_neighbors = cfg.caokth_neighbors;  end;

    if ~isfield(cfg, 'tau'),              cfg.caotau = 1.5;               end;
    % check input for cao
    if ~isfield(cfg, 'caodim'),
        cfg.caodim = [1,2,3,4,5,6,7,8,9,10];
    else

        l=length(cfg.caodim);
        diff=zeros(l-1,1);

        if l > 1
            for ii = 2:l
                diff(ii-1)=cfg.caodim(ii)-cfg.caodim(ii-1);
            end
        end

        if any(diff~=1)
            fprintf('\n')
            error('TRENTOOL ERROR: cfg.caodim must contain a row of numbers with stepwith 1, see help!');
        end

        if length(cfg.caodim) < 5
            fprintf('\n')
            error('TRENTOOL ERROR: cfg.caodim must contain at least 5 entries, see help!');
        end
    end;

    if size(cfg.caodim,1)>size(cfg.caodim,2)
        cfg.caodim=cfg.caodim';
    elseif size(cfg.caotau,1)>size(cfg.caotau,2)
        cfg.caotau=cfg.caotau';
    elseif size(cfg.caokth_neighbors,1)>size(cfg.caokth_neighbors,2)
        cfg.caokth_neighbors=cfg.caokth_neighbors';
    end;
end;


% check TE parameter
if isempty(cfg.predicttime_u),
    fprintf('\n')
    error('TRENTOOL ERROR: specify cfg.predicttime_u, see help!');
elseif length(cfg.predicttime_u) > 1
    fprintf('\n')
    error('TRENTOOL ERROR: cfg.predicttime_u must be a single value, see help!');
end



if ~isfield(cfg, 'kth_neighbors'),  cfg.kth_neighbors = 4;  end;

if ~isfield(cfg, 'TheilerT'),       cfg.TheilerT = 'ACT';   end;
if ~strcmp(cfg.TheilerT, 'ACT');
    if size(cfg.TheilerT,1)>1 || size(cfg.TheilerT,2)>1
        fprintf('\n')
        error('TRENTOOL ERROR: cfg.TheilerT must contain a scalar, see help!');
    end
end


% check if channel or channelcombinations are defined
if ~isfield(cfg, 'channel') && ~isfield(cfg, 'sgncmb') ,
    fprintf('\n')
    error('TRENTOOL ERROR: specify cfg.channel OR cfg.sgncmb, see help!');
elseif isfield(cfg, 'channel') && isfield(cfg, 'sgncmb') ,
    fprintf('\n')
    error('TRENTOOL ERROR: specify cfg.channel OR cfg.sgncmb, see help!');
elseif isfield(cfg, 'channel') && ~isfield(cfg, 'sgncmb') ,
    if size(cfg.channel,2)>size(cfg.channel,1)
        cfg.channel=cfg.channel';
    end
    channelselect = 1;
    % a warning because of some issue if only a subselection of
    % channels enters the analysis
    if max(size(cfg.channel))<size(data.trial{1},1) % If there are less channels
        fprintf('\nTRENTOOL WARNING: your are specifying a subselection of channels \n - please use cfg.sgncmb to specify channelcombinations directly'); 
    end
elseif ~isfield(cfg, 'channel') && isfield(cfg, 'sgncmb') ,
    if size(cfg.sgncmb) ~= 2
        fprintf('\n')
        error('TRENTOOL ERROR: cfg.sgncmb has wrong dimensions, see help!');
    end
    channelselect = 2;
end;


% check the format of input vectors
if size(cfg.toi,1)>2 || size(cfg.toi,1) >2
    fprintf('\n')
    error('\nTRENTOOL ERROR: cfg.toi has more than two entries');
end

if size(cfg.toi,1)>size(cfg.toi,2)
    cfg.toi=cfg.toi';
end



% check alternative ways of trial selection and the required related inputs
if strcmp(cfg.trialselect, 'ACT')
    if isempty(cfg.actthrvalue) || isempty(cfg.minnrtrials)
        fprintf('\n')
        error('TRENTOOL ERROR: specify cfg.actthrvalue and cfg.minnrtrials for ACT thresholding, see help!');
    end
elseif strcmp(cfg.trialselect, 'range')
    if isempty(cfg.trial_from) || isempty(cfg.trial_to)
        fprintf('\n')
        error('TRENTOOL ERROR: specify cfg.trial_from and cfg.trial_to for setting the range of used trials, see help!');
    end
end


fprintf(' - ok');


%% building channelpairs
% -------------------------------------------------------------------------
fprintf('\nBuilding channelpairs');

% OUTPUT:   channelcombi      = nx2 matrix of indices of the channel
%                               combinations.
%           channelcombilabel = nx2 cell array of labels  of the channel
%                               combinations.
[channelcombi,channelcombilabel] = TEchannelselect(cfg, data, channelselect);
TEprepare.cfg.channelcombi = channelcombi;
TEprepare.cfg.channelcombilabel = channelcombilabel;

TEprepare.channelcombi=channelcombi;
TEprepare.channelcombilabel=channelcombilabel;

fprintf(' - ok')



%% read data
% -------------------------------------------------------------------------
fprintf('\nRead data');

% create datacell {channelcombi x 2} including the matrix (trial x
% timepoints) for each channel.
datacell = cell(size(channelcombi,1),2);
for cc = 1:size(channelcombi,1)
    for pp = 1:2
        datamat = zeros(size(data.trial,2),size(data.trial{1},2));
        for ii = 1:size(data.trial,2)
            datamat(ii,:)=data.trial{ii}(channelcombi(cc,pp),:);
        end
        datacell{cc,pp}=datamat;
        clear datamat;
    end
end


% read time values of the data
if iscell(data.time)
    alltime=cell2mat(data.time(1));
else
    alltime=data.time;
end

% find correct indices for the samples in alltime/cfg.toi
% to be used later
% look in the time dimension of the data
timeindices=zeros(1,2);
for ii = 1:size(cfg.toi,2)
    [col]=nearest(alltime, cfg.toi(ii));
    timeindices(ii)=col;
end
TEprepare.timeindices = timeindices;



fprintf(' - ok')

%% define ACT and trials
% ------------------------------------------------------------------------

% calculate ACT
% calculate ACT matrix (channelcombi x 2 x trial)  of           
% the channelpairs in the datacell
[ACT]=TEactdetect(datacell,cfg.maxlag,timeindices);
if isfield(data, 'datatype')
    if strcmp(data.datatzpe, 'fMRI') && strcmp(cfg.embedding_delay_unit, 'Volumes')
        ACT(:,:,:) = 1;
        fprintf('TRENTOOL WARNING: In case of using fMRI data using with cao and cfg.embedding_delay_unit=''Volumes'' the ACT values are set to 1!')
    end
end
TEprepare.ACT=ACT;
TEprepare.maxact=max(max(squeeze(ACT(:,2,:))));

% select trials
% select trials surviving the ACT criterion for all channelcombinations:
% OUTPUTS:
% nrtrials: number of trials for each channelcombi surviving the ACT criterion
%         (channelcombi x 2)
% trials: cell containing the indices of these trials
%           {channelcombi x 2}(nrtrials)
fprintf('\nSelect trials');
[trials,nrtrials]=TEtrialselect(cfg,datacell,ACT,channelcombi);
TEprepare.trials=trials;
TEprepare.nrtrials=nrtrials;


fprintf(' - ok')


% convert u value from ms to sampling points
dimu=round(cfg.predicttime_u/1000*data.fsample);
TEprepare.u_in_samples = dimu;



%% optimize embedding parameters
% -------------------------------------------------------------------------


% Ragwitz criterion
% ------------------

if isfield(cfg, 'datatype')
    if strcmp(cfg.datatype, 'fMRI') && strcmp(cfg.fmridatatype, '3DAsEmbed')
        fprintf('\n')
        warning('WarnTests:convertTest',...
            'No optimization will be performed! TEprepare.optdim will be defined by the number of surrounding voxels specified in Data.Data4Embedding!!!!')
        TEprepare.optdim = size(Data.Data4Embedding{1},2);
    end
else

    if strcmp(cfg.optimizemethod, 'ragwitz') == 1

        fprintf('\nCalculate optimization with Ragwitz criterion');
        
        % define channel on which ragwitz is performed
        targetchannel = 2; % this is index of the target channel in the matrices
        
        
        % define max tau in samples
        maxtau = ceil( min( [max(max(ACT(:,targetchannel,:))) cfg.actthrvalue] ) * max(cfg.ragtau) );
    
        % check if enough data points for embedding exist
        if strcmp(cfg.TheilerT, 'ACT');
            if cfg.repPred >= size(datacell{1,1},2) -  min( [max(max(ACT(:,targetchannel,:))) cfg.actthrvalue]) - (max(cfg.ragdim)-1)*maxtau - dimu 
                fprintf('\n')
                error('TRENTOOL ERROR: Not enough points in timeseries for current analysis settings: cfg.repPred too big, or max(cfg.ragdim)*max(cfg.ragtaurange) too big')
            end
        else
            if cfg.repPred >= size(datacell{1,1},2) - cfg.TheilerT - (max(cfg.ragdim)-1)*maxtau - dimu 
                fprintf('\n')
                error('TRENTOOL ERROR: Not enough points in timeseries for current analysis settings: cfg.repPred too big, or max(cfg.ragdim)*max(cfg.ragtaurange) too big')
            end
        end
    
        
        if TEprepare.u_in_samples < 0.5*min(max(ACT(:,targetchannel,:)))
            fprintf(['\nTRENTOOL WARNING: cfg.predicttime_u is too small! should be bigger than half the autocorrelation time: ', num2str(  .5*min(max(ACT(:,targetchannel,:)))/data.fsample*1000 )]);
        end
        
        % create matrices with nans
        optdim = nan(size(channelcombi,1),max(nrtrials(:,targetchannel)));
        opttau = nan(size(channelcombi,1),max(nrtrials(:,targetchannel)));
        opttau_act = nan(size(channelcombi,1),max(nrtrials(:,targetchannel)));

        % create vector of time points of interest (toi)
        toi=timeindices(1):timeindices(2);
        %     T=length(toi);

        % create comand line waitbar
        fprintf('\n')
        for ii=1:size(channelcombi,1)
            fprintf('-')
        end
        fprintf('\n')

        for channel = 1:size(channelcombi,1) % loop over used channels
            fprintf('-')
            for nt = 1:nrtrials(channel,targetchannel) % loop over trials

                % define trainingpoints for Ragwitz criteria depending on
                % TheilerT
                if strcmp(cfg.TheilerT, 'ACT');
                    TheilerT=ACT(channel,2,trials{channel,targetchannel}(nt));
                else
                    TheilerT=cfg.TheilerT;
                end

                % get trial data from data matrix
                dat=squeeze(datacell{channel,targetchannel}(trials{channel,targetchannel}(nt),toi));

                %create emtpy result matrix with nans
                mre = nan(length(cfg.ragdim),length(cfg.ragtau));


                % loop over ragdim and ragtau
                for rd = 1:length(cfg.ragdim)
                    for rt = 1:length(cfg.ragtau)
                        % tau from multiples of ACT to samples
                        tau_sample = ceil(ACT(channel,2,trials{channel,targetchannel}(nt)) * cfg.ragtau(rt));

                        % calculate Ragwitz - different prediction times have
                        % to be used here depending on the use of the 'V'icente
                        % or the 'V'icente-'W'ibral estimator
                        if strcmp( cfg.TEcalctype ,'V') % use u as the self-prediction time
                            [mre(rd,rt)] = TEragwitz(cfg,dat,cfg.repPred,TEprepare.u_in_samples,cfg.flagNei,cfg.sizeNei,cfg.ragdim(rd),tau_sample,TheilerT,max(cfg.ragdim),maxtau);
                        elseif strcmp( cfg.TEcalctype ,'VW') % use tau as the self-prediction time (and u as the interaction delay in TECvalue)
                            [mre(rd,rt)] = TEragwitz(cfg,dat,cfg.repPred,tau_sample,cfg.flagNei,cfg.sizeNei,cfg.ragdim(rd),tau_sample,TheilerT,max(cfg.ragdim),maxtau);
                        elseif strcmp( cfg.TEcalctype ,'VW_ds') 
                            % use a d-sepration criterion to ensure that
                            % source(t-u-delta)->source(t-u)->target(t)
                            % form a Markov chain, when additonally
                            % conditioning on target(t-1)
                            [mre(rd,rt)] = TEragwitz(cfg,dat,cfg.repPred,1,cfg.flagNei,cfg.sizeNei,cfg.ragdim(rd),tau_sample,TheilerT,max(cfg.ragdim),maxtau);
                        else
                            error('Unsupported option in cfg.TEcalctype')
                        end
                    end
                end
                
                if size(mre,2)>2 % if we can convolve with the kernel                
                Smin=eye(size(cfg.ragtau,2)-1);
                S=eye(size(cfg.ragtau,2));
                S(2:end,1:end-1)=S(2:end,1:end-1)+Smin;
                S(1:end-1,2:end)=S(1:end-1,2:end)+Smin;
                S(3,1)=1; S(end-2,end)=1;
                Smoothedmre = (mre*S)./3;
                else % ... forget about smoothing
                    Smoothedmre = mre;
                end

                [optdimidx, opttauidx]= find(Smoothedmre == min(min(Smoothedmre)));
                optdim(channel,nt) = cfg.ragdim(min(optdimidx));
                opttau(channel,nt) = cfg.ragtau(min(opttauidx));

            end
        end

        % tau from samples to multiples of ACT
        for channel = 1:size(channelcombi,1)
            for nt = 1:nrtrials(channel,targetchannel)
                tauinact = opttau(channel,nt) ./ ACT(channel,2,nt);
                [tauindex]=nearest(cfg.ragtau, tauinact);
                opttau_act(channel,nt) =  cfg.ragtau(tauindex);
            end
        end
        
        % find max tau
        % TEprepare.opttaumat includes a vector with the maximum tau for
        % each channel combination
        TEprepare.opttaumat = max(opttau_act,[],2);
        [chanidx, trialidx] = find(opttau_act == max(max(opttau_act)));
        opttaumultiplier = opttau_act(min(chanidx), min(trialidx));
        % TEprepare.opttau includes a scalar with the maximum tau for
        % all channel combinations
        TEprepare.opttau = opttaumultiplier;

        fprintf(strcat(['\nOptimal tau for this dataset may be: ', num2str(opttaumultiplier),'\n']))

        % find max dimension
        % TEprepare.optdimmat includes a vector with the maximum (over trials) dimension for
        % each channel combination
        TEprepare.optdimmattrial = optdim;
        
        TEprepare.optdimmat = max(optdim,[],2);
        % TEprepare.optdim includes a scalar with the maximum dimension for
        % all channel combinations
        TEprepare.optdim = max(max(optdim));
        fprintf(strcat(['\nOptimal dimension for this dataset may be: ', num2str(max(max(optdim))),'\n']))




    % Cao criterion
    % --------------
    elseif strcmp(cfg.optimizemethod, 'cao') == 1

        fprintf('\nCalculate optimization with cao criteria\n');
        
        % Change to directory containing mex files for the nearest neighbors search
        [dir_mex] = TEarch(cfg);
        cd(dir_mex);


        % scan dimensions for each channel and trial
        % -------------------------------------------------------------------------
        maxdim = max(cfg.caodim)+1;
        TEprepare.nrreferencepoints=zeros(size(datacell,1),size(datacell{1,1},1));
        optdim=zeros(size(datacell,1),maxdim-1);
        
        % channel of the channelpair used to calculate for: 2 = target channel
        targetchannel = 2; % this is index of the target channel in the matrices
        
        ch_count = 0;
        for chanpair = 1:size(channelcombi,1) %
            ch_count = ch_count +1;

            nt_count = 0;
            for nt = 1:nrtrials(chanpair,targetchannel) 
                nt_count = nt_count +1;
                
                
                dat = squeeze(datacell{chanpair,targetchannel}(nt,timeindices(1)+1:timeindices(2)));
                tau = ceil(cfg.caotau*squeeze(ACT(chanpair,2,trials{chanpair,2}(nt))));

                % embedding
                T = length(dat);     % length of full time series
                M = T-(maxdim-1)*tau; % number of points inside the time series ready for delay embedding
                query = (1:M);

                if M < 1
                    error(strcat(['TRENTOOL ERROR: maximum caodim (',num2str(max(cfg.caodim)),') is to high for channel ',channelcombilabel{chanpair,targetchannel}]))
                end

                caodimadded = [cfg.caodim ,max(cfg.caodim)+1];

                pointset = zeros(M,maxdim);
                for ii = 1:M
                    for jj = caodimadded
                        pointset(ii,jj) = dat(ii+(maxdim-1)*tau-(jj-1)*tau);
                    end
                end
                TEprepare.nrreferencepoints(chanpair,nt)=size(pointset,1); 

                % calculate cao
                [E,Ex]=cao(pointset,query,cfg.caokth_neighbors);

                % calculate E1 and E2
                E1(chanpair,nt,:)=E(2:end)./E(1:end-1);
                E2(chanpair,nt,:)=Ex(2:end)./Ex(1:end-1);

                % change because cao !!!!!!!!!!!!!!!!!
                %E1der2=zeros(1,maxdim-2);
                %for kk = 2:maxdim-2
                E1der2=zeros(1,maxdim-3);
                for kk = 2:maxdim-3
                    E1der2(kk) = E1(chanpair,nt,kk-1)+E1(chanpair,nt,kk+1)-2*E1(chanpair,nt,kk);
                end
                optdim(chanpair,nt)=find(E1der2==min(E1der2), 1 );
            end
        end

        % find max dimension
        TEprepare.optdimmat = max(optdim,[],2);
        TEprepare.optdim = max(max(optdim));

        if max(max(optdim)) == max(cfg.caodim)
            fprintf('\n')
            error(strcat('TRENTOOL ERROR: Optimal dimension found: ', num2str(max(max(optdim))),' is the highest in cfg.caodim. Rerun the data with higher values for cfg.caodim!'))
        else
            fprintf(strcat(['Optimal dimension for this dataset may be: ', num2str(max(max(optdim))),'\n']))
        end

        TEprepare.cao.E1=E1;
        TEprepare.cao.E2=E2;

    end


end


cd(working_directory)

fprintf(' - ok\n');



%% add TEprepare structure to the data
% -------------------------------------------------------------------------
fprintf('\nadd TEprepare structure to original data structure ')

TEprepare.cfg=cfg;
varargin{2}.TEprepare = TEprepare;
DataOut = varargin{2};

fprintf(' - ok\ndone\n');

end