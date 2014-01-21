function [TEresult]=transferentropy(cfg, data, varargin)

%
% TRANSFERENTROPY computes the transfer entropy (TE) among given pairs of
% channels for a sequence of trials over periods of time
%
% This function is called by the function TEsurrogatestats,
% TEconditionstatssingle,TEconditionsstatsgroup, and TEgroupstats
%
% !! varargin is only used for the shift test when called from the function
% TEsurrogatestats, TEconditionstatssingle or TEgroup_calculate !!
%
%
% * REFERENCE INFORMATION
%     - The concept of TE appears in Schreiber's article,
%       "Measuring Information Transfer", Phys. Rev. Lett. 85, 461 - 464
%       (2000).
%     - For the estimation of probability densities needed for the TE
%       computation, the function implements the Kraskov-Stoegbauer-
%       Grassberger estimator described in Kraskov et al. "Estimating
%       mutual information", Phys. Rev. E 69 (6) 066138, (2004).
%
% * DEPENDENCIES
%     - Package TSTOOL is used at nearest neighbors searches
%       required for the KSG estimator. (Gnu Public License)
%       http://www.dpi.physik.uni-goettingen.de/tstool/
%     - The following Matlab toolboxes:
%           - signal processing toolbox
%           - statistic toolbox
%     - The functions
%           - TEactdetect
%           - TEchannelselect % ToDo: change to Fieldtrip's CHANNELSELECT
%           function
%           - TEtrialselect
%           - TEvalues
%           - TECvalues
%
%
% * INPUT PARAPETERS
%
%   data            = Fieldtrip datastructure MUST contain:
%       .trials     = three dimensional data matrix
%       .time       = vector 1 x numtoi, the time included in the
%                     data
%       .label      = vector 1 x numlabel, labels of channelnames included
%                     in the data
%       .fsample    = value of sampling rate
%
%   The configuration MUST contain:
%   cfg.sgncmb      = list of channelpairs
%                     cell array (Nx(source, target))
%   or
%   cfg.channel     = list of channels testing all by all
%
%   and
%
%   cfg.toi         = the time range of interest (vector 1 x numtoi) in ms
%                     e.g. (time_from, time_to)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   WARNING:
%   The span of time needed for embedding is: (max(dim)-1)*max(tau)
%   The prediction time starts after this embedding time. Hence the span of
%   time defined in cfg.toi must be a good deal longer than the embedding
%   time, at least embedding time plus 150 samples or max(cfg.predicttime_u).
%
%       |<  embedding time  >|< prediction time u...
%   ----|--------------------|-----------------------------------|-->
%       |<                       cfg.toi                        >|
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   cfg.dim         = embedding dimension
%   cfg.predicttime_u = points ahead for the advance vector in ms
%
%   cfg.kth_neighbors = number of neighbors for fixed mass search (controls
%                     balance of bias/statistical errors) (default = 4)
%   cfg.TheilerT    = number of temporal neighbors excluded to avoid serial
%                     correlations (Theiler correction) (default = 'ACT')
%   cfg.trialselect = ACT threshholding of trials - 'ACT' ,'range' or 'no'
%                     (default = 'ACT')
%       if you chose 'ACT' or nothing add:
%       cfg.actthrvalue = max threshold for ACT; min threshold default = 0
%       cfg.minnrtrials = minimum Nr of trials with ACT < actthrest used to
%                     calculate transfer entropy
%       if you chose 'range' add:
%       cfg.trial_from  = Inferior limit for the trials to be considered
%       cfg.trial_to    = Superior limit for the trials to be considered
%   cfg.maxlag      = the range of lags for computing the auto correlation
%                     time: from -MAXLAG to MAXLAG (default = 1000)
%   cfg.surrogatetype = 'trialshuffling','trialreverse','blockresampling',
%                     'blockreverse1','blockreverse2', 'blockreverse3', or
%                     'swapneighbor'.
%
%                       original trial:     1 2 3 4 5 6
%                       trialshufling:      2 3 4 5 6 1
%                       trialreverse:       6 5 4 3 2 1
%                       blockresampling:    4 5 6 1 2 3
%                       blockreverse1:      3 2 1 6 5 4
%                       blockreverse2:      6 5 4 1 2 3
%                       blockreverse3:      4 5 6 3 2 1
%                       swapneighbors:      2 1 4 3 6 5
%
% * OUTPUT PARAMETERS
%
%  TEresult             = Output structure
%          .TEmat       = resultmatrix including transfer entropy(TE)
%                         values
%          .MImat       = resultmatrix including mutual information (MI)
%                         values
%          .dimord      = dimensions of TEmat and MImat
%          .cfg         = configuration file used to calculate TE
%          .trials      = trial numbers selcted from raw dataset
%          .act         = ACT matrix (channelcombi x 2 trial)
%          .sgncmb      = labels of channel combinations (source -> target)
%
% This program is free software; you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation;
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY;
%
% Version 2.0 by Raul Vicente, Michael Wibral, and Michael Lindner
% Bonn 2011
%
% CHNAGELOG:
% 2011-12-28: ML changed the internal cells and matrices (datacell, datamat,
% ACT, trials, nrtrials) to a common structure (channelcombi x ??)
% Additionally options for the fMRI data analysis are added.
% 2011-08-09:  MW - at each call to TEvalues.m I added an if else statement
% to sewicth to the use of TCvalues if data.TEprepare.cfg.TEcalctype is
% 'VW' - i.e. the use of the new predictor is desired by the user.

%% Remember the working directory
working_directory = pwd;

% check data
% -------------------------------------------------------------------------
fprintf('\nCheck data and config');

[data] = ft_checkdata(data, 'datatype','raw');

% check the data structure
if ~isfield(data, 'trial'),
    error('\nTRENTOOL ERROR: data must be in ''.trial''-structure, see help!');
end;
if ~isfield(data, 'time'),
    error('\nTRENTOOL ERROR: data contains no ''.time''-structure, see help!');
end;
if ~isfield(data, 'label'),
    error('\nTRENTOOL ERROR: data contains no ''.label''-structure, see help!');
end;
if ~isfield(data, 'fsample'),
    error('\nTRENTOOL ERROR: data contains no ''.fsample''-structure, see help!');
end;
if size(data.time,1)>size(data.time,2)
    data.time=data.time';
end



% check configuration and set defaults
% -------------------------------------------------------------------------

% if not defined set defaults
if ~isfield(cfg, 'maxlag'),       cfg.maxlag = 1000;                 end;
if ~isfield(cfg, 'trialselect'),  cfg.trialselect = 'ACT';           end;
if ~isfield(cfg, 'shuffle'),      cfg.shuffle = 'no';                end;

% check if channel or channelcombinations are defined
if isfield(cfg, 'channel') && isfield(cfg, 'sgncmb') ,
    error('\nTRENTOOL ERROR: specify cfg.channel OR sfg.sgncmb, see help!');
elseif isfield(cfg, 'channel') && ~isfield(cfg, 'sgncmb') ,
    channelselect = 1;
elseif ~isfield(cfg, 'channel') && isfield(cfg, 'sgncmb') ,
    channelselect = 2;
end;

% check TheilerT input
if isfield(cfg, 'TheilerT'),
    if strcmp(cfg.TheilerT, 'ACT')
        exc_type = 1;
    else
        exc_type = 2;
    end
end;


% check the format of input vectors
if size(cfg.toi,1)>size(cfg.toi,2)
    cfg.toi=cfg.toi';
elseif size(cfg.dim,1)>size(cfg.dim,2)
    cfg.dim=cfg.dim';
elseif size(cfg.tau,1)>size(cfg.tau,2)
    cfg.tau=cfg.tau';
elseif size(cfg.predicttime_u,1)>size(cfg.predicttime_u,2)
    cfg.predicttime_u=cfg.predicttime_u';
elseif size(cfg.kth_neighbors,1)>size(cfg.kth_neighbors,2)
    cfg.kth_neighbors=cfg.kth_neighbors';
elseif size(cfg.TheilerT,1)>size(cfg.TheilerT,2)
    cfg.TheilerT=cfg.TheilerT';
end



if nargin == 2;
    shifttest = 0;
elseif nargin == 3 && strcmp(varargin{1}, 'shifttest') ;
    shifttest = 1;
end


fprintf(' - ok');



% get values from cfg
% -------------------------------------------------------------------------
channelcombi=data.TEprepare.channelcombi ;
channelcombilabel=data.TEprepare.channelcombilabel ;
ACT=data.TEprepare.ACT;
trials=data.TEprepare.trials;
nrtrials=data.TEprepare.nrtrials;





% read data
% -------------------------------------------------------------------------
fprintf('\nRead data');

% read data in to a cell {channelcombi x 2} including data matrices
% (trial x time)
% for each channelcombination only data will be read into a cell array that
% are allowed by the criteria put on their ACT values

data4TE = cell(size(channelcombi,1),2);
for cc = 1:size(channelcombi,1)
    for pp = 1:2
        datamat = zeros(nrtrials(cc,pp),size(data.trial{1},2)); % MW: check if trial{1} should be trial{2} because the valid trials of the TARGET matter
        for ii = 1:nrtrials(cc,pp) % should be 1:nrtrials(cc,2) to take the data for the source at the valid trials of the TARGET
            datamat(ii,:)=data.trial{trials{cc,pp}(ii)}(channelcombi(cc,pp),:);
        end
        data4TE{cc,pp}=datamat;
        clear datamat;
    end
end

% read data for spatial embedding for fMRI Data as '3DAsEmbed'
if isfield(data, 'Data4Embedding')
    embcell = cell(size(channelcombi,1),2);
    for cc = 1:size(channelcombi,1)
        for pp = 1:2
            embdatamat = zeros(size(Data.Data4Embedding,2),size(Data.Data4Embedding{1,1},2),size(Data.Data4Embedding{1,1},3));
            for ii = 1:size(data.trial,2)
                embdatamat(ii,:,:)=data.Data4Embedding{ii}(channelcombi(cc,pp),:,:);
            end
            embcell{cc,pp}=embdatamat;
            clear embdatamat
        end
    end
end

% get time indices from TEprepare
timeindices = data.TEprepare.timeindices;


% convert k value from ms to sampling points
dimu = data.TEprepare.u_in_samples;
%dimu = round(cfg.predicttime_u/1000*data.fsample);


% create empty result structure
TEresult=[];

fprintf(' - ok');




% calculate number of points inside the time series used for advance and delay
% embedding and check whether there is a sufficient number of them left
% -------------------------------------------------------------------------
if strcmp(cfg.trialselect, 'ACT')
    multiplyact= min( [max(max(ACT(:,2,:))) cfg.actthrvalue] );
    mindatapoints = (timeindices(2)+1-timeindices(1))-(max(cfg.dim)-1)*max(cfg.tau)*multiplyact-max(dimu);
else
    mindatapoints = (timeindices(2)+1-timeindices(1))-(max(cfg.dim)-1)*max(cfg.tau)*max(max(ACT(:,2,:)))-max(dimu);
end


if isfield(data, 'datatype')
    if strcmp(data.datatype, 'fMRI')
        minsamples = 50;
    else
        minsamples = 150;
    end
else
    minsamples = 150;
end

if mindatapoints <= minsamples
    disp(mindatapoints)
    error('\nTRENTOOL ERROR: not enough data points left after embedding ');
end


% Change to directory containing mex files for the nearest neighbors search
[dir_mex] = TEarch(cfg);
cd(dir_mex);



% Check calculation time by calculating one TE with pessimistic values for
% tau, dim, .... etc
% -------------------------------------------------------------------------
if strcmp(cfg.calctime, 'yes')
    if isfield(data, 'Data4Embedding') && strcmp(data.TEprepare.cfg.datatype, 'fMRI')
        %fprintf('\ncalculation of time is not implementend for fMRI data.\n')
    else
        fprintf(strcat('\nCheck calculation time of TE. Please wait...'))
        
        %get time of single TE calculation (pessimistic case)
        timetest = 0;
        for ii = 1:5
            tic
            if exc_type == 1
                % use either the old predictor from 'V'(icente)
                % or the new one 'V(icente)W(ibral)'
                if strcmp(data.TEprepare.cfg.TEcalctype,'V')
                    [te, mi] = TEvalues(squeeze(data4TE{1,1}(1,timeindices(1):timeindices(2))),squeeze(data4TE{1,2}(1,timeindices(1):timeindices(2))),cfg.dim(1),round(cfg.tau(1)*data.TEprepare.maxact),dimu,cfg.kth_neighbors,data.TEprepare.maxact);
                elseif strcmp(data.TEprepare.cfg.TEcalctype,'VW')
                    [te, mi] = TECvalues(squeeze(data4TE{1,1}(1,timeindices(1):timeindices(2))),squeeze(data4TE{1,2}(1,timeindices(1):timeindices(2))),cfg.dim(1),round(cfg.tau(1)*data.TEprepare.maxact),dimu,cfg.kth_neighbors,data.TEprepare.maxact);
                end
            elseif exc_type == 2
                if strcmp(data.TEprepare.cfg.TEcalctype,'V')
                    [te, mi] = TEvalues(squeeze(data4TE{1,1}(1,timeindices(1):timeindices(2))),squeeze(data4TE{1,2}(1,timeindices(1):timeindices(2))),cfg.dim(1),round(cfg.tau(1)*data.TEprepare.maxact),dimu,cfg.kth_neighbors,cfg.TheilerT);
                elseif strcmp(data.TEprepare.cfg.TEcalctype,'VW')
                    [te, mi] = TECvalues(squeeze(data4TE{1,1}(1,timeindices(1):timeindices(2))),squeeze(data4TE{1,2}(1,timeindices(1):timeindices(2))),cfg.dim(1),round(cfg.tau(1)*data.TEprepare.maxact),dimu,cfg.kth_neighbors,cfg.TheilerT);
                end
            end
            timetest = timetest + toc;
        end
        timetest = timetest/5;
        
        % time * nr of loops
        % nrloops = size(cfg.sgncmb,1)*size(trials,2)*size(cfg.dim,2)*size(cfg.tau,2)*size(dimu,2)*size(cfg.kth_neighbors,2)*size(cfg.TheilerT,2);
        timeappr = timetest*size(channelcombi,1)*mean(mean(nrtrials))*size(cfg.TheilerT,2);
        
        
        % if unshuffled and shuffeled data are calculated double the time
        if isfield(cfg, 'permtest')
            timeappr = timeappr*2;
        elseif isfield(cfg, 'permtest') && strcmp(cfg.shifttest, 'yes')
            timeappr = timeappr*3;
        elseif isfield(cfg, 'permtest') && isfield(cfg, 'NrSubjects')
            timeappr = timeappr*2*cfg.NrSubjects;
        elseif isfield(cfg, 'permtest') && strcmp(cfg.shifttest, 'yes') && isfield(cfg, 'NrSubjects')
            timeappr = timeappr*3*cfg.NrSubjects;
        end
        
        % time in minutes OR hours and days
        timehh = floor(timeappr/60^2);                      %hours
        if timehh<1
            timemm = floor(mod((timeappr/60), 60));         %minutes
            fprintf(strcat('\n!!! The calculation of TE takes appr. : ~',num2str(timemm),' minutes  !!!\n'));
        else
            fprintf(strcat('\n!!! The calculation of TE takes appr. : ~',num2str(timehh),' hours (',num2str(timehh/24),' days) !!!\n'));
        end
    end
end

% Start calculation of TE
% -------------------------------------------------------------------------
fprintf('\nCalculating transfer entropy');


% prepare text waitbar
fprintf('\nPlease wait...\n');
for ii = 1:size(channelcombi,1)
    fprintf('-')
end
fprintf('\n')

% create zeros result matrices

TEresult.TEmat=zeros(size(channelcombi,1),max(max(nrtrials)));
TEresult.MImat=zeros(size(channelcombi,1),max(max(nrtrials)));

% loops for scanning channels with different parameter values for TE
for channelpair = 1:size(channelcombi,1)
    fprintf('-');
    
    
    for t4t = 1:nrtrials(channelpair,2)
        
        % OLD trial1 = trials{channelpair,2}(t4t);
        % trial1 (and trial2) will be used as indices into the datamats
        % that are inside data4TE, these are indexed by the indices of the
        % trial numbers, not the trial numbers!
        
        trial1 =t4t;
        
        if strcmp(cfg.shuffle, 'no')
            trial2 = trial1;
            timespan = timeindices(1):timeindices(2);
        elseif strcmp(cfg.shuffle, 'yes')
            if strcmp(cfg.surrogatetype, 'trialshuffling')
                if mod(t4t,nrtrials(channelpair,2)) == 0
                    trial2 = 1;
                else
                    trial2 = t4t+1;
                end
                timespan = timeindices(1):timeindices(2);
                
            elseif strcmp(cfg.surrogatetype, 'blockresampling')
                trial2 = t4t;
                cutpoint = round( (timeindices(2)-timeindices(1)+1) * rand(1));
                timespan =[cutpoint:timeindices(2),timeindices(1):cutpoint-1];
                
            elseif strcmp(cfg.surrogatetype, 'trialreverse')
                trial2 = t4t;
                timespan = timeindices(1):timeindices(2);
                flipdim(timespan,2)
                
            elseif strcmp(cfg.surrogatetype, 'blockreverse1')
                trial2 = t4t;
                cutpoint = round( (timeindices(2)-timeindices(1)+1) * rand(1));
                timespan =flipdim([cutpoint:timeindices(2),timeindices(1):cutpoint-1],2);
                
            elseif strcmp(cfg.surrogatetype, 'blockreverse2')
                trial2 = t4t;
                cutpoint = round( (timeindices(2)-timeindices(1)+1) * rand(1));
                timespan =[flipdim(cutpoint:timeindices(2),2),timeindices(1):cutpoint-1];
                
            elseif strcmp(cfg.surrogatetype, 'blockreverse3')
                trial2 = t4t;
                cutpoint = round( (timeindices(2)-timeindices(1)+1) * rand(1));
                timespan =[cutpoint:timeindices(2),flipdim(timeindices(1):cutpoint-1,2)];
            elseif strcmp(cfg.surrogatetype, 'swapneighbors')
                if mod(t4t,2)==0
                    trial2 = t4t-1;
                else
                    trial2 = t4t+1;
                end
                timespan = timeindices(1):timeindices(2);
            end
            
        end
        
        
        
        if shifttest == 1
            if strcmp(cfg.shifttype, 'onesample')
                a=squeeze(data4TE{channelpair,1}(trial1,timeindices(1)+1:timeindices(2)));
                b=squeeze(data4TE{channelpair,2}(trial2,timeindices(1):timeindices(2)-1));
            elseif strcmp(cfg.shifttype, 'predicttime')
                a=squeeze(data4TE{channelpair,1}(trial1,timeindices(1)+dimu:timeindices(2)));
                b=squeeze(data4TE{channelpair,2}(trial2,timeindices(1):timeindices(2)-dimu));
            end
            
        else
            
            a=squeeze(data4TE{channelpair,1}(trial1,timespan));
            b=squeeze(data4TE{channelpair,2}(trial2,timespan));
            if isfield(data, 'Data4Embedding') && strcmp(data.TEprepare.cfg.datatype, 'fMRI')
                a_e=squeeze(embcell{channelpair,1}(trial1,:,timespan));
                b_e=squeeze(embcell{channelpair,2}(trial2,:,timespan));
            end
        end
        
        if exc_type == 1
            if strcmp(data.TEprepare.cfg.TEcalctype,'V')
                if isfield(data, 'Data4Embedding') && strcmp(data.TEprepare.cfg.datatype, 'fMRI')
                    error('TRENTOOL ERROR: cfg.TEcalctyp=''V'' is not implemented for fMRI Data 3DAsEmbed')
                else
                    [te, mi] = TEvalues(a,b,cfg.dim(channelpair),round(cfg.tau(channelpair)*ACT(channelpair,2,trials{channelpair,2}(t4t))),dimu,cfg.kth_neighbors,ACT(channelpair,2,trials{channelpair,2}(t4t)));
                end
            elseif strcmp(data.TEprepare.cfg.TEcalctype,'VW')
                if isfield(data, 'Data4Embedding') && strcmp(data.TEprepare.cfg.datatype, 'fMRI')
                    [te, mi] = TECvalues3D(a,a_e,b,b_e,round(cfg.tau(channelpair)*ACT(channelpair,2,trials{channelpair,2}(t4t))),dimu,cfg.kth_neighbors,ACT(channelpair,2,trials{channelpair,2}(t4t)));
                else
                    [te, mi] = TECvalues(a,b,cfg.dim(channelpair),round(cfg.tau(channelpair)*ACT(channelpair,trials{channelpair,2}(t4t))),dimu,cfg.kth_neighbors,ACT(channelpair,2,trials{channelpair,2}(t4t)));
                end
            elseif strcmp( cfg.TEcalctype ,'VW_ds')
                % use a d-sepration criterion to ensure that
                % source(t-u-delta)->source(t-u)->target(t)
                % form a Markov chain, when additonally
                % conditioning on target(t-1)
                [te, mi] = TEC_dsvalues(a,b,cfg.dim(channelpair),round(cfg.tau(channelpair)*ACT(channelpair,trials{channelpair,2}(t4t))),dimu,cfg.kth_neighbors,ACT(channelpair,2,trials{channelpair,2}(t4t)));
            end
        elseif exc_type == 2
            if strcmp(data.TEprepare.cfg.TEcalctype,'V')
                if isfield(data, 'Data4Embedding') && strcmp(data.TEprepare.cfg.datatype, 'fMRI')
                    error('TRENTOOL ERROR: cfg.TEcalctyp=''V'' is not implemented for fMRI Data ')
                else
                    [te, mi] = TEvalues(a,b,cfg.dim(channelpair),round(cfg.tau(channelpair)*ACT(channelpair,2,trials{channelpair,2}(t4t))),dimu,cfg.kth_neighbors,cfg.TheilerT);
                end
            elseif strcmp(data.TEprepare.cfg.TEcalctype,'VW')
                if isfield(data, 'Data4Embedding') && strcmp(data.TEprepare.cfg.datatype, 'fMRI')
                    [te, mi] = TECvalues3D(a,a_e,b,b_e,round(cfg.tau(channelpair)*ACT(channelpair,2,trials{channelpair,2}(t4t))),dimu,cfg.kth_neighbors,cfg.TheilerT);
                else
                    [te, mi] = TECvalues(a,b,cfg.dim(channelpair),round(cfg.tau(channelpair)*ACT(channelpair,2,trials{channelpair,2}(t4t))),dimu,cfg.kth_neighbors,cfg.TheilerT);
                end
            elseif strcmp( cfg.TEcalctype ,'VW_ds')
                % use a d-sepration criterion to ensure that
                % source(t-u-delta)->source(t-u)->target(t)
                % form a Markov chain, when additonally
                % conditioning on target(t-1)
                [te, mi] = TEC_dsvalues(a,b,cfg.dim(channelpair),round(cfg.tau(channelpair)*ACT(channelpair,trials{channelpair,2}(t4t))),dimu,cfg.kth_neighbors,ACT(channelpair,2,trials{channelpair,2}(t4t)));
            end
        end
        
        TEresult.TEmat(channelpair,t4t)=te;
        TEresult.MImat(channelpair,t4t)=mi;
        
        
    end
    
end
fprintf('\nCalculation finished')



% results of unshuffled data
% -------------------------------------------------------------------------

if shifttest ==1
    TEresult.shifttest='yes';
end

if strcmp(cfg.shuffle, 'no')
    
    TEresult.act=ACT;
    TEresult.trials = trials; % save used trials in Result matrix
    TEresult.dimord = 'chanpair_trial';
    TEresult.cfg = cfg;
    if channelselect == 1
        TEresult.label=cfg.channel;
        TEresult.sgncmb=channelcombilabel;
    elseif channelselect == 2
        TEresult.sgncmb=cfg.sgncmb;
    end
    
    
end

fprintf('\n')

%% Returning to the working directory
cd(working_directory)

return;

