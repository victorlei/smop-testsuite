function TEgroupprepare = TEpreparegroup(Data)

% TEGROUPPREPARE: This function checks the parameters of all datasets for
% equality. TEgroupprepare is a called from TEgroupstats and
% TEconditionstats
%
% This function is called by the function TEconditionsstatsgroup,
% TEconditionstatssingle, and TEgroupstats
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
% CHANGELOG
% 2011-12-29: ML added new parameters to check for and some output
% parameters.
%

%% Remember the working directory
% working_directory = pwd;


%% check data
% -------------------------------------------------------------------------
fprintf('\nTEgroupprepare: check input data');

% check datasets
NrSubjects = size(Data,2);
for ii = 1:NrSubjects
    if isstruct(Data{ii}) && isfield(Data{ii},'trial') && isfield(Data{ii},'label')
        %fprintf('\ncorrect input\n');
    else
        fprintf('\n');
        error('TRENTOOL ERROR: incorrect input values, see help!');
    end
    % check if TEprepare was performed
    if ~isfield(Data{ii}, 'TEprepare'),
        fprintf(strcat(['\nIncorrect data structure in dataset ',num2str(ii)]))
        fprintf('\n');
        error('TRENTOOL ERROR: The function TEprepare must be performed on all datasets, see help!');
    end
    
    % check data using checkdata from Fieldtrip
    [data] = ft_checkdata(Data{ii}, 'datatype','raw');
    clear data;
    
    if ~isfield(Data{ii}, 'trial'),
        fprintf('\n');
        error('TRENTOOL ERROR: data must be in ''.trial''-structure, see help!');
    end;
    if ~isfield(Data{ii}, 'time'),
        fprintf('\n');
        error('TRENTOOL ERROR: data contains no ''.time''-structure, see help!');
    end;
    if ~isfield(Data{ii}, 'label'),
        fprintf('\n');
        error('TRENTOOL ERROR: data contains no ''.label''-structure, see help!');
    end;
    if ~isfield(Data{ii}, 'fsample'),
        fprintf('\n');
        error('TRENTOOL ERROR: data contains no ''.fsample''-structure, see help!');
    end;
    if size(Data{ii}.time,1)>size(Data{ii}.time,2)
        data.time=data.time';
    end
    
end



%% creat empty cells and vectors
% cells
Schannel = cell(NrSubjects,1);
Su = cell(NrSubjects,1);
Sk = cell(NrSubjects,1);
ST = cell(NrSubjects,1);
Soptdimmat = cell(NrSubjects,1);
 
STSTOOL = cell(NrSubjects,1);
STEcalctype = cell(NrSubjects,1);
Soptmethod = cell(NrSubjects,1);
Scalctype = cell(NrSubjects,1);
Scaodim = cell(NrSubjects,1);
Sragdim = cell(NrSubjects,1);
SflagNei = cell(NrSubjects,1);
SsizeNei = cell(NrSubjects,1);
SrepPred = cell(NrSubjects,1);
Strialselect = cell(NrSubjects,1);

% vectors
Schannellength = nan(1,NrSubjects);
Stoifrom = nan(1,NrSubjects);
Stoito = nan(1,NrSubjects);
Stau = nan(1,NrSubjects);
Smaxlag = nan(1,NrSubjects);
Soptdim = nan(1,NrSubjects);
%Strials = nan(1,NrSubjects);
Scaokth = nan(1,NrSubjects);
Sactthrvalue = nan(1,NrSubjects);
Sminnrtrials = nan(1,NrSubjects);


%% get data
% -------------------------------------------------------------------------
% loop over subjects
for ii = 1:NrSubjects
    % channels
    Schannel{ii} = Data{ii}.TEprepare.channelcombilabel;
    %Schannel{ii} = TEprepare{ii}.channelcombilabel;
    Schannellength(ii) = size(Data{ii}.TEprepare.channelcombilabel,1);
    % toi
    Stoifrom(ii) = Data{ii}.TEprepare.cfg.toi(1);
    Stoito(ii) = Data{ii}.TEprepare.cfg.toi(2);
    % tau
    if strcmp(Data{ii}.TEprepare.cfg.optimizemethod, 'ragwitz');
        Stau_min(ii) = Data{ii}.TEprepare.cfg.ragtaurange(1);
        Stau_max(ii) = Data{ii}.TEprepare.cfg.ragtaurange(2);
        Stau_steps(ii) = Data{ii}.TEprepare.cfg.ragtausteps;
        Stau(ii) = Data{ii}.TEprepare.opttau;
    else
        Stau(ii) = Data{ii}.TEprepare.cfg.caotau;
    end
    % trialselect
    Strialselect{ii} = Data{ii}.TEprepare.cfg.trialselect;
    % maxlag
    Smaxlag(ii) = Data{ii}.TEprepare.cfg.maxlag;
    % optimal dim
    Soptdim(ii) = Data{ii}.TEprepare.optdim;
    
    Soptdimmat{ii} = Data{ii}.TEprepare.optdimmat;
    
%     Snrtrials(ii,:) = Data{ii}.TEprepare.nrtrials;
    
    
    
    
    %%%%%%%%%%
    Su{ii} = Data{ii}.TEprepare.cfg.predicttime_u;
    
    Sk{ii} = Data{ii}.TEprepare.cfg.kth_neighbors;
    
    ST{ii} = Data{ii}.TEprepare.cfg.TheilerT;
    
    Soptmethod{ii} = Data{ii}.TEprepare.cfg.optimizemethod;
    
    Scalctype{ii} = Data{ii}.TEprepare.cfg.TEcalctype;
    
    STSTOOL{ii} = Data{ii}.TEprepare.cfg.Path2TSTOOL;
    
    STEcalctype{ii} = Data{ii}.TEprepare.cfg.TEcalctype;
    
end


%% create empty vectors for test output
Resvector = zeros(NrSubjects,2);
Resvector(:,1)=1:NrSubjects;


%% check char structure input for equality
% -------------------------------------------------------------------------
for jj = 2:NrSubjects
    if strcmp(Soptmethod{1},Soptmethod{ii}) == 0
        fprintf('\n');
        error('TRENTOOL ERROR: optimizemethods were not the same for all subjects, see help!')
    end
    if strcmp(Scalctype{1},Scalctype{ii}) == 0
        fprintf('\n');
        error('TRENTOOL ERROR: calctype were not the same for all subjects, see help!')
    end
    if strcmp(STSTOOL{1},STSTOOL{ii}) == 0
        fprintf('\n');
        error('TRENTOOL ERROR: Path2TSTOOL were not the same for all subjects, see help!')
    end
    if strcmp(STEcalctype{1},STEcalctype{ii}) == 0
        fprintf('\n');
        error('TRENTOOL ERROR: TEcalctype were not the same for all subjects, see help!')
    end
    
end

%% check structure input depending on optmethod for equality
% -------------------------------------------------------------------------

if strcmp(Soptmethod{1},'cao')
    
    for ii = 1:NrSubjects
        Scaodim{ii} = Data{ii}.TEprepare.cfg.caodim;
        Scaokth(ii) = Data{ii}.TEprepare.cfg.caokth_neighbors;
    end
    if min(Scaokth) ~= max(Scaokth)
        Resvector(:,2)=Scaokth;
        disp(Resvector)
        fprintf('\n');
        error('TRENTOOL ERROR: caokth_neighbors were not the same for all subjects, see help!')
    end
    for ii = 2:NrSubjects
        if isequal(Scaodim{1},Scaodim{ii}) == 0
            fprintf('\n');
            error('TRENTOOL ERROR: caodim were not the same for all subjects, see help!')
        end
    end
    
elseif strcmp(Soptmethod{1},'ragwitz')
    for ii = 1:NrSubjects
        Sragdim{ii} = Data{ii}.TEprepare.cfg.ragdim;
        SflagNei{ii} = Data{ii}.TEprepare.cfg.flagNei;
        SsizeNei{ii} = Data{ii}.TEprepare.cfg.sizeNei;
        SrepPred{ii} = Data{ii}.TEprepare.cfg.repPred;
        
    end
    
%     if min(SflagNei) ~= max(SflagNei)
%         Resvector(:,2)=SflagNei;
%         disp(Resvector)
%         error('TRENTOOL ERROR: flagNei were not the same for all subjects, see help!')
%     else
    if min(SsizeNei{:}) ~= max(SsizeNei{:})
        Resvector(:,2)=SsizeNei;
        disp(Resvector)
        fprintf('\n');
        error('TRENTOOL ERROR: sizeNei were not the same for all subjects, see help!')
    elseif min(SrepPred{:}) ~= max(SrepPred{:})
        Resvector(:,2)=SrepPred;
        disp(Resvector)
        fprintf('\n');
        error('TRENTOOL ERROR: repPredwere not the same for all subjects, see help!')
    end
    for ii = 2:NrSubjects
        if isequal(Sragdim{1},Sragdim{ii}) == 0
            fprintf('\n');
            error('TRENTOOL ERROR: ragdim were not the same for all subjects, see help!')
        end
    end
    
    
end


%% check number vectors for equality
% -------------------------------------------------------------------------
for jj = 2:NrSubjects
    if isequal(Su{1},Su{jj}) == 0
        fprintf('\n');
        error('TRENTOOL ERROR: predicttime_u were not the same for all subjects, see help!')
    end
end




%% check scalar structure inputs for equality
% -------------------------------------------------------------------------
Resvector = zeros(NrSubjects,2);
Resvector(:,1)=1:NrSubjects;

if min(Stoifrom) ~= max(Stoifrom)
    Resvector(:,2)=Stoifrom;
    disp(Resvector)
    error('TRENTOOL ERROR: toi onsets were not the same for all subjects, see help!')
elseif min(Stoito) ~= max(Stoito)
    Resvector(:,2)=Stoito;
    disp(Resvector)
    error('TRENTOOL ERROR: toi offsets were not the same for all subjects, see help!')
elseif min(Schannellength) ~= max(Schannellength)
    Resvector(:,2)=Schannellength;
    disp(Resvector)
    error('TRENTOOL ERROR: number of channels were not the same for all subjects, see help!')
% elseif min(Scaodimlength) ~= max(Scaodimlength)
%     Resvector(:,2)=Scaodimlength;
%     disp(Resvector)
%     error('TRENTOOL ERROR: number of dimensions for cao calculation were not the same for all subjects, see help!')
elseif min(Smaxlag) ~= max(Smaxlag)
    Resvector(:,2)=Smaxlag;
    disp(Resvector)
    error('TRENTOOL ERROR: values for maxlag were not the same for all subjects, see help!')

end


if strcmp(Data{ii}.TEprepare.cfg.optimizemethod, 'ragwitz');
    if min(Stau_steps) ~= max(Stau_steps)
    Resvector(:,2)=Stau_steps;
    disp(Resvector)
    error('TRENTOOL ERROR: tau were not the same for all subjects, see help!')
    elseif min(Stau_min) ~= max(Stau_min)
    Resvector(:,2)=Stau_steps;
    disp(Resvector)
    error('TRENTOOL ERROR: tau were not the same for all subjects, see help!')
    elseif min(Stau_max) ~= max(Stau_max)
    Resvector(:,2)=Stau_max;
    disp(Resvector)
    error('TRENTOOL ERROR: tau were not the same for all subjects, see help!')
    end
else
    if min(Stau) ~= max(Stau)
    Resvector(:,2)=Stau;
    disp(Resvector)
    error('TRENTOOL ERROR: tau were not the same for all subjects, see help!')
    end
end


for ii = 2:NrSubjects
    if isequal(Sk{1},Sk{ii}) == 0
        for jj = 1:NrSubjects
            Resvector(:,2)=Sk{jj};
            disp(Resvector)
            error('TRENTOOL ERROR: values for kth_neighbors were not the same for all subjects, see help!')
        end
    end
end


% check TheilerT

Resvector = zeros(NrSubjects,2);
Resvector(:,1)=1:NrSubjects;

for ii = 2:NrSubjects
    
    if ischar(ST{1})
        if strcmp(ST{1}, ST{ii}) == 0
            Resvector(:,2)=ST;
            disp(Resvector)
            error('TRENTOOL ERROR: values for TheilerT were not the same for all subjects, see help!')
        end
    elseif isnumeric(ST{1})
        if ST{1} ~= ST{ii}
            Resvector(:,2)=ST;
            disp(Resvector)
            error('TRENTOOL ERROR: values for TheilerT were not the same for all subjects, see help!')
        end
    end
    
end


%% check channels and caodim for equality
% -------------------------------------------------------------------------
% create empty result matrices
Result = zeros(NrSubjects+1,NrSubjects+1);
Result(2:end,1)=1:NrSubjects;
Result(1,2:end)=1:NrSubjects;

resentry1 = zeros(NrSubjects,NrSubjects);
resentry2 = zeros(NrSubjects,NrSubjects);

for ii = 1:NrSubjects
    for jj = ii:NrSubjects
        if ii~=jj
            comp1 = 0;comp2 = 0;
            
            % channel
            for c1 = 1:Schannellength(ii)
                for c2 = 1:Schannellength(jj)
                    if strcmp(Schannel{ii}(c1,1),Schannel{jj}(c2,1)) == 1 && strcmp(Schannel{ii}(c1,2),Schannel{jj}(c2,2)) == 1
                        comp1 = comp1 + 1;
                    end
                    
                    
                end
            end
            if comp1 ~= Schannellength(ii)
                resentry1(ii,jj) = 1;
                %Result(ii+1,jj+1) = 1;
            end
            
            % cao dim
            for cd1 = 1:length(Scaodim{ii})
                for cd2 = 1:length(Scaodim{ii})
                    if Scaodim{ii}(cd1) == Scaodim{jj}(cd2)
                        comp2 = comp2 + 1;
                    end
                end
            end
            if comp2 ~= length(Scaodim{ii})
                resentry2(ii,jj) = 1;
            end
            
        end
    end
end

% differences were found
if sum(sum(resentry1)) ~= 0
    Result(2:end,2:end) = resentry1;
    fprintf('\nDataset comparison matrix (ones indicate mismatches between datasets): \n')
    disp(Result)
    error('TRENTOOL ERROR: Mismatch between channels used in TEprepare. Check the dataset comparison matrix above and see help!\n')
end

if sum(sum(resentry2)) ~= 0
    Result(2:end,2:end) = resentry2;
    fprintf('\nDataset comparison matrix (ones indicate mismatches between datasets): \n')
    disp(Result)
    error('TRENTOOL ERROR: Mismatch between scanned dimensions for cao in TEprepare. Check the dataset comparison matrix above and see help')
end




%% check trialselect
% -------------------------------------------------------------------------
comp3 = 0;
for tt = 2:length(Strialselect)
    if strcmp(Strialselect(tt-1),Strialselect(tt))
        comp3 = comp3 + 1;
    end
end

if comp3 ~= length(Strialselect)-1
    fprintf('\n');
    error('TRENTOOL ERROR: trialselect type was not the same for all subjects, see help!')
end

if strcmp(Strialselect(tt),'ACT')
    for ii = 1:NrSubjects
        Sactthrvalue(ii) = Data{ii}.TEprepare.cfg.actthrvalue;
        Sminnrtrials(ii) = Data{ii}.TEprepare.cfg.minnrtrials;
    end
    if min(Sactthrvalue) ~= max(Sactthrvalue)
        Resvector(:,2)=Sactthrvalue;
        disp(Resvector)
        error('TRENTOOL ERROR: actthrvalues were not the same for all subjects, see help!')
%     elseif min(Sminnrtrials) ~= max(Sminnrtrials)
%         
%         Resvector(:,2)=Sminnrtrials;
%         disp(Resvector)
%         error('TRENTOOL ERROR: minnrtrials were not the same for all subjects, see help!')
    end
end

fprintf(' - ok\n\n')



%% create output structure
% -------------------------------------------------------------------------
% optdim
TEgroupprepare.groupoptdim = max(Soptdim);
TEgroupprepare.groupoptdimmat = Soptdimmat;
% maxtrials
TEgroupprepare.nrchannelcombis = length(Schannel{1});

if strcmp(Data{ii}.TEprepare.cfg.optimizemethod, 'ragwitz');
    TEgroupprepare.groupopttau = max(Stau);
else
    TEgroupprepare.groupopttau = Data{ii}.TEprepare.cfg.caotao
end
