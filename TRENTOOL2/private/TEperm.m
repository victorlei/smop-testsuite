function [TEpermtest] = TEperm(cfg,TEresult1,TEresult2)

% TEPERM: This function performs a permutation test on two sets of data and
% is called from the functions TEsurrogatestats, TEconditionstastssingle
% and TEgroup_stats.
%
% * INPUT PARAMETERS
%
%   TEresult1 and TEresult = result structures of the function
%                            transferentropy
%
% AND
%
%   cfg: The configuration MUST contain:
%
%   cfg.alpha          = required significance level
%   cfg.tail           = 1 tail or 2 tail testing; tail 1 = one-tailed test
%                          for TEresult1 > TEresult2
%   cfg.numpermutation = nr of permutations
%   cfg.correctm       = for cmc
%   cfg.permstatstype  = 'mean', 'indepsamplesT' or 'depsamplesT'
%
% * OUTPUT PARAMETERS
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
%                               after correction for mulitple comparisons
%                               (or not)
%                           4 - the values of the test statistic, mean
%                               difference or tvalue of mean
%                               difference depending on cfg.permstatstype
%                           5 - 1 (0), if instantaneous mixing (volume
%                               conduction) exists (or not)
%            .cfg           = configuration file used to calculate TE and
%                             permtest
%            .label         = labels of used channels
%            .sgncmb        = labels of channel combinations (source ->
%                             target)
%            .numpermutation = number of permutations
%            .ACT           = structure including
%                .act       = ACT matrix (channel x trial)
%                .label     = label of channels in ACT matrix
%            .TEprepare     = results of the function TEprepare fron the
%                             data
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
% 2011-12-30: ML reduce the dimension of TEresult and TEpermtest to (channelcombi)

%% Remember the working directory
working_directory = pwd;

%% set new stream for random numbers

RandStream.setDefaultStream(RandStream('mt19937ar','seed',sum(100*clock)));



fprintf('\nPerform permutation tests');

datax = TEresult1.TEmat;
datay = TEresult2.TEmat;


% Some definitions (extracting the number of trials)
n = size(datax,2);
m = size(datay,2);

% sanity check
if strcmp(cfg.permstatstype, 'depsamplesT')
    if n~=m
        error('TRENTOOL ERROR: illegal option size of the two datasets are not equal, see help!');
    end
end

%% Computing the basic statistics
meandatax = mean(datax,2);
meandatay = mean(datay,2);

if strcmp(cfg.permstatstype, 'mean')
    TEstatistic = meandatax-meandatay;
elseif strcmp(cfg.permstatstype, 'indepsamplesT')
    
    SoSx= sum((datax-repmat(meandatax,[1 size(datax,2)])).^2,2);
    SoSy= sum((datay-repmat(meandatay,[1 size(datay,2)])).^2,2);
    DiffSoSxy = (SoSx - SoSy);
    S = sqrt( abs( DiffSoSxy ./ (n+m-2) ));
    TEstatistic = (((meandatax-meandatay)./S) .* (sqrt(n*m/(n+m))));
    clear SoSx SoSy DiffSoSxy S
elseif strcmp(cfg.permstatstype, 'depsamplesT')
    Diffd = datax-datay;
    Dd = mean((datax-datay),2);
    Sdd = sqrt(var(Diffd,0,2));
    TEstatistic = sqrt(n)*Dd./Sdd;
    clear Diffd Dd Sdd
end



% Start permutation test
% -------------------------------------------------------------------------
% (To achieve a CV of 0.1% at the ALS of 0.05 we need 1901 different
% permutations)

TEpermdist=zeros(size(datax,1),cfg.numpermutation);

if strcmp(cfg.permstatstype, 'mean')
    dist_x=zeros(size(datax,1),cfg.numpermutation);
    dist_y=zeros(size(datay,1),cfg.numpermutation);
end

fprintf('\nGenerate permutations\n');

for kk = 1:20
    fprintf('-')
end
fprintf('\n')

for pp = 1:cfg.numpermutation
    
    %   text waitbar
    if mod(pp,floor(cfg.numpermutation/20))==0
        fprintf('-')
    end
    
    if strcmp(cfg.permstatstype, 'mean')
        
        % Pool the data
        data_pool = cat(2, datax, datay);
        
        % Permuting indexes from 1 to n+m
        [ignore,permu] = sort(rand(1,m+n));
        kkt=permu(1:n);
        data4test_x=data_pool(:,kkt);
        kkt2=permu(n+1:m+n);
        data4test_y=data_pool(:,kkt2);
        
        dist_x(:,pp) = mean(data4test_x,2);
        dist_y(:,pp) = mean(data4test_y,2);
        
        
    elseif strcmp(cfg.permstatstype, 'indepsamplesT')
        
        % Pool the data
        data_pool = cat(2, datax, datay);
        
        % Permuting indexes from 1 to n+m
        [ignore,permu] = sort(rand(1,m+n));
        kkt=permu(1:n);
        data4test_x=data_pool(:,kkt);
        kkt2=permu(n+1:m+n);
        data4test_y=data_pool(:,kkt2);
        meandata4test_x = mean(data4test_x,2);
        meandata4test_y = mean(data4test_y,2);
        
        pSoSx= sum((data4test_x-repmat(meandata4test_x,[1 size(data4test_x,2)])).^2,2);
        pSoSy= sum((data4test_y-repmat(meandata4test_y,[1 size(data4test_y,2)])).^2,2);
        
        Sp = sqrt( abs(pSoSx - pSoSy)./ (n+m-2) );
        Tp = ((meandata4test_x-meandata4test_y)./Sp) .* (sqrt(n*m/(n+m)));
        TEpermdist(:,pp) = Tp;
        
    elseif strcmp(cfg.permstatstype, 'depsamplesT')
        
        % Permuting pairs
        
        a = rand(n,1); % UNIFORMLY distributed random numbers between zero and 1
        permvector = zeros(n,1);
        permvector(a<=.50)=-1;
        permvector(a>.50)=1;
        % This would fail if there is only ONE signal combination
        % (cfg.sgncmb) because the last dimension will be singleton and
        % flattened therefore
        % BUT permute (2D-matrix,[3 2 1]) will in this case create a third dimension
        mm = permute(repmat(permvector, [1,size(datax,1)]),[2 1]);
        data4test_x = datax .* mm;%repmat(permvector, [size(datax,1),size(datax,2),size(datax,3)]);
        data4test_y = datay .* mm;%repmat(permvector, [size(datay,1),size(datay,2),size(datay,3)]);
        
        Diff = data4test_x-data4test_y;
        D = mean((data4test_x-data4test_y),2);
        Sd = sqrt(var(Diff,0,2));
        Tp = sqrt(size(D,1))*D./Sd;
        TEpermdist(:,pp) = Tp;
    end
    
end

if strcmp(cfg.permstatstype, 'mean')
    TEpermdist = dist_x-dist_y;
end


fprintf(' - ok');

% Evaluating the quantiles of the true results in the permdistribution
fprintf('\nStart permutation tests');

z = sort(TEpermdist,2);

TEpermvalues = NaN(size(datax,1),5);

% prepare text waitbar
fprintf('\nPlease wait...\n\n');
for ii = 1:size(datax, 1)
    fprintf('-')
end
fprintf('\n')

for channelpair = 1:size(datax,1) % loop over singalcombinations
    fprintf('-');
    
    if isnan(TEstatistic(channelpair)) == 1 % check if a preceeding shiftest has indicated inst. mixing
        TEpermvalues(channelpair,1) = 1;
        TEpermvalues(channelpair,2) = 0;
        TEpermvalues(channelpair,3) = 0;
        TEpermvalues(channelpair,4) = 0;
        TEpermvalues(channelpair,5) = 1; % if a NaN was found indicate this as instantaneous mixing
        
    else
        % Checking the significance level
        if cfg.tail == 1 % one-tailed testing
            TEpermvalues(channelpair,1) = length(find(z(channelpair,:)>TEstatistic(channelpair)))/cfg.numpermutation;
            if TEpermvalues(channelpair,1) == 0 % MW: why 0.0 not 0 ??
                TEpermvalues(channelpair,1) = 1/cfg.numpermutation;
            end
            % MW 2012-09-04 moved significance determination (threshold comparison) to two
            % sections for cfg.tail=1 (here) and  cfg.tail=2 (below)
            % threshold comparison
            if TEpermvalues(channelpair,1) <cfg.alpha
                TEpermvalues(channelpair,2) = 1;
            else
                TEpermvalues(channelpair,2) = 0;
            end
            
        elseif cfg.tail == 2 %two-tailed testing
            
            percentilebigger = length(find(z(channelpair,:)>TEstatistic(channelpair)))/cfg.numpermutation;
            percentilesmaller = length(find(z(channelpair,:)<TEstatistic(channelpair)))/cfg.numpermutation;
            percentilemoreextreme = min(percentilebigger,percentilesmaller); % take minimum of two POSITIVE numbers!
            TEpermvalues(channelpair,1) = percentilemoreextreme;
            if percentilemoreextreme == 0
                TEpermvalues(channelpair,1) = 1/cfg.numpermutation;
            end
            % threshold comparison
            if TEpermvalues(channelpair,1) <cfg.alpha/2
                TEpermvalues(channelpair,2) = 1;
            else
                TEpermvalues(channelpair,2) = 0;
            end
            
            
        end
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        TEpermvalues(channelpair,5) = 0;
    end
    TEpermvalues(channelpair,4) = TEstatistic(channelpair);
    
end


fprintf(' - ok');

%% Correction for multiple comparisons
fprintf('\nCorrection for multiple comparison...')
pvalues = TEpermvalues(:,1);

% correct only for all passing instantaneous mixing test
if isfield(TEresult1, 'instantaneousmixing')
    instmixing1 = TEresult1.instantaneousmixing;
else
    instmixing1 = zeros(size(pvalues,1),1);
end
if isfield(TEresult2, 'instantaneousmixing')
    instmixing2 = TEresult2.instantaneousmixing;
else
    instmixing2 = zeros(size(pvalues,1),1);
end
mixmask = instmixing1 + instmixing2;

% mm0 = find(mixmask==0);

nrinstmix =  size(pvalues,1) - length(find(mixmask==0));


significance = TEcmc(pvalues, cfg.correctm, cfg.alpha, nrinstmix);
TEpermvalues(:,3)=significance;
fprintf(' - ok\n');

%TEpermtest.TEpermdist=TEpermdist;
TEpermtest.TEpermvalues=TEpermvalues;


%% Returning to the working directory
cd(working_directory)

return;

