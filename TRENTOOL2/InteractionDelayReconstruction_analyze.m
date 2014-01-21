function TGA=InteractionDelayReconstruction_analyze(cfg,data)

% analyzes and compacts the output of InteractionDelayReconstruction_calculate
%
% the output of InteractionDelayReconstruction_calculate which serves as an input here
% is a cell array of results from separate TEsurrogatestats processes --
% each TEsurrogatestats has been called with an individual prediction
% time u one, and the output is collected as one result of TEsurrogatestats
% per cell (see below for details)
% The ouput of InteractionDelayReconstruction_analyze looks like the output of
% TEpermstats with a column for the optimal interaction
%  delay (predicttime_u) added as a seventh column
%
% You can call this function directly as follows:
%         dataout=TEprepare(cfg, data)
%
% * DEPENDENCIES
%
% * INPUT PARAMETERS
%
%   data           = output of InteractionDelayReconstruction_calculate. Cell array with as many
%                        cells as predictime_u's that were scanned in InteractionDelayReconstruction_calculate.
%                        Each cell contains the output of a call to TEsurrogatestats (TEpermtest and TEresult)
%                        (see below for details)
%
%             TEpermtest
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
%            AND
%
%           TEresult             (= Output structure of the function tranferentropy)
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
%  cfg     a configuration structure with the fields
%
%          .select_opt_u =  selects the way the optimal u is determined
%                          optiomns are:
%
%                          'min_p' - optimal predictiontime u is the one
%                          with the largest statistical distance (smallest
%                          randomization p-value) to  surrogate data.
%                          This option might be problematic with
%                          respect to later testing of existence of
%                          a link if not used on independent data first.
%
%                          'max_TEdiff' - optimal predictiontime u is the
%                          one with the largest difference in the test
%                          statistic between data and surrogates.
%                          This option might be problematic if different
%                          predictiontimes u lead to vastly different
%                          embedding via the optimization in the ragwitz
%                          criterion.
%                                                                               
%                         'product_evidence' - optimal predictiontime u is
%                         the one which maximes the product (1-p)*TEdiff.
%                         is is a statistically weighted measure of
%                         TEdifferences between data and surrogates
%                         (experimental feature)
%
%          .select_opt_u_pos= 'shortest' select the shortest u if multiple u's
%                             optimize the target quantity (minimum p,
%                             maximum TE difference); 'longest' select the
%                             longest u that optimizes the target quantity
%
% This program is free software; you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation;
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY;
% version2.0
% (C) Michael Wibral, Raul Vicente and Michael Lindner 2012

% CHANGELOG
% Nicu Pampu:bugfix for the nan index (initial was without value when using function max) now set to 1 

NumOfUs=length(data); % the number of different prediction times scanned in previous call to InteractionDelayReconstruction_calculate
NumSgnCmbs=size(data{1}.sgncmb,1);% the number of signal combinations in the input, taken from the first cell

% collect data into an efficient structure
TGA=data{1}; % dummy copy
% create a container to collect  indivicual TEpermvalues into a 4-dim array
TGA.TEpermvaluesTmp=nan([size(data{1}.TEpermvalues) NumOfUs]);
% preallocate the output array
TGA.TEpermvalues=nan(size(data{1}.TEpermvalues));

TGA.sgncmb=data{1}.sgncmb;
% remove misleading old information
%TGA=rmfield(TGA.cfg,'predicttime_u');

TEDiffMat=nan(size(data{1}.TEpermvalues,1),NumOfUs); % called TEDiffmat, because it contains TE values in the form of differences against surrogates
uvec=nan(NumOfUs,1);
for uu=1:NumOfUs
    TGA.TEpermvaluesTmp(:,:,uu)=data{uu}.TEpermvalues;
    uvec(uu)=data{uu}.cfg.predicttime_u;
    TEDiffMat(:,uu)=data{uu}.TEpermvalues(:,4);
end

minp = nan(NumSgnCmbs,1);
IdxMinP = nan(NumSgnCmbs,1);
OptUTmp = nan(NumSgnCmbs,1);
for cc=1:NumSgnCmbs
     if strcmp(cfg.select_opt_u,'min_p') % look for the u with the smallest p-value
         minp(cc)=min(squeeze(TGA.TEpermvaluesTmp(cc,1,:)));
         IdxMinPTmp=find(TGA.TEpermvaluesTmp(cc,1,:)==minp(cc));
         
         if strcmp(cfg.select_opt_u_pos,'shortest')
             IdxMinP(cc)=IdxMinPTmp(1); 
         elseif strcmp(cfg.select_opt_u_pos,'longest')
             IdxMinP(cc)= IdxMinPTmp(end);
         end
         TGA.TEpermvalues(cc,:)=TGA.TEpermvaluesTmp(cc,:,IdxMinP(cc));
         OptUTmp(cc)=data{IdxMinP(cc)}.cfg.predicttime_u;
     elseif strcmp(cfg.select_opt_u,'max_TEdiff') % look for the u with the largest TE difference
         maxTE(cc)=max(squeeze(TGA.TEpermvaluesTmp(cc,4,:)));
         IdxMaxTETmp=find(TGA.TEpermvaluesTmp(cc,4,:)==maxTE(cc));
         
         if isnan(maxTE(cc))            %if it is nan the index must have a value
           IdxMaxTE(cc) = 1;            %otherwhise it will have a error
         else 
                
            if strcmp(cfg.select_opt_u_pos,'shortest')
                 IdxMaxTE(cc)=IdxMaxTETmp(1);
            elseif strcmp(cfg.select_opt_u_pos,'longest')
                 IdxMaxTE(cc)=IdxMaxTETmp(end);
            end
         end   
         TGA.TEpermvalues(cc,:)=TGA.TEpermvaluesTmp(cc,:,IdxMaxTE(cc));
         OptUTmp(cc)=data{IdxMaxTE(cc)}.cfg.predicttime_u;
     elseif strcmp(cfg.select_opt_u,'product_evidence') % get a evidence weighted TEdifference metric
         Product = (1-squeeze(TGA.TEpermvaluesTmp(cc,1,:))) .* squeeze(TGA.TEpermvaluesTmp(cc,4,:));
         maxProduct = max(Product);
         IdxMaxProductTmp= find(Product==maxProduct);
         if isnan(maxProduct)            %if it is nan the index must have a value
           IdxMaxTE(cc) = 1;             %otherwhise it will have a error
         else       
            if strcmp(cfg.select_opt_u_pos,'shortest')
                IdxMaxProduct = IdxMaxProductTmp(1);
            elseif strcmp(cfg.select_opt_u_pos,'longest')
                IdxMaxProduct = IdxMaxProductTmp(end);
            end  
         end
         TGA.TEpermvalues(cc,:)=TGA.TEpermvaluesTmp(cc,:,IdxMaxProduct);
         OptUTmp(cc)=data{IdxMaxProduct}.cfg.predicttime_u;
     end
end
% % insert the vector of u-value where minimum p was found, remove volume
% % conduction (volume conduction will result in  0 ms).

VolCondIndicator=(ones(size(squeeze(TGA.TEpermvalues(:,5))))-squeeze(TGA.TEpermvalues(:,5))); % 0 for volume conduction, 1 for OK

TGA.TEpermvalues(:,6)=(OptUTmp).*VolCondIndicator;
TGA.uvec=uvec;
TGA.TEDiffMat=TEDiffMat;
