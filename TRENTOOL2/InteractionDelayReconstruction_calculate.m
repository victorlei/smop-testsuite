function TGA_results=InteractionDelayReconstruction_calculate(cfgTEP,cfgTESS,data)

% InteractionDelayReconstruction_calculate
% Graph Timing calculation based on TE with scanned prediction times u
% 
% This function has to be supplied with the folowing INPUT:
%
% data      -    the data in the fieldtrip raw data format; see TEprepare for more
%           details
% cfgTEP    -    a configuration structure that has all the fields required for
%           TEprepare; see TEprepare for more details. The only difference is that instead of
%           cfg.predictime_u which is a single number, cfg.predicttimemin_u
%           cfg.predicttimemax_u,cfg.predicttimestepsize
%           have to be supplied, indicating the minimum and maximum prediction time of
%           inteterest and the stepsize of the resolution
% cfgTSS    -   a configuration structure that has all the fields required
%           for TEsurrogatestats
%
% The OUTPUT TGA_results results is a cell array of TEpermtest results,
% each cell containing the results for one prediction u 

%%% checks and parameter preparations
% cfg.predicttimevec_u supplied ?
if isfield(cfgTEP,'predicttimemax_u')
    error(' No cfgTEP.predicttimemax_u specified - see HELP InteractionDelayReconstruction_calculate for more information');
end
if ~isfield(cfgTEP,'predicttimemin_u')
    error(' No cfgTEP.predicttimemin_u specified - see HELP InteractionDelayReconstruction_calculate for more information');
end
if ~isfield(cfgTEP,'predicttimestepsize')
    error(' No cfgTEP.predicttimestepsize specified - see HELP InteractionDelayReconstruction_calculate for more information');
end

%
predicttimevec_u=cfgTEP.predicttimemin_u:cfgTEP.predicttimestepsize:cfgTEP.predicttimemax_u;

if ~(predicttimevec_u(end)==cfgTEP.predicttimemax_u)
    predicttimevec_u(end+1)=cfgTEP.predicttimemax_u; % make sure the last intended ptredictiontime is also investigated
end
% all other checks are left to the subsidiary functions

%%% loop over the different prediction times to be investigated
fileidout=cfgTESS.fileidout;

for uu=1:max(size(predicttimevec_u))
    
    % TEprepare part
    % fix config 
    cfgTEP.predicttime_u = predicttimevec_u(uu);
    dataprep=TEprepare(cfgTEP,data);
    
    % TEsurrogatestats part
    % remove all fields meant for TEprepare from the structure
    %cfg=rmfield(cfg,'badfield')
    
    % update fileidout to include information on u    
    cfgTESS.fileidout=strcat(fileidout,'_RAG4_TGA_u_',num2str(cfgTEP.predicttime_u));
    TGA_results{uu}=TEsurrogatestats(cfgTESS,dataprep);
   
end
