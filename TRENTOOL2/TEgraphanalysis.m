function [data_paths,cfg] = TEgraphanalysis(cfg,data);
% TEGRAPHANALYSIS: Detects potentially spurious edges in a graph
% constructed from single subject or single condition TE data. Note that
% the delay times u have to be integer values.
%
% TEgraphanalysis is a wrapper-function, calls TEdfs, TEdyn and 
% TEbacktracking to detect alternative paths for any edge in the TE-graph.
%
% The function constructs a graph from the provided data, such that
%    - edges are defined by data.sgncmb (significant interactions only), 
%    - edge-weights are defined by delay times from data.TEpermvalues,
%    - vertices are enumerated according to their appearance in data.sgncmb
%    - at this point the function considers interactions SIGNIFICANT AT THE 
%      PRESCRIBED ALPHA LEVEL only (no correction for multiple comparison)!
%
% Than the function iteratively
%   - removes an edge from the graph (the weight of this edge is defined as 
%     w_crit)
%   - looks for alternative paths for this edge by running TEdyn
%   - if an alternative path exists, it is reconstructed by TEbacktracking
%
% Alternative paths are collected for all edges. Finally, spurious edges
% are flagged by calling TEflagedges.
% See the reference information for a more detailed description.
%
%
% * REFERENCE INFORMATION
%
%   - graph algorithm
%         - Bsc Thesis Patricia Wollstadt 
%           (email: p.wollstadt@stud.uni-frankfurt.de)
%
%
% * DEPENDENCIES
%     - The functions
%         - TEdfs
%         - TEdyn
%         - TEbacktracking
%     - FieldTrip, see http://www.ru.nl/neuroimaging/fieldtrip, Copyright 
%       (C) 2004-2008, Robert Oostenveld (GNU General Public License)
%         - ft_progress
%
%
% * INPUT PARAMETERS
%
%   cfg: The configuration MUST contain:
%
%       .threshold    = scalar value (in ms): tolerance that is used to 
%                       define the reconstruction interval around w_crit - 
%                       all paths that have a summed weight that falls 
%                       within this interval are considered an alternative 
%                       path
%
%   data
%       .sgncmb       = sgncmb used for definition of edges and vertices
%       .TEpermvalues = matrix with size channelpair x 6
%                           The second dimension includes (row-wise):
%                           1 - p_values of the statistic within the
%                               distribution given by the permutations
%                           2 - 1 (0), if the statistics is significant at
%                               the prescribed alpha level (or not)
%                           3 - 1 (0), if the statistics is significant
%                               after correction for mulitple comparisons
%                               (or not)
%                           4 - 1 (0), mean difference or tvalue of mean
%                               difference depending on cfg.permstatstype
%                           5 - 1 (0), if instantaneous mixing (volume
%                               conduction) exists (or not)
%                           6 - delay times u
%
%
% * OUTPUT PARAMETERS 
%   data
%       .TEpermvalues = matrix with size channelpair x 6 (for the exact
%                       specification see INPUT PARAMETERS, if an 
%                       alternative path was found the following changes 
%                       are made for the respectice channelpair:
%                           1 - p-value is set to 1
%                           2 - significance at the prescribed alpha level 
%                               is set to 0
%                           3 - significance after correction for 
%                               multiple comparison is set to 0
%                           4 - mean difference is set to NaN
%                           5 - is set to 2/3/4 according to the type of
%                               spurious interaction:   
%                               2 = cascade effect
%                               3 = cascade effect triangle
%                               4 = common drive link triangle
%                           6 - delay times are set to 0
%
%       .graphanalysis   = contains information on the constructed graph as
%                          n_vertices = number of vertices, 
%                          n_edges    = number of edges
%                          density    = graph density, defined as
%                                       dens = E/(V*(V-1))
%                                       V = n_vertices and E = n_edges
%                          threshold  = user provided threshold (see INPUT
%                                       PARAMETERS)
%
% PW - 07/09/2012
%
%
% This program is free software; you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation;
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY;
%
% Version 1.21 by Patricia Wollstadt
% Frankfurt 2012

% CHANGELOG
%
% 2012-04-27: PW added exception for graphs with only two nodes,
% computation is now aborted with a hint, that this doesn't make sense
%
% 2012-06-06: PW changed naming of vertices -> individual sources are
% enumerated, enumeration is used for the naming of vertices
%
% 2012-06-08: PW added documentaion
%
% 2012-07-04: PW added documentaion and references and changed command-line 
%             feedback (uses now ft_progress)
% 2012-07-04: NP minnor changes

% check if a threshold is provided
if isfield(cfg,'threshold')
	threshold = cfg.threshold;
else
	error('No threshold defined');
end;

% check if fieldtrip version is new enough
if exist('ft_progress','file')==0;
    error('You have no current fieldtrip version in your path, that provides the function ft_progress. Please update to a version fieldtrip-201201xx or higher.')
end;

% check if edge weights are integer numbers
if sum(mod(data.TEpermvalues(:,6),1))>0, error('Delay times have to be integer values!'), end;  

% remember labels of vertices
labels_vertices = unique(cat(1,data.sgncmb(:,1),data.sgncmb(:,2)));

% find edges and weights from input data
weights = data.TEpermvalues;
edges_temp = data.sgncmb(weights(:,2)==1,:);
weights = weights(weights(:,2)==1,6);

% replace labels by numbers (for edges only and for original sgncmb-list,
% the latter is needed for flagging of edges)
edges       = zeros(size(edges_temp));
sgncmb_enum = zeros(size(data.sgncmb));
for i=1:size(labels_vertices,1);
    edges(strcmp(edges_temp(:,1),labels_vertices(i)),1)=i;
    edges(strcmp(edges_temp(:,2),labels_vertices(i)),2)=i;
    
    sgncmb_enum(strcmp(data.sgncmb(:,1),labels_vertices(i)),1)=i;
    sgncmb_enum(strcmp(data.sgncmb(:,2),labels_vertices(i)),2)=i;
end;
clear edges_temp;

% remember no. nodes and vertices
n_vertices = length(labels_vertices);
n_edges = length(edges);

if n_vertices < 3;
    disp('Graphanalysis does not work for graphs with less than 3 nodes! Return...');
    return;
end;

% graph-related info goes into a seperate substructure of the data
disp(['no of edges: ' num2str(n_edges) ', no of vertices: ' num2str(n_vertices)]);
graphanalysis = [];
graphanalysis.edges = n_edges;
graphanalysis.vertices = n_vertices;
graphanalysis.density  = getDensity(n_edges,n_vertices);
graphanalysis.threshold = cfg.threshold;

% collects all alternative paths within the reconstruction interval 
% for all edges of the graph, contains: 
% [edge number] [starting node] [target node] [number of altpaths] [TEbacktracking output]
all_paths = {};
% generate output structure
data_paths = data;

%% find alternative paths for all neighbours
% init progress bar
ft_progress('init', 'text',    'Starting graph analysis...')

% count number of cases
no_nopath_TEdyn          = 0;
no_nopath_TEbacktracking = 0;

for i=1:n_edges;
    ft_progress(i/n_edges, 'Processing edge %d of %d ...', i, n_edges)
    %disp(['Computing edge ' num2str(i) ' of ' num2str(n_edges) '.................................']);
    
    % define current source, target and upper limit k
    k = weights(i) + threshold;      
    s = edges(i,1);
    t = edges(i,2);    
    
    if k<=0;
        error('Something is wrong with your threshold!');
    end;
    
    % remove current edge
    edges_temp = edges;
    edges_temp(i,:) = [];
    weights_temp = weights;
    weights_temp(i) = [];
    
    % rearrange labels, thus source=1 and target=end, the mapping, enumeration can be changed back after
    % backtracking  
    labels_vertices_temp = 1:n_vertices;
    labels_vertices_temp(labels_vertices_temp==s) = [];
    labels_vertices_temp(labels_vertices_temp==t) = [];
    labels_vertices_temp = [s; labels_vertices_temp'; t];
    % mask is needed for the new enumeration of the vertices
    mask    = ones(size(edges_temp));
    % 
    
    % enumerate nodes, masking is needed so that already changed nodes,
    % don't get changed again (happens if a node is changed to a higher
    % number n and if j=n, this node is overwritten again)
    for j=1:length(labels_vertices_temp);
        mask_temp = edges_temp==labels_vertices_temp(j)&mask;
        edges_temp(edges_temp==labels_vertices_temp(j)&mask) = j;
        mask(mask_temp) = 0;
    end;
    clear mask mask_temp;
        
    % create 'inverted' adjacency list
    adjacency_list = cell(n_vertices,1);
    for j=1:n_vertices;
        % inverted adjacency list (find all predecessors of a vertex)
        adjacency_list{j} = edges_temp(edges_temp(:,2)==j,1)'; 
        % find corresponding edge-weights
        adjacency_list{j} = cat(1,adjacency_list{j},weights_temp(edges_temp(:,2)==j)');
    end;   

	% if s and t are part of the same subgraph, look for alternative paths
	if(TEdfs(adjacency_list))
        %disp('  Looking for alternative paths...');
		solution = TEdyn(adjacency_list,k);
        
        % check if alternative paths were found
        alt_paths = 0;
        for j=k-2*threshold:k;
            if j<1; continue; end;
            if ~isempty(solution{j,end});
                alt_paths = 1;
                break;
            end;
        end;
        
        % if alternative paths exist, do backtracking
        if logical(alt_paths);
            %disp('  Alternative paths found by TEdyn.');
            path_tree = TEbacktracking(solution,k,threshold);
            if ~isempty(path_tree)                
                path_count = 0;
                
                % change enumeration back to original format (before
                % deletion of current edge) and count alternative paths
                for j=1:size(path_tree,1); 
                    path_count = size(path_tree{j},1) + path_count;
                    for jj=1:size(path_tree{j},1);
                        for jjj=1:length(path_tree{j}{jj});
                            path_tree{j}{jj}(jjj) = labels_vertices_temp(path_tree{j}{jj}(jjj));
                        end;
                    end;
                end;
                
                % collect alternative paths
                all_paths = [all_paths; {i s t path_count path_tree}];   

            else
                %disp('  No alternative paths found by TEbacktracking.');
                no_nopath_TEbacktracking = no_nopath_TEbacktracking+1;
            end;
        else
            %disp('  No alternative paths found by TEdyn.');
            no_nopath_TEdyn          = no_nopath_TEdyn+1;            
        end;
    %else
        %disp('  Source and target are not in the same subgraph');
 	end;

end;
ft_progress('close');

if ~isempty(all_paths)
    
    disp(['For ' num2str([n_edges-no_nopath_TEdyn-no_nopath_TEbacktracking]) ' of ' ...
        num2str(n_edges) ' edges alternative paths were found.']);
    disp(['     In ' num2str(no_nopath_TEdyn) ' of ' num2str(n_edges) ...
        ' cases TEdyn did not detect alternative paths.']);
    disp(['     In ' num2str(no_nopath_TEbacktracking) ' of ' num2str(n_edges) ...
        ' cases TEbacktracking did not return valid alternative paths.']); 
    disp(' ');
    
    % flag all edges to which alternative paths exist
    data_paths = TEflagedges(data,all_paths,edges,sgncmb_enum);
    % add alternative paths and graph info to datastructure
    data_paths.altpaths_thresh = cfg.threshold;
    data_paths.graphanalysis = graphanalysis;
    % update TEsteps
if ~isfield(data,'TEsteps')      %adding structure with changings; added modified by nicu
      data_paths.TEsteps = 'GA';
else data_paths.TEsteps = strcat(data.TEsteps,'_GA');
end
    %data_paths.TEsteps = [data_paths.TEsteps '_GA'];%old
else
    disp('No alternative paths were found!')
end;

ft_progress('close');