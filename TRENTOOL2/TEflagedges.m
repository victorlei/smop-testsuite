function data = TEflagedges(data,altpaths,edges_orig,sgncmb_enum);
% TEFLAGEDGES: Flags spurious edges detected by TEdyn and TEbacktracking. 
% Flags used by TEflagedges:
%      2 = cascade effect
%      3 = cascade effect triangle
%      4 = common drive link triangle
%
% 1 denotes instantaneous mixing/volume conduction found by shift/surrogate 
% testing.
%
%
% * INPUT PARAMETERS
%      data        = original data, spurious edges are flagged inside this
%                    structure
%      altpaths    = alternative paths detected by TEdyn and TEbacktracking
%      edges_orig  = edges from the original data structure with the
%                    original enumeration (enumeration is changed within
%                    TEgraphanalysis, a requirement of the dynamic
%                    programming used in TEdyn)
%      sgncmb_enum = same as edges_orig
%
%
% * OUTPUT PARAMETERS
%      data       = original data structure with flagged spurious edges.
%                   Flagging is done by setting the following parameters in
%                   the respective columns of .TEpermvalues:
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
% This program is free software; you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation;
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY;
%
% Version 1.2 by Patricia Wollstadt
% Frankfurt 2012
%
% PW - 08/06/2012

% CHANGELOG
%
% 2012-06-13: PW corrected indexing of flagged edges
%
% 2012-07-04: PW added documentation and changes command-line feedback

N           = size(altpaths,1);

% make a list from all cascade effects (flagged with a 2)
% list contains: [edge index] [starting node] [target node] [flag type]
flag_list = [cell2mat(altpaths(:,1:3)) 2*ones(N,1)];

%% check for triangles
for i=1:N
    % find edge index in original data.sgncmb-structure
    new_edge_ind  = edges_orig(altpaths{i,1},:);
    flag_list(i,1) = find(sgncmb_enum(:,1)==new_edge_ind(1)&sgncmb_enum(:,2)==new_edge_ind(2));
    
    no_triangles = 0;
    
    for j=1:length(altpaths{i,5}{1})
        % check for triangles (alternative paths of length 3: s -> u -> t)
        if (length(altpaths{i,5}{1}{j}) == 3);
            %disp('Triangle!');
            no_triangles = no_triangles+1;
            % remember new spurious edge
            new_edge = [altpaths{i,5}{1}{j}(end-1) altpaths{i,5}{1}{j}(end)];
            % find edge index in original data.sgncmb-structure
            new_edge_ind = edges_orig(edges_orig(:,1)==new_edge(1)&edges_orig(:,2)==new_edge(2),:);
            new_edge_ind = find(sgncmb_enum(:,1)==new_edge_ind(1)&sgncmb_enum(:,2)==new_edge_ind(2));
            % add common drive link (4) to deletion list
            flag_list = cat(1, flag_list, [new_edge_ind new_edge 4]);
            % set edge type to triangle: cascade link (3)
            flag_list(i,4) = 3; 
        end;
    end;
end;

disp([num2str(no_triangles) ' triangle(s) were found by TEflagedges.']);

%% flag all spurious edges (cascade and common drive effects)

% check for duplicates in deletion list
duplicates = [];
for i=1:size(flag_list,1)
    ind = flag_list(i,1) == flag_list(i+1:end,1);
    if (sum(ind)>0)
        ind = find(ind)+i;
        duplicates = [duplicates; ind];
    end;
end;
flag_list(duplicates,:) = [];

% delete list by resetting values in TEpermvalues
data.n_spuriousedges = size(flag_list,1);
for i=1:size(flag_list,1)
    if isnan(data.TEpermvalues(flag_list(i,1),4));
        warning('This edge has already been flagged!');
    end;
    ind = flag_list(i,1);
    data.TEpermvalues(ind,:) = [1 0 0 NaN flag_list(i,4) 0];
end;
      
