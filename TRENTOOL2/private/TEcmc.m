function [significance] = TEcmc(data, correctm, alpha, nrinstmix)

% TEcmc: This function implements the correction for multiple comparisons
% with false discovery rate or the more conservative Bonferroni correction.
%
% This function is called by the functions TEperm
%
% * REFERENCE INFORMATION
%   Genovese, C.R., Lazar, N.A., & Nichols, T. (2002). Thresholding of
%   statistical maps in functional neuroimaging using the false discovery
%   rate. Neuroimage, 15(4), 870-878.
%
%
% * OUTPUT PARAMETERS
%   significance = matrix including significances (1) after correction for
%   multiple comparison
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


% if number of multiple comparisons are smaller than 10 Bonferroni correction
% will be used automatically
if numel(data) <=10 && strcmp(correctm, 'FDR');
    cfg.correctm = 'Bonf';
    fprintf('\nTRENTOOL WARNING: Number of Data to small for FDR -> Bonf was used instead');
end



if strcmp(correctm, 'FDR')
    dim = size(data);
    nrdata = numel(data);
    data = reshape(data, 1, nrdata);
    [sorteddata, index] = sort(data);
    
    nrdatacor = nrdata-nrinstmix;
    sorteddatacor = sorteddata(1:end-nrinstmix);
    
    if nrdata < 1000
        thresh = ((1:nrdatacor)/nrdatacor)  * alpha / sum(1.0/1:nrdatacor);
        %thresh = ((1:nrdata-nrinstmix)/nrdata-nrinstmix)  * alpha / sum(1.0/1:nrdata-nrinstmix);
    else
        thresh = ((1:nrdatacor)/nrdatacor)  * alpha / (log(nrdatacor) + 0.57721566490153286060651209008240243104215933593992359880576723488486772677766467093694706329174674951463144724980708248096050401448654283622417399764492353625350033374293733773767394279259525824709491600873520394816567);
        %thresh = ((1:nrdata-nrinstmix)/nrdata-nrinstmix)  * alpha / (log(nrdata-nrinstmix) + 0.57721566490153286060651209008240243104215933593992359880576723488486772677766467093694706329174674951463144724980708248096050401448654283622417399764492353625350033374293733773767394279259525824709491600873520394816567);
    end
    
    %significance = (sorteddata<=thresh);
    significancecor = zeros(1,nrdatacor);
    significancecor(find(sorteddatacor <=thresh))=1;
    significance = zeros(1,nrdata);
    significance(1:end-nrinstmix) = significancecor;
    
    [dummy, unsorteddata] = sort(index);
    significance = significance(unsorteddata);
    
    
    significance = reshape(significance, dim);
    
elseif strcmp(correctm, 'BONF')
    
    nrofcomp = size(data,1) * size(data,2) -nrinstmix; %* size(TEresult.TEmat,3);
    
    significance = (data<=(alpha/nrofcomp));
    
end