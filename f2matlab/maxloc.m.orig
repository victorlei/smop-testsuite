function out=maxloc(x,dim)
%function out=maxloc(x,dim)
%
%  replicates the functionality of maxloc in fortran
%
%  INPUTS:       x -> data variable    
%              dim -> dimension upon which to operate
%                     (same in Fortran and Matlab)
%             mask -> elements of x to consider (not yet implemented)
%
% OUTPUTS:     out -> set to second output of Matlab's max() if dim is specified
%                     otherwise, is the maximum location for the entire array
%
% author: Ben Barrowes 3/2008, barrowes@alum.mit.edu


if nargin<2, dim=0; end

xdim=length(size(x));
if dim==0
 [dumvar,tempOut]=max(x(:));
 out=cell(1,xdim);
 [out{:}]=ind2sub(size(x),tempOut);
 out=[out{:}];
else
 if ~isnumeric(dim), error('dim must be anumber in maxloc'); end
 if dim~=round(dim), error('dim must be an integer in maxloc'); end
 if dim>xdim || dim<1, error('dim must be 0<dim<=length(size(x))'); end
 [dumvar,out]=max(x,[],dim);
end

