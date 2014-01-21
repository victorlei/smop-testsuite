function [localVar,thisLV]=insertLocalVar(localVar,name,varargin)
%inserts a localVar

%%%if (strcmp(name,'m'))
%%% '[[[[[[[[[[1',localVar,kb
%%%end
if iscell(name)
 %received a localVar type to insert
 if ~any(strcmp(name{1,1},{localVar{:,1}}))
  localVar(end+1,:)=name(1,:);
  thisLV=size(localVar,1); 
 else
  thisLV=find(strcmp(name{1,1},{localVar{:,1}}));
  localVar(thisLV,:)=name(1,:);
 end
else
 if ~any(strcmp(name,{localVar{:,1}}))
  localVar{end+1,1}=name;
  thisLV=size(localVar,1);
 else
  thisLV=find(strcmp(name,{localVar{:,1}}));
 end
end

attrib={'nDim','type','common','extents','data','save','protect','param','intent','alloc','external','input','optional','result','handle'};
%%%if (strcmp(name,'m'))
%%% '[[[[[[[[[[2',localVar,kb
%%%end
if length(varargin)>0
 for ii=1:2:length(varargin)
  whichAtt=find(strcmp(varargin{ii},attrib));
  if ~isempty(whichAtt)
   localVar{thisLV,whichAtt+1}=varargin{ii+1};
  end
 end
end

%'[[[[[[[[[[',localVar,kb
 