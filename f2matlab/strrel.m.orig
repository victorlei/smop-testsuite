function out=strrel(in1,in2,relop)
% function out=strrel(in1,in2,relop)
%
%  Compares strings according to strcmp and issorted according to relop
%  in1 and in2 can be character arrays or cell arrays
%  relop can be <, <=, >, >=, ==, and ~=, or their string equivalents 
%    such as 'ne' or 'gt', etc.
%
%  Examples:
%  c = {'How','much','wood','would','a','woodchuck','chuck?'};
%  s = 'wood';
%  r = strrel(s,c,'<')
%  r = strrel(s,c,'gt')

% Author: Ben Barrowes, barrowes@alum.mit.edu, 10/2007

out=[];
% empty out if empty in (like []==3)
if isempty(in1) || isempty(in2), return, end

%doesn't handle numerical inputs
if isnumeric(in1) || isnumeric(in2)
 error('arguments to strrel must be char arrays or cell arrays of strings');
end

%pomote char arrays to cell arrays
if ischar(in1), in1={in1}; end
if ischar(in2), in2={in2}; end

%repmat smaller cell array to size of larger if needed
if ~all(size(in1)==size(in2))
 if prod(size(in1))==1
  in1=repmat(in1,size(in2));
 elseif prod(size(in2))==1
  in2=repmat(in2,size(in1));
 else
  error('cell array size mismatch in strrel');
 end
end

%should both be the same size cell arrays by now
out=false(size(in1));

for i=1:numel(in1)
 isEqual =strcmp(in1{i},in2{i});
 isSorted=issorted({in1{i},in2{i}});
 switch lower(relop)
  case {'<','lt'}
   out(i)=isSorted & ~isEqual;
  case {'<=','le'}
   out(i)=isSorted | isEqual;
  case {'==','eq'}
   out(i)=isEqual;
  case {'~=','ne'}
   out(i)=~isEqual;
  case {'>','gt'}
   out(i)=~isSorted & ~isEqual;
  case {'>=','gt'}
   out(i)=~isSorted | isEqual;
  otherwise
   error(['Relational operator, ',relop,', not recognized in strrel.'])
 end
end

