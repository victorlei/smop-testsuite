function [out]=functionresultsize(i,j);
% This will return a letter corresponding to the function result size and type
%  out => 'c' comp array,      out => 'x' comp vector,      out => 't' comp scalar,
%  out => 'r' real array,      out => 'w' real vector,      out => 's' real scalar,
%  out => 'i' int array,       out => 'y' int vector,       out => 'u' int scalar,
%  out => 'l' log array,       out => 'z' log vector,       out => 'v' log scalar,
declare_globals
r=char(10);s=size(funstr,1);
[howmany,subscripts,centercomma,parens]=hassubscript(i,j);
tempstr=makeMATLABcallstring(howmany,subscripts,centercomma,parens,i,j);
out=tempstr(1);
% [tempstr2,tempval]=determineshape(howmany,subscripts,centercomma,parens,i,j);
if ~isempty(tempstr)
 for k=1:length(tempstr)
  if ~isempty(find(strcmp(tempstr(k),barr)))
   [a(k),b(k)]=find(strcmp(tempstr(k),barr)); b2(k)=0;
  else
   [a(k),b(k)]=find(strcmp(tempstr(k),barr2));b2(k)=1;
  end
 end
end
switch funstrwords{i}{j}
 case 'abs',          if ~b2(k),out=barr{2,b};else out=barr2{2,b};end
 case 'acos'
 case 'aimag',        if ~b2(k),out=barr{1,b};else out=barr2{1,b};end
 case 'aint',         if ~b2(k),out=barr{2,b};else out=barr2{2,b};end
 case 'all'
 case 'anint',        if ~b2(k),out=barr{2,b};else out=barr2{2,b};end
 case 'any',          out='v';
 case 'asin'
 case 'atan'
 case 'atan2'
 case 'ceiling',      if ~b2(k),out=barr{3,b};else out=barr2{3,b};end
 case 'cmplx',        if ~b2(k),out=barr{1,b};else out=barr2{1,b};end
 case 'conjg',        out=tempstr;
 case 'cos'
 case 'cosh'
 case 'cotan'
 case 'count'
 case 'dble',         if ~b2(k),out=barr{2,b};else out=barr2{2,b};end
 case 'dot_product'
  if howmany==2
   if b2(1) | (~b2(1)&b(1)==2) %row or col or 1-D
    out=barr{a(1),3};
   else %2-D vecs, so result is row
    out=barr2{min(a),1};
   end
  elseif howmany==3
   [tempval]=determineshape(howmany,subscripts,centercomma,parens,i,j,3);
   if tempval==1
    out=barr2{min(a),1};
   elseif tempval==2
    out=barr2{min(a),2};
   end
  end
 case 'exp',          if ~b2(k),out=barr{min(2,a(1)),b};else out=barr2{min(2,a(1)),b};end
 case 'floor',        if ~b2(k),out=barr{3,b};else out=barr2{3,b};end
 case 'huge'
 case 'int',          if ~b2(k),out=barr{3,b};else out=barr2{3,b};end
 case 'isnan',        if ~b2(k),out=barr{4,b};else out=barr2{4,b};end
 case 'log'
 case 'log10'
 case 'logical',      if ~b2(k),out=barr{4,b};else out=barr2{4,b};end
 case 'matmul',       out=barr{min(a),1};
 case 'max'
  if howmany==1
   out=tempstr;    %This should never happen
  elseif howmany==2
   out=barr{min(a),min(b)};
  elseif howmany==3
   out=tempstr;    %This should never happen
  end
 case 'maxloc'
 case 'maxval'
  if howmany==1
   if any(strcmp(tempstr(1),typs{1}))     % Full matrix
    out=barr2{a(1),1};                      % Result is 2-D row
   else                                   % Scalar argument
    out=barr{a(1),3};                       % Result is scalar
   end
  elseif howmany==2
   [tempval]=determineshape(howmany,subscripts,centercomma,parens,i,j,2);
   if any(strcmp(tempstr(1),typs{11}))        % 2-D Array argument
    if any(strcmp(tempstr(1),typs{1}))     % Full matrix
     if tempval==1
      out=barr2{a(1),1};                      % Result is 2-D row
     elseif tempval==2
      out=barr2{a(1),2};                      % Result is 2-D col
     end
    elseif any(strcmp(tempstr(1),typs{9})) % Row, OK, fortran sums entire thing
     if tempval==1
      out=barr2{a(1),1};                      % Result is 2-D row (no change)
     elseif tempval==2
      out=barr{a(1),3};                       % Result is scalar
     end
    elseif any(strcmp(tempstr(1),typs{10}))% column, OK, fortran sums entire thing
     if tempval==1
      out=barr{a(1),3};                       % Result is scalar
     elseif tempval==2
      out=barr2{a(1),2};                      % Result is 2-D col (no change)
     end
    end
   elseif any(strcmp(typs{4},tempstr))    % 1-D Array argument
    out=barr{a(1),3};                       % Result is scalar
   else                                   % Scalar argument
    out=barr{a(1),3};                       % Result is scalar
   end
  end
 case 'min'
  if howmany==1
   out=tempstr;    %This should never happen
  elseif howmany==2
   out=barr{min(a),min(b)};
  elseif howmany==3
   out=tempstr;    %This should never happen
  end
 case 'minloc'
 case 'minval'
  if howmany==1
   if any(strcmp(tempstr(1),typs{1}))     % Full matrix
    out=barr2{a(1),1};                      % Result is 2-D row
   else                                   % Scalar argument
    out=barr{a(1),3};                       % Result is scalar
   end
  elseif howmany==2
   [tempval]=determineshape(howmany,subscripts,centercomma,parens,i,j,2);
   if any(strcmp(tempstr(1),typs{11}))        % 2-D Array argument
    if any(strcmp(tempstr(1),typs{1}))     % Full matrix
     if tempval==1
      out=barr2{a(1),1};                      % Result is 2-D row
     elseif tempval==2
      out=barr2{a(1),2};                      % Result is 2-D col
     end
    elseif any(strcmp(tempstr(1),typs{9})) % Row, OK, fortran sums entire thing
     if tempval==1
      out=barr2{a(1),1};                      % Result is 2-D row (no change)
     elseif tempval==2
      out=barr{a(1),3};                       % Result is scalar
     end
    elseif any(strcmp(tempstr(1),typs{10}))% column, OK, fortran sums entire thing
     if tempval==1
      out=barr{a(1),3};                       % Result is scalar
     elseif tempval==2
      out=barr2{a(1),2};                      % Result is 2-D col (no change)
     end
    end
   elseif any(strcmp(typs{4},tempstr))    % 1-D Array argument
    out=barr{a(1),3};                       % Result is scalar
   else                                   % Scalar argument
    out=barr{a(1),3};                       % Result is scalar
   end
  end
 case 'mod'
 case 'modulo'
 case 'nint',         if ~b2(k),out=barr{3,b};else out=barr2{3,b};end
 case 'not',          if ~b2(k),out=barr{4,b};else out=barr2{4,b};end
 case 'pack',         out=barr{a(1),2};
 case 'product'
  if howmany==1
   if any(strcmp(tempstr(1),typs{1}))     % Full matrix
    out=barr2{a(1),1};                      % Result is 2-D row
   else                                   % Scalar argument
    out=barr{a(1),3};                       % Result is scalar
   end
  elseif howmany==2
   [tempval]=determineshape(howmany,subscripts,centercomma,parens,i,j,2);
   if any(strcmp(tempstr(1),typs{11}))        % 2-D Array argument
    if any(strcmp(tempstr(1),typs{1}))     % Full matrix
     if tempval==1
      out=barr2{a(1),1};                      % Result is 2-D row
     elseif tempval==2
      out=barr2{a(1),2};                      % Result is 2-D col
     end
    elseif any(strcmp(tempstr(1),typs{9})) % Row, OK, fortran sums entire thing
     if tempval==1
      out=barr2{a(1),1};                      % Result is 2-D row (no change)
     elseif tempval==2
      out=barr{a(1),3};                       % Result is scalar
     end
    elseif any(strcmp(tempstr(1),typs{10}))% column, OK, fortran sums entire thing
     if tempval==1
      out=barr{a(1),3};                       % Result is scalar
     elseif tempval==2
      out=barr2{a(1),2};                      % Result is 2-D col (no change)
     end
    end
   elseif any(strcmp(typs{4},tempstr))    % 1-D Array argument
    out=barr{a(1),3};                       % Result is scalar
   else                                   % Scalar argument
    out=barr{a(1),3};                       % Result is scalar
   end
  end
 case 'real',         if ~b2(k),out=barr{2,b};else out=barr2{2,b};end
 case 'reshape'
%%%  funstr{i},subscripts
%%%  if howmany>2
%%%   out=barr{a(1),1};
%%%  elseif howmany==2
%%%   temp=find(funstrwords_b{i}>centercomma);
%%%   if ~isempty(temp), temp=temp(1); else temp=inf; end
%%%   temp1=find(funstrnumbers_b{i}>centercomma);
%%%   if ~isempty(temp1), temp1=temp1(1); else temp1=inf; end
%%%   if temp<temp1
%%%    [temp2,howmany2,subscripts2,centercomma2,parens2]=inwhichlast(i,funstrwords_b{i}(temp));
%%%   else
%%%    [temp2,howmany2,subscripts2,centercomma2,parens2]=inwhichlast(i,funstrnumbers_b{i}(temp1));
%%%   end
%%%   if temp2==1 %In bracket
%%%    if length(find(funstrwords_b{i}>centercomma & funstrwords_b{i}<parens2(2)))
%%%     out=barr{a(1),1}; %return 2-D array
%%%    else
%%%     out=barr{a(1),2}; %return 1-D array
%%%    end
%%%   else
%%%    if any(strcmp(tempstr(2),typs{4}))
%%%     out=barr{a(1),1}; %return 2-D array
%%%    else
%%%     out=barr{a(1),2}; %return 1-D array
%%%    end
%%%   end
%%%  end
  out=barr{a(1),1};
  %funstr{i},subscripts,out,kb
 case 'shape',        out=barr{3,2};
 case 'sign'
 case 'sin'
 case 'sinh'
 case 'size'
  if howmany==1
   out=barr{3,1};
  elseif howmany==2
   out=barr{3,3};
  end
 case 'spread'
  if any(strcmp(tempstr(1),typs{5})) %scalar, promote to 1-D
   out=barr{a(1),2};
  elseif any(strcmp(tempstr(1),typs{4})) %1-D, promote to row
   out=barr2{a(1),1};
  end
 case 'sqrt'
  %'frssssssssss',kb
  if want_cs
   if ~b2(k),out=barr{1,b};else out=barr2{1,b};end
  else
   if any(strcmp(tempstr(1),typs{3}))
    if ~b2(k)
     out=barr{1,b};
    else
     out=barr2{1,b};
    end
   else
    if ~b2(k)
     out=barr{2,b};
    else
     out=barr2{2,b};
    end
   end
  end
 case 'sum'
  %'sososos',funstr{i}
  if howmany==1
   if any(strcmp(tempstr(1),typs{1}))     % Full matrix
    out=barr2{a(1),1};                      % Result is 2-D row
   else                                   % Scalar argument
    out=barr{a(1),3};                       % Result is scalar
   end
  elseif howmany==2
   [tempval]=determineshape(howmany,subscripts,centercomma,parens,i,j,2);
   if any(strcmp(tempstr(1),typs{11}))        % 2-D Array argument
    if any(strcmp(tempstr(1),typs{1}))     % Full matrix
     if tempval==1
      out=barr2{a(1),1};                      % Result is 2-D row
     elseif tempval==2
      out=barr2{a(1),2};                      % Result is 2-D col
     end
    elseif any(strcmp(tempstr(1),typs{9})) % Row, OK, fortran sums entire thing
     if tempval==1
      out=barr2{a(1),1};                      % Result is 2-D row (no change)
     elseif tempval==2
      out=barr{a(1),3};                       % Result is scalar
     end
    elseif any(strcmp(tempstr(1),typs{10}))% column, OK, fortran sums entire thing
     if tempval==1
      out=barr{a(1),3};                       % Result is scalar
     elseif tempval==2
      out=barr2{a(1),2};                      % Result is 2-D col (no change)
     end
    end
   elseif any(strcmp(typs{4},tempstr))    % 1-D Array argument
    out=barr{a(1),3};                       % Result is scalar
   else                                   % Scalar argument
    out=barr{a(1),3};                       % Result is scalar
   end
  end
  %'summmmmmmm',out,kb
 case 'tan'
 case 'tanh'
 case 'tiny'
 case 'transpose',    %here next, make sure matmul works, fix scalar fixing in main to vecs etc.
  if any(strcmp(tempstr(1),typs{11}))        % 2-D Array argument
   if any(strcmp(tempstr(1),typs{1}))     % Full matrix
    out=tempstr;
   elseif any(strcmp(tempstr(1),typs{9})) % Row
    out=barr2{a(1),2};                      % Result is 2-D col
   elseif any(strcmp(tempstr(1),typs{10}))% column
    out=barr2{a(1),1};                      % Result is 2-D row (no change)
   end
  elseif any(strcmp(typs{4},tempstr))    % 1-D Array argument
   out=barr{a(1),2};                       % should never happen
  else                                   % Scalar argument
   out=barr{a(1),3};                       % should never happen
  end
 case 'ubound'
  if howmany==1
   out=barr{3,2};
  elseif howmany==2
   out=barr{3,3};
  end
 case 'unpack'
  %also put here some of the mxfunctions like mxa, mxs, etc.
 case 'mxs',          out=barr{a,3};
 case 'mxa',          out=barr{a,1};
  %   if any(strcmp('reshapef',funstrwords{i}))
  %    '222222222222222'
  %    funstr{i},funstrwords{i}{j},a,b,out,kb
  %   end
 case 'mxi',          out=barr{3,2};
end
