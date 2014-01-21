function [outflag,nif,suborfun]=wordconverter(i,j,suborfun);
% This will change the Matlab function to its Fortran equivalent syntax.
% outflag(1) is 1 if its taken care of here. 0 if not.
% outflag(2) is 1 if we need to update funstr, NOTE if you added to fortranfunwords, this is set automagically.
% outflag(3) indicates this can be translated on a per/call basis (newer method)
declare_globals
nif=cell(2,1);fortranfunwordslength=length(fortranfunwords);
outflag=[0 0 0];r=char(10);s=size(funstr,1);
arr={'r';'c';'i';'l'}; noi={'r';'c';'s';'t';'v'}; com={'c';'t'}; log={'l';'v'};
if length(find(strcmp(filename_all,funstrwords{i}{j})))>0
 outflag(1)=1;
 temp=find(strcmp(filename_all,funstrwords{i}{j}));
 [howmany,subscripts,centercomma,parens]=hassubscript(i,j);
 [howmany2,subscripts2,centercomma2,parens2]=hasoutput(i,j);
 if howmany2==1
  suborfun(temp)=1;
  tempstr=makeMATLABcallstring(howmany,subscripts,centercomma,parens,i,j);
  tempfun=funstr{i};
  funstr{i}=funstr{i}(1:parens(1));
  repstr='dble';
  for ii=1:howmany
   if ii~=howmany
    if any(strcmp(tempstr(ii),typs{7}))
     goon=1;
     if length(subscripts{ii})>3
      if strncmp('dble',subscripts{ii},4)
       goon=0;
      end
     end
     if goon
      funstr{i}=[funstr{i},'dble(',subscripts{ii},'),'];
      outflag(2)=1;
     else
      funstr{i}=[funstr{i},subscripts{ii},','];
     end
    else
     funstr{i}=[funstr{i},subscripts{ii},','];
    end
   else
    if any(strcmp(tempstr(ii),typs{7}))
     goon=1;
     if length(subscripts{ii})>3
      if strncmp('dble',subscripts{ii},4)
       goon=0;
      end
     end
     if goon
      funstr{i}=[funstr{i},'dble(',subscripts{ii},')',tempfun(parens(2):length(tempfun))];
      outflag(2)=1;
     else
      funstr{i}=[funstr{i},subscripts{ii},tempfun(parens(2):length(tempfun))];
     end
    else
     funstr{i}=[funstr{i},subscripts{ii},tempfun(parens(2):length(tempfun))];
    end
   end
  end
  if outflag(2)==1
   fortranfunwords{length(fortranfunwords)+1}=repstr;
  end
 elseif howmany2>1
  suborfun(temp)=0;
  tempstr=makeMATLABcallstring(howmany2,subscripts2,centercomma2,parens2,i,j);
  tempstr2=['call ',filename_all{temp},'('];
  for ii=1:howmany2
   if any(strcmp(tempstr(ii),typs{7}))
    tempstr2=[tempstr2,'dble(',subscripts2{ii},'),'];
   else
    tempstr2=[tempstr2,subscripts2{ii},','];
   end
  end
  tempstr=makeMATLABcallstring(howmany,subscripts,centercomma,parens,i,j);
  for ii=1:howmany
   if ii~=howmany
    if any(strcmp(tempstr(ii),typs{7}))
     tempstr2=[tempstr2,'dble(',subscripts{ii},'),'];
    else
     tempstr2=[tempstr2,subscripts{ii},','];
    end
   else
    if any(strcmp(tempstr(ii),typs{7}))
     tempstr2=[tempstr2,'dble(',subscripts{ii},'));'];
    else
     tempstr2=[tempstr2,subscripts{ii},');'];
    end
   end
  end
  funstr{i}=tempstr2;
 end
else
switch funstrwords{i}{j}
 case 'ss2in'
  repstr='ss2inf';fortranfunwords{length(fortranfunwords)+1}=repstr;outflag(1)=1;
  funstr=replaceword(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr); 
  outflag(3)=1;
 case 'max'
  [howmany,subscripts,centercomma,parens]=hassubscript(i,j);
  tempstr=makeMATLABcallstring(howmany,subscripts,centercomma,parens,i,j);
  goon=1;  foo=1;  foo2=0;
  %funstr{i},foo,foo2,'what?',kb
  temp=cell(howmany,2);
  for k=1:howmany
   if any(strcmp(tempstr(k),typs{3}))
    temp{k,1}='abs(';temp{k,2}=')';
    outflag(2)=1;  %Because we need an update
   else
    temp{k,1}='';temp{k,2}='';
   end
  end
  if howmany==1                         % One argument
   if any(strcmp(typs{11},tempstr))        % 2-D Array argument
    ;% Decide what to do based on the shape
    if foo
     if any(strcmp(tempstr(1),typs{1}))     % Full matrix, just add ,1 to the min statement
      funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),'spread(maxval(',temp{1,1},funstr{i}(parens(1)+1:parens(2)-1),temp{1,2},',1),1,1)',funstr{i}(parens(2)+1:length(funstr{i}))];
     elseif any(strcmp(tempstr(1),typs{9})) % Row, OK, fortran mins entire thing
      funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),'maxval(',temp{1,1},funstr{i}(parens(1)+1:parens(2)-1),temp{1,2},')',funstr{i}(parens(2)+1:length(funstr{i}))];
     elseif any(strcmp(tempstr(1),typs{10}))% column, OK, fortran mins entire thing
      funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),'maxval(',temp{1,1},funstr{i}(parens(1)+1:parens(2)-1),temp{1,2},')',funstr{i}(parens(2)+1:length(funstr{i}))];
     end
     goon=0;   outflag(2)=1;
    end
   elseif any(strcmp(typs{4},tempstr))    % 1-D Array argument
    funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),'maxval(',temp{1,1},subscripts{1},temp{1,2},')',funstr{i}(parens(2)+1:length(funstr{i}))];
    goon=0;    outflag(2)=1;
   else                                   % Scalar argument
    funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),funstr{i}((funstrwords_e{i}(j)+1):length(funstr{i}))];
    goon=0;    outflag(2)=1;
   end
  elseif howmany==2                     % Two arguments
   for k=1:length(tempstr)
    if ~isempty(find(strcmp(tempstr(k),barr)))
     [a(k),b(k)]=find(strcmp(tempstr(k),barr));
    else
     [a(k),b(k)]=find(strcmp(tempstr(k),barr2));
     b(k)=b(k)+3;
    end
   end
   if b(1)~=b(2) & ~(b(1)==3 | b(2)==3)
    disp(['Problem with min. Array sizes don''t match.'])
    disp(['First subscript ',subscripts{1}])
    disp(['Second subscript ',subscripts{2}])
    funstr{i},keyboard
   else
    goon=0;%Do nothing except add abs if needed. Use min as is in fortran
    funstr{i}=[funstr{i}(1:parens(1)),temp{1,1},funstr{i}(parens(1)+1:centercomma-1),temp{1,2},',',temp{2,1},funstr{i}(centercomma+1:parens(2)),temp{2,2},funstr{i}(parens(2)+1:length(funstr{i}))];
   end
  elseif howmany==3                     % three arguments
   if j>1
    if strcmp(funstrwords{i}{j-1},'spread')
     foo=0; foo2=0;
    end
   end
   [tempval]=determineshape(howmany,subscripts,centercomma,parens,i,j,3);
   if ~((tempval==1)|(tempval==2)) ,     foo=0; foo2=1;     end
   if foo
    temp2=cell(1,2);temp2{1}='';temp2{2}='';
    if ~any(strcmp(tempstr(3),typs{5})),temp2{1}='mxs(';temp2{2}=')';end
    if tempval==1
     temp5{1}='';                temp5{2}='';
    else
     temp5{1}='transpose(';      temp5{2}=')';
    end
    if any(strcmp(tempstr(1),typs{1}))     % Full matrix, just add ,1 to the sum statement
     funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),temp5{1},'spread(maxval(',temp{1,1},subscripts{1},temp{1,2},',',temp2{1},temp{3,1},subscripts{3},temp{3,2},temp2{2},'),1,1)',temp5{2},funstr{i}(parens(2)+1:length(funstr{i}))];
    elseif any(strcmp(tempstr(1),typs{9})) %Row
     if tempval==1 %remove min and 2nd sub
      funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),funstr{i}(funstrwords_e{i}(j)+1:centercomma(1)-1),funstr{i}(parens(2):length(funstr{i}))];
     elseif tempval==2
      funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),'maxval(',temp{1,1},subscripts{1},temp{1,2},funstr{i}(parens(2):length(funstr{i}))];
     end
    elseif any(strcmp(tempstr(1),typs{10}))% column
     if tempval==1
      funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),'maxval(',temp{1,1},subscripts{1},temp{1,2},funstr{i}(parens(2):length(funstr{i}))];
     elseif tempval==2
      funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),funstr{i}(funstrwords_e{i}(j)+1:centercomma(1)-1),funstr{i}(parens(2):length(funstr{i}))];
     end
    elseif any(strcmp(tempstr(1),typs{4}))% 1-D
     funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),'maxval(',temp{1,1},subscripts{1},temp{1,2},')',funstr{i}(parens(2)+1:length(funstr{i}))];
    elseif any(strcmp(tempstr(1),typs{5}))% scalar
     funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),'(',temp{1,1},subscripts{1},temp{1,2},')',funstr{i}(parens(2)+1:length(funstr{i}))];
    end
    goon=0;      outflag(2)=1;
   else
    if foo2  ;    goon=1;     else      goon=0;     end
   end
  end
  if goon
   repstr='maxf';fortranfunwords{length(fortranfunwords)+1}=repstr;outflag(1)=1;
   funstr=replaceword(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);
   outflag(3)=1;
  else
   outflag(1)=1;
  end
  %funstr{i},goon,outflag,kb
 case 'min'
  [howmany,subscripts,centercomma,parens]=hassubscript(i,j);
  [howmany2,subscripts2,centercomma2,parens2]=hasoutput(i,j);
  tempstr=makeMATLABcallstring(howmany,subscripts,centercomma,parens,i,j);
  goon=1;  foo=1;  foo2=0;
  %funstr{i},foo,foo2,'what?',kb
  temp=cell(howmany,2);
  for k=1:howmany
   if any(strcmp(tempstr(k),typs{3}))
    temp{k,1}='abs(';temp{k,2}=')';
    outflag(2)=1;  %Because we need an update
    %foo=0; foo2=1; Uncomment if you want to make a minf for the complexes
   else
    temp{k,1}='';temp{k,2}='';
   end
  end
  if foo
   if howmany==1                         % One argument
    if any(strcmp(typs{11},tempstr))        % 2-D Array argument
     ;% Decide what to do based on the shape
     if foo
      if any(strcmp(tempstr(1),typs{1}))     % Full matrix, just add ,1 to the min statement
       funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),'spread(minval(',temp{1,1},funstr{i}(parens(1)+1:parens(2)-1),temp{1,2},',1),1,1)',funstr{i}(parens(2)+1:length(funstr{i}))];
      elseif any(strcmp(tempstr(1),typs{9})) % Row, OK, fortran mins entire thing
       funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),'minval(',temp{1,1},funstr{i}(parens(1)+1:parens(2)-1),temp{1,2},')',funstr{i}(parens(2)+1:length(funstr{i}))];
      elseif any(strcmp(tempstr(1),typs{10}))% column, OK, fortran mins entire thing
       funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),'minval(',temp{1,1},funstr{i}(parens(1)+1:parens(2)-1),temp{1,2},')',funstr{i}(parens(2)+1:length(funstr{i}))];
      end
      goon=0;   outflag(2)=1;
     end
    elseif any(strcmp(typs{4},tempstr))    % 1-D Array argument
     funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),'minval(',temp{1,1},subscripts{1},temp{1,2},')',funstr{i}(parens(2)+1:length(funstr{i}))];
     goon=0;    outflag(2)=1;
    else                                   % Scalar argument
     funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),funstr{i}((funstrwords_e{i}(j)+1):length(funstr{i}))];
     goon=0;    outflag(2)=1;
    end
   elseif howmany==2                     % Two arguments
    for k=1:length(tempstr)
     if ~isempty(find(strcmp(tempstr(k),barr)))
      [a(k),b(k)]=find(strcmp(tempstr(k),barr));
     else
      [a(k),b(k)]=find(strcmp(tempstr(k),barr2));
      b(k)=b(k)+3;
     end
    end
    if b(1)~=b(2) & ~(b(1)==3 | b(2)==3)
     disp(['Problem with min. Array sizes don''t match.'])
     disp(['First subscript ',subscripts{1}])
     disp(['Second subscript ',subscripts{2}])
     funstr{i},keyboard
    else
     goon=0;    %outflag(2)=1;%Do nothing except add abs if needed. Use min as is in fortran
     funstr{i}=[funstr{i}(1:parens(1)),temp{1,1},funstr{i}(parens(1)+1:centercomma-1),temp{1,2},',',temp{2,1},funstr{i}(centercomma+1:parens(2)),temp{2,2},funstr{i}(parens(2)+1:length(funstr{i}))];
    end
   elseif howmany==3                     % three arguments
    if j>1
     if strcmp(funstrwords{i}{j-1},'spread')
      foo=0; foo2=0;
     end
    end
    [tempval]=determineshape(howmany,subscripts,centercomma,parens,i,j,3);
    if ~((tempval==1)|(tempval==2))  ;    foo=0; foo2=1;     end
    if foo
     temp2=cell(1,2);temp2{1}='';temp2{2}='';
     if ~any(strcmp(tempstr(3),typs{5})),temp2{1}='mxs(';temp2{2}=')';end
     if tempval==1
      temp5{1}='';                temp5{2}='';
     else
      temp5{1}='transpose(';      temp5{2}=')';
     end
     if any(strcmp(tempstr(1),typs{1}))     % Full matrix, just add ,1 to the sum statement
      funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),temp5{1},'spread(minval(',temp{1,1},subscripts{1},temp{1,2},',',temp2{1},temp{3,1},subscripts{3},temp{3,2},temp2{2},'),1,1)',temp5{2},funstr{i}(parens(2)+1:length(funstr{i}))];
     elseif any(strcmp(tempstr(1),typs{9})) %Row
      if tempval==1 %remove min and 2nd sub
       funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),funstr{i}(funstrwords_e{i}(j)+1:centercomma(1)-1),funstr{i}(parens(2):length(funstr{i}))];
      elseif tempval==2
       funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),'minval(',temp{1,1},subscripts{1},temp{1,2},funstr{i}(parens(2):length(funstr{i}))];
      end
     elseif any(strcmp(tempstr(1),typs{10}))% column
      if tempval==1
       funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),'minval(',temp{1,1},subscripts{1},temp{1,2},funstr{i}(parens(2):length(funstr{i}))];
      elseif tempval==2
       funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),funstr{i}(funstrwords_e{i}(j)+1:centercomma(1)-1),funstr{i}(parens(2):length(funstr{i}))];
      end
     elseif any(strcmp(tempstr(1),typs{4}))% 1-D
      funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),'minval(',temp{1,1},subscripts{1},temp{1,2},')',funstr{i}(parens(2)+1:length(funstr{i}))];
     elseif any(strcmp(tempstr(1),typs{5}))% scalar
      funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),'(',temp{1,1},subscripts{1},temp{1,2},')',funstr{i}(parens(2)+1:length(funstr{i}))];
     end
     goon=0;      outflag(2)=1;
    else
     if foo2  ;    goon=1;     else      goon=0;     end
    end
   end
  else
   if howmany==3 %This should eliminate 2nd sub, and come back here for a minf
    funstr{i}=[funstr{i}(1:centercomma(1)),subscripts{3},funstr{i}(parens(2):end)];
    goon=0;      outflag(2)=1;
   else
    if foo2  ;    goon=1;     else      goon=0;     end
   end
  end
  if goon
   %funstr{i},'minnnnnnnnnn',kb
   repstr='minf';fortranfunwords{length(fortranfunwords)+1}=repstr;outflag(1)=1;
   funstr=replaceword(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);
   outflag(3)=1;
  else
   outflag(1)=1;
  end
  %funstr{i},goon,outflag,kb
 case 'mean'
  [howmany,subscripts,centercomma,parens]=hassubscript(i,j);
  tempstr=makeMATLABcallstring(howmany,subscripts,centercomma,parens,i,j);
  goon=1;  foo=1;  foo2=0;
  %funstr{i},foo,foo2,'what?',kb
  if howmany==1                         % One argument
   if any(strcmp(typs{11},tempstr))        % 2-D Array argument
    ;% Decide what to do based on the shape
    if foo
     if any(strcmp(tempstr(1),typs{1}))     % Full matrix, just add ,1 to the min statement
      funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),'(spread(sum(',subscripts{1},',1),1,1)/ubound(',subscripts{1},',1))',funstr{i}(parens(2)+1:length(funstr{i}))];
     elseif any(strcmp(tempstr(1),typs{9})) % Row, OK, fortran mins entire thing
      funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),'(sum(',subscripts{1},')/ubound(',subscripts{1},',2))',funstr{i}(parens(2)+1:length(funstr{i}))];
     elseif any(strcmp(tempstr(1),typs{10}))% column, OK, fortran mins entire thing
      funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),'(sum(',subscripts{1},')/ubound(',subscripts{1},',1))',funstr{i}(parens(2)+1:length(funstr{i}))];
     end
     goon=0;   outflag(2)=1;
    end
   elseif any(strcmp(typs{4},tempstr))    % 1-D Array argument
    funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),'(sum(',subscripts{1},')/ubound(',subscripts{1},'))',funstr{i}(parens(2)+1:length(funstr{i}))];
    goon=0;    outflag(2)=1;
   else                                   % Scalar argument
    funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),funstr{i}((funstrwords_e{i}(j)+1):length(funstr{i}))];
    goon=0;    outflag(2)=1;
   end
  elseif howmany==2                     % Two arguments
   if j>1
    if strcmp(funstrwords{i}{j-1},'spread')
     foo=0; foo2=0;
    end
   end
   [tempval]=determineshape(howmany,subscripts,centercomma,parens,i,j,2);
   if ~((tempval==1)|(tempval==2)) ;     foo=0; foo2=1;     end
   if foo
    temp2=cell(1,2);temp2{1}='';temp2{2}='';
    if ~any(strcmp(tempstr(2),typs{5})),temp2{1}='mxs(';temp2{2}=')';end
    if tempval==1
     temp5{1}='';                temp5{2}='';
    else
     temp5{1}='transpose(';      temp5{2}=')';
    end
    if any(strcmp(tempstr(1),typs{1}))     % Full matrix
     funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),temp5{1},'(spread(sum(',subscripts{1},',',temp2{1},subscripts{2},temp2{2},'),1,1)/ubound(',subscripts{1},',',temp2{1},subscripts{2},temp2{2},'))',temp5{2},funstr{i}(parens(2)+1:length(funstr{i}))];
    elseif any(strcmp(tempstr(1),typs{9})) %Row
     if tempval==1
      funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),funstr{i}(funstrwords_e{i}(j)+1:centercomma(1)-1),funstr{i}(parens(2):length(funstr{i}))];
     elseif tempval==2
      funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),'(sum(',subscripts{1},')/ubound(',subscripts{1},',2))',funstr{i}(parens(2)+1:length(funstr{i}))];
     end
    elseif any(strcmp(tempstr(1),typs{10}))% column
     if tempval==1
      funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),'(sum(',subscripts{1},')/ubound(',subscripts{1},',1))',funstr{i}(parens(2)+1:length(funstr{i}))];
     elseif tempval==2
      funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),funstr{i}(funstrwords_e{i}(j)+1:centercomma(1)-1),funstr{i}(parens(2):length(funstr{i}))];
     end
    elseif any(strcmp(tempstr(1),typs{4}))% 1-D
     funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),'(sum(',subscripts{1},')/ubound(',subscripts{1},',1))',funstr{i}(parens(2)+1:length(funstr{i}))];
    elseif any(strcmp(tempstr(1),typs{5}))% scalar
     funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),'(',subscripts{1},')',funstr{i}(parens(2)+1:length(funstr{i}))];
    end
    goon=0;      outflag(2)=1;
   else
    if foo2  ;    goon=1;     else      goon=0;     end
   end
  end
  if goon
   repstr='meanf';fortranfunwords{length(fortranfunwords)+1}=repstr;outflag(1)=1;
   funstr=replaceword(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);
   outflag(3)=1;
  else
   outflag(1)=1;
  end
 case 'median'
  repstr='medianf';fortranfunwords{length(fortranfunwords)+1}=repstr;outflag(1)=1;
  funstr=replaceword(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr); 
  outflag(3)=1;
 case 'sort'
 case 'sortrows'
 case 'std'
 case 'subspace'
 case 'break'
  repstr='exit';fortranfunwords{length(fortranfunwords)+1}=repstr;outflag(1)=1;
  funstr=replaceword(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);
 case 'sum'
  [howmany,subscripts,centercomma,parens]=hassubscript(i,j);
  tempstr=makeMATLABcallstring(howmany,subscripts,centercomma,parens,i,j);
  goon=1;  foo=1;  foo2=1;
  %funstr{i},'what?',kb
  if howmany==1                         % One argument
   if any(strcmp(typs{11},tempstr))        % 2-D Array argument
    ;% Decide what to do based on the shape
    if j>1
     if strcmp(funstrwords{i}{j-1},'spread')
      foo=0;
     end
    end
    if foo
     if any(strcmp(tempstr(1),typs{1}))     % Full matrix, just add ,1 to the sum statement
      funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),'spread(',funstr{i}((funstrwords_b{i}(j)):parens(2)-1),',1),1,1)',funstr{i}(parens(2)+1:length(funstr{i}))];outflag(2)=1;
     elseif any(strcmp(tempstr(1),typs{9})) % Row, OK, fortran sums entire thing
     elseif any(strcmp(tempstr(1),typs{10}))% column, OK, fortran sums entire thing
     end
     goon=0;
    end
   elseif any(strcmp(typs{4},tempstr))    % 1-D Array argument
    %We're fine. Do nothing.
    goon=0;
   else                                   % Scalar argument
    funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),funstr{i}((funstrwords_e{i}(j)+1):length(funstr{i}))];
    goon=0;    outflag(2)=1;
   end
  elseif howmany==2                     % Two arguments
   [tempval]=determineshape(howmany,subscripts,centercomma,parens,i,j,2);
   if any(strcmp(typs{11},tempstr(1)))     % 2-D Array argument
    ;%Then simply make the output an array
    if j>1
     if strcmp(funstrwords{i}{j-1},'spread')
      foo=0; foo2=0;
     end
    end
    [tempval]=determineshape(howmany,subscripts,centercomma,parens,i,j,2);
    if ~((tempval==1)|(tempval==2))
     foo=0; foo2=1;
    end
    if foo
     if tempval==1
      temp5{1}='';                temp5{2}='';
     else
      temp5{1}='transpose(';      temp5{2}=')';
     end
     if any(strcmp(tempstr(1),typs{1}))     % Full matrix, just add ,1 to the sum statement
      funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),temp5{1},'spread(',funstr{i}((funstrwords_b{i}(j)):parens(2)-1),'),1,1)',temp5{2},funstr{i}(parens(2)+1:length(funstr{i}))];
     elseif any(strcmp(tempstr(1),typs{9})) %Row
      if tempval==1 %remove sum and 2nd sub
       funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),funstr{i}(funstrwords_e{i}(j)+1:centercomma(1)-1),funstr{i}(parens(2):length(funstr{i}))];
      elseif tempval==2
       funstr{i}=[funstr{i}(1:centercomma(1)-1),funstr{i}(parens(2):length(funstr{i}))];
      end
     elseif any(strcmp(tempstr(1),typs{10}))% column
      if tempval==1
       funstr{i}=[funstr{i}(1:centercomma(1)-1),funstr{i}(parens(2):length(funstr{i}))];
      elseif tempval==2
       funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),funstr{i}(funstrwords_e{i}(j)+1:centercomma(1)-1),funstr{i}(parens(2):length(funstr{i}))];
      end
     end
     goon=0;      outflag(2)=1;
    else
     if foo2 ;     goon=1;     else      goon=0;     end
    end
   elseif any(strcmp(typs{4},tempstr(1))) % 1-D Array argument
    %Chill for now, just remove 2nd sub
    goon=0;outflag(2)=1;
    funstr{i}=[funstr{i}(1:centercomma(1)-1),funstr{i}(parens(2):length(funstr{i}))];
   else                                   % Scalar argument
    funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),funstr{i}(funstrwords_e{i}(j)+1:centercomma(1)-1),funstr{i}(parens(2):length(funstr{i}))];
    goon=0;    outflag(2)=1;
   end
  end
  if goon
   repstr='sumf';fortranfunwords{length(fortranfunwords)+1}=repstr;outflag(1)=1;
   funstr=replaceword(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);
   outflag(3)=1;
  else
   outflag(1)=1;
  end
  %funstr{i},'what2?',kb
 case 'trapz'
 case 'var'
 case 'char'
 case 'double'
  repstr='dble';fortranfunwords{length(fortranfunwords)+1}=repstr;outflag(1)=1;
  funstr=replaceword(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);
 case 'cat'
 case 'single'
 case 'abs'
  outflag(1)=1;
 case 'acos'
  repstr='acosf';fortranfunwords{length(fortranfunwords)+1}=repstr;outflag(1)=1;
  [howmany,subscripts,centercomma,parens]=hassubscript(i,j);
  tempstr=makeMATLABcallstring(howmany,subscripts,centercomma,parens,i,j);  
  if any(strcmp(tempstr(1),typs{4}))
   funstr{i}=[funstr{i}(1:parens(1)),'spread(',subscripts{1},',1,1)',funstr{i}(parens(2):end)];
  else
   funstr=replaceword(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);
  end
  outflag(3)=1;
 case 'acosh'
  repstr='acoshf';fortranfunwords{length(fortranfunwords)+1}=repstr;outflag(1)=1;
  [howmany,subscripts,centercomma,parens]=hassubscript(i,j);
  tempstr=makeMATLABcallstring(howmany,subscripts,centercomma,parens,i,j);  
  if any(strcmp(tempstr(1),typs{4}))
   funstr{i}=[funstr{i}(1:parens(1)),'spread(',subscripts{1},',1,1)',funstr{i}(parens(2):end)];
  else
   funstr=replaceword(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);
  end
  outflag(3)=1;
 case 'acot'
  repstr='acotf';fortranfunwords{length(fortranfunwords)+1}=repstr;outflag(1)=1;
  [howmany,subscripts,centercomma,parens]=hassubscript(i,j);
  tempstr=makeMATLABcallstring(howmany,subscripts,centercomma,parens,i,j);  
  if any(strcmp(tempstr(1),typs{4}))
   funstr{i}=[funstr{i}(1:parens(1)),'spread(',subscripts{1},',1,1)',funstr{i}(parens(2):end)];
  else
   funstr=replaceword(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);
  end
  outflag(3)=1;
 case 'acoth'
  repstr='acothf';fortranfunwords{length(fortranfunwords)+1}=repstr;outflag(1)=1;
  [howmany,subscripts,centercomma,parens]=hassubscript(i,j);
  tempstr=makeMATLABcallstring(howmany,subscripts,centercomma,parens,i,j);  
  if any(strcmp(tempstr(1),typs{4}))
   funstr{i}=[funstr{i}(1:parens(1)),'spread(',subscripts{1},',1,1)',funstr{i}(parens(2):end)];
  else
   funstr=replaceword(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);
  end
  outflag(3)=1;
 case 'acsc'
  repstr='acscf';fortranfunwords{length(fortranfunwords)+1}=repstr;outflag(1)=1;
  [howmany,subscripts,centercomma,parens]=hassubscript(i,j);
  tempstr=makeMATLABcallstring(howmany,subscripts,centercomma,parens,i,j);  
  if any(strcmp(tempstr(1),typs{4}))
   funstr{i}=[funstr{i}(1:parens(1)),'spread(',subscripts{1},',1,1)',funstr{i}(parens(2):end)];
  else
   funstr=replaceword(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);
  end
  outflag(3)=1;
 case 'acsch'
  repstr='acschf';fortranfunwords{length(fortranfunwords)+1}=repstr;outflag(1)=1;
  [howmany,subscripts,centercomma,parens]=hassubscript(i,j);
  tempstr=makeMATLABcallstring(howmany,subscripts,centercomma,parens,i,j);  
  if any(strcmp(tempstr(1),typs{4}))
   funstr{i}=[funstr{i}(1:parens(1)),'spread(',subscripts{1},',1,1)',funstr{i}(parens(2):end)];
  else
   funstr=replaceword(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);
  end
  outflag(3)=1;
 case 'angle'
 case 'asec'
  repstr='asecf';fortranfunwords{length(fortranfunwords)+1}=repstr;outflag(1)=1;
  [howmany,subscripts,centercomma,parens]=hassubscript(i,j);
  tempstr=makeMATLABcallstring(howmany,subscripts,centercomma,parens,i,j);  
  if any(strcmp(tempstr(1),typs{4}))
   funstr{i}=[funstr{i}(1:parens(1)),'spread(',subscripts{1},',1,1)',funstr{i}(parens(2):end)];
  else
   funstr=replaceword(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);
  end
  outflag(3)=1;
 case 'asech'
  repstr='asechf';fortranfunwords{length(fortranfunwords)+1}=repstr;outflag(1)=1;
  [howmany,subscripts,centercomma,parens]=hassubscript(i,j);
  tempstr=makeMATLABcallstring(howmany,subscripts,centercomma,parens,i,j);  
  if any(strcmp(tempstr(1),typs{4}))
   funstr{i}=[funstr{i}(1:parens(1)),'spread(',subscripts{1},',1,1)',funstr{i}(parens(2):end)];
  else
   funstr=replaceword(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);
  end
  outflag(3)=1;
 case 'asin'
  repstr='asinf';fortranfunwords{length(fortranfunwords)+1}=repstr;outflag(1)=1;
  [howmany,subscripts,centercomma,parens]=hassubscript(i,j);
  tempstr=makeMATLABcallstring(howmany,subscripts,centercomma,parens,i,j);  
  if any(strcmp(tempstr(1),typs{4}))
   funstr{i}=[funstr{i}(1:parens(1)),'spread(',subscripts{1},',1,1)',funstr{i}(parens(2):end)];
  else
   funstr=replaceword(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);
  end
  outflag(3)=1;
 case 'asinh'
  repstr='asinhf';fortranfunwords{length(fortranfunwords)+1}=repstr;outflag(1)=1;
  [howmany,subscripts,centercomma,parens]=hassubscript(i,j);
  tempstr=makeMATLABcallstring(howmany,subscripts,centercomma,parens,i,j);  
  if any(strcmp(tempstr(1),typs{4}))
   funstr{i}=[funstr{i}(1:parens(1)),'spread(',subscripts{1},',1,1)',funstr{i}(parens(2):end)];
  else
   funstr=replaceword(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);
  end
  outflag(3)=1;
 case 'atan'
  repstr='atanf';fortranfunwords{length(fortranfunwords)+1}=repstr;outflag(1)=1;
  [howmany,subscripts,centercomma,parens]=hassubscript(i,j);
  tempstr=makeMATLABcallstring(howmany,subscripts,centercomma,parens,i,j);  
  if any(strcmp(tempstr(1),typs{4}))
   funstr{i}=[funstr{i}(1:parens(1)),'spread(',subscripts{1},',1,1)',funstr{i}(parens(2):end)];
  else
   funstr=replaceword(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);
  end
  outflag(3)=1;
 case 'atan2'
  outflag(1)=1;
 case 'atanh'
  repstr='atanhf';fortranfunwords{length(fortranfunwords)+1}=repstr;outflag(1)=1;
  [howmany,subscripts,centercomma,parens]=hassubscript(i,j);
  tempstr=makeMATLABcallstring(howmany,subscripts,centercomma,parens,i,j);  
  if any(strcmp(tempstr(1),typs{4}))
   funstr{i}=[funstr{i}(1:parens(1)),'spread(',subscripts{1},',1,1)',funstr{i}(parens(2):end)];
  else
   funstr=replaceword(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);
  end
  outflag(3)=1;
 case 'ceil'
  repstr='ceiling';fortranfunwords{length(fortranfunwords)+1}=repstr;outflag(1)=1;
  dummy=find(funstr{i}=='(');
  dummy=dummy(dummy>funstrwords_e{i}(j));
  dummy=dummy(1);
  rightparen=findrights(dummy,funstr{i});
  funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),repstr,funstr{i}((funstrwords_e{i}(j)+1):rightparen),funstr{i}((rightparen+1):length(funstr{i}))];
 case 'complex'
  repstr='cmplx';fortranfunwords{length(fortranfunwords)+1}=repstr;outflag(1)=1;
  funstr=replaceword(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);
 case 'conj'
  repstr='conjg';fortranfunwords{length(fortranfunwords)+1}=repstr;outflag(1)=1;
  funstr=replaceword(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);
 case 'cos'
  outflag(1)=1;
 case 'cosh'
  outflag(1)=1;
 case 'cot'
  outflag(1)=1;
 case 'coth'
  repstr='cothf';fortranfunwords{length(fortranfunwords)+1}=repstr;outflag(1)=1;
  [howmany,subscripts,centercomma,parens]=hassubscript(i,j);
  tempstr=makeMATLABcallstring(howmany,subscripts,centercomma,parens,i,j);  
  if any(strcmp(tempstr(1),typs{4}))
   funstr{i}=[funstr{i}(1:parens(1)),'spread(',subscripts{1},',1,1)',funstr{i}(parens(2):end)];
  else
   funstr=replaceword(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);
  end
  outflag(3)=1;
 case 'cplxpair'
 case 'csc'
  outflag(1)=1;
 case 'csch'
  repstr='cschf';fortranfunwords{length(fortranfunwords)+1}=repstr;outflag(1)=1;
  [howmany,subscripts,centercomma,parens]=hassubscript(i,j);
  tempstr=makeMATLABcallstring(howmany,subscripts,centercomma,parens,i,j);  
  if any(strcmp(tempstr(1),typs{4}))
   funstr{i}=[funstr{i}(1:parens(1)),'spread(',subscripts{1},',1,1)',funstr{i}(parens(2):end)];
  else
   funstr=replaceword(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);
  end
  outflag(3)=1;
 case 'exp'
  outflag(1)=1;
 case 'fix'
  repstr='int';fortranfunwords{length(fortranfunwords)+1}=repstr;outflag(1)=1;
  funstr=replaceword(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);
  updatefunstr(i);
  [howmany,subscripts,centercomma,parens]=hassubscript(i,j);
  tempstr=makeMATLABcallstring(howmany,subscripts,centercomma,parens,i,j);
  if ((tempstr(1)=='i')|(tempstr(1)=='u'))
   outflag(2)=1;
   funstr{i}=[funstr{i}(1:parens(1)),'dble(',funstr{i}(parens(1)+1:parens(2)-1),')',funstr{i}(parens(2):end)];   
  end
 case 'floor'
  outflag(1)=1;
  [howmany,subscripts,centercomma,parens]=hassubscript(i,j);
  tempstr=makeMATLABcallstring(howmany,subscripts,centercomma,parens,i,j);  
  if ((tempstr(1)=='i')|(tempstr(1)=='u'))
   outflag(2)=1;
   funstr{i}=[funstr{i}(1:parens(1)),'dble(',funstr{i}(parens(1)+1:parens(2)-1),')',funstr{i}(parens(2):end)];
  end
 case 'return'
  outflag(1)=1;
 case 'imag'
  repstr='aimag';fortranfunwords{length(fortranfunwords)+1}=repstr;outflag(1)=1;
  funstr=replaceword(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);
 case 'log'
  outflag(1)=1;
 case 'log10'
  repstr='log';fortranfunwords{length(fortranfunwords)+1}=repstr;outflag(1)=1;
  dummy=find(funstr{i}=='(');
  dummy=dummy(dummy>funstrwords_e{i}(j));
  dummy=dummy(1);
  rightparen=findrights(dummy,funstr{i});
  funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),'(',repstr,funstr{i}((funstrwords_e{i}(j)+1):rightparen),'/log(10.0))',funstr{i}((rightparen+1):length(funstr{i}))];
 case 'log2'
  repstr='log';fortranfunwords{length(fortranfunwords)+1}=repstr;outflag(1)=1;
  dummy=find(funstr{i}=='(');
  dummy=dummy(dummy>funstrwords_e{i}(j));
  dummy=dummy(1);
  rightparen=findrights(dummy,funstr{i});
  funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),'(',repstr,funstr{i}((funstrwords_e{i}(j)+1):rightparen),'/log(2.0))',funstr{i}((rightparen+1):length(funstr{i}))]; 
 case 'mod'
  repstr='modulo';fortranfunwords{length(fortranfunwords)+1}=repstr;outflag(1)=1;
  [howmany,subscripts,centercomma,parens]=hassubscript(i,j);
  tempstr=makeMATLABcallstring(howmany,subscripts,centercomma,parens,i,j);  
  temp1{1,1}='';temp1{1,2}=''; temp1{2,1}='';temp1{2,2}='';
  if (tempstr(1)=='i')|(tempstr(1)=='u'), temp1{1,1}='dble(';temp1{1,2}=')';end
  if (tempstr(2)=='i')|(tempstr(2)=='u'), temp1{2,1}='dble(';temp1{2,2}=')';end
  funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),repstr,'(',temp1{1,1},subscripts{1},temp1{1,2},',',temp1{2},subscripts{2},temp1{2,2},')',funstr{i}(parens(2)+1:end)];   
 case 'nextpow2'
 case 'pow2'
 case 'real'
  outflag(1)=1;
 case 'rem'
  repstr='mod';  outflag(1)=1;
  [howmany,subscripts,centercomma,parens]=hassubscript(i,j);
  tempstr=makeMATLABcallstring(howmany,subscripts,centercomma,parens,i,j);  
  temp1{1,1}='';temp1{1,2}=''; temp1{2,1}='';temp1{2,2}='';
  if (tempstr(1)=='i')|(tempstr(1)=='u'), temp1{1,1}='dble(';temp1{1,2}=')';end
  if (tempstr(2)=='i')|(tempstr(2)=='u'), temp1{2,1}='dble(';temp1{2,2}=')';end
  funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),repstr,'(',temp1{1,1},subscripts{1},temp1{1,2},',',temp1{2},subscripts{2},temp1{2,2},')',funstr{i}(parens(2)+1:end)];   
 case 'round'
  repstr='anint';fortranfunwords{length(fortranfunwords)+1}=repstr;outflag(1)=1;
  [howmany,subscripts,centercomma,parens]=hassubscript(i,j);
  tempstr=makeMATLABcallstring(howmany,subscripts,centercomma,parens,i,j);
  if any(strcmp(tempstr,typs{3}))      %complex
   temp=funstr{i}(parens(1)+1:parens(2)-1);
   funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),'cmplx(anint(dble(',temp,')),anint(dble(aimag(',temp,'))))',funstr{i}(parens(2)+1:end)];
  elseif any(strcmp(tempstr,typs{7}))  %integer
   funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),'(',temp,')',funstr{i}(parens(2)+1:end)];
  else
   funstr=replaceword(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);
  end
 case 'sec'
  outflag(1)=1;
 case 'sech'
  repstr='sechf';fortranfunwords{length(fortranfunwords)+1}=repstr;outflag(1)=1;
  [howmany,subscripts,centercomma,parens]=hassubscript(i,j);
  tempstr=makeMATLABcallstring(howmany,subscripts,centercomma,parens,i,j);  
  if any(strcmp(tempstr(1),typs{4}))
   funstr{i}=[funstr{i}(1:parens(1)),'spread(',subscripts{1},',1,1)',funstr{i}(parens(2):end)];
  else
   funstr=replaceword(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);
  end
  outflag(3)=1;
 case 'sign'
 case 'sin'
  outflag(1)=1;
 case 'sinh'
  outflag(1)=1;
 case 'sqrt'
  outflag(1)=1;
  %if tempflagin(11)~=0
  if want_cs
   [howmany,subscripts,centercomma,parens]=hassubscript(i,j);
   funstr{i}=[funstr{i}(1:parens(1)),'cmplx(',funstr{i}(parens(1)+1:parens(2)-1),')',funstr{i}(parens(2):length(funstr{i}))];
  end
 case 'tan'
  outflag(1)=1;
 case 'tanh'
  outflag(1)=1;
 case 'unwrap'
 case 'close'
 case 'display'
  repstr='print';fortranfunwords{length(fortranfunwords)+1}=repstr;outflag(1)=1;
  [howmany,subscripts,centercomma,parens]=hassubscript(i,j);
  if howmany>0
   temp2=findstr('[',funstr{i});
   if ~isempty(temp2)
    temp2=temp2(1); temp3=findrights(temp2,funstr{i});
    tempstr=funstr{i}((temp2+1):(temp3-1));
   else
    tempstr=funstr{i}((parens(1)+1):(parens(2)-1));
   end
   found=1;
   while found==1
    found=0;
    for k=1:(length(tempstr)-6)
     if strncmp(tempstr(k:length(tempstr)),'num2str',7)
      tempstr=[tempstr(1:(k-1)),tempstr((k+7):length(tempstr))];
      found=1;
     end
    end
   end
   apost=0;
   for k=1:length(tempstr)
    if tempstr(k)=='''', apost=apost+1; end
    if ((tempstr(k)==' ')&(mod(apost,2)==0)), tempstr(k)=',';tempstr; end
   end
   funstr{i}=['print *,',tempstr,';'];
  end
 case 'get'
 case 'horzcat'
 case 'set'
 case 'vertcat'
 case 'disp'
  repstr='print';fortranfunwords{length(fortranfunwords)+1}=repstr;outflag(1)=1;
  [howmany,subscripts,centercomma,parens]=hassubscript(i,j);
  if howmany>0
   temp2=findstr('[',funstr{i});
   if ~isempty(temp2)
    temp2=temp2(1); temp3=findrights(temp2,funstr{i});
    tempstr=funstr{i}((temp2+1):(temp3-1));
   else
    tempstr=funstr{i}((parens(1)+1):(parens(2)-1));
   end
   found=1;
   while found==1
    found=0;
    for k=1:(length(tempstr)-6)
     if strncmp(tempstr(k:length(tempstr)),'num2str',7)
      tempstr=[tempstr(1:(k-1)),tempstr((k+7):length(tempstr))];
      found=1;
     end
    end
   end
   apost=0;
   for k=1:length(tempstr)
    if tempstr(k)=='''', apost=apost+1; end
    if ((tempstr(k)==' ')&(mod(apost,2)==0)), tempstr(k)=',';tempstr; end
   end
   funstr{i}=['print *,',tempstr,';'];
  end
 case 'eq'
 case 'isequal'
 case 'isvalid'
 case 'length'
  repstr='lengthf';fortranfunwords{length(fortranfunwords)+1}=repstr;outflag(1)=1;
  funstr=replaceword(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);
  outflag(3)=1;
 case 'ne'
 case 'size'
  repstr='shape';
  [howmany,subscripts,centercomma,parens]=hassubscript(i,j);
  tempstr=makeMATLABcallstring(howmany,subscripts,centercomma,parens,i,j);
  goon=1;
  %funstr{i},'what?',kb
  if howmany==1                         % One argument
   if any(strcmp(typs{11},tempstr))       % 2-D Array argument
    ;% Decide what to do based on the shape
    funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),'spread(ubound(',subscripts{1},'),1,1)',funstr{i}((parens(2)+1):length(funstr{i}))];
   elseif any(strcmp(typs{9},tempstr))    % 2-D row Array argument
    funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),'spread([1,length(',subscripts{1},')],1,1)',funstr{i}((parens(2)+1):length(funstr{i}))];
   elseif any(strcmp(typs{10},tempstr))    % 2-D col Array argument
    funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),'spread([length(',subscripts{1},'),1],1,1)',funstr{i}((parens(2)+1):length(funstr{i}))];
   else                                   % Scalar argument
    funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),'spread([1,1],1,1)',funstr{i}((parens(2)+1):length(funstr{i}))];
    %funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),'spread([1,shape([',subscripts{1},'])],1,1)',funstr{i}((parens(2)+1):length(funstr{i}))];
   end
   goon=0;    outflag(2)=1;
  elseif howmany==2                     % Two arguments
   temp2=cell(1,2);temp2{1}='';temp2{2}='';
   if ~any(strcmp(tempstr(2),typs{5})),temp2{1}='mxs(';temp2{2}=')'; goon=0; outflag(2)=1;end
   if any(strcmp(typs{11},tempstr(1)))  % 2-D Array, should be fine
    funstr{i}=[funstr{i}(1:centercomma),temp2{1},subscripts{2},temp2{2},funstr{i}(parens(2):end)];
   elseif any(strcmp(typs{4},tempstr(1))) % 1-D Array
    funstr{i}=[funstr{i}(1:parens(1)),'spread(',subscripts{1},',1,1),',temp2{1},subscripts{2},temp2{2},funstr{i}(parens(2):length(funstr{i}))];
    outflag(2)=1;
   else % scalar
    funstr{i}=[funstr{i}(1:centercomma-1),funstr{i}((parens(2)):length(funstr{i}))];
    %funstr{i}=[funstr{i}(1:centercomma-1),funstr{i}((parens(2)):length(funstr{i}))];
    outflag(2)=1;
   end
   goon=0;
  end
  if goon
   if ~strcmp(funstr{i}(funstrwords_b{i}(j)-2:funstrwords_b{i}(j)-1),',[')
    repstr='sizef';fortranfunwords{length(fortranfunwords)+1}=repstr;outflag(1)=1;
    funstr=replaceword(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);
    outflag(3)=1;
   else
    outflag(1)=1;
   end
  else
   outflag(1)=1;
  end
  %funstr{i},goon,outflag,kb
 case 'intersect'
 case 'ismember'
 case 'setdiff'
 case 'setxor'
 case 'union'
 case 'unique'
 case 'all'
  outflag(1)=1;
 case 'and'
 case 'any'
  outflag(1)=1;
 case 'colon'
 case 'eq'
 case 'ge'
 case 'gt'
 case 'kron'
 case 'le'
 case 'lt'
 case 'ne'
 case 'not'
 case 'or'
 case 'plus'
 case 'power'
 case 'slash'
 case 'times'
 case 'transpose'
  outflag(1)=1;
  [howmany,subscripts,centercomma,parens]=hassubscript(i,j);
  tempstr=makeMATLABcallstring(howmany,subscripts,centercomma,parens,i,j);
  if any(strcmp(tempstr(1),typs{11}))     %Do nothing, already good to go
  elseif any(strcmp(tempstr(1),typs{4}))  %1-D, so spread and transpose to col
   funstr{i}=[funstr{i}(1:funstrwords_e{i}(j)),'(spread(',subscripts{1},',1,1))',funstr{i}(parens(2)+1:length(funstr{i}))];
  else                                   %Remove transpose for scalars
   funstr=replaceword(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,'');   
   outflag(2)=1;
  end
 case 'ctranspose'
  outflag(1)=1;  outflag(2)=1;
  [howmany,subscripts,centercomma,parens]=hassubscript(i,j);
  tempstr=makeMATLABcallstring(howmany,subscripts,centercomma,parens,i,j);
  if any(strcmp(tempstr(1),typs{3}))       %Complex argument
   temp5{1}='conjg(';  temp5{2}=')';
  else                                     %Real argument
   temp5{1}='';        temp5{2}='';
  end
  if any(strcmp(tempstr(1),typs{11}))     %Do nothing, already good to go
   funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),'transpose(',temp5{1},funstr{i}(funstrwords_e{i}(j)+1:parens(2)),temp5{2},')',funstr{i}(parens(2)+1:length(funstr{i}))];
  elseif any(strcmp(tempstr(1),typs{4}))  %1-D, so spread and transpose to col
   funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),'transpose(spread(',temp5{1},funstr{i}(funstrwords_e{i}(j)+1:parens(2)),temp5{2},',1,1))',funstr{i}(parens(2)+1:length(funstr{i}))];
  else                                   %Remove transpose for scalars
   funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),temp5{1},funstr{i}(funstrwords_e{i}(j)+1:parens(2)),temp5{2},funstr{i}(parens(2)+1:length(funstr{i}))];
  end
 case 'uplus'
 case 'xor'
 case 'poly'
 case 'polyfit'
 case 'polyval'
 case 'rectint'
 case 'resi2'
 case 'residue'
 case 'roots'
 case 'airy'
  repstr='airyf';fortranfunwords{length(fortranfunwords)+1}=repstr;outflag(1)=1;
  [howmany,subscripts,centercomma,parens]=hassubscript(i,j);
  tempstr=makeMATLABcallstring(howmany,subscripts,centercomma,parens,i,j);  
  if any(strcmp(tempstr(1),typs{4}))
   if length(tempstr)==1
    funstr{i}=[funstr{i}(1:parens(1)),'spread(',subscripts{1},',1,1)',funstr{i}(parens(2):end)];
   else
    funstr{i}=[funstr{i}(1:parens(1)),'spread(',subscripts{1},',1,1)',funstr{i}(centercomma:end)];
   end
  else
   funstr=replaceword(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);
  end
  outflag(3)=1;
  if isempty(findstrexact('slatec',libraries)),libraries=[libraries,' -lslatec -llapack '];end  
 case 'besselh'
  repstr='besselhf';fortranfunwords{length(fortranfunwords)+1}=repstr;outflag(1)=1;
  [howmany,subscripts,centercomma,parens]=hassubscript(i,j);
  tempstr=makeMATLABcallstring(howmany,subscripts,centercomma,parens,i,j);  
  if any(strcmp(tempstr(1),typs{4}))&any(strcmp(tempstr(2),typs{4}))
   if length(tempstr)==2
    funstr{i}=[funstr{i}(1:parens(1)),'spread(',subscripts{1},',1,1),','spread(',subscripts{2},',1,1)',funstr{i}(parens(2):end)];
   else
    funstr{i}=[funstr{i}(1:parens(1)),'spread(',subscripts{1},',1,1),','spread(',subscripts{2},',1,1)',funstr{i}(centercomma(2):end)];
   end
  elseif any(strcmp(tempstr(1),typs{4}))
   funstr{i}=[funstr{i}(1:parens(1)),'spread(',subscripts{1},',1,1)',funstr{i}(centercomma(1):end)];
  elseif any(strcmp(tempstr(2),typs{4}))
   if length(tempstr)==2
    funstr{i}=[funstr{i}(1:centercomma(1)),'spread(',subscripts{2},',1,1)',funstr{i}(parens(2):end)];
   else
    funstr{i}=[funstr{i}(1:centercomma(1)),'spread(',subscripts{2},',1,1)',funstr{i}(centercomma(2):end)];
   end
  else
   funstr=replaceword(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);
  end
  outflag(3)=1;
  if isempty(findstrexact('slatec',libraries)),libraries=[libraries,' -lslatec -llapack '];end
 case 'besseli'
  repstr='besselif';fortranfunwords{length(fortranfunwords)+1}=repstr;outflag(1)=1;
  [howmany,subscripts,centercomma,parens]=hassubscript(i,j);
  tempstr=makeMATLABcallstring(howmany,subscripts,centercomma,parens,i,j);  
  if any(strcmp(tempstr(1),typs{4}))&any(strcmp(tempstr(2),typs{4}))
   if length(tempstr)==2
    funstr{i}=[funstr{i}(1:parens(1)),'spread(',subscripts{1},',1,1),','spread(',subscripts{2},',1,1)',funstr{i}(parens(2):end)];
   else
    funstr{i}=[funstr{i}(1:parens(1)),'spread(',subscripts{1},',1,1),','spread(',subscripts{2},',1,1)',funstr{i}(centercomma(2):end)];
   end
  elseif any(strcmp(tempstr(1),typs{4}))
   funstr{i}=[funstr{i}(1:parens(1)),'spread(',subscripts{1},',1,1)',funstr{i}(centercomma(1):end)];
  elseif any(strcmp(tempstr(2),typs{4}))
   if length(tempstr)==2
    funstr{i}=[funstr{i}(1:centercomma(1)),'spread(',subscripts{2},',1,1)',funstr{i}(parens(2):end)];
   else
    funstr{i}=[funstr{i}(1:centercomma(1)),'spread(',subscripts{2},',1,1)',funstr{i}(centercomma(2):end)];
   end
  else
   funstr=replaceword(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);
  end
  outflag(3)=1;
  if isempty(findstrexact('slatec',libraries)),libraries=[libraries,' -lslatec -llapack '];end
 case 'besselj'
  repstr='besseljf';fortranfunwords{length(fortranfunwords)+1}=repstr;outflag(1)=1;
  [howmany,subscripts,centercomma,parens]=hassubscript(i,j);
  tempstr=makeMATLABcallstring(howmany,subscripts,centercomma,parens,i,j);  
  if any(strcmp(tempstr(1),typs{4}))&any(strcmp(tempstr(2),typs{4}))
   if length(tempstr)==2
    funstr{i}=[funstr{i}(1:parens(1)),'spread(',subscripts{1},',1,1),','spread(',subscripts{2},',1,1)',funstr{i}(parens(2):end)];
   else
    funstr{i}=[funstr{i}(1:parens(1)),'spread(',subscripts{1},',1,1),','spread(',subscripts{2},',1,1)',funstr{i}(centercomma(2):end)];
   end
  elseif any(strcmp(tempstr(1),typs{4}))
   funstr{i}=[funstr{i}(1:parens(1)),'spread(',subscripts{1},',1,1)',funstr{i}(centercomma(1):end)];
  elseif any(strcmp(tempstr(2),typs{4}))
   if length(tempstr)==2
    funstr{i}=[funstr{i}(1:centercomma(1)),'spread(',subscripts{2},',1,1)',funstr{i}(parens(2):end)];
   else
    funstr{i}=[funstr{i}(1:centercomma(1)),'spread(',subscripts{2},',1,1)',funstr{i}(centercomma(2):end)];
   end
  else
   funstr=replaceword(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);
  end
  outflag(3)=1;
  if isempty(findstrexact('slatec',libraries)),libraries=[libraries,' -lslatec -llapack '];end
 case 'besselk'
  repstr='besselkf';fortranfunwords{length(fortranfunwords)+1}=repstr;outflag(1)=1;
  [howmany,subscripts,centercomma,parens]=hassubscript(i,j);
  tempstr=makeMATLABcallstring(howmany,subscripts,centercomma,parens,i,j);  
  if any(strcmp(tempstr(1),typs{4}))&any(strcmp(tempstr(2),typs{4}))
   if length(tempstr)==2
    funstr{i}=[funstr{i}(1:parens(1)),'spread(',subscripts{1},',1,1),','spread(',subscripts{2},',1,1)',funstr{i}(parens(2):end)];
   else
    funstr{i}=[funstr{i}(1:parens(1)),'spread(',subscripts{1},',1,1),','spread(',subscripts{2},',1,1)',funstr{i}(centercomma(2):end)];
   end
  elseif any(strcmp(tempstr(1),typs{4}))
   funstr{i}=[funstr{i}(1:parens(1)),'spread(',subscripts{1},',1,1)',funstr{i}(centercomma(1):end)];
  elseif any(strcmp(tempstr(2),typs{4}))
   if length(tempstr)==2
    funstr{i}=[funstr{i}(1:centercomma(1)),'spread(',subscripts{2},',1,1)',funstr{i}(parens(2):end)];
   else
    funstr{i}=[funstr{i}(1:centercomma(1)),'spread(',subscripts{2},',1,1)',funstr{i}(centercomma(2):end)];
   end
  else
   funstr=replaceword(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);
  end
  outflag(3)=1;
  if isempty(findstrexact('slatec',libraries)),libraries=[libraries,' -lslatec -llapack '];end
 case 'bessely'
  repstr='besselyf';fortranfunwords{length(fortranfunwords)+1}=repstr;outflag(1)=1;
  [howmany,subscripts,centercomma,parens]=hassubscript(i,j);
  tempstr=makeMATLABcallstring(howmany,subscripts,centercomma,parens,i,j);  
  if any(strcmp(tempstr(1),typs{4}))&any(strcmp(tempstr(2),typs{4}))
   if length(tempstr)==2
    funstr{i}=[funstr{i}(1:parens(1)),'spread(',subscripts{1},',1,1),','spread(',subscripts{2},',1,1)',funstr{i}(parens(2):end)];
   else
    funstr{i}=[funstr{i}(1:parens(1)),'spread(',subscripts{1},',1,1),','spread(',subscripts{2},',1,1)',funstr{i}(centercomma(2):end)];
   end
  elseif any(strcmp(tempstr(1),typs{4}))
   funstr{i}=[funstr{i}(1:parens(1)),'spread(',subscripts{1},',1,1)',funstr{i}(centercomma(1):end)];
  elseif any(strcmp(tempstr(2),typs{4}))
   if length(tempstr)==2
    funstr{i}=[funstr{i}(1:centercomma(1)),'spread(',subscripts{2},',1,1)',funstr{i}(parens(2):end)];
   else
    funstr{i}=[funstr{i}(1:centercomma(1)),'spread(',subscripts{2},',1,1)',funstr{i}(centercomma(2):end)];
   end
  else
   funstr=replaceword(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);
  end
  outflag(3)=1;
  if isempty(findstrexact('slatec',libraries)),libraries=[libraries,' -lslatec -llapack '];end
 case 'beta'
 case 'betacore'
 case 'betainc'
 case 'betaln'
 case 'cross'
 case 'dot_product'
  outflag(1)=1;
 case 'dot'
  [howmany,subscripts,centercomma,parens]=hassubscript(i,j);
  tempstr=makeMATLABcallstring(howmany,subscripts,centercomma,parens,i,j);
  %funstr{i},foo,foo2,'what?',kb
  if howmany==2                     % Two arguments
   for k=1:length(tempstr)
    if ~isempty(find(strcmp(tempstr(k),barr)))
     [a(k),b(k)]=find(strcmp(tempstr(k),barr));
    else
     [a(k),b(k)]=find(strcmp(tempstr(k),barr2));
     b(k)=b(k)+3;
    end
   end
   %If first arg is 1-D, row, or col, then OK, handle here
   temp=cell(howmany,2);
   for k=1:length(tempstr)
    if any(strcmp(tempstr(k),typs{11}))|any(strcmp(tempstr(k),typs{5}))
     temp{k,1}='[';       temp{k,2}=']';      else       temp{k,1}='';       temp{k,2}='';
    end
   end
   if any(strcmp(tempstr(k),typs{9}))|any(strcmp(tempstr(k),typs{10}))|any(strcmp(tempstr(k),typs{4}))
    outflag(2)=1;%Do nothing except replace with dot_product. result is scalar
    funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),'dot_product(',temp{1,1},subscripts{1},temp{1,2},',',temp{2,1},subscripts{2},temp{2,2},funstr{i}(parens(2):end)];
   elseif any(strcmp(tempstr(k),typs{5}))
    outflag(2)=1;%Do nothing except replace with *
    funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),'(',subscripts{1},'*',subscripts{2},funstr{i}(parens(2):end)];
   else %Need to call dotf, but add dimension of 1
    funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),'dot1f',funstr{i}(parens(1):parens(2)-1),funstr{i}(parens(2):end)];
    fortranfunwords{length(fortranfunwords)+1}='dot1f';outflag(1)=1;
    outflag(3)=1;
   end
  else %Just spread 1-D subscripts
   temp=cell(howmany,2);
   for k=1:length(tempstr)
    if any(strcmp(tempstr(k),typs{4}))
     temp{k,1}='spread(';temp{k,2}=',1,1)';else temp{k,1}='';temp{k,2}='';
    end
   end
   [tempval]=determineshape(howmany,subscripts,centercomma,parens,i,j,3);
   if tempval==1
    funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),'dot1f(',temp{1,1},subscripts{1},temp{1,2},',',temp{2,1},subscripts{2},temp{2,2},funstr{i}(parens(2):end)];
    fortranfunwords{length(fortranfunwords)+1}='dot1f';outflag(1)=1;
   elseif tempval==2
    funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),'dot2f(',temp{1,1},subscripts{1},temp{1,2},',',temp{2,1},subscripts{2},temp{2,2},funstr{i}(parens(2):end)];
    fortranfunwords{length(fortranfunwords)+1}='dot2f';outflag(1)=1;
   end
   outflag(3)=1;
  end
  outflag(1)=1;
  %funstr{i},goon,outflag,kb
 case 'ellipj'
 case 'ellipke'
 case 'erf'
 case 'erfc'
 case 'erfcore'
 case 'erfcx'
 case 'erfinv'
 case 'expint'
 case 'factor'
 case 'factorial'
 case 'gamma'
  repstr='gammaf';fortranfunwords{length(fortranfunwords)+1}=repstr;outflag(1)=1;
  [howmany,subscripts,centercomma,parens]=hassubscript(i,j);
  tempstr=makeMATLABcallstring(howmany,subscripts,centercomma,parens,i,j);  
  if any(strcmp(tempstr(1),typs{4}))
   funstr{i}=[funstr{i}(1:parens(1)),'spread(',subscripts{1},',1,1)',funstr{i}(parens(2):end)];
  else
   funstr=replaceword(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);
  end
  outflag(3)=1;
  if isempty(findstrexact('slatec',libraries)),libraries=[libraries,' -lslatec -llapack '];end  
 case 'gammainc'
  repstr='gammaincf';fortranfunwords{length(fortranfunwords)+1}=repstr;outflag(1)=1;
  [howmany,subscripts,centercomma,parens]=hassubscript(i,j);
  tempstr=makeMATLABcallstring(howmany,subscripts,centercomma,parens,i,j);  
  if any(strcmp(tempstr(1),typs{4}))
   if length(tempstr)==1
    funstr{i}=[funstr{i}(1:parens(1)),'spread(',subscripts{1},',1,1)',funstr{i}(parens(2):end)];
   else
    funstr{i}=[funstr{i}(1:parens(1)),'spread(',subscripts{1},',1,1)',funstr{i}(centercomma:end)];
   end
  else
   funstr=replaceword(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);
  end
  outflag(3)=1;
  if isempty(findstrexact('slatec',libraries)),libraries=[libraries,' -lslatec -llapack '];end    
 case 'gammaln'
  repstr='gammalnf';fortranfunwords{length(fortranfunwords)+1}=repstr;outflag(1)=1;
  [howmany,subscripts,centercomma,parens]=hassubscript(i,j);
  tempstr=makeMATLABcallstring(howmany,subscripts,centercomma,parens,i,j);  
  if any(strcmp(tempstr(1),typs{4}))
   funstr{i}=[funstr{i}(1:parens(1)),'spread(',subscripts{1},',1,1)',funstr{i}(parens(2):end)];
  else
   funstr=replaceword(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);
  end
  outflag(3)=1;
  if isempty(findstrexact('slatec',libraries)),libraries=[libraries,' -lslatec -llapack '];end  
 case 'gcd'
 case 'isprime'
 case 'lcm'
 case 'legendre'
 case 'nchoosek'
 case 'perms'
 case 'primes'
 case 'rat'
 case 'rats'
 case 'diag'
  repstr='diagf';fortranfunwords{length(fortranfunwords)+1}=repstr;outflag(1)=1;
  [howmany,subscripts,centercomma,parens]=hassubscript(i,j);
  tempstr=makeMATLABcallstring(howmany,subscripts,centercomma,parens,i,j);  
  %funstr{i},tempstr,kb
  if length(tempstr)==2
   if ~any(strcmp(tempstr(2),typs{7})) %scalar
    temp4{1}='int(';  temp4{2}=')';  else   temp4{1}='';  temp4{2}='';
   end
   if ~any(strcmp(tempstr(2),typs{5})) %scalar
    temp3{1}='mxs(';  temp3{2}=')';  else   temp3{1}='';  temp3{2}='';
   end
  end
  if any(strcmp(tempstr(1),typs{1})) %Full 2-D matrix, result is col
   if howmany==1
    funstr=replaceword(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);
   else
    funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),repstr,funstr{i}(funstrwords_e{i}(j)+1:centercomma),temp4{1},temp3{1},subscripts{2},temp3{2},temp4{2},funstr{i}(parens(2):end)];
   end
  else                               %Anything else
   if howmany==1
    funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),repstr,funstr{i}(funstrwords_e{i}(j)+1:parens(1)),'[',subscripts{1},']',funstr{i}(parens(2):end)];
   else
    funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),repstr,funstr{i}(funstrwords_e{i}(j)+1:parens(1)),'[',subscripts{1},'],',temp4{1},temp3{1},subscripts{2},temp3{2},temp4{2},funstr{i}(parens(2):end)];
   end
  end
  outflag(3)=1;
 case 'eps'
 case 'eye'
 case 'find'
  repstr='findf';fortranfunwords{length(fortranfunwords)+1}=repstr;outflag(1)=1;
  [howmany,subscripts,centercomma,parens]=hassubscript(i,j);
  temp2=inwhichlast(i,funstrwords_b{i}(j));
  if temp2==1
   funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),'nint([',repstr,funstr{i}(funstrwords_e{i}(j)+1:parens(2)),'])',funstr{i}(parens(2)+1:length(funstr{i}))];
  elseif temp2==4
   funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),'',repstr,funstr{i}(funstrwords_e{i}(j)+1:parens(2)),'',funstr{i}(parens(2)+1:length(funstr{i}))];
  else
   funstr=replaceword(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);
  end
  outflag(3)=1;
 case 'flipdim'
 case 'fliplr'
  repstr='fliplrf';fortranfunwords{length(fortranfunwords)+1}=repstr;outflag(1)=1;
  [howmany,subscripts,centercomma,parens]=hassubscript(i,j);
  tempstr=makeMATLABcallstring(howmany,subscripts,centercomma,parens,i,j);  
  if any(strcmp(tempstr(1),typs{4}))
   funstr{i}=[funstr{i}(1:parens(1)),'spread(',subscripts{1},',1,1)',funstr{i}(parens(2):end)];
  else
   funstr=replaceword(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);
  end
  outflag(3)=1;
 case 'flipud'
  repstr='flipudf';fortranfunwords{length(fortranfunwords)+1}=repstr;outflag(1)=1;
  [howmany,subscripts,centercomma,parens]=hassubscript(i,j);
  tempstr=makeMATLABcallstring(howmany,subscripts,centercomma,parens,i,j);  
  if any(strcmp(tempstr(1),typs{4}))
   funstr{i}=[funstr{i}(1:parens(1)),'spread(',subscripts{1},',1,1)',funstr{i}(parens(2):end)];
  else
   funstr=replaceword(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);
  end
  outflag(3)=1;
 case 'hankel'
 case 'hilb'
 case 'inf'
 case 'invhilb'
 case 'isempty'
 case 'isequal'
 case 'isfinite'
 case 'isinf'
  repstr='isinff';fortranfunwords{length(fortranfunwords)+1}=repstr;outflag(1)=1;
  funstr=replaceword(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);
  outflag(3)=1;
 case 'islogical'
 case 'isnan'
  repstr='isnanf';fortranfunwords{length(fortranfunwords)+1}=repstr;outflag(1)=1;
  funstr=replaceword(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);
  outflag(3)=1;  
 case 'isnumeric'
 case 'j'
 case 'linspace'
  repstr='linspacef';fortranfunwords{length(fortranfunwords)+1}=repstr;outflag(1)=1;
  funstr=replaceword(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);
  outflag(3)=1;
 case 'logical'
  repstr='';fortranfunwords{length(fortranfunwords)+1}=repstr;outflag(1)=1;
  funstr=replaceword(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);
 case 'logspace'
 case 'nan'
 case 'ndims'
 case 'ones'
  repstr='onesf';fortranfunwords{length(fortranfunwords)+1}=repstr;outflag(1)=1;
  % if the preceding is an equals, then replace with a 1.0
  temp1=findBefore(funstr{i},funstrwords_b{i}(j));
  if temp1=='='
   funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),'1.0'];
  else
   funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),repstr,funstr{i}(1:funstrwords_e{i}(j)+1)];
  end % if temp1=='='
 case 'pascal'
 case 'pi'
 case 'cauchy'
 case 'randcolu'
 case 'randcorr'
 case 'rand'
  outflag(1)=1;
  fortranfunwords{length(fortranfunwords)+1}='random_number';
  [howmany,subscripts,centercomma,parens]=hassubscript(i,j);
  temp4=cell(1,2)
  if howmany==1
   temp4{1}=subscripts{1};    temp4{2}=subscripts{1};
  else
   temp4{1}=subscripts{1};    temp4{2}=subscripts{2};
  end
  %Here we have to remake a few lines with the mxTR# and mxTC#
  %First, what is the highest count of mxTRC# on this line
  vType='r';
  if strcmp(funstrwords{i-1}{1},'deallocate')
   temp1=gethighesttemp(funstr{i},vType);
  else
   temp1=gethighesttemp([funstr{i-1},funstr{i}],vType);
  end
  for k=size(funstr,1)+2:-1:i+2
   funstr{k}=funstr{k-2};
  end
  maxTRC=updatetemp(maxTRC,temp1+1,vType);
  funstr{i}=['allocate(mxT',upper(vType),num2str(temp1+1),'(nint(',temp4{1},'),nint(',temp4{2},')));call random_number(mxT',upper(vType),num2str(temp1+1),');'];
  funstr{i+1}=[funstr{i+2}(1:funstrwords_b{i}(j)-1),'mxT',upper(vType),num2str(temp1+1),funstr{i+2}(parens(2)+1:length(funstr{i+2}))];
  funstr{i+2}=['deallocate(mxT',upper(vType),num2str(temp1+1),');'];
  updatefunstr;
  %'juuuuuuuuuuuu',funstr{i:i+2},kb
 case 'randn'
 case 'repmat'
  repstr='repmatf';fortranfunwords{length(fortranfunwords)+1}=repstr;outflag(1)=1;
  funstr=replaceword(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);
  outflag(3)=1;
 case 'reshape'
  %'reshapeeeee',funstr{i}
  goon=0;
  if ~goon
   [howmany,subscripts,centercomma,parens]=hassubscript(i,j);
   tempstr=makeMATLABcallstring(howmany,subscripts,centercomma,parens,i,j);  
%%%   if zzz
%%%    'ressssssss',tempstr,kb
%%%   end
   temp1=cell(4,2);  temp2=cell(1,3);
   temp1{1,1}='';temp1{1,2}='';temp1{2,1}='';temp1{2,2}='';
   temp1{3,1}='';temp1{3,2}='';temp1{4,1}='';temp1{4,2}='';
   temp2{1}='';  temp2{2}='';
   if ~any(strcmp(tempstr(2),typs{7})) %Not an integer, so we need int( )
    temp1{1,1}='int(';    temp1{1,2}=')';
   end
   if any(strcmp(tempstr(2),typs{11})) %2-D, so we need reshape( ,[1])
    temp1{3,1}='[';    temp1{3,2}=']';
   end  
   if howmany==3
    if ~any(strcmp(tempstr(3),typs{7})) %Not an integer, so we need int( )
     temp1{2,1}='int(';    temp1{2,2}=')';
    end
    if any(strcmp(tempstr(3),typs{11})) %2-D, so we need reshape( ,[1])
     temp1{4,1}='[';    temp1{4,2}=']';
    end
    if ~any(strcmp(tempstr(1),typs{5})) %not a scalar
     funstr{i}=[funstr{i}(1:centercomma(end-1)),'[',temp1{3,1},temp1{1,1},subscripts{2},temp1{1,2},temp1{3,2},',',temp1{4,1},temp1{2,1},subscripts{3},temp1{2,2},temp1{4,2},']',funstr{i}(parens(2):end)];
    else                                %Scalar
     funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),'(',subscripts{1},')',funstr{i}(parens(2)+1:end)];
    end
    outflag(2)=1;
   else
    if ~any(strcmp(tempstr(2),typs{4})) %Not an 1-D, so we need reshape( ,[1])
     temp1{3,1}='[';    temp1{3,2}=']';
    end
    if ~any(strcmp(tempstr(1),typs{5})) %not a scalar
     if ~all(strcmp('',{temp1{:}}))
      funstr{i}=[funstr{i}(1:centercomma(end)),temp1{3,1},temp1{1,1},subscripts{2},temp1{1,2},temp1{3,2},funstr{i}(parens(2):end)];
      outflag(2)=1;
     end
    else
     funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),'(',subscripts{1},')',funstr{i}(parens(2)+1:end)];
    end
   end
  end
  if goon
   repstr='reshapef';fortranfunwords{length(fortranfunwords)+1}=repstr;outflag(1)=1;
   funstr=replaceword(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);
   outflag(3)=1;
  else
   outflag(1)=1;
  end
  %funstr{i},outflag,kb
 case 'rot90'
 case 'toeplitz'
 case 'tril'
 case 'triu'
 case 'vander'
 case 'zeros'
  repstr='zerosf';fortranfunwords{length(fortranfunwords)+1}=repstr;outflag(1)=1;
  % if the preceding is an equals, then replace with a 0.0
  temp1=findBefore(funstr{i},funstrwords_b{i}(j));
  if temp1=='='
   funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),'0.0'];
  else
   funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),repstr,funstr{i}(funstrwords_e{i}(j)+1:end)];
  end % if temp1=='='
 case 'numel'
 case 'argnames'
 case 'cat'
 case 'dblquad'
 case 'fcnchk'
 case 'fmin'
 case 'fminbnd'
 case 'fmins'
 case 'fminsearch'
 case 'foptions'
 case 'fzero'
 case 'numjac'
 case 'ode113'
 case 'ode15s'
 case 'ode23'
 case 'ode23s'
 case 'ode23t'
 case 'ode23tb'
 case 'ode45'
 case 'odefile'
 case 'odeget'
 case 'odephas2'
 case 'odephas3'
 case 'odeplot'
 case 'odeprint'
 case 'odeset'
 case 'optimget'
 case 'optimset'
 case 'quad'
 case 'quad8'
 case 'quadl'
 case 'delete'
 case 'memory'
 case 'mex'
 case 'who'
 case 'whos'
 case 'workspace'
 case 'beep'
 case 'plot'
 case 'plotyy'
 case 'polar'
 case 'text'
 case 'title'
 case 'xlabel'
 case 'ylabel'
 case 'zoom'
 case 'hold'
 case 'case'
 case 'catch'
 case 'else'
 case 'elseif'
 case 'end'  %Do something only if this is in a subscript
  [temp,howmany,subscripts,centercomma,parens,callword]=inwhichlast(i,funstrwords_b{i}(j));
  if temp==2 %In a subscript
   if howmany==2  %which subscript is it in
    if funstrwords_b{i}(j)>centercomma  %Second subscript
     funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),'size(',funstrwords{i}{callword},',2)',funstr{i}(funstrwords_e{i}(j)+1:length(funstr{i}))];
    else                                %First subscript
     funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),'size(',funstrwords{i}{callword},',1)',funstr{i}(funstrwords_e{i}(j)+1:length(funstr{i}))];
    end
   elseif howmany==1
    if size(getfield(cw,funstrwords{i}{callword}),1)==1 %row vector var
     funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),'size(',funstrwords{i}{callword},',2)',funstr{i}(funstrwords_e{i}(j)+1:length(funstr{i}))];
    elseif size(getfield(cw,funstrwords{i}{callword}),2)==1 %column vector var
     funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),'size(',funstrwords{i}{callword},',1)',funstr{i}(funstrwords_e{i}(j)+1:length(funstr{i}))];
    end
   end
  end
  outflag(1)=1;
 case 'error'
  repstr='call mexErrMsgTxt';fortranfunwords{length(fortranfunwords)+1}='mexErrMsgTxt';outflag(1)=1;
  funstr=replaceword(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);
  outflag(1)=1; outflag(2)=1;
 case 'eval'
 case 'evalc'
 case 'evalin'
 case 'exist'
 case 'feval'
 case 'det'
 case 'eig'
  repstr='eigf';fortranfunwords{length(fortranfunwords)+1}=repstr;outflag(1)=1;
  [howmany,subscripts,centercomma,parens]=hassubscript(i,j);
  tempstr=makeMATLABcallstring(howmany,subscripts,centercomma,parens,i,j);  
  if any(strcmp(tempstr(1),typs{4}))
   funstr{i}=[funstr{i}(1:parens(1)),'spread(',subscripts{1},',1,1)',funstr{i}(parens(2):end)];
  else
   funstr=replaceword(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);
  end
  outflag(3)=1;
  if isempty(findstrexact('slatec',libraries)),libraries=[libraries,' -lslatec -llapack '];end 
 case 'inv'
 case 'logm'
 case 'lscov'
 case 'lsqnonneg'
 case 'svd'
  repstr='svdf';fortranfunwords{length(fortranfunwords)+1}=repstr;outflag(1)=1;
  [howmany,subscripts,centercomma,parens]=hassubscript(i,j);
  tempstr=makeMATLABcallstring(howmany,subscripts,centercomma,parens,i,j);  
  if any(strcmp(tempstr(1),typs{4}))
   funstr{i}=[funstr{i}(1:parens(1)),'spread(',subscripts{1},',1,1)',funstr{i}(parens(2):end)];
  else
   funstr=replaceword(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);
  end
  outflag(3)=1;
  if isempty(findstrexact('slatec',libraries)),libraries=[libraries,' -lslatec -llapack '];end
 case 'trace'
 case 'norm'
  repstr='normf';fortranfunwords{length(fortranfunwords)+1}=repstr;outflag(1)=1;
  [howmany,subscripts,centercomma,parens]=hassubscript(i,j);
  tempstr=makeMATLABcallstring(howmany,subscripts,centercomma,parens,i,j);  
  if any(strcmp(tempstr(1),typs{4}))
   if length(tempstr)==1
    funstr{i}=[funstr{i}(1:parens(1)),'spread(',subscripts{1},',1,1)',funstr{i}(parens(2):end)];
   else
    funstr{i}=[funstr{i}(1:parens(1)),'spread(',subscripts{1},',1,1)',funstr{i}(centercomma:end)];
   end
  else
   funstr=replaceword(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);
  end
  updatefunstr(i);
  [howmany,subscripts,centercomma,parens]=hassubscript(i,j);
  if howmany==2
   if length(findstrexact('-inf',subscripts{2}))>0
    funstr{i}=[funstr{i}(1:centercomma),'-101.0',funstr{i}(parens(2):length(funstr{i}))];
   elseif length(findstr('inf',subscripts{2}))>0
    funstr{i}=[funstr{i}(1:centercomma),'101.0',funstr{i}(parens(2):length(funstr{i}))];
   end
  end
  tempstr=makeMATLABcallstring(howmany,subscripts,centercomma,parens,i,j);
  tempstr=tempstr(1);
  eval(['[temp3{1},temp3{2},temp3{3}]=svd_make(''',tempstr,''');']);
  %'sosos',needed_interfaces{1,1},kb
  for m=1:length(tempstr)
   temp=any(strcmp(tempstr(m),{'d','m'})); if temp,tempstr(m)='c';end
   temp=any(strcmp(tempstr(m),{'e','n'})); if temp,tempstr(m)='r';end
   temp=any(strcmp(tempstr(m),{'f','o'})); if temp,tempstr(m)='i';end
   temp=any(strcmp(tempstr(m),{'g','p'})); if temp,tempstr(m)='l';end
  end
  if length(find(strcmpi('svdf',needed_interfaces{1,1})))==0
   needed_interfaces{1,1}{length(needed_interfaces{1,1})+1}='svdf';
   needed_interfaces{1,2}{length(needed_interfaces{1,1})}{1,1}=tempstr;
   needed_interfaces{1,2}{length(needed_interfaces{1,1})}{1,2}=temp3{3}{1};
   needed_interfaces{1,2}{length(needed_interfaces{1,1})}{1,3}=temp3{3}{2};
  else
   temp2=find(strcmpi('svdf',needed_interfaces{1,1}));
   if length(find(strcmpi(tempstr,needed_interfaces{1,2}{temp2}(:,1))))==0
    needed_interfaces{1,2}{temp2}{size(needed_interfaces{1,2}{temp2},1)+1,1}=tempstr;
    needed_interfaces{1,2}{temp2}{size(needed_interfaces{1,2}{temp2},1),2}=temp3{3}{1};
    needed_interfaces{1,2}{temp2}{size(needed_interfaces{1,2}{temp2},1),3}=temp3{3}{2};
   end
  end
  outflag(3)=1;
  if isempty(findstrexact('slatec',libraries)),libraries=[libraries,' -lslatec -llapack '];end
 case 'realmin'
  repstr='tiny(1.0)';fortranfunwords{length(fortranfunwords)+1}='tiny';outflag(1)=1;
  funstr=replaceword(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);
  updatefunstr(i);
 case 'realmax'
  repstr='huge(1.0)';fortranfunwords{length(fortranfunwords)+1}='huge';outflag(1)=1;
  funstr=replaceword(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);
  updatefunstr(i);
 case 'bicg'
 case 'bicgstab'
 case 'cgs'
 case 'eigs'
 case 'full'
 case 'gmres'
 case 'gplot'
 case 'luinc'
 case 'nnz'
  outflag(1)=1;fortranfunwords{length(fortranfunwords)+1}='count';
  [howmany,subscripts,centercomma,parens]=hassubscript(i,j);
  tempstr=makeMATLABcallstring(howmany,subscripts,centercomma,parens,i,j);  
  if any(strcmp(tempstr(1),typs{5}))
   funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),'count(spread(',funstr{i}(parens(1)+1:parens(2)-1),',1,1)/=0)',funstr{i}(parens(2)+1:end)];
  else
   funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),'count(',funstr{i}(parens(1)+1:parens(2)-1),'/=0.0)',funstr{i}(parens(2)+1:end)];
  end
 case 'nonzeros'
 case 'nzmax'
 case 'pcg'
 case 'qmr'
 case 'randperm'
 case 'rjr'
 case 'eigs2'
 case 'lsqr'
 case 'minres'
 case 'cubic'
 case 'cubici1'
 case 'cubici2'
 case 'cubici3'
 case 'fmincon'
 case 'fminimax'
 case 'fminu'
 case 'fminunc'
 case 'fseminf'
 case 'fsolve'
 case 'leastsq'
 case 'linprog'
 case 'lp'
 case 'lsint'
 case 'lsqcurvefit'
 case 'lsqlin'
 case 'lsqnonlin'
 case 'minimax'
 case 'mmole'
 case 'molecule'
 case 'xcorr'
 case 'xcorr2'
 case 'xcov'
 case 'diff'
 case 'int'
 case 'prod'
  %Of this is complex, what do we do?
  [howmany,subscripts,centercomma,parens]=hassubscript(i,j);
  tempstr=makeMATLABcallstring(howmany,subscripts,centercomma,parens,i,j);
  goon=1;  foo=1;  foo2=0;
  %funstr{i},foo,foo2,'what?',kb
  temp=cell(howmany,2);
  for k=1:howmany
   if any(strcmp(tempstr(k),typs{3}))
    temp{k,1}='abs(';temp{k,2}=')';
    outflag(2)=1;  %Because we need an update
   else
    temp{k,1}='';temp{k,2}='';
   end
  end
  if howmany==1                         % One argument
   if any(strcmp(typs{11},tempstr))        % 2-D Array argument
    ;% Decide what to do based on the shape
    if foo
     if any(strcmp(tempstr(1),typs{1}))     % Full matrix, just add ,1 to the min statement
      funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),'spread(product(',temp{1,1},funstr{i}(parens(1)+1:parens(2)-1),temp{1,2},',1),1,1)',funstr{i}(parens(2)+1:length(funstr{i}))];
     elseif any(strcmp(tempstr(1),typs{9})) % Row, OK, fortran mins entire thing
      funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),'product(',temp{1,1},funstr{i}(parens(1)+1:parens(2)-1),temp{1,2},')',funstr{i}(parens(2)+1:length(funstr{i}))];
     elseif any(strcmp(tempstr(1),typs{10}))% column, OK, fortran mins entire thing
      funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),'product(',temp{1,1},funstr{i}(parens(1)+1:parens(2)-1),temp{1,2},')',funstr{i}(parens(2)+1:length(funstr{i}))];
     end
     goon=0;   outflag(2)=1;
    end
   elseif any(strcmp(typs{4},tempstr))    % 1-D Array argument
    funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),'product(',temp{1,1},subscripts{1},temp{1,2},')',funstr{i}(parens(2)+1:length(funstr{i}))];
    goon=0;    outflag(2)=1;
   else                                   % Scalar argument
    funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),funstr{i}((funstrwords_e{i}(j)+1):length(funstr{i}))];
    goon=0;    outflag(2)=1;
   end
  elseif howmany==2                     % Two arguments
   if j>1
    if strcmp(funstrwords{i}{j-1},'spread')
     foo=0; foo2=0;
    end
   end
   [tempval]=determineshape(howmany,subscripts,centercomma,parens,i,j,2);
   if ~((tempval==1)|(tempval==2)) ;     foo=0; foo2=1;     end
   if foo
    temp2=cell(1,2);temp2{1}='';temp2{2}='';
    if ~any(strcmp(tempstr(2),typs{5})),temp2{1}='mxs(';temp2{2}=')';end
    if tempval==1
     temp5{1}='';                temp5{2}='';
    else
     temp5{1}='transpose(';      temp5{2}=')';
    end
    if any(strcmp(tempstr(1),typs{1}))     % Full matrix, just add ,1 to the sum statement
     funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),temp5{1},'spread(product(',temp{1,1},subscripts{1},temp{1,2},',',temp2{1},temp{2,1},subscripts{2},temp{2,2},temp2{2},'),1,1)',temp5{2},funstr{i}(parens(2)+1:length(funstr{i}))];
    elseif any(strcmp(tempstr(1),typs{9})) %Row
     if tempval==1 %remove min and 2nd sub
      funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),funstr{i}(funstrwords_e{i}(j)+1:centercomma(1)-1),funstr{i}(parens(2):length(funstr{i}))];
     elseif tempval==2
      funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),'product(',temp{1,1},subscripts{1},temp{1,2},funstr{i}(parens(2):length(funstr{i}))];
     end
    elseif any(strcmp(tempstr(1),typs{10}))% column
     if tempval==1
      funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),'product(',temp{1,1},subscripts{1},temp{1,2},funstr{i}(parens(2):length(funstr{i}))];
     elseif tempval==2
      funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),funstr{i}(funstrwords_e{i}(j)+1:centercomma(1)-1),funstr{i}(parens(2):length(funstr{i}))];
     end
    elseif any(strcmp(tempstr(1),typs{4}))% 1-D
     funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),'product(',temp{1,1},subscripts{1},temp{1,2},')',funstr{i}(parens(2)+1:length(funstr{i}))];
    elseif any(strcmp(tempstr(1),typs{5}))% scalar
     funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),'(',temp{1,1},subscripts{1},temp{1,2},')',funstr{i}(parens(2)+1:length(funstr{i}))];
    end
    goon=0;      outflag(2)=1;
   else
    if foo2   ;   goon=1;     else      goon=0;     end
   end
  end
  if goon
   repstr='prodf';fortranfunwords{length(fortranfunwords)+1}=repstr;outflag(1)=1;
   funstr=replaceword(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);
   outflag(3)=1;
  else
   outflag(1)=1;
  end
  %funstr{i},goon,outflag,kb
 case 'rank'
end
end
if length(fortranfunwords)>fortranfunwordslength, outflag(2)=1; end
