function [out,typeDefs]=buildTypeDefLine(typeDefs,i,var_words,want_row,funwords,fortranVarOrRes,MLapp)

%this doesn't handle non-1 indexing (see varp)

out='struct(';
r=char(10);
for j=size(typeDefs{i,2},1):-1:2
 temp=find(strcmp({typeDefs{:,1}},typeDefs{i,2}{j,3}));
 %'eeeeeeeeeeee',temp,i,typeDefs,typeDefs{i,:},j,kb
 if ~isempty(temp)
  %then this is a nested typeDef
  out=[out,'''',typeDefs{i,2}{j,1},''',',typeDefs{i,2}{j,3}];
  if ~isempty(typeDefs{i,2}{j,5})
   out=[out,'(ones('];
   if length(typeDefs{i,2}{j,5})==1
    if want_row
     out=[out,'1,',aftC(typeDefs{i,2}{j,5}{1})];
    else
     out=[out,typeDefs{i,2}{j,5}{1},',1'];
    end
   else
    for k=1:length(typeDefs{i,2}{j,5})
     if k==length(typeDefs{i,2}{j,5})
      out=[out,aftC(typeDefs{i,2}{j,5}{k})];
     else
      out=[out,aftC(typeDefs{i,2}{j,5}{k}),','];
     end
    end
   end % if length(typeDefs{i,
   out=[out,'))'];
  end
  %out=[out,buildTypeDefLine(typeDefs,temp(1))];
 else
  
   howmany=length(typeDefs{i,2}{j,5});
   localVar=typeDefs{i,2};
   temp6=j;
   outZ='';
   
  if ~isempty(typeDefs{i,2}{j,11})
   if strcmp(typeDefs{i,2}{j,3},'character')
    outZ=[''''''];
   else
    outZ=['[]'];
   end
  else
   if howmany==0
    goon=1;
    if ~any(strcmp(localVar{temp6,3},var_words))
     outZ=[localVar{temp6,3}];
     goon=0;
    end % if localVar{temp6,
    if strcmp(localVar{temp6,3},'character')
     if ischar(localVar{temp6,2})
      outZ=['repmat('' '',1,',localVar{temp6,2},')'];
     else
      outZ=[''''''];
     end
     goon=0;
    end % if localVar{temp6,
    if strcmp(localVar{temp6,3},'logical')
     outZ=['false'];
     goon=0;
    end % if strcmp(localVar{temp6,
    if goon && ~strcmp(localVar{temp6,3},'character')
     outZ=['0'];
    end
   elseif howmany==1
    if ~any(strcmp(localVar{temp6,3},var_words))
     if want_row
      outZ=[localVar{temp6,3},'(ones(1,',aftC(localVar{temp6,5}{1}),'))'];
     else
      outZ=[localVar{temp6,3},'(ones(',aftC(localVar{temp6,5}{1}),',1))'];
     end
    elseif strcmp(localVar{temp6,3},'character')% && length(localVar{temp6,5})>0
     outZ=['{cell(1,',char(aftC(localVar{temp6,5}{1})),')}'];
     %outZ=[localVar{j,1},'=cell(1,',localVar{temp6,2},');'];
    else
     if want_row
      outZ=['zeros(1,',aftC(localVar{temp6,5}{1}),')'];
     else
      outZ=['zeros(',aftC(localVar{temp6,5}{1}),',1)'];
     end
    end
   else
    if ~any(strcmp(localVar{temp6,3},var_words))
     subscripts2=[localVar{temp6,3},'(ones('];
    else
     subscripts2='zeros(';
    end
    for k=1:howmany
     if k~=howmany
      subscripts2=[subscripts2,aftC(localVar{temp6,5}{k}),','];
     else
      subscripts2=[subscripts2,aftC(localVar{temp6,5}{k}),')'];
     end
    end
    if ~any(strcmp(localVar{temp6,3},var_words))
     subscripts2=[subscripts2(1:end-1),')'];
    end
    outZ=[subscripts2];
   end

   %typeDefs{i,2},i,outZ
  end % if ~isempty(typeDefs{i,

  %'bbbbbbbbbbbb11',out,outZ,kb
  temp5='';
  if any(strcmp(typeDefs{i,2}{j,1},{funwords{:},{fortranVarOrRes{:}}}))
   temp5=MLapp;
   typeDefs{i,2}{j,1}=[typeDefs{i,2}{j,1},MLapp];
  end
  out=[out,'''',typeDefs{i,2}{j,1},''',',outZ];
  %out=[out,'''',typeDefs{i,2}{j,1},''',[]'];
 end % if ~isempty(temp)
 if j>2
  out=[out,',...',r];
 end
end % for j=1:size(typeDefs{i,
% these ellipses are there so that when I changeCase later, I can identify 
% struct strings and change them (whereas I don't change other strings)
%out=[out,' ...',r,')'];
out=[out,')'];
%'bbbbbbbbbbbb',out,outZ,kb

function out=aftC(str)
lastsemi=find(str==':',1,'last');
if isempty(lastsemi)
 out=str;
else
 out=str(lastsemi+1:end);
end