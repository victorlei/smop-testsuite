function out=zeroVarDec(funstr,i,temp2,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,localVar,temp6,varp,funwords,want_row,var_words,override)

if ~exist('override','var')
 override=0;
end
out='';
[hms,subscripts,centercomma,parens]=hassubscript_f(i,temp2,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
howmany=length(localVar{temp6,5});
%%%if ischar(localVar{temp6,2})
%%% howmany=max([iscell(localVar{temp6,5})*length(localVar{temp6,5}),hms]);
%%%else
%%% howmany=max([localVar{temp6,2},iscell(localVar{temp6,5})*length(localVar{temp6,5}),hms]);
%%%end
if hms==0 && howmany>0 %assign a parens for later processing
 parens=[funstrwords_b{i}(temp2),funstrwords_e{i}(temp2)];
end
subscripts=localVar{temp6,5};
%treat these differently if there is an equals sign after
temp3=find(funstr{i}=='=');  temp3=temp3(temp3>funstrwords_e{i}(temp2)); 
%'smack',kb

%%%if (strcmp('x',funstrwords{i}{temp2}))
%%% out,'uuuuuuuuuuu3',kb
%%%end      

if isempty(temp3) || override %no equals sign
 if ~isempty(localVar{temp6,11}) && ~override
  if strcmp(localVar{temp6,3},'character')
   out=[funstrwords{i}{temp2},'='''';'];
  else
   out=[funstrwords{i}{temp2},'=[];'];
  end
 else
  temp7=cell(0);
  for j=1:howmany
   cc=[parens(1),centercomma,parens(2)];
   temp3=strfind(subscripts{j},':');
   %temp8=find(funstrwords_b{i}>(cc(j)+temp3(end)),1,'first');
   if ~isempty(temp3)
    temp7{j}=subscripts{j}(temp3(end)+1:end);
    temp8=find(~isspace(temp7{j}),1,'last');
    if temp7{j}(temp8)==']', temp7{j}(temp8)=''; end
    %temp7{j}=funstr{i}(cc(j)+1+temp3(end):funstrwords_e{i}(find(funstrwords_e{i}>cc(j)&funstrwords_e{i}<cc(j+1),1,'last')));
    %temp7{j}=subscripts{j}(temp3(end)+1:end);
   else
    temp7{j}=subscripts{j};
   end
   % Does it have a +# needed from varp?
   temp5=find(strcmpi(funstrwords{i}{temp2},{varp{:,1}}));
   if ~isempty(temp5)
    % does that subscript need a +# ?
    goon=find(j==varp{temp5,2});
    if ~isempty(goon)
     temp7{j}=[temp7{j},'+',num2str(varp{temp5,3}(goon))];
    end
   end
  end % for j=1:howmany
      %'jjjjjjjjjjjjj',kb
%%%  if (strcmp('filter',funstrwords{i}{temp2}))
%%%   funstr{i},'uuuuuuuuuuu3444',kb
%%%  end      
  if howmany==0
   goon=1;
   if ~any(strcmp(localVar{temp6,3},var_words))
    out=[funstrwords{i}{temp2},'=',localVar{temp6,3},';'];
    goon=0;
   end % if localVar{temp6,
   if strcmp(localVar{temp6,3},'character')
    if ischar(localVar{temp6,2})
     out=[funstrwords{i}{temp2},'=repmat('' '',1,',localVar{temp6,2},');'];
    else
     out=[funstrwords{i}{temp2},'='''';'];
    end
    goon=0;
   end % if localVar{temp6,
   if strcmp(localVar{temp6,3},'logical')
    out=[funstrwords{i}{temp2},'=false;'];
    goon=0;
   end % if strcmp(localVar{temp6,
   if goon && ~strcmp(localVar{temp6,3},'character')
    if isempty(localVar{temp6,16})
     out=[funstrwords{i}{temp2},'=0;'];
    else %this is a function handle
     out=[funstrwords{i}{temp2},'={};'];
    end
   end
  elseif howmany==1
   if ~any(strcmp(localVar{temp6,3},var_words))
    if want_row
     out=[funstrwords{i}{temp2},'=',localVar{temp6,3},'(ones(1,',temp7{1},'));'];
    else
     out=[funstrwords{i}{temp2},'=',localVar{temp6,3},'(ones(',temp7{1},',1));'];
    end
   elseif strcmp(localVar{temp6,3},'character')% && length(localVar{temp6,5})>0
    out=[funstrwords{i}{temp2},'=cell(1,',char(localVar{temp6,5}),');'];
    %out=[funstrwords{i}{temp2},'=cell(1,',localVar{temp6,2},');'];
   else
    if isempty(localVar{temp6,16}), temp8='zeros'; else, temp8='cell'; end
    if want_row
     out=[funstrwords{i}{temp2},'=',temp8,'(1,',temp7{1},');'];
    else
     out=[funstrwords{i}{temp2},'=',temp8,'(',temp7{1},',1);'];
    end
   end
  else
   if ~any(strcmp(localVar{temp6,3},var_words))
    subscripts2=['=',localVar{temp6,3},'(ones('];
   else
    if strcmp(localVar{temp6,3},'character')
     subscripts2='=cell(';
    else
     subscripts2='=zeros(';
    end
   end
   for j=1:howmany
    if j~=howmany
     subscripts2=[subscripts2,temp7{j},','];
    else
     subscripts2=[subscripts2,temp7{j},');'];
    end
   end
   if ~any(strcmp(localVar{temp6,3},var_words))
    subscripts2=[subscripts2(1:end-1),');'];
   end
   out=[funstrwords{i}{temp2},subscripts2];
  end
 end
%'sayyyyyyyyy',localVar,out,keyboard
else %has an equals sign
     %if no subscripts, then we are OK, scalar. Otherwise, add limits
 if howmany>0
  if strcmp(localVar{temp6,3},'character')
   out=[funstrwords{i}{temp2},...
        strrep(strrep(funstr{i}(parens(2)+1:end),'(/','{'),'/)','}')];
%%%            if strcmp(funstrwords{i}{temp2},'month')
%%%             'dddddd22',out,keyboard
%%%            end
  else
   temp4=cell(0,1);
   for j=1:howmany
    subscripts{j}=strtrim(subscripts{j});
    temp5=strfind(subscripts{j},':');
    temp4{j}=subscripts{j};
    if ~isempty(temp5)
     %does it need a 1: on the front?
     if subscripts{j}(1)==':'
      temp4{j}=['1',subscripts{j}];
     end % if subscripts{j}(1)==':'
    else
     temp4{j}=['1:',subscripts{j}];
    end
    temp4{j}=['[',temp4{j},']'];
    % Does it have a +# needed from varp?
    temp5=find(strcmpi(funstrwords{i}{temp2},{varp{:,1}}));
    if ~isempty(temp5)
     % does that subscript need a +# ?
     goon=find(j==varp{temp5,2});
     if ~isempty(goon)
      temp4{j}=[temp4{j},'+',num2str(varp{temp5,3}(goon))];
     end
    end
   end     
   subscripts2='';
   for j=1:howmany
    if j~=howmany
     subscripts2=[subscripts2,temp4{j},','];
    else
     subscripts2=[subscripts2,temp4{j}];
    end
   end
   out=[funstrwords{i}{temp2},'(',subscripts2,')',funstr{i}(parens(2)+1:end)];
  end % if strcmp(localVar{temp6, 
 else %scalar, keep what is there
  out=funstr{i}(funstrwords_b{i}(temp2):end);
 end % if howmany>0
end % if isempty(temp3)
% [s,fs_good]=updatefunstr_1line_f(funstr,fs_good,i);

