%'wwwwwwwwwwww',funstr{i},kb
%get rid of unit=, note we assume that is the first subscript!
funstr{i}=regexprep(funstr{i},['\s*unit\s*=\s*'],'');
[s,fs_good]=updatefunstr_1line_f(funstr,fs_good,i);
outflag(1)=1;
[howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
write1Asterisk=strcmp('*',strtrim(subscripts{1}));
write1_6=strcmp('6',strtrim(subscripts{1}));
if howmany>1
 write2Asterisk=strcmp('*',strtrim(subscripts{2}));
else
 write2Asterisk=0;   centercomma=parens(2);   subscripts{2}='';
end

nums=find(funstrnumbers_b{i}>=centercomma(1) & funstrnumbers_b{i}<=parens(2));
wrds=find(funstrwords_b{i}>=centercomma(1) & funstrwords_b{i}<=parens(2));
if isempty(strfind(subscripts{2},'''(')) & length(nums)~=1
 % we have a local format to try and deal with
 %punt
 %write2Asterisk=1;
end

temp5='writef';
if write1Asterisk | write1_6
 thisfid='1';
else
 temp4=fidStr;
 if ~isempty(find(funstrwords_b{i}>parens(1),1))
  if any(strcmp(funstrwords{i}{find(funstrwords_b{i}>parens(1),1)},localVar(:,1)))
   temp4='';
  end
 end
 thisfid=[temp4,strtrim(subscripts{1})];
end  
if write2Asterisk %then this has no other format statement
%%%   if ~isempty(strfind(funstr{i},'SOS'))
%%%    'aaaaaaaaaaaa1',funstr{i},kb
%%%   end
 %find top level commas after write() and separate into fprintf's
 temp7=parens(2); temp8=nextNonSpace(funstr{i},parens(2));
 if funstr{i}(temp8)==',',  temp7=temp8;  end
 [groups,temp4]=getTopGroupsAfterLoc(funstr{i},temp7);
 temp4=[parens(2),temp4];

 
 %now build up fprintf strings
 groupFormatStr='';
 groupsStr=[temp5,'(',thisfid,',','['''];
 groupsTemp='';
 for ii=1:length(groups)
  %find the first word after the toplevel comma here and see if its a string or char
  goonimag=0;
  temp3=find(funstrwords_b{i}>temp4(ii));
  if ~isempty(temp3)
   temp6=find(strcmp(funstrwords{i}{temp3(1)},{localVar{:,1}}));
   tempstr=strtrim(groups{ii});
   if (~isempty(temp6) && strcmp(localVar{temp6,3},'character')) || ...
        any(strcmp(funstrwords{i}{temp3(1)},{'deblank'})) || ...
        tempstr(1)=='['
     goonimag=1;
   end
  end % if ~isempty(temp3)
  %'aaaaaaaaaaaa1',funstr{i},kb
  if ii==length(groups), temp='\n'; else temp=''; end
  if strncmp(fliplr(deblank(groups{ii})),'''',1) | goonimag
   thisFormat='%s';
  else
   thisFormat='%0.15g';
  end
  groupsTemp=[groupsTemp,',',groups{ii}];
  groupFormatStr=[groupFormatStr,thisFormat,sp2,temp];
 end
 groupsStr=[groupsStr,groupFormatStr,''']',groupsTemp,');'];
 %'bbbbbbbbbb',funstr{i},groupsStr,groupFormatStr,groupsTemp,kb
 
 %now put it together
 funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),groupsStr];
 
 out=funstr{i};
 temp=find(out=='/');
 temp1=find(~inastring_f(out,temp));
 for ii=length(temp1):-1:1
  temp2=''; if ii==length(temp1), temp2=','; end
  bar=find(~isspace(out));
  bar=bar(bar>temp(temp1(ii)));
  bar=bar(1);
  if isletter(out(bar))
   temp2=',';
  end
  out=[out(1:temp(temp1(ii))-1),', ''\n '' ',temp2,out(temp(temp1(ii))+1:end)];
 end
 funstr{i}=out;
else % not an asterisk on subscript 2
 goon=0;
 if any(subscripts{2}=='(') %there may be format specifiers in the write(,here)
  pl=find(subscripts{2}=='(');
  for ii=1:length(pl)
   if inastring_f(subscripts{2},pl(ii))
    goon=1;
   end
  end
  temp3=strtrim(subscripts{2});
  if temp3(1)=='(', goon=1; end
 end

%%% if any(strcmp(funstrwords{i},'check'))
%%%  '-----------',funstr{i},subscripts{2},keyboard
%%% end

 if goon %any(subscripts{2}=='(') %format specifiers in the write(,here)
  pl=find(subscripts{2}=='(');
  pr=find(subscripts{2}==')');
  groups=getTopGroupsAfterLoc(subscripts{2}(pl(1)+1:pr(end)-1),0);
  
  groupsStr='';
  for ii=1:length(groups)
   temp1=',';if ii==length(groups), temp1='';end
   %this may be a string but with DQ{1} there
   global DQ
   isString=0;
   if any(~cellfun('isempty',regexp(groups{ii},DQ)))
    isString=1;
   end
   groupsStr=[groupsStr,convertFormatField(groups{ii},[],isString),temp1];
  end
  temp=''; if write1Asterisk | write1_6, temp=',''\n'''; end
  groupsStr=['[',groupsStr,temp,']'];
  %'aaaaaaaaaaaa3',funstr{i},kb
 
  % this might be a string conversion
  nums=find(funstrnumbers_b{i}>=parens(1) & funstrnumbers_b{i}<=centercomma(1));
  wrds=find(funstrwords_b{i}>=parens(1) & funstrwords_b{i}<=centercomma(1));
  goonimag=1;
  if ~isempty(wrds)
   temp6=find(strcmp(funstrwords{i}{wrds(1)},{localVar{:,1}}));
   if isempty(temp6)
    if strcmp(funstrwords{i}{wrds(1)},this_fun_name) || ...
         strcmp(funstrwords{i}{wrds(1)},[this_fun_name,'Result'])
     temp6=1;
    end
   end
   %'dddddddd1234',funstr{i},kb
   if ~isempty(temp6)
    if strcmp(localVar{temp6,3},'character') || ...
         strcmp(funstrwords{i}{wrds(1)},this_fun_name)
     funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),subscripts{1},'=sprintf(',groupsStr,',',funstr{i}(parens(2)+1:end-1),');'];
     goonimag=0;
    end % if strcmp(localVar{temp6,
   end % if ~isempty(temp6)
  end % if ~isempty(wrds)
  if goonimag
   funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),temp5,'(',thisfid,',',groupsStr,',',funstr{i}(parens(2)+1:end-1),');'];
  end
 else %no asterisk on 2, no paren in sub2
%%%      funstr{i},'ccccccccc2',kb
  groupsStr='';
  whichFormat=find(strcmp({formats{:,1}},...
                          strrep(strrep(strrep(subscripts{2},'fmt',''),'=',''),' ','')));
  if ~isempty(whichFormat)
   for ii=1:formats{whichFormat,3}
    temp=',';if ii==formats{whichFormat,3}, temp=' '' \n''';end
    %funstr{i},temp,formats{whichFormat,4}{ii},'ccccccccc',kb
    groupsStr=[groupsStr,convertFormatField(formats{whichFormat,4}{ii}),temp];
   end
  else
   groupsStr=[''];
  end
  %funstr{i},groupsStr,'ccccccccc',kb
  if ~isempty(groupsStr)
   %groupsStr=['[',groupsStr,'''\n'']'];
   groupsStr=['[',groupsStr,']'];
   % if thisfid is a variable name and it is a string, then sprintf with output instead
   fidIsVar=find(strcmp(thisfid,localVar(:,1)));
   if ~isempty(fidIsVar) && strcmp(localVar{fidIsVar,3},'character')
    funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),thisfid,'=sprintf(',groupsStr,',',funstr{i}(parens(2)+1:end-1),');'];
   else
    if ~isempty(strtrim(funstr{i}(parens(2)+1:end-1)))
     temp9=[',',funstr{i}(parens(2)+1:end-1)];
    else
     temp9='';
    end
     funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),temp5,'(',thisfid,',',groupsStr,temp9,');'];
   end
  else
   [groups,temp4]=getTopGroupsAfterLoc(funstr{i},parens(2));
   %'dssaaaaaaaaaa',funstr{i},kb
   funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),'disp({',funstr{i}(parens(2)+1:end-1),'});'];
  end
  %groupsStr,funstr{i},kb
 end
end
%%%  if strcmp(subscripts{2},'620')
%%%   funstr{i},'ddddddddd11',kb
%%%  end
