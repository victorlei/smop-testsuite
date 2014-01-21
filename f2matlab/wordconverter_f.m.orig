function [funstr,fortranfunwords,outflag,needAllUpdated,extraFunctions]=wordconverter_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,fs_good,funwords,fortranfunwords,formats,localVar,modLocalVar,MLapp,varp,want_row,var_words,this_fun_name,TFops,typeDefs,funwordsML,statementFunction,fun_name,dumvar,want_gl,fundecline,funargs);
% This will change the Matlab function to its Fortran equivalent syntax.
% outflag(1) is 1 if its taken care of here. 0 if not.
%fortranfunwords=[];
outflag=[0];r=char(10);s=length(funstr);
fidStr='fid_'; needAllUpdated=0;
global sp sp2
sp=''; sp2=' ';
extraFunctions=[];
%if i>=33, funstr{i},funstrwords{i}{j},kb,end
%trgtoptOutMod
switch funstrwords{i}{j}
%%% case {'include'}
%%%  outflag(1)=1;
%%%  temp=strfind(funstr{i},'''');
%%%  temp2=funstr{i}(temp(1):temp(end));
%%%  if 0
%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),['eval(',temp2,');']];
%%%  else
%%%   disp(['   there is an include file ',temp2,' ... Converting']);
%%%   temp4=strfind(temp2,'.');
%%%   if isempty(temp4)
%%%    filename=temp2(2:end-1);
%%%   else
%%%    filename=temp2(2:temp4(end)-1);
%%%   end
%%%   filename=strrep(filename,'.','_');
%%%
%%%   temp3=['includeout=f2matlab(',temp2,',1);'];
%%%   eval(temp3);
%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),['eval(',filename,');']];
%%%  end
  case {'stop'}
    outflag(1)=1;
    funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),['error([''stop encountered in original fortran code  '',char(10),''',strrep(funstr{i}(funstrwords_e{i}(j)+1:end),'''',''),''']);']];
  case {'common'}
    outflag(1)=1;
    funstr{i}=[funstr{i}(funstrwords_e{i}(j)+2:end)];
  case {'use'}
    outflag(1)=1;
    if want_gl
     funstr{i}=['% ',funstr{i}];
    else
     temp1=find(strcmp(funstrwords{i}{2},{modLocalVar{:,1}}));
     %usedMods=unique([usedMods,temp1]);
     %'uuuuuuuuuuuu',usedMods,kb
     funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),funstr{i}(funstrwords_e{i}(j)+1:funstrwords_e{i}(j+1)),';'];
     %funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),funstr{i}(funstrwords_e{i}(j)+1:end)];
    end
  case {'save','contains'}
    outflag(1)=1;
    funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),'%',funstr{i}(funstrwords_b{i}(j):end)];
  case {'allocate'}
    outflag(1)=1;
    [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
    tempstr=cell(howmany,1);
    if howmany>0
     temp1=[parens(1),centercomma,parens(2)];
     for jj=1:howmany
      temp=find(funstrwords_b{i}>temp1(jj),1,'first');
      temp3=find(funstrwords_b{i}>temp1(jj),1,'last');
%%%    funstrwords{i}{temp}
%%%    'vvvvvvvvvv',funstr{i},kb
      if any(strcmpi(funstrwords{i}{temp},{'stat','status'}))
       tempstr{jj}=[tempstr{jj},funstrwords{i}{temp},'=0;'];
      elseif any(strcmpi(funstrwords{i}{temp},{localVar{:,1}}))
       [temp5,temp6,temp7]=varType(i,temp,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords,localVar,typeDefs,var_words);
       [howmany2,subscripts2,centercomma2,parens2]=hassubscript_f(i,temp7,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
       temp6{5}=subscripts2;
       temp8=zeroVarDec(funstr,i,temp7,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,temp6,1,varp,funwords,want_row,var_words,1);
       temp9=[funstr{i}(temp1(jj)+1:funstrwords_b{i}(temp7)-1),temp8];
       if want_gl && iscell(temp6) && ~any(strcmpi(var_words,temp6{3}))
        temp9=['global ',temp6{3},'; ',temp9];
       end
       tempstr{jj}=[tempstr{jj},temp9];
       %'\\\\\\\\\\\\\\\11',funstr{i},tempstr{jj},kb
       %tempstr{jj}=subscripts{jj};
      else
       tempstr{jj}=[''];
      end % if strcmp(funstrwords{i}{temp}, 
          %if jj;=howmany, tempstr{jj}=[tempstr{jj},';']; end
     end % for j=1:howmany
     funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),funstr{i}(parens(2)+1:end-1)];
     temp2=length(tempstr);
     funstr(i+temp2:end+temp2-1)=funstr(i+1:end);
     funstr(i:i+temp2-1)=tempstr;
%%%     '\\\\\\\\\\\\\\\',funstr{i:i+temp2},kb
    end
  case {'deallocate','nullify'}
    outflag(1)=1;
    [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
    tempstr=cell(howmany,1);
    temp1=[parens(1),centercomma,parens(2)];
    for jj=1:howmany
     temp=find(funstrwords_b{i}>temp1(jj));
     temp=temp(1);
     if ~isempty(temp)
      if any(strcmpi(funstrwords{i}{temp},{'stat','status'}))
       tempstr{jj}=[tempstr{jj},funstrwords{i}{temp},'=0;'];
      elseif any(strcmp(funstrwords{i}{temp},{localVar{:,1}}))
       [temp5,temp6,temp7]=varType(i,temp,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords,localVar,typeDefs,var_words);
       tempstr{jj}=[tempstr{jj},subscripts{jj},'='];
       if strcmp(temp6{1,3},'character')
        tempstr{jj}=[tempstr{jj},''''''];
       else
        tempstr{jj}=[tempstr{jj},'[]'];
       end % if strcmp(localVar{temp6,
      else
       % just set it to empty anyway
       tempstr{jj}=[subscripts{jj},'=[]'];     
      end % if strcmp(funstrwords{i}{temp}, 
     end % if ~isempty(temp)
         %funstr{i},tempstr{jj},'jjjjjjjjj',kb
     tempstr{jj}=[tempstr{jj},';'];
    end % for j=1:howmany
    funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),[tempstr{:}],funstr{i}(parens(2)+1:end)];
%%%  temp2=length(tempstr);
%%%  funstr(i+temp2+1:end+temp2)=funstr(i+1:end);
%%%  funstr(i+1:i+temp2)=tempstr;
%%%  funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),funstr{i}(parens(2)+1:end)];
%%%  temp2=length(tempstr);
%%%  funstr(i+temp2:end+temp2-1)=funstr(i+1:end);
%%%  funstr(i:i+temp2-1)=tempstr;
%'dddddddddddd',funstr{i},tempstr,kb
  case {'close'}
    outflag(1)=1;
    [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
    %funstr{i}=['fclose(',fidStr,subscripts{1}(~isspace(subscripts{1})),');'];
    unit=strrep(subscripts{1}(~isspace(subscripts{1})),'unit=','');
    if any(isletter(unit))
     funstr{i}=['fclose(',unit,');'];
    else
     funstr{i}=['fclose(',fidStr,unit,');'];
    end
  case {'open'}
    outflag(1)=1;
    [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
    %now get the filename
    fileWord=find(strcmpi('file',funstrwords{i}));
    temp=findNext(funstrwords_b{i}(fileWord),'=',funstr{i});
    temp1=[parens(1),centercomma,parens(2)];
    temp2=find(temp1>temp);
    temp2=temp1(temp2(1));
    filename=funstr{i}(temp+1:temp2-1);
    unit=strrep(subscripts{1}(~isspace(subscripts{1})),'unit=','');
    perm='w+';
    %check for status, etc.
    if any(strcmpi('readonly',funstrwords{i}))
     perm='r';
    elseif any(strcmpi('status',funstrwords{i}))
     temp1=find(strcmpi('status',funstrwords{i}));
     if strcmpi(funstrwords{i}{temp1+1},'old')
      perm='r+';
     end
    end
    %'fooooooo',funstr{i},kb
    if any(isletter(unit))
     funstr{i}=[unit,'=fopen(',filename,',''',perm,''');'];
    else
     funstr{i}=[fidStr,unit,'=fopen(',filename,',''',perm,''');'];
    end
  case {'cycle'}
    if any(strcmp(funstrwords{i}{j},{localVar{:,1}}))
     repstr=['cycle',MLapp];
    else
     repstr='continue';
    end
    outflag(1)=1;
    funstr=replaceword_f(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);  
  case {'data'}
    %now taken care of in main
  case {'print'}
    convertWrite
%%%    outflag(1)=1;
%%%    temp=find(~isspace(funstr{i}));
%%%    temp=temp(temp>funstrwords_e{i}(j));
%%%    temp=temp(1);
%%%    
%%%    %find next comma after print
%%%    temp2=findNext(funstrwords_e{i}(j),',',funstr{i});
%%%    
%%%    if funstr{i}(temp)=='*' %then this has no other format statement
%%%                            %find top level commas after print *, and separate into fprintf's
%%%     [groups,temp4]=getTopGroupsAfterLoc(funstr{i},temp2);
%%%     temp4=[temp2,temp4];
%%%     %now build up fprintf strings
%%%     groupsStr='';
%%%     for ii=1:length(groups)
%%%      %if strcmp(funstrwords{i}{2},'month'),'=======',funstr{i},keyboard,end
%%%      %find the first word after the toplevel comma here and see if its a string or char
%%%      goonimag=0;
%%%      temp3=find(funstrwords_b{i}>temp4(ii));
%%%      if ~isempty(temp3)
%%%       temp6=find(strcmp(funstrwords{i}{temp3(1)},{localVar{:,1}}));
%%%       if ~isempty(temp6)
%%%        if strcmp(localVar{temp6,3},'character')% | strcmp(localVar{temp6,3},'string')
%%%         goonimag=1;
%%%        end
%%%       end % if ~isempty(temp6)
%%%      end % if ~isempty(temp3)    
%%%      if ii==length(groups), temp='\n'; else temp=''; end
%%%      if strncmp(fliplr(deblank(groups{ii})),'''',1) | goonimag
%%%       groupsStr=[groupsStr,'fprintf(1,''%s',sp2,'',temp,''',',groups{ii},');'];
%%%      else
%%%       groupsStr=[groupsStr,'fprintf(1,''%0.15g',sp2,'',temp,''',',groups{ii},');'];
%%%      end      
%%%     end
%%%     %now put it together
%%%     funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),groupsStr];
%%%    elseif funstr{i}(temp)==''''
%%%     [groups,temp4]=getTopGroupsAfterLoc(funstr{i},temp-1)
%%%     groupsStr=convertFormatField(groups{1}(2:end-1),[],1)
%%%     temp5=''; if ~isempty(temp4), temp5=funstr{i}(temp4:end-1); end
%%%     funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),'fprintf(1,',groupsStr,temp5,');'];
%%%     %'fffffffff',funstr{i},groups,temp4,kb
%%%    else % this assumes that there is a number "print <here>, ..." that referes to a format
%%%     groupsStr='';
%%%     whichFormat=find(strcmp({formats{:,1}},...
%%%                             strrep(strrep(strrep(funstr{i}(funstrwords_e{i}(j)+1:temp2-1),'fmt',''),'=',''),' ','')));
%%%     for ii=1:formats{whichFormat,3}
%%%      temp=',';if ii==formats{whichFormat,3}, temp='';end
%%%      groupsStr=[groupsStr,convertFormatField(formats{whichFormat,4}{ii}),temp];
%%%     end
%%%     %groupsStr=['[',groupsStr,'''\n'']'];
%%%     groupsStr=['[',groupsStr,']'];
%%%     funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),'fprintf(1,',groupsStr,',',funstr{i}(temp2+1:end-1),');'];
%%%    end
  case {'format'}
    outflag(1)=1;
    funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),'%',funstr{i}(funstrwords_b{i}(j):end)];
  case {'write'}
    convertWrite
  case {'read'}
    %'dddddddddddd111',funstr,kb
    outflag(1)=1;
    [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
    if howmany>0
     write1Asterisk=strcmp('*',subscripts{1}(~isspace(subscripts{1})));
     write1_6=strcmp('5',subscripts{1}(~isspace(subscripts{1})));
     if howmany>1
      write2Asterisk=strcmp('*',subscripts{2}(~isspace(subscripts{2})));  
     else
      write2Asterisk=0;
      centercomma=parens(2);
      subscripts{2}='';
     end
    else
     write1Asterisk=1;
     write1_6=0;
     write2Asterisk=1;
    end
    if write1Asterisk | write1_6
     thisfid='1';
    else
     temp4=fidStr;
     if ~isempty(find(funstrwords_b{i}>parens(1),1))
      temp3=funstrwords{i}{find(funstrwords_b{i}>parens(1),1)};
      if any(strcmp(temp3,localVar(:,1))) ||...
           (~isempty(findstr(temp3,MLapp)) && any(strcmp(temp3(1:end-3),localVar(:,1))))
       temp4='';
      end
     end
     thisfid=[temp4,strtrim(subscripts{1})];
     %thisfid=[fidStr,subscripts{1}(~isspace(subscripts{1}))];
    end
%%%  if write1Asterisk & write2Asterisk
%%%   write1Asterisk,write2Asterisk,funstr{i},'----------------',kb
%%%  end % if write1Asterisk & write2Asterisk   
    if write1Asterisk
     groups=getTopGroupsAfterLoc(funstr{i},parens(2));
     funstr{i}='';
     for ii=1:length(groups)
      funstr{i}=[funstr{i},groups{ii},'=input('''',','''s'');'];
     end % for ii=1:length(groups)
         %'mmmmmmmmmmmm',funstr{i},keyboard
%%%   if howmany>0
%%%    funstr{i}=['input(''',strrep(funstr{i}(parens(2)+1:end),';',''),''');'];
%%%   else
%%%    funstr{i}=['input(''',strrep(funstr{i}(funstrwords_e{i}(j)+1:end),';',''),''');'];
%%%   end
%%%   warning('interactive input not written yet for f2matlab')
    elseif write2Asterisk %then this has no other format statement
                          %find top level commas after write() and separate into fscanf's
     groups=getTopGroupsAfterLoc(funstr{i},parens(2));
     %now build up fprintf strings
     groupsStr='';
     for ii=1:length(groups)
      if ii==length(groups), temp=''; else temp=''; end
%%%    if any(strcmp(funstrwords{i},'twoa1'))
%%%    write1Asterisk,write2Asterisk,funstr{i},'----------------22',kb
%%%    end
      if ~isempty(groups{ii})
       % does thisfid point to a string or variable?
       temp5='fscanf';     if isempty(temp4), temp5='sscanf'; end
       if strncmp(fliplr(deblank(groups{ii})),'''',1)
        groupsStr=[groupsStr,groups{ii},'=',temp5,'(',thisfid,',''%s',sp,'',temp,''',size(',groups{ii},'));'];
       else
        groupsStr=[groupsStr,groups{ii},'=',temp5,'(',thisfid,',''%g',sp,'',temp,''',size(',groups{ii},'));'];
       end   
      else
       %groupsStr=[groupsStr,'fscanf(',thisfid,',''%s',sp,'',temp,''',1);'];
       groupsStr=[groupsStr,'fgetl(',thisfid,');'];
      end
     end
     %now put it together
     funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),groupsStr];
    else
     goon=0;
     if any(subscripts{2}=='(') %there may be format specifiers in the write(,here)
      pl=find(subscripts{2}=='(');
      for ii=1:length(pl)
       if inastring_f(subscripts{2},pl(ii))
        goon=1;
       end
      end % for ii=1:length(pl)
     end
     %goon,funstr{i},kb
     if goon %any(subscripts{2}=='(') %format specifiers in the write(,here)
      pl=find(subscripts{2}=='(');
      pr=find(subscripts{2}==')');
      groups=getTopGroupsAfterLoc(subscripts{2}(pl(1)+1:pr(end)-1),0);
      
      groupsStr='';
      for ii=1:length(groups)
       temp1=',';if ii==length(groups), temp1='';end
       groupsStr=[groupsStr,convertFormatField(groups{ii},'r'),temp1];
      end
      groupsStr=['[',groupsStr,']'];
      funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),'[',funstr{i}(parens(2)+1:end-1),']=readf(',thisfid,',',groupsStr,',',num2str(length(find(groupsStr=='%'))),');'];
      %funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),'readf(',thisfid,',',groupsStr,',',funstr{i}(parens(2)+1:end-1),');'];
      %funstr{i},'ffffffffffffffffffffffffffff',kb
     else
      groupsStr='';
      whichFormat=find(strcmp({formats{:,1}},...
                              strrep(strrep(strrep(subscripts{2},'fmt',''),'=',''),' ','')));
      %funstr{i},'ffffffffffffffffffffffffffff',kb
      if ~isempty(whichFormat)
       for ii=1:formats{whichFormat,3}
        temp=',';if ii==formats{whichFormat,3}, temp='';end
        groupsStr=[groupsStr,convertFormatField(formats{whichFormat,4}{ii},'r'),temp];
       end
      else
       groupsStr='''%f''';
      end
      %groupsStr=['[',groupsStr,'''\n'']'];
      groupsStr=['[',groupsStr,']'];
      funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),'[',funstr{i}(parens(2)+1:end-1),']=readf(',thisfid,',',groupsStr,',',num2str(length(find(groupsStr=='%'))),');'];
      %funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),'readf(',thisfid,',',groupsStr,',',funstr{i}(parens(2)+1:end-1),');'];
      %groupsStr,funstr{i},kb
     end
    end
    %'dddddddddddd',funstr,kb
  case {'abs'}
    outflag(1)=1;
  case {'iabs','jiabs','habs','iiabs','babs1','dabs','cabs','zabs','cdabs','qabs'}
    repstr='abs';outflag(1)=1;
    funstr=replaceword_f(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);  
  case {'achar'}
    repstr='char';outflag(1)=1;
    funstr=replaceword_f(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);    
  case {'acos'}
    outflag(1)=1;  
  case {'dacos','qacos'}
    repstr='acos';outflag(1)=1;
    funstr=replaceword_f(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);  
  case {'acosd','dacosd','qacosd'}
    repstr='acos';outflag(1)=1;
    dummy=find(funstr{i}=='(');
    dummy=dummy(dummy>funstrwords_e{i}(j));
    dummy=dummy(1);
    rightparen=findrights_f(dummy,funstr{i});
    funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),'(',repstr,funstr{i}((funstrwords_e{i}(j)+1):rightparen),'*180/pi)',funstr{i}((rightparen+1):end)];
  case {'adjustl'}
    repstr='strtrim';outflag(1)=1;
    funstr=replaceword_f(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);
%%%  repstr='deblank';outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  temp=funstr{i}(parens(1):parens(2));
%%%  funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),'[fliplr(deblank(fliplr',temp,'))',...
%%%             ',repmat('' '',1,length',temp,'-length(fliplr(deblank(fliplr',temp,'))))]',...
%%%             funstr{i}(parens(2)+1:end)];  
  case {'adjustr'}
    repstr='deblank';outflag(1)=1;
    [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
    temp=funstr{i}(parens(1):parens(2));
    funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),'[repmat('' '',1,length',temp,...
               '-length(deblank',temp,')),deblank',temp,']',...
               funstr{i}(parens(2)+1:end)];  
  case {'acosh'}
    outflag(1)=1;
  case {'dacosh','qacosh'}
    repstr='acosh';outflag(1)=1;
    funstr=replaceword_f(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);  
  case {'imag'}
    outflag(1)=1;
  case {'aimag','dimag','qimag'}
    repstr='imag';outflag(1)=1;
    funstr=replaceword_f(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);    
  case {'int','aint','dint','ddint','qint'}
    repstr='fix';outflag(1)=1;
    %change only if there is an argument
    [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
    if howmany>0
     funstr=replaceword_f(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);
    end
  case {'all'}
    outflag(1)=1;
  case {'allocated','associated'}
    outflag(1)=1;
    [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
    temp6=find(strcmpi(funstrwords{i}{j+1},{localVar{:,1}}));
    funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),'~isempty(',strtrim(subscripts{1}),')',...
               funstr{i}(parens(2)+1:end)];
%%%  funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),'any(size(',funstrwords{i}{j+1},')~=1)',...
%%%             funstr{i}(parens(2)+1:end)];
  case {'anint','dnint','qnint'}
    repstr='round';outflag(1)=1;
    funstr=replaceword_f(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);  
  case {'any'}
    outflag(1)=1; 
  case {'asin'}
    outflag(1)=1;
  case {'dasin','qasin'}
    repstr='asin';outflag(1)=1;
    funstr=replaceword_f(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);  
  case {'asinh'}
    outflag(1)=1;
  case {'dasinh','qasinh'}
    repstr='asinh';outflag(1)=1;
    funstr=replaceword_f(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);  
  case {'asind','dasind','qasind'}
    repstr='asin';outflag(1)=1;
    dummy=find(funstr{i}=='(');
    dummy=dummy(dummy>funstrwords_e{i}(j));
    dummy=dummy(1);
    rightparen=findrights_f(dummy,funstr{i});
    funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),'(',repstr,funstr{i}((funstrwords_e{i}(j)+1):rightparen),'*180/pi)',funstr{i}((rightparen+1):end)];
  case {'asm'}
  case {'atan'}
    outflag(1)=1;
  case {'datan','qatan'}
    repstr='atan';outflag(1)=1;
    funstr=replaceword_f(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);  
  case {'atand','datand','qatand'}
    repstr='atan';outflag(1)=1;
    dummy=find(funstr{i}=='(');
    dummy=dummy(dummy>funstrwords_e{i}(j));
    dummy=dummy(1);
    rightparen=findrights_f(dummy,funstr{i});
    funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),'(',repstr,funstr{i}((funstrwords_e{i}(j)+1):rightparen),'*180/pi)',funstr{i}((rightparen+1):end)];
  case {'atanh'}
    outflag(1)=1;
  case {'datanh','qatanh'}
    repstr='atanh';outflag(1)=1;
    funstr=replaceword_f(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);  
  case {'atan2'}
    outflag(1)=1;
  case {'datan2','qatan2'}
    repstr='atan2';outflag(1)=1;
    funstr=replaceword_f(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);  
  case {'atan2d','datan2d','qatan2d'}
    repstr='atan2';outflag(1)=1;
    dummy=find(funstr{i}=='(');
    dummy=dummy(dummy>funstrwords_e{i}(j));
    dummy=dummy(1);
    rightparen=findrights_f(dummy,funstr{i});
    funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),'(',repstr,funstr{i}((funstrwords_e{i}(j)+1):rightparen),'*180/pi)',funstr{i}((rightparen+1):end)];
  case {'bit'}
  case {'btest'}
    outflag(1)=1;
    [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
    funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),'logical(bitget(uint16(',...
               strtrim(subscripts{1}),'),(',...
               strtrim(subscripts{2}),')+1)',funstr{i}(parens(2):end)];
  case {'ceiling'}
    repstr='ceil';outflag(1)=1;
    funstr=replaceword_f(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);  
  case {'char'}
    outflag(1)=1;
  case {'cmplx'}
    repstr='complex';outflag(1)=1;
    [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
    if howmany<=2
     funstr=replaceword_f(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);  
    else
     funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),repstr,funstr{i}(parens(1):centercomma(2)-1),funstr{i}(parens(2):end)];
    end
  case {'conjg','dconjg'}
    repstr='conj';outflag(1)=1;
    funstr=replaceword_f(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);  
  case {'cos'}
    outflag(1)=1;
  case {'dcos','ccos','zcos','cdcos','qcos'}
    repstr='cos';outflag(1)=1;
    funstr=replaceword_f(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);  
  case {'cosd','dcosd','qcosd'}
    repstr='cos';outflag(1)=1;
    dummy=find(funstr{i}=='(');
    dummy=dummy(dummy>funstrwords_e{i}(j));
    dummy=dummy(1);
    rightparen=findrights_f(dummy,funstr{i});
    funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),repstr,funstr{i}((funstrwords_e{i}(j)+1):dummy),'pi/180*(',funstr{i}((dummy+1):rightparen),')',funstr{i}((rightparen+1):end)];
  case {'cosh'}
    outflag(1)=1;
  case {'dcosh','qcosh'}
    repstr='cosh';outflag(1)=1;
    funstr=replaceword_f(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);  
  case {'cotan','dcotan','qcotan'}
    repstr='cot';outflag(1)=1;
    funstr=replaceword_f(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);  
  case {'cotand','dcotand','qcotand'}
    repstr='cot';outflag(1)=1;
    dummy=find(funstr{i}=='(');
    dummy=dummy(dummy>funstrwords_e{i}(j));
    dummy=dummy(1);
    rightparen=findrights_f(dummy,funstr{i});
    funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),repstr,funstr{i}((funstrwords_e{i}(j)+1):dummy),'pi/180*(',funstr{i}((dummy+1):rightparen),')',funstr{i}((rightparen+1):end)];
  case {'count'}
    repstr='nnz';
    [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
    if howmany~=0
     outflag(1)=1;
     funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),repstr,funstr{i}((funstrwords_e{i}(j)+1):end)];
    end
  case {'cpu_time'}
    repstr='cputime';outflag(1)=1;
    dummy=find(funstr{i}=='(');
    dummy=dummy(dummy>funstrwords_e{i}(j));
    dummy=dummy(1);
    rightparen=findrights_f(dummy,funstr{i});
    funstr{i}=[funstr{i}((dummy+1):rightparen-1),'=',repstr,funstr{i}((rightparen+1):end)];
  case {'cshift'}
    repstr='wshift';outflag(1)=1;
    dummy=find(funstr{i}=='(');
    dummy=dummy(dummy>funstrwords_e{i}(j));
    dummy=dummy(1);
    rightparen=findrights_f(dummy,funstr{i});
    funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),repstr,funstr{i}((funstrwords_e{i}(j)+1):dummy),'1,',funstr{i}((dummy+1):end)];
  case {'date'}
  case {'date_and_time'}
    outflag(1)=1;
    [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
    tempstr=[]; tempstr{1}=''; tempstr{2}=''; tempstr{3}=''; tempstr{4}='';
    if howmany>0
     funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j-1)-1)),subscripts{1},'=datestr(now,''yyyymmdd'');'];
    end
    if howmany>1
     [funstr{i+1:end+1}]=deal(funstr{i:end});
     funstr{i+1}=[subscripts{2},'=[datestr(now,''HHMMSS''),''.'',num2str(round((sum(clock)-fix(sum(clock)))*1000),3)];'];
    end
    if howmany>2
     %how to do this?
     %tempstr{3}=[funstr{i}(1:(funstrwords_b{i}(j-1)-1)),subscripts{1},'=feval(date);'];
    end
    if howmany>3
     [funstr{i+10:end+8}]=deal(funstr{i+2:end});
     funstr{i+2}=[subscripts{4},'(1)=str2num(datestr(now,''yyyy''));'];
     funstr{i+3}=[subscripts{4},'(2)=str2num(datestr(now,''mm''));'];
     funstr{i+4}=[subscripts{4},'(3)=str2num(datestr(now,''dd''));'];
     funstr{i+5}=[subscripts{4},'(4)=0;'];
     funstr{i+6}=[subscripts{4},'(5)=str2num(datestr(now,''HH''));'];
     funstr{i+7}=[subscripts{4},'(6)=str2num(datestr(now,''MM''));'];
     funstr{i+8}=[subscripts{4},'(7)=str2num(datestr(now,''SS''));'];
     funstr{i+9}=[subscripts{4},'(8)=round((sum(clock)-fix(sum(clock)))*1000);'];
    end
    %'ddddddddd222',funstr{i:i+12},kb
    if howmany>1
     needAllUpdated=1;
    end
    
%%%   if howmany>0
%%%   tempstr{1}=[funstr{i}(1:(funstrwords_b{i}(j-1)-1)),subscripts{1},'=datestr(now,''yyyymmdd'');'];
%%%  end
%%%  if howmany>1
%%%   tempstr{2}=[subscripts{2},'=[datestr(now,''HHMMSS''),''.'',num2str(round((sum(clock)-fix(sum(clock)))*1000),3)];'];
%%%  end
%%%  if howmany>2
%%%   %how to do this?
%%%   %tempstr{3}=[funstr{i}(1:(funstrwords_b{i}(j-1)-1)),subscripts{1},'=feval(date);'];
%%%  end
%%%  if howmany>3
%%%   tempstr{4}=[subscripts{4},'(1)=datestr(now,''yyyy'');',...
%%%               subscripts{4},'(2)=datestr(now,''mm'');',...
%%%               subscripts{4},'(3)=datestr(now,''dd'');',...
%%%               subscripts{4},'(4)=0;',...
%%%               subscripts{4},'(5)=datestr(now,''HH'');',...
%%%               subscripts{4},'(5)=datestr(now,''MM'');',...
%%%               subscripts{4},'(5)=datestr(now,''SS'');',...
%%%               subscripts{4},'(5)=num2str(round((sum(clock)-fix(sum(clock)))*1000),3);',...
%%%              ];
%%%  end
%%%  %'ddddddddd',funstr{i}
%%%  funstr{i}=[tempstr{1},tempstr{2},tempstr{3},tempstr{4},funstr{i}(parens(2)+1:end)];
%%%  %'ddddddddd',funstr{i},kb
%%% 
    
  case {'dble'}
    repstr='real';outflag(1)=1;
    [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
    funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),'(',subscripts{1},funstr{i}(parens(2):end)];
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany==1
%%%   funstr=replaceword_f(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);  
%%%  else
%%%   funstr{i}=[funstr{i}(1:parens(1)),subscripts{1},funstr{i}(parens(2):end)];
%%%  end
  case {'dcmplx'}
    repstr='complex';outflag(1)=1;
    funstr=replaceword_f(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);  
  case {'float','dfloat','qfloat'}
    repstr='';outflag(1)=1;
    funstr=replaceword_f(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);  
  case {'digits'}
  case {'dim','idim','jidim','hdim','iidim','bdim1','ddim','qdim'}
    outflag(1)=1;
    temp1=find(~isspace(funstr{i}));  temp1=temp1(temp1>funstrwords_e{i}(j));
    if ~isempty(temp1)
     if funstr{i}(temp1(1))~='='
      [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
      if howmany==2
       funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),'max(',subscripts{1},'-',subscripts{2},',zeros(size(',subscripts{1},')))',funstr{i}(parens(2)+1:end)];
      else
       warning(['~=2 inputs to fortran''s ',funstrwords{i}{j},' function. ',funstrwords{i}{j},' used as a variable? No action taken.']);
      end
     end
    end
  case {'dot_product'}
    repstr='dot';outflag(1)=1;
    funstr=replaceword_f(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);  
  case {'dprod','qprod'}
    repstr='times';outflag(1)=1;
    funstr=replaceword_f(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);  
  case {'eof'}
  case {'eoshift'}
  case {'epsilon'}
    warn=['No Matlab function for ''epsilon'' except replacing with ''eps'' in Matlab.'];
    %'ddddddddd',kb
    f2mWarning(warn,funstr,i,funstrwords_b{i}(j));
    repstr='eps';outflag(1)=1;
    dummy=find(funstr{i}=='(');
    dummy=dummy(dummy>funstrwords_e{i}(j));
    dummy=dummy(1);
    rightparen=findrights_f(dummy,funstr{i});
    funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),repstr,funstr{i}((rightparen+1):end)];
  case {'errsns'}
  case {'exit'}
    repstr='break';outflag(1)=1;
    funstr=replaceword_f(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);  
  case {'exp'}
    outflag(1)=1;
  case {'dexp','cexp','zexp','cdexp','qexp'}
    repstr='exp';outflag(1)=1;
    funstr=replaceword_f(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);  
  case {'exponent'}
  case {'floor'}
    outflag(1)=1;
  case {'fp_class'}
  case {'fraction'}
  case {'free'}
  case {'getarg'}
    % interesting one where we are modifying the line with "call" still there
    outflag(1)=1;
    [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
    if howmany==2
     funstr{i}=[funstr{i}(1:funstrwords_b{i}(j-1)-1),'[',strtrim(subscripts{2}),'] = ',funstr{i}(funstrwords_b{i}(j):end)];
    elseif howmany==3
     funstr{i}=[funstr{i}(1:funstrwords_b{i}(j-1)-1),'[',strtrim(subscripts{2}),',',strtrim(subscripts{3}),'] = ',funstr{i}(funstrwords_b{i}(j):end)];
    end
  case {'huge'}
    [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
    if howmany>0
     disp('No Matlab function for ''huge'' except replacing with ''realmax'' in Matlab');
     repstr='realmax';outflag(1)=1;
%%%   funstr=replaceword_f(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);
     dummy=find(funstr{i}=='(');
     dummy=dummy(dummy>funstrwords_e{i}(j));
     dummy=dummy(1);
     rightparen=findrights_f(dummy,funstr{i});
     funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),repstr,funstr{i}((rightparen+1):end)];
    else
     outflag(1)=1;
    end
  case {'ichar','iachar'}
    repstr='double';outflag(1)=1;
    funstr=replaceword_f(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);  
  case {'iand'}
    repstr='bitand';outflag(1)=1;
    funstr=replaceword_f(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);  
  case {'iargcount'}
  case {'iargptr'}
  case {'ibchng'}
  case {'ibclr'}
    repstr='bitset';outflag(1)=1;
    dummy=find(funstr{i}=='(');
    dummy=dummy(dummy>funstrwords_e{i}(j));
    dummy=dummy(1);
    rightparen=findrights_f(dummy,funstr{i});
    funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),repstr,funstr{i}((funstrwords_e{i}(j)+1):rightparen-1),',0',funstr{i}((rightparen):end)];
  case {'ibits'}
  case {'ibset'}
    repstr='bitset';outflag(1)=1;
    dummy=find(funstr{i}=='(');
    dummy=dummy(dummy>funstrwords_e{i}(j));
    dummy=dummy(1);
    rightparen=findrights_f(dummy,funstr{i});
    funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),repstr,funstr{i}((funstrwords_e{i}(j)+1):rightparen-1),funstr{i}((rightparen):end)];
  case {'idate'}
  case {'ieor'}
    repstr='bitxor';outflag(1)=1;
    funstr=replaceword_f(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);  
  case {'ilen'}
  case {'index'}
    repstr='regexpi';
    [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
    if howmany~=0
     outflag(1)=1;
     funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),'empt0(',repstr,funstr{i}((funstrwords_e{i}(j)+1):parens(2)-1),',''once''))',funstr{i}(parens(2)+1:end)];
    end
  case {'ior'}
    repstr='bitor';outflag(1)=1;
    funstr=replaceword_f(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);  
  case {'inquire'}
    outflag(1)=1;
    [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
    if howmany==2
     funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),...
                subscripts{2},'=logical(exist(',subscripts{1},',''file''));',...
                funstr{i}(parens(2)+1:end)];
    else
     warn=['inquire -- f2matlab can only handle inquire(file=,exists=) at the moment'];
     f2mWarning(warn,funstr,i,funstrwords_b{i}(j));
    end
  case {'isha'}
  case {'ishc'}
  case {'ishft'}
  case {'ishftc'}
  case {'ishl'}
  case {'isnan'}
    outflag(1)=1;
  case {'kind'} %should never impact anything, but
    outflag(1)=1;
    [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
    funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),funstr{i}(parens(1):end)];
  case {'lbound'}
  case {'leadz'}
  case {'len'}
    repstr='length';outflag(1)=1;
    funstr=replaceword_f(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);  
  case {'len_trim'}
    repstr='length';outflag(1)=1;
    dummy=find(funstr{i}=='(');
    dummy=dummy(dummy>funstrwords_e{i}(j));
    dummy=dummy(1);
    rightparen=findrights_f(dummy,funstr{i});
    funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),repstr,funstr{i}((funstrwords_e{i}(j)+1):dummy),'deblank(',funstr{i}((dummy+1):rightparen),')',funstr{i}((rightparen+1):end)];
  case {'lge'}
    outflag(1)=1;  extraFunctions=1;
    [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
    funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),'(strlexcmp(',subscripts{1},', ',subscripts{2},') >= 0)',funstr{i}(parens(2)+1:end)];
  case {'lgt'}
    outflag(1)=1;  extraFunctions=1;
    [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
    funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),'(strlexcmp(',subscripts{1},', ',subscripts{2},') > 0)',funstr{i}(parens(2)+1:end)];
  case {'lle'}
    outflag(1)=1;  extraFunctions=1;
    [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
    funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),'(strlexcmp(',subscripts{1},', ',subscripts{2},') <= 0)',funstr{i}(parens(2)+1:end)];
  case {'llt'}
    outflag(1)=1;  extraFunctions=1;
    [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
    funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),'(strlexcmp(',subscripts{1},', ',subscripts{2},') < 0)',funstr{i}(parens(2)+1:end)];
  case {'log'}
    outflag(1)=1;
  case {'alog','dlog','clog','zlog','cdlog','qlog'}
    repstr='log';outflag(1)=1;
    funstr=replaceword_f(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);  
  case {'log10'}
    outflag(1)=1;
  case {'alog10','dlog10','qlog10'}
    repstr='log10';outflag(1)=1;
    funstr=replaceword_f(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);  
  case {'logical'}
    outflag(1)=1; %just do away with the second subscript of present
    [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
    if howmany>1
     funstr{i}=[funstr{i}(1:centercomma(1)-1),funstr{i}(parens(2):end)];
    end
  case {'malloc'}
  case {'matmul'}
    outflag(1)=1;
    [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if strcmp(funstrwords{i}{1},'a')
%%%   'mmmmmmmmmmmm',funstr{i},keyboard
%%%  end
    funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),funstr{i}((funstrwords_e{i}(j)+1):centercomma-1),'#####_#####',funstr{i}((centercomma+1):end)];
  case {'max','max0','imax0','jmax0','amax1','dmax1','amax0','aimax0','ajmax0','max1','imax1','jmax1','qmax1'}
    %'mmmmmmmmmmmm',funstr{i},kb
    [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
    repstr='max';outflag(1)=1;
    if howmany>2
     dummy=find(funstr{i}=='('); dummy=dummy(dummy>funstrwords_e{i}(j)); dummy=dummy(1);
     rightparen=findrights_f(dummy,funstr{i});
     funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),repstr,funstr{i}((funstrwords_e{i}(j)+1):dummy),'[',funstr{i}((dummy+1):rightparen-1),']',funstr{i}((rightparen):end)];
    else
     funstr=replaceword_f(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);  
    end
  case {'maxexponent'}
  case {'maxloc'} %only works with 2 arguments (and dim is second) 
                  %AND there is an equals sign right before this
    outflag(1)=1; 
    [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
    if howmany==2
     eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
     if eqLoc~=-1
      funstr{i}=['[tempOut,',funstr{i}(1:eqLoc-1),']=max(',strtrim(subscripts{1}),',[],',strtrim(subscripts{2}),')',funstr{i}(parens(2)+1:end)];
     end
    end
  case {'maxval'}
    repstr='max';outflag(1)=1;
    [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
    funstr(i+1:end+1)=funstr(i:end);
    temp3=1; 
    funstr{i}=['tempmaxval=',subscripts{1},';'];
    if howmany>1
     temp1=subscripts{howmany};
     temp2=howmany-1;
     nWords=find(funstrwords_b{i}>centercomma(end) & funstrwords_b{i}<parens(2));
     nNums =find(funstrnumbers_b{i}>centercomma(end) & funstrnumbers_b{i}<parens(2));
     temp4=0;
     if (length(nWords)>1 || length(nNums)>1) || ...
          (any(temp1=='<' | temp1=='>' | temp1=='=' | temp1=='~'))%criterion to be a mask
      temp4=1;
     end
     if temp4 | howmany==3
      funstr{i}=[funstr{i},' tempmaxval(~(',subscripts{2},'))=nan;'];
     end
     if howmany==2
      if temp4
       funstr{i+1}=[funstr{i+1}(1:(funstrwords_b{i}(j)-1)),repstr,'(tempmaxval(:))',...
                    funstr{i+1}(parens(2)+1:end)];
      else %it's a dim
       funstr{i+1}=[funstr{i+1}(1:(funstrwords_b{i}(j)-1)),repstr,'(tempmaxval,[],',...
                    subscripts{2},')',funstr{i+1}(parens(2)+1:end)];
      end
     elseif howmany==3
      funstr{i+1}=[funstr{i+1}(1:(funstrwords_b{i}(j)-1)),repstr,'(tempmaxval,[],',...
                   subscripts{2},')',funstr{i+1}(parens(2)+1:end)];
     end 
    else
     funstr{i+1}=[funstr{i+1}(1:(funstrwords_b{i}(j)-1)),repstr,'(tempmaxval(:))',...
                  funstr{i+1}(parens(2)+1:end)];
    end
    %'ddddddddd123',funstr{i},funstr{i+1},kb
  case {'merge'}
    outflag(1)=1;
    [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
    funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),'(',...
               '(',subscripts{1},')','*','(',subscripts{3},')+',...
               '(',subscripts{2},')','*','~(',subscripts{3},')',...
               ')',...
               funstr{i}(parens(2)+1:end)];
  case {'min','min0','imin0','jmin0','amin1','dmin1','amin0','aimin0','ajmin0','min1','imin1','jmin1','qmin1'}
    [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
    repstr='min';outflag(1)=1;
    if howmany>2
     dummy=find(funstr{i}=='('); dummy=dummy(dummy>funstrwords_e{i}(j)); dummy=dummy(1);
     rightparen=findrights_f(dummy,funstr{i});
     funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),repstr,funstr{i}((funstrwords_e{i}(j)+1):dummy),'[',funstr{i}((dummy+1):rightparen-1),']',funstr{i}((rightparen):end)];
    else
     funstr=replaceword_f(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);  
    end
  case {'minexponent'}
  case {'minloc'}%only works with 2 arguments (and dim is second) 
                 %AND there is an equals sign right before this
    outflag(1)=1; 
    [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
    if howmany==2
     eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
     if eqLoc~=-1
      funstr{i}=['[tempOut,',funstr{i}(1:eqLoc-1),']=min(',strtrim(subscripts{1}),',[],',strtrim(subscripts{2}),')',funstr{i}(parens(2)+1:end)];
     end
    end
  case {'minval'}
    repstr='min';outflag(1)=1;
    [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
    funstr(i+1:end+1)=funstr(i:end);
    temp3=1; 
    funstr{i}=['tempminval=',subscripts{1},';'];
    if howmany>1
     temp1=subscripts{howmany};
     temp2=howmany-1;
     nWords=find(funstrwords_b{i}>centercomma(end) & funstrwords_b{i}<parens(2));
     nNums =find(funstrnumbers_b{i}>centercomma(end) & funstrnumbers_b{i}<parens(2));
     temp4=0;
     if (length(nWords)>1 || length(nNums)>1) || ...
          (any(temp1=='<' | temp1=='>' | temp1=='=' | temp1=='~'))%criterion to be a mask
      temp4=1;
     end
     if temp4 | howmany==3
      funstr{i}=[funstr{i},' tempminval(~(',subscripts{2},'))=nan;'];
     end
     if howmany==2
      if temp4
       funstr{i+1}=[funstr{i+1}(1:(funstrwords_b{i}(j)-1)),repstr,'(tempminval(:))',...
                    funstr{i+1}(parens(2)+1:end)];
      else %it's a dim
       funstr{i+1}=[funstr{i+1}(1:(funstrwords_b{i}(j)-1)),repstr,'(tempminval,[],',...
                    subscripts{2},')',funstr{i+1}(parens(2)+1:end)];
      end
     elseif howmany==3
      funstr{i+1}=[funstr{i+1}(1:(funstrwords_b{i}(j)-1)),repstr,'(tempminval,[],',...
                   subscripts{2},')',funstr{i+1}(parens(2)+1:end)];
     end 
    else
     funstr{i+1}=[funstr{i+1}(1:(funstrwords_b{i}(j)-1)),repstr,'(tempminval(:))',...
                  funstr{i+1}(parens(2)+1:end)];
    end
  case {'mod','jmod','hmod','imod','bmod1','amod','dmod','qmod'}
    repstr='rem';outflag(1)=1;
    funstr=replaceword_f(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);  
  case {'module'}
    outflag(1)=1;
    funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),'%%%--- ',funstr{i}(funstrwords_b{i}(j):end)];
  case {'modulo'}
    repstr='mod';outflag(1)=1;
    funstr=replaceword_f(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);  
  case {'mult_high'}
  case {'mvbits'}
  case {'nearest'}
  case {'nint','inint','jnint','idnint','iidnnt','jidnnt','iqnint','iiqnnt','jiqnnt'}
    repstr='round';outflag(1)=1;
    funstr=replaceword_f(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);  
  case {'not'}
    if funstr{i}(funstrwords_b{i}(j)-1)~='.'
     repstr='bitcmp';outflag(1)=1;
     funstr=replaceword_f(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);  
    end
  case {'null'}
  case {'nworkers'}
  case {'pack'}
    outflag(1)=1;
    dummy=find(funstr{i}=='('); dummy=dummy(dummy>funstrwords_e{i}(j)); dummy=dummy(1);
    rightparen=findrights_f(dummy,funstr{i});
    [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
    if ~isempty(centercomma)
     funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),subscripts{1},'(:)',funstr{i}(centercomma(1):end)];
    else
     funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),subscripts{1},'(:)',funstr{i}(parens(2)+1:end)];
    end
  case {'popcnt'}
  case {'poppar'}
  case {'precision'}
  case {'present'}
    outflag(1)=1;
    [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
    if howmany>0
     %which input arg is it?
     temp1=find(strcmpi(strtrim(subscripts{1}),{funstrwords{fundecline}{funargs}}));
     if ~isempty(temp1) && length(temp1)==1
      funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),'(exist(''',subscripts{1},''',''var'')&&(~isempty(',subscripts{1},')||~isempty(inputname(',num2str(temp1),'))))',funstr{i}(parens(2)+1:end)];
     else
      funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),'(exist(''',subscripts{1},''',''var'')&&~isempty(',subscripts{1},'))',funstr{i}(parens(2)+1:end)];
     end
    end
%%%  repstr='exist';outflag(1)=1;
%%%  dummy=find(funstr{i}=='(');dummy=dummy(dummy>funstrwords_e{i}(j));dummy=dummy(1);
%%%  rightparen=findrights_f(dummy,funstr{i});
%%%  funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),repstr,'(''',funstr{i}(dummy+1:rightparen-1),''',''var'')',funstr{i}(rightparen+1:end)];
  case {'processors_shape'}
  case {'product'}
    repstr='prod';outflag(1)=1;
    funstr=replaceword_f(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);  
  case {'qcmplx'}
    repstr='complex';outflag(1)=1;
    funstr=replaceword_f(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);  
  case {'qext'}
    repstr='double';outflag(1)=1;
    funstr=replaceword_f(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);  
  case {'radix'}
%%% case {'ran'}
%%%  repstr='rand';outflag(1)=1;
%%%  dummy=find(funstr{i}=='('); dummy=dummy(dummy>funstrwords_e{i}(j)); dummy=dummy(1);
%%%  rightparen=findrights_f(dummy,funstr{i});
%%%  funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),repstr,funstr{i}(dummy:end)];
  case {'random'}
  case {'random_number'}
    repstr='rand';outflag(1)=1;
    dummy=find(funstr{i}=='('); dummy=dummy(dummy>funstrwords_e{i}(j)); dummy=dummy(1);
    rightparen=findrights_f(dummy,funstr{i});
    [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
    %funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),repstr,'(size(',funstr{i}(parens(1)+1:parens(2)-1),'))'];
    tempstr=strrep(strrep(funstr{i}(parens(1)+1:parens(2)-1),'harvest',''),'=','');
    funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j-1)-1)),tempstr,'=',repstr,'(size(',tempstr,'));'];
  case {'random_seed'}
    %uses matlab v4 generator
    repstr='rand';outflag(1)=1;
    [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
    if howmany==0
     funstr{i}=['rand(''seed'',sum(100000*clock))'];
    elseif howmany==1
     % is this a size, get, or put?
     temp=find(strcmp(funstrwords{i},'size'));
     temp=temp(funstrwords_b{i}(temp)>parens(1) & funstrwords_b{i}(temp)<parens(2));
     if ~isempty(temp)
      %?????
      temp=find(funstrwords_b{i}>parens(1) & funstrwords_b{i}<parens(2));
      temp=temp(end);
      funstr{i}=[funstrwords{i}{temp},'=1;'];
     end % if ~isempty
     temp=find(strcmp(funstrwords{i},'get'));
     temp=temp(funstrwords_b{i}(temp)>parens(1) & funstrwords_b{i}(temp)<parens(2));
     if ~isempty(temp)
      temp=find(funstrwords_b{i}>parens(1) & funstrwords_b{i}<parens(2));
      temp=temp(end);
      funstr{i}=[funstrwords{i}{temp},'=rand(''seed'');'];
     end % if ~isempty   
     temp=find(strcmp(funstrwords{i},'put'));
     temp=temp(funstrwords_b{i}(temp)>parens(1) & funstrwords_b{i}(temp)<parens(2));
     if ~isempty(temp)
      temp=find(funstrwords_b{i}>parens(1) & funstrwords_b{i}<parens(2));
      temp=temp(end);    
      funstr{i}=['rand(''seed'',',funstrwords{i}{temp},');'];
     end % if ~isempty
    end % if howmany==0
        %'3333333333',funstr{i},keyboard 
  case {'randu'}
  case {'range'}
  case {'real','dreal','qreal'}
    repstr='real';outflag(1)=1;
    [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
    if howmany==1
     funstr=replaceword_f(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);  
    else
     funstr{i}=[funstr{i}(1:parens(1)),subscripts{1},funstr{i}(parens(2):end)];
    end
  case {'repeat'}
    repstr='repmat';outflag(1)=1;
    [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
    dummy=find(funstr{i}=='(');
    dummy=dummy(dummy>funstrwords_e{i}(j));
    dummy=dummy(1);
    rightparen=findrights_f(dummy,funstr{i});
    funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),repstr,funstr{i}((funstrwords_e{i}(j)+1):centercomma(end)),'1,',funstr{i}(centercomma(end)+1:end)];
  case {'reshape'}
    outflag(1)=1;
  case {'rrspacing'}
  case {'scale'}
  case {'scan'}
    repstr='findstr';outflag(1)=1;
    funstr=replaceword_f(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);  
  case {'secnds'}
  case {'selected_int_kind'}
    repstr='4';outflag(1)=1;
    [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
    funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),repstr,funstr{i}(parens(2)+1:end)];
  case {'selected_real_kind'}
    repstr='8';outflag(1)=1;
    [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
    funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),repstr,funstr{i}(parens(2)+1:end)];
  case {'shape'}
    outflag(1)=1;
    %'dggggggggggg',fun_name,funstr{i},kb
    if ~any(strcmp(funstrwords{i}{j},fun_name))
     repstr='size';
     funstr=replaceword_f(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);  
    else%if any(strcmp(funstrwords{i}{j},{localVar{:,1}}))
     repstr=['shape',MLapp];
     funstr=replaceword_f(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);  
    end
  case {'sign','isign','jisign','hsign','iisign','kisign','dsign','qsign'}
    [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
    if howmany>0
     repstr='sign';outflag(1)=1;
     if howmany==2
      funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),'(abs(',subscripts{1},...
                 ')*sign(',subscripts{2},'))',funstr{i}(parens(2)+1:end)];
     else
      error(['Fortran ',repstr,' function does not have 2 inputs'])
     end
    else
     outflag(1)=1;
    end
  case {'sin'}
    outflag(1)=1;
  case {'dsin','csin','zsin','cdsin','qsin'}
    repstr='sin';outflag(1)=1;
    funstr=replaceword_f(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);  
  case {'sind','dsind','qsind'}
    repstr='sin';outflag(1)=1;
    dummy=find(funstr{i}=='(');
    dummy=dummy(dummy>funstrwords_e{i}(j));
    dummy=dummy(1);
    rightparen=findrights_f(dummy,funstr{i});
    funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),repstr,funstr{i}((funstrwords_e{i}(j)+1):dummy),'pi/180*(',funstr{i}((dummy+1):rightparen),')',funstr{i}((rightparen+1):end)];
  case {'sinh'}
    outflag(1)=1;
  case {'dsinh','qsinh'}
    repstr='sinh';outflag(1)=1;
    funstr=replaceword_f(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);  
  case {'size'}
    outflag(1)=1;
    [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
    if howmany==1
     funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),'prod(',funstr{i}(funstrwords_b{i}(j):parens(2)),')',funstr{i}(parens(2)+1:end)];
    elseif howmany==2
     temp=find((funstrnumbers_b{i}>centercomma)&(funstrnumbers_b{i}<parens(2)));
     funstr{i}=[funstr{i}(1:centercomma),funstrnumbers{i}{temp(end)},funstr{i}(parens(2):end)];
    end
  case {'sizeof'}
  case {'sngl'}
    outflag(1)=1;
    funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),funstr{i}(funstrwords_e{i}(j)+1:end)];
  case {'spacing'}
  case {'spread'}
    outflag(1)=1;
    [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
    fid=''; 
    temp=str2num(subscripts{2});
    if ~isempty(temp) && temp==2, fid='.'''; end
    funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),'repmat(',subscripts{1},',',...
               subscripts{3},',1)',fid,funstr{i}(parens(2)+1:end)];
  case {'sqrt'}
    outflag(1)=1;
  case {'dsqrt','csqrt','zsqrt','cdsqrt','qsqrt'}
    repstr='sqrt';outflag(1)=1;
    funstr=replaceword_f(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);  
  case {'sum'}
    outflag(1)=1;
    [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
    %'ffffffffff',funstr{i},kb
    if howmany==2 %if it has a mask, if words or more than 1 number
     temp1=','; temp2=''; temp3=''; temp4='';
     nWords=find(funstrwords_b{i}>centercomma(1) & funstrwords_b{i}<parens(2));
     nNums =find(funstrnumbers_b{i}>centercomma(1) & funstrnumbers_b{i}<parens(2));
     if ~isempty(nWords) || length(nNums)>1
      temp1='*'; temp2='sum'; temp3=')'; temp4='(';
     end
     %we have to add sum twice in case it is a 2-d array. Won't sum 3d or higher...
     funstr{i}=[funstr{i}(1:parens(1)),temp2,temp4,temp4,...
                funstr{i}(parens(1)+1:centercomma(1)-1),temp3,temp1,temp4,...
                funstr{i}(centercomma(1)+1:parens(2)-1),temp3,temp3,funstr{i}(parens(2):end)];
    elseif howmany==1
     funstr{i}=[funstr{i}(1:parens(1)),'sum(',funstr{i}(parens(1)+1:parens(2)-1),')',...
                funstr{i}(parens(2):end)];    
    end
    
%%%  if howmany==1
%%%   %we have to add sum twice in case it is a 2-d array. Won't sum 3d or higher...
%%%   funstr{i}=[funstr{i}(1:parens(1)),'sum(',funstr{i}(parens(1)+1:parens(2)-1),')',...
%%%              funstr{i}(parens(2):end)];
%%%  end
  case {'system'}
    repstr='!';outflag(1)=1;
    funstr=replaceword_f(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);  
  case {'system_clock'}
    outflag(1)=1;
    [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
    switch howmany
      case 0
        funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),'floor(datenum(now))',funstr{i}(funstrwords_e{i}(j)+1:end)];
      case 1
        funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),subscripts{1},'=floor(datenum(now));',funstr{i}(parens(2)+1:end)];
      case 2
        [funstr{i+1:end+1}]=deal(funstr{i:end});
        funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),subscripts{1},'=floor(datenum(now));'];
        funstr{i+1}=[subscripts{2},'=1000000;',funstr{i}(parens(2)+1:end)];
      case 3
        [funstr{i+3:end+2}]=deal(funstr{i+1:end});
        funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),subscripts{1},'=floor(datenum(now));'];
        funstr{i+1}=[subscripts{2},'=1000000;',funstr{i}(parens(2)+1:end)];
        funstr{i+2}=[subscripts{3},'=2147483648;',funstr{i}(parens(2)+1:end)];
    end
    if howmany>1,   needAllUpdated=1;    end
  case {'tan'}
    outflag(1)=1;
  case {'dtan','ctan','ztan','qtan'}
    repstr='tan';outflag(1)=1;
    funstr=replaceword_f(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);  
  case {'tand','dtand','qtand'}
    repstr='tan';outflag(1)=1;
    dummy=find(funstr{i}=='('); dummy=dummy(dummy>funstrwords_e{i}(j)); dummy=dummy(1);
    rightparen=findrights_f(dummy,funstr{i});
    funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),repstr,funstr{i}((funstrwords_e{i}(j)+1):dummy),'pi/180*(',funstr{i}((dummy+1):rightparen),')',funstr{i}((rightparen+1):end)];
  case {'tanh'}
    outflag(1)=1;
  case {'dtanh','qtanh'}
    repstr='tanh';outflag(1)=1;
    funstr=replaceword_f(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);  
  case {'time'}
%%%  'dddddddd',kb
%%%  outflag(1)=1;
%%%  funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),'datestr(rem(now,1))',funstr{i}(funstrwords_e{i}(j)+1:end)];
  case {'tiny'}
    [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
    if howmany>0
     disp('No Matlab function for ''tiny'' except replacing with ''realmin'' in Matlab');
     repstr='realmin';outflag(1)=1;
     funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),repstr,funstr{i}(parens(2)+1:end)];
    else
     outflag(1)=1;
    end
  case {'trailz'}
  case {'transfer'}
  case {'transpose'}
    outflag(1)=1;
    dummy=find(funstr{i}=='('); dummy=dummy(dummy>funstrwords_e{i}(j)); dummy=dummy(1);
    rightparen=findrights_f(dummy,funstr{i});
    funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),funstr{i}(dummy:rightparen),'$$$$$_$$$$$',funstr{i}((rightparen+1):end)];
  case {'trim'}
    repstr='deblank';outflag(1)=1;
    funstr=replaceword_f(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);  
  case {'ubound'}
    repstr='size';outflag(1)=1;
    funstr=replaceword_f(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);  
  case {'unpack'}
  case {'val','ref'} %we just have to get rid of the .val() type constructs
    outflag(1)=1;
    goon=0;
    temp=lastNonSpace(funstr{i},funstrwords_b{i}(j));
    if ~isempty(temp)
     temp1=lastNonSpace(funstr{i},temp);
     if ~isempty(temp1)
      if ~iskeep_f(funstr{i}(temp1))
       temp2=nextNonSpace(funstr{i},funstrwords_e{i}(j));
       if ~isempty(temp2) && strcmp(funstr{i}(temp2),'(')
        goon=1;
       end % if ~isempty(temp2) && strcmp(funstr{i}(temp2),
      end % if ~iskeep_f(funstr{i}(temp1))
     end % if ~isempty(temp1)
    end % if ~isempty(temp)
    if goon
     [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
     funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-2)),funstr{i}(parens(1)+1:parens(2)-1),...
                funstr{i}(parens(2)+1:end)];
    end
    %funstrwords{i}{j},j,goon,   funstr{i},'fffffffffval',kb
  case {'loc'} %we just have to get rid of the .val() type constructs
    outflag(1)=1;
    goon=0;
    temp=lastNonSpace(funstr{i},funstrwords_b{i}(j));
    if ~isempty(temp)
     temp1=lastNonSpace(funstr{i},temp);
     if ~isempty(temp1)
      if ~iskeep_f(funstr{i}(temp1))
       temp2=nextNonSpace(funstr{i},funstrwords_e{i}(j));
       if ~isempty(temp2) && strcmp(funstr{i}(temp2),'(')
        goon=1;
       end % if ~isempty(temp2) && strcmp(funstr{i}(temp2),
      end % if ~iskeep_f(funstr{i}(temp1))
     end % if ~isempty(temp1)
    end % if ~isempty(temp)
    if goon
     [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
     %if there is a CPX in this line, then get rid of it, otherwise set it to an function handle
     if ~isempty(regexpi(funstr{i},'cpx'))
      funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-2)),funstr{i}(parens(1)+1:parens(2)-1),...
                 funstr{i}(parens(2)+1:end)];
     else %function handle
      funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-2)),'@',funstr{i}(parens(1)+1:parens(2)-1),...
                 funstr{i}(parens(2)+1:end)];
     end
    end
    %funstrwords{i}{j},j,goon,   funstr{i},'fffffffffval',kb
  case {'verify'}
  case {'zext'}


%%%%%%%%%% for use when converting fortran programs using CPLEX
%%% case {'cpxpsn'}
%%%  outflag(1)=1;
%%%  eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%  argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');
%%%  funstr{i}=[funstr{i}(1:funstrwords_b{i}(argWordOut)-1),'%',...
%%%             funstr{i}(funstrwords_b{i}(argWordOut):end)];
%%% case {'cpxopencplex','cpxclosecplex'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%  argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');
%%%  funstr{i}=[funstr{i}(1:funstrwords_b{i}(argWordOut)-1),'[',funstrwords{i}{argWordOut},...
%%%             ',',strtrim(subscripts{1}),'] = ',...
%%%             funstr{i}(funstrwords_b{i}(j):end)];
%%% case {'cpxsetintparam'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%  argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');
%%%  funstr{i}=[funstr{i}(1:funstrwords_b{i}(argWordOut)-1),'[',funstrwords{i}{argWordOut},...
%%%             ',',strtrim(subscripts{1}),'] = ',...
%%%             funstr{i}(funstrwords_b{i}(j):centercomma(1)),...
%%%             '''',strtrim(subscripts{2}),'''',funstr{i}(centercomma(2):end)];
%%% case {'cpxsetdblparam'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%  argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');
%%%  funstr{i}=[funstr{i}(1:funstrwords_b{i}(argWordOut)-1),'[',funstrwords{i}{argWordOut},...
%%%             ',',strtrim(subscripts{1}),'] = ',...
%%%             funstr{i}(funstrwords_b{i}(j):centercomma(1)),...
%%%             '''',strtrim(subscripts{2}),'''',funstr{i}(centercomma(2):end)];
%%% case {'cpxcreateprob','cpxcopylp','cpxcopyqpsep','cpxwriteprob','cpxqpwrite','cpxmbasewrite','cpxcopyctype','cpxchgbds','cpxcopyorder','cpxcopysos','cpxmipopt','cpxlpopt','cpxfreeprob','cpxprimopt','cpxbaropt'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%  argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');
%%%  funstr{i}=[funstr{i}(1:funstrwords_b{i}(argWordOut)-1),'[',funstrwords{i}{argWordOut},...
%%%             ',',strtrim(subscripts{2}),'] = ',...
%%%             funstr{i}(funstrwords_b{i}(j):end)];
%%% case {'cpxgetnumcols'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),strtrim(subscripts{2}),'.numcols;',...
%%%            funstr{i}(parens(2)+1:end)];
%%% case {'cpxgetnumrows'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),strtrim(subscripts{2}),'.numrows;',...
%%%            funstr{i}(parens(2)+1:end)];
%%% case {'cpxgetx','cpxgetmipx'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%  argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');
%%%  funstr{i}=[funstr{i}(1:funstrwords_b{i}(argWordOut)-1),strtrim(subscripts{3}),'=',...
%%%             strtrim(subscripts{2}),'.x( ',...
%%%             '(',strtrim(subscripts{4}),')+1 : (',strtrim(subscripts{5}),')+1 );',...
%%%             funstr{i}(parens(2)+1:end)];
%%% case {'cpxgetobjval','cpxgetmipobjval'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%  argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');
%%%  funstr{i}=[funstr{i}(1:funstrwords_b{i}(argWordOut)-1),strtrim(subscripts{3}),'=',...
%%%             strtrim(subscripts{2}),'.objval;',funstr{i}(parens(2)+1:end)];
%%% case {'cpxgetstat'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),strtrim(subscripts{2}),'.status;',...
%%%            funstr{i}(parens(2)+1:end)];
%%%  %funstr{i},  'ddddddddd',kb
%%%
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%% other specifics to 07 consulting
%%% case {'str_upcase'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  funstr{i}=[subscripts{1},'=upper(',strtrim(subscripts{2}),');',funstr{i}(parens(2)+1:end)];
%%% case {'charupper'}
%%%  repstr='upper';outflag(1)=1;
%%%  funstr=replaceword_f(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);    
%%% case {'slot2str'}
%%%  repstr='int2str';outflag(1)=1;
%%%  funstr=replaceword_f(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);    
%%%%%% case {'str2integer'}
%%%%%%  outflag(1)=1;
%%%%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%%%%  funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),'fix(str2num(',strtrim(subscripts{1}),'))',funstr{i}(parens(2)+1:end)];
%%%%%% case {'str2real'}
%%%%%%  outflag(1)=1;
%%%%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%%%%  funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),'str2num(',strtrim(subscripts{1}),')',funstr{i}(parens(2)+1:end)];
%%%%%% case {'computeweightedaverage'}
%%%%%%  outflag(1)=1;
%%%%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%%%%  funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),'mfwgtdavg',funstr{i}(parens(1):centercomma(end)-1),')',funstr{i}(parens(2)+1:end)];  
%%% case {'beep'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany==1
%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),'for beepN=1:(',strtrim(subscripts{1}),'),beep; end;',funstr{i}(parens(2)+1:end)];
%%%  end
%%% case {'character_binsearch'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0
%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(j-1)-1),strtrim(subscripts{4}),' = ',funstrwords{i}{j},funstr{i}(parens(1):centercomma(2)-1),funstr{i}(parens(2):end)];
%%%  end
%%% case {'location'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0
%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),'character_binsearch',funstr{i}(parens(1):centercomma(2)-1),funstr{i}(parens(2):end)];
%%%  end
%%%%%% case {'output_msg'}
%%%%%%  %case {'gmo_msg','output_msg'}
%%%%%%  outflag(1)=1;
%%%%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%%%%  funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),'gmomsg(',strtrim(subscripts{1}),funstr{i}(parens(2):end)];
%%% case {'gmo_msg'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0 && j>1 && strcmp(funstrwords{i}{j-1},'call')
%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(j-1)-1),funstr{i}(funstrwords_b{i}(j):end)];
%%%  end
%%% case {'compute_turnover'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0 & j>1
%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),'turnover',funstr{i}(parens(1):centercomma(end)-1),')',funstr{i}(parens(2)+1:end)];  
%%%  end
%%% case {'get_user_existing_filename'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),'error([''Could not find the template file: '',',strtrim(subscripts{2}),'])',funstr{i}(parens(2)+1:end)];
%%%%%% case {'gmo_global_init'}
%%%%%%  outflag(1)=1;
%%%%%%  if j>1 && strcmp(funstrwords{i}{j-1},'call')
%%%%%%   [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(j-1)-1),'set_domain',funstr{i}(parens(1):end)];
%%%%%%  end
%%% case {'readobjectfromfile'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0
%%%   eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%   argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');
%%%   funstr(i+1:end+1)=funstr(i:end);
%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(argWordOut)-1),strtrim(subscripts{2}),'=',...
%%%              'readobject(',strtrim(subscripts{1}),')',funstr{i}(parens(2)+1:end)];
%%%   funstr{i+1}=[funstrwords{i}{argWordOut},'=0;'];
%%%  end
%%%%%% case {'get_data_type'}
%%%%%%  outflag(1)=1;
%%%%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%%%%  if howmany>0
%%%%%%   eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%%%%   argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');
%%%%%%   funstr(i+1:end+1)=funstr(i:end);
%%%%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(argWordOut)-1),strtrim(subscripts{2}),'=',...
%%%%%%              'get_data_type(',strtrim(subscripts{1}),')',funstr{i}(parens(2)+1:end)];
%%%%%%   funstr{i+1}=[funstrwords{i}{argWordOut},'=0;'];
%%%%%%  end
%%% case {'getattributecount'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0
%%%   eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%   %funstr{i},'wwwwwwwwwwww',kb
%%%   if eqLoc~=-1
%%%    argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');
%%%    funstr(i+1:end+1)=funstr(i:end);
%%%    funstr{i}=[funstr{i}(1:funstrwords_b{i}(argWordOut)-1),strtrim(subscripts{3}),'=',...
%%%               'getattrcount(',funstr{i}(parens(1)+1:centercomma(end)-1),...
%%%               funstr{i}(parens(2):end)];
%%%    funstr{i+1}=[funstrwords{i}{argWordOut},'=0;'];
%%%   else
%%%    funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),...
%%%               'getattrcount2',funstr{i}(parens(1):end)];
%%%%%%    funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),...
%%%%%%               'getattrcount(',funstr{i}(parens(1)+1:centercomma(end)-1),...
%%%%%%               funstr{i}(parens(2):end)];
%%%   end
%%%  end
%%% case {'getdtable'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0
%%%   eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%   if eqLoc~=-1
%%%    argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');
%%%    funstr(i+1:end+1)=funstr(i:end);
%%%    funstr{i}=[funstr{i}(1:funstrwords_b{i}(argWordOut)-1),strtrim(subscripts{3}),'=',...
%%%               funstrwords{i}{j},'(',funstr{i}(parens(1)+1:centercomma(2)),...
%%%               strtrim(subscripts{4}),funstr{i}(parens(2):end)];
%%%    funstr{i+1}=[funstrwords{i}{argWordOut},'=0;'];
%%%   else
%%%    funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),...
%%%               funstrwords{i}{j},'(',funstr{i}(parens(1)+1:centercomma(2)),...
%%%               strtrim(subscripts{4}),funstr{i}(parens(2):end)];
%%%   end
%%%  end
%%% case {'getdtablecount'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0
%%%   eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%   if eqLoc~=-1
%%%    argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');
%%%    funstr(i+1:end+1)=funstr(i:end);
%%%    funstr{i}=[funstr{i}(1:funstrwords_b{i}(argWordOut)-1),strtrim(subscripts{3}),'=',...
%%%               funstrwords{i}{j},'(',funstr{i}(parens(1)+1:centercomma(end)-1),...
%%%               funstr{i}(parens(2):end)];
%%%    funstr{i+1}=[funstrwords{i}{argWordOut},'=0;'];
%%%   else
%%%    funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),...
%%%               funstrwords{i}{j},'(',funstr{i}(parens(1)+1:centercomma(end)-1),...
%%%               funstr{i}(parens(2):end)];
%%%   end
%%%  end
%%% case {'getdtabledatapointarray'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0
%%%   eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%   if eqLoc~=-1
%%%    argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');
%%%    funstr(i+1:end+1)=funstr(i:end);
%%%    funstr{i}=[funstr{i}(1:funstrwords_b{i}(argWordOut)-1),strtrim(subscripts{6}),'=',...
%%%               funstrwords{i}{j},'(',funstr{i}(parens(1)+1:centercomma(end)-1),...
%%%               funstr{i}(parens(2):end)];
%%%    funstr{i+1}=[funstrwords{i}{argWordOut},'=0;'];
%%%   else
%%%    funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),...
%%%               funstrwords{i}{j},'(',funstr{i}(parens(1)+1:centercomma(end)-1),...
%%%               funstr{i}(parens(2):end)];
%%%   end
%%%  end
%%% case {'getattributename'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0
%%%   eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%   argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');
%%%   funstr(i+1:end+1)=funstr(i:end);
%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(argWordOut)-1),strtrim(subscripts{3}),'=',...
%%%              'getattrname(',funstr{i}(parens(1)+1:centercomma(2)),strtrim(subscripts{4}),...
%%%              funstr{i}(parens(2):end)];
%%%   funstr{i+1}=[funstrwords{i}{argWordOut},'=0;'];
%%%  end
%%% case {'getrealattribute'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0
%%%   eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%   argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');
%%%   funstr(i+1:end+1)=funstr(i:end);
%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(argWordOut)-1),strtrim(subscripts{3}),'=',...
%%%              'getrealattr(',funstr{i}(parens(1)+1:centercomma(2)),strtrim(subscripts{4}),...
%%%              funstr{i}(parens(2):end)];
%%%   funstr{i+1}=[funstrwords{i}{argWordOut},'=0;'];
%%%  end
%%% case {'getintegerattribute'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0
%%%   eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%   argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');
%%%   funstr(i+1:end+1)=funstr(i:end);
%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(argWordOut)-1),strtrim(subscripts{3}),'=',...
%%%              'getintegerattr(',funstr{i}(parens(1)+1:centercomma(2)),strtrim(subscripts{4}),...
%%%              funstr{i}(parens(2):end)];
%%%   funstr{i+1}=[funstrwords{i}{argWordOut},'=0;'];
%%%  end
%%% case {'getstringattribute'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0
%%%   eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%   argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');
%%%   funstr(i+1:end+1)=funstr(i:end);
%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(argWordOut)-1),strtrim(subscripts{3}),'=',...
%%%              'getstringattr(',funstr{i}(parens(1)+1:centercomma(2)),strtrim(subscripts{4}),...
%%%              funstr{i}(parens(2):end)];
%%%   funstr{i+1}=[funstrwords{i}{argWordOut},'=0;'];
%%%  end
%%% case {'getsubobject'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0
%%%   eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%   argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');
%%%   funstr(i+1:end+1)=funstr(i:end);
%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(argWordOut)-1),strtrim(subscripts{3}),'=',...
%%%              'getsubobject(',funstr{i}(parens(1)+1:centercomma(2)),strtrim(subscripts{4}),...
%%%              funstr{i}(parens(2):end)];
%%%   funstr{i+1}=[funstrwords{i}{argWordOut},'=0;'];
%%%  end
%%% case {'getsubobjectcount'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0
%%%   eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%   argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');
%%%   funstr(i+1:end+1)=funstr(i:end);
%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(argWordOut)-1),strtrim(subscripts{3}),'=',...
%%%              'getsubobjcount(',funstr{i}(parens(1)+1:centercomma(end)-1),...
%%%              funstr{i}(parens(2):end)];
%%%   funstr{i+1}=[funstrwords{i}{argWordOut},'=0;'];
%%%  end
%%% case {'getsubobjectname'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0
%%%   eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%   argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');
%%%   funstr(i+1:end+1)=funstr(i:end);
%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(argWordOut)-1),strtrim(subscripts{3}),'=',...
%%%              'getsubobjname(',funstr{i}(parens(1)+1:centercomma(2)),strtrim(subscripts{4}),...
%%%              funstr{i}(parens(2):end)];
%%%   funstr{i+1}=[funstrwords{i}{argWordOut},'=0;'];
%%%  end
%%% case {'destroydobject'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0 & j>1
%%%   eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%   argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');
%%%   funstr(i+1:end+1)=funstr(i:end);
%%%   funstr{i}=['%',funstr{i}(funstrwords_b{i}(argWordOut):end)];
%%%   funstr{i+1}=[funstrwords{i}{argWordOut},'=0;'];   
%%%%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(argWordOut)-1),'%',...
%%%%%%              funstr{i}(funstrwords_b{i}(argWordOut):end)];
%%%  end
%%% case {'mexprintf'}
%%%  repstr='disp';outflag(1)=1;
%%%  funstr=replaceword_f(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);
%%%%%% case {'parse_list'}
%%%%%%  outflag(1)=1;
%%%%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%%%%  funstr{i}=[funstr{i}(1:funstrwords_b{i}(j-1)-1),strtrim(subscripts{3}),...
%%%%%%             '=strread(',strtrim(subscripts{1}),',''%s'',''delimiter'',',strtrim(subscripts{2}),'); ',...
%%%%%%             strtrim(subscripts{4}),'=length(',strtrim(subscripts{3}),')',...
%%%%%%             funstr{i}(parens(2)+1:end)];
%%%%%% case {'parse_list'}
%%%%%%  outflag(1)=1;
%%%%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%%%%  if howmany>0
%%%%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(j-1)-1),'[',strtrim(subscripts{3}),',', ...
%%%%%%              strtrim(subscripts{4}),']=parse_list_cell(strtrim(',strtrim(subscripts{1}),'),',...
%%%%%%              subscripts{2},funstr{i}(parens(2):end)];
%%%%%%  end
%%% case {'parse_list'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0 && strcmp(funstrwords{i}{j-1},'call')
%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(j-1)-1),'[',strtrim(subscripts{3}),',', ...
%%%              strtrim(subscripts{4}),']=parselist',funstr{i}(parens(1):end)];
%%%  end
%%% case {'parse_list_to_reals','parse_list_to_integers'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  funstr{i}=[funstr{i}(1:funstrwords_b{i}(j-1)-1),strtrim(subscripts{3}),...
%%%             '=strread(',strtrim(subscripts{1}),',''%f'',''delimiter'',',strtrim(subscripts{2}),'); ',...
%%%             strtrim(subscripts{4}),'=length(',strtrim(subscripts{3}),')',...
%%%             funstr{i}(parens(2)+1:end)];
%%% case {'parsed_list_length'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0
%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),'length(strread(',strtrim(subscripts{1}),...
%%%              ',''%s'',''delimiter'',',strtrim(subscripts{2}),'))',funstr{i}(parens(2)+1:end)];
%%%  end
%%% case {'fix_up_mnemonic'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  funstr{i}=[funstr{i}(1:funstrwords_b{i}(j-1)-1),strtrim(subscripts{1}),...
%%%             '=fix_up_mnemonic(',strtrim(subscripts{2}),')',funstr{i}(parens(2)+1:end)];
%%%%%% case {'gmo_fname'}
%%%%%%  repstr='gmomap';outflag(1)=1;
%%%%%%  funstr=replaceword_f(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);
%%% case {'spanout_whitespace'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  funstr{i}=[funstr{i}(1:funstrwords_b{i}(j-1)-1),strtrim(subscripts{1}),'=fliplr(deblank(fliplr(',...
%%%             strtrim(subscripts{1}),'))',funstr{i}(parens(2):end)];
%%%%%% case {'get_global_init_domain'}
%%%%%%  outflag(1)=1;
%%%%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%%%%  funstr{i}=[funstr{i}(1:funstrwords_b{i}(j-1)-1),strtrim(subscripts{1}),...
%%%%%%             '=upper(get_domain)',funstr{i}(parens(2)+1:end)];
%%% case {'get_security_portfolio_proxy'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0
%%%   eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%   argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');
%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(argWordOut)-1),'[',funstrwords{i}{argWordOut},...
%%%              ',',funstr{i}(parens(1)+1:parens(2)-1),']=',funstr{i}(funstrwords_b{i}(j):end)];
%%%  end
%%%%%% case {'get_portfolio_domain'}
%%%%%%  outflag(1)=1;
%%%%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%%%%  if howmany>0
%%%%%%   eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%%%%   argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');
%%%%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(argWordOut)-1),'[',funstrwords{i}{argWordOut},...
%%%%%%              ',',strtrim(subscripts{2}),']=',funstr{i}(funstrwords_b{i}(j):end)];
%%%%%%  end
%%%%%% % Can't remember why I did this, but I think we can handle it through ftm_util.for now
%%%%%% case {'fmt_good_real_or_dne'}
%%%%%%  outflag(1)=1;
%%%%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%%%%  if howmany>0
%%%%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),funstr{i}(funstrwords_e{i}(j)+1:end)];
%%%%%%  end
%%% case {'get_portfolio_liquidity_wgt'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0
%%%   eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%   argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');
%%%   funstr(i+1:end+1)=funstr(i:end);
%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(argWordOut)-1),strtrim(subscripts{2}),'=',...
%%%              'get_portfolio_lwgt(',strtrim(subscripts{1}),',',strtrim(subscripts{3}),')',...
%%%              funstr{i}(parens(2)+1:end)];
%%%   funstr{i+1}=[funstrwords{i}{argWordOut},'=0;'];
%%%  end  
%%% case {'get_portfolio_wgts'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0
%%%   eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%   argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');
%%%   funstr(i+1:end+1)=funstr(i:end);
%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(argWordOut)-1),strtrim(subscripts{2}),'=',...
%%%              'get_portfolio_wgts(',strtrim(subscripts{1}),',',strtrim(subscripts{3}),')',...
%%%              funstr{i}(parens(2)+1:end)];
%%%   funstr{i+1}=[funstrwords{i}{argWordOut},'=0;'];
%%%  end  
%%% case {'createdobject','createdtable'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0
%%%   eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%   argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');
%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(argWordOut)-1),'[',funstrwords{i}{argWordOut},...
%%%              ',',strtrim(subscripts{2}),']=',funstr{i}(funstrwords_b{i}(j):end)];
%%%  end
%%%%%% case {'group_activate'}
%%%%%%  outflag(1)=1;
%%%%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%%%%  if howmany>0 & j>1
%%%%%%   eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%%%%   argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');
%%%%%%   funstr(i+1:end+1)=funstr(i:end);
%%%%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(argWordOut)-1),...
%%%%%%              'group_activate(',strtrim(subscripts{1}),')',...
%%%%%%              funstr{i}(parens(2)+1:end)];
%%%%%%   funstr{i+1}=[funstrwords{i}{argWordOut},'=0;'];
%%%%%%  end  
%%%%%% case {'group_query_from_string'}
%%%%%%  outflag(1)=1;
%%%%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%%%%  if howmany>0 & j>1
%%%%%%   eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%%%%   argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');
%%%%%%   funstr(i+2:end+2)=funstr(i:end);
%%%%%%   funstr{i+2}=[funstr{i}(1:funstrwords_b{i}(argWordOut)-1),'[',funstrwords{i}{argWordOut},...
%%%%%%                ',',funstr{i}(parens(1)+1:parens(2)-1),']=trgtopt_',funstr{i}(funstrwords_b{i}(j):end)];
%%%%%%   funstr{i}='global rcm_is_matdomain trgtoptismatdomain';
%%%%%%   funstr{i+1}='trgtoptismatdomain=rcm_is_matdomain;';
%%%%%%  end
%%% case {'trgtopt_group_query_from_string'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0 & j>1
%%%   eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%   argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');
%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(argWordOut)-1),'[',funstrwords{i}{argWordOut},...
%%%              ',',funstr{i}(parens(1)+1:parens(2)-1),']=',funstr{i}(funstrwords_b{i}(j):end)];
%%%  end
%%% case {'get_default_domain'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0 & j>1
%%%   eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%   argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');
%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(argWordOut)-1),'[dumvar1,',...
%%%              funstr{i}(parens(1)+1:parens(2)-1),'] = get_user',funstr{i}(parens(1):end)];
%%%  end
%%% case {'get_domain_current_month'}
%%%  repstr='get_curr_month';outflag(1)=1;
%%%  funstr=replaceword_f(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);    
%%% case {'month_to_date'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0 & j>1
%%%   eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%   argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');
%%%   if str2num(subscripts{3})==0
%%%    tempstr='date2month';
%%%    funstr{i}=[funstr{i}(1:funstrwords_b{i}(j-1)-1),strtrim(subscripts{1}),' = ',...
%%%               tempstr,'(',strtrim(subscripts{2}),funstr{i}(parens(2):end)];
%%%   else
%%%    tempstr='month2date';
%%%    funstr{i}=[funstr{i}(1:funstrwords_b{i}(j-1)-1),strtrim(subscripts{2}),' = ',...
%%%               tempstr,'(',strtrim(subscripts{1}),funstr{i}(parens(2):end)];
%%%   end
%%%  end
%%% case {'month_to_date_string'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0
%%%   eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%   argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');
%%%   tempstr='month2date';
%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),tempstr,funstr{i}(parens(1):end)];
%%%  end
%%%%%% case {'domain_day_to_date'}
%%%%%%  outflag(1)=1;
%%%%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%%%%  if howmany>0
%%%%%%   eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%%%%   argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');
%%%%%%   tempstr='periodindex2date';
%%%%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),tempstr,'(',strtrim(subscripts{2}),',',...
%%%%%%              subscripts{1},funstr{i}(parens(2):end)];
%%%%%%  end
%%% case {'domain_dbday_in_month'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0
%%%   eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%   argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');
%%%   tempstr='datamonth2day';
%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),tempstr,'(',strtrim(subscripts{1}),',',...
%%%              subscripts{2},funstr{i}(parens(2):end)];
%%%  end
%%% case {'get_daily_portfolio_wgts'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0
%%%   eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%   argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');
%%%   funstr(i+1:end+1)=funstr(i:end);
%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(argWordOut)-1),'[',strtrim(subscripts{2}),']=',...
%%%              'get_portfolio_dwgt(',subscripts{1},',',subscripts{3},',',subscripts{2},')',...
%%%              funstr{i}(parens(2)+1:end)];
%%%   funstr{i+1}=[funstrwords{i}{argWordOut},'=0;'];
%%%  end  
%%%%%%  outflag(1)=1;
%%%%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%%%%  if howmany>0
%%%%%%   eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%%%%   argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');
%%%%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),...
%%%%%%              'get_portfolio_dwgt(',strtrim(subscripts{1}),',',strtrim(subscripts{3}),',',strtrim(subscripts{2}),')',...
%%%%%%              funstr{i}(parens(2)+1:end)];
%%%%%%  end  
%%% case 'get_daily_portfolio_wgts_subr'
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0
%%%   eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%   argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');
%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(argWordOut)-1),'[',funstrwords{i}{argWordOut},',',(subscripts{2}),',',...
%%%              (subscripts{5}),']=',...
%%%              funstrwords{i}{j},'(',subscripts{1},',',subscripts{2},',',subscripts{3},', real(',subscripts{4},'), real(',subscripts{5},'))',...
%%%              funstr{i}(parens(2)+1:end)];
%%%  end  
%%%%%%  outflag(1)=1;
%%%%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%%%%  if howmany>0
%%%%%%   eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%%%%   argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');
%%%%%%   funstr(i+1:end+1)=funstr(i:end);
%%%%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(argWordOut)-1),'[',(subscripts{2}),',',...
%%%%%%              (subscripts{5}),']=',...
%%%%%%              'get_portfolio_dwgt(',subscripts{1},',',subscripts{3},',',subscripts{2},')',...
%%%%%%              funstr{i}(parens(2)+1:end)];
%%%%%%   funstr{i+1}=[funstrwords{i}{argWordOut},'=0;'];
%%%%%%  end  
%%% case {'get_portfolio_is_realtime'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0
%%%   eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%   argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');
%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(argWordOut)-1),'[',funstrwords{i}{argWordOut},...
%%%              ',',strtrim(subscripts{2}),']=',funstr{i}(funstrwords_b{i}(j):parens(1)),strtrim(subscripts{1}),...
%%%             funstr{i}(parens(2):end)];
%%%  end  
%%% case {'readobjectfromfilewithname'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0
%%%   eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%   argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');
%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(argWordOut)-1),'[',funstrwords{i}{argWordOut},...
%%%              ',',strtrim(subscripts{2}),']=',funstr{i}(funstrwords_b{i}(j):centercomma(1)),...
%%%              subscripts{3},funstr{i}(parens(2):end)];
%%%  end  
%%% case {'addsubobject'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0
%%%   eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%   argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');
%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(argWordOut)-1),'[',funstrwords{i}{argWordOut},...
%%%              ',',strtrim(subscripts{1}),',',strtrim(subscripts{2}),']=',...
%%%              funstr{i}(funstrwords_b{i}(j):end)];
%%%  end  
%%% case {'addstringattribute','addintegerattribute','addrealattribute'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0
%%%   if strcmp(funstrwords{i}{1},'call')
%%%    funstr(i+1:end+1)=funstr(i:end);
%%%    % and the function this is in
%%%    [howmany2,subscripts2,centercomma2,parens2]=hassubscript_f(i,2,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%    %'fffffffff',kb
%%%    temp1=[parens2(1),centercomma2,parens2(2)];
%%%    funstr{i+1}=['status=',funstrwords{i}{2},'(status',funstr{i}(temp1(2):end)];
%%%    funstr{i}=['[status,',strtrim(subscripts{1}),']=',funstrwords{i}{j},...
%%%               funstr{i}(parens(1):parens(2)),';'];
%%%   else
%%%    eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%    argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');
%%%    funstr{i}=[funstr{i}(1:funstrwords_b{i}(argWordOut)-1),'[',funstrwords{i}{argWordOut},...
%%%               ',',strtrim(subscripts{1}),']=',...
%%%               funstr{i}(funstrwords_b{i}(j):end)];
%%%   end
%%%  end  
%%% case {'getobjectname'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0
%%%   eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%   argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');
%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(argWordOut)-1),'[',funstrwords{i}{argWordOut},...
%%%              ',',strtrim(subscripts{2}),']=',...
%%%              funstr{i}(funstrwords_b{i}(j):end)];
%%%  end  
%%% %case {'tagged_character_heapsort','tagged_real_down_heapsort'}
%%% case {'tagged_character_heapsort'}
%%% %case {'tagged_character_heapsort','tagged_real_heapsort','tagged_real_down_heapsort'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0 && j>1 && strcmp(funstrwords{i}{j-1},'call')
%%%   funstr(i+1:end+1)=funstr(i:end);
%%%   temp=''; if strcmp(funstrwords{i}{j},'tagged_real_down_heapsort'),temp=',''descend'''; end
%%%   temp1=['(1:',strtrim(subscripts{3}),')'];
%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(j-1)-1),'[',strtrim(subscripts{1}),temp1,...
%%%              ', tempOut ] = sort(',...
%%%              strtrim(subscripts{1}),temp1,temp,funstr{i}(parens(2):end)];
%%%   funstr{i+1}=[subscripts{2},temp1,'=',strtrim(subscripts{2}),'(tempOut);'];
%%%  end
%%%  %funstr{i},'wwwwwwwwwwww',kb
%%% case {'matlabnl'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if ~isempty(parens) && j>1 && strcmp(funstrwords{i}{j-1},'call')
%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(j-1)-1),'disp(char(10))',funstr{i}(parens(2)+1:end)];
%%%  end  
%%% case {'matrix2mat'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if ~isempty(parens) && j>1 && strcmp(funstrwords{i}{j-1},'call')
%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(j-1)-1),'eval([',...
%%%              strtrim(subscripts{4}),',''=',strtrim(subscripts{1}),';'']);'...
%%%              'save(',strtrim(subscripts{5}),',',strtrim(subscripts{4}),',''-mat'');',...
%%%              funstr{i}(parens(2)+1:end)];
%%%  end  
%%%  % put brackets on the outputs of these so the nmber of outputs doesn't change in f2matlab
%%%  % these are the mex files
%%% case {'get_portfolio_domain','is_valid_group_mnemonic','is_valid_portfolio','is_valid_security'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0
%%%   eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%   if eqLoc~=-1
%%%    temp=lastNonSpace(funstr{i},funstrwords_b{i}(j));
%%%    if temp>0 && funstr{i}(temp)=='='
%%%     temp=lastNonSpace(funstr{i},temp);
%%%     if temp>0 && funstr{i}(temp)~=']'
%%%      argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');
%%%      if strcmp(funstrwords{i}{j},'get_portfolio_domain')
%%%       funstr{i}=[funstr{i}(1:funstrwords_b{i}(argWordOut)-1),'[',funstrwords{i}{argWordOut},...
%%%                  ',dumvar2,',strtrim(subscripts{2}),']=',funstr{i}(funstrwords_b{i}(j):end)];
%%%      else
%%%       funstr{i}=[funstr{i}(1:funstrwords_b{i}(argWordOut)-1),'[',funstrwords{i}{argWordOut},...
%%%                  ']=',funstr{i}(funstrwords_b{i}(j):end)];
%%%      end
%%%     end % if temp>0 && funstr{i}(temp)~=']'
%%%    end % if temp>0 && funstr{i}(temp)=='='
%%%   end % if eqLoc~=-1
%%%  end % if howmany>0
%%% case {'countdtablerows'}
%%% %case {'countdtablerows','get_group_long_name'}
%%% %case {'countdtablerows','get_data_period','get_group_long_name'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0
%%%   eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%   argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');
%%%   funstr(i+1:end+1)=funstr(i:end);
%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(argWordOut)-1),'[',strtrim(subscripts{2}),']=',...
%%%              funstrwords{i}{j},'(',strtrim(subscripts{1}),')',...
%%%              funstr{i}(parens(2)+1:end)];
%%%   funstr{i+1}=[funstrwords{i}{argWordOut},'=0;'];
%%%  end  
%%% case {'data_write'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0 & j>1
%%%   eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%   argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');
%%%   funstr(i+1:end+1)=funstr(i:end);
%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(argWordOut)-1),...
%%%              funstrwords{i}{j},...
%%%              funstr{i}(parens(1):end)];
%%%   funstr{i+1}=[funstrwords{i}{argWordOut},'=0;'];
%%%   %funstr(i:i+1),kb
%%%  end  
%%% case {'data_open'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0 & j>1
%%%   eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%   argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');
%%%   funstr(i+1:end+1)=funstr(i:end);
%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(argWordOut)-1),...
%%%              funstrwords{i}{j},...
%%%              funstr{i}(parens(1):end)];
%%%   funstr{i+1}=[funstrwords{i}{argWordOut},'=0;'];
%%%   %funstr(i:i+1),kb
%%%  end  
%%% case {'add_variable_to_matrix'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany==8 || howmany==9
%%%   funstr(i+8:end+8)=funstr(i:end);
%%%   if howmany==8
%%%    funstr{i+6}=[subscripts{8},'(',subscripts{4},') = ',subscripts{3},' * sosref(jj);'];
%%%   elseif howmany==9
%%%    funstr{i+6}=[subscripts{8},'(',subscripts{4},') = ',subscripts{3},' * abs( sosref(jj) - ',subscripts{9},');'];
%%%   end
%%%   funstr{i+0}='%%%%%% These next lines replace the add_variable_to_matrix line, but are faster - BEB Aug-07';
%%%   funstr{i+1}=['[ ',subscripts{1},' ,link0 , link1 ]=sos2links( ',subscripts{1},' ,link0 , link1);'];
%%%   funstr{i+2}='for jj=link0:link1;';
%%%   funstr{i+3}=[subscripts{4},' = ',subscripts{4},' + 1;'];
%%%   funstr{i+4}=[subscripts{6},'(',subscripts{4},') = ',subscripts{2},';'];
%%%   funstr{i+5}=[subscripts{7},'(',subscripts{4},') = jj;'];
%%%   funstr{i+7}=['end'];
%%%   funstr{i+8}=['%%% ',funstr{i+8}]; % original line
%%%  end
%%% case {'timera'}
%%%  repstr='tic';outflag(1)=1;
%%%  funstr=replaceword_f(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);  
%%% case {'timerb'}
%%%  repstr='toc';outflag(1)=1;
%%%  funstr=replaceword_f(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr);  
%%% case {'loadconstraintvectorsparse'} %capital
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany==6 && j>1
%%%   funstr(i+2:end+2)=funstr(i:end);
%%%   %temp4=findNext(funstrwords_b{i}(j),']',funstr{i},-1)
%%%   temp5=',    ncons,consnzs,truemlv,notenoughnzspace,dynnzspace,notenoughrowspace,dynrowspace,constraintname{ncons1},constraintlowerbound(ncons1),constraintupperbound(ncons1),constraintrow(consnzst),constraintcol(consnzst),constraintmatrix(consnzst)';
%%%   %temp5=',    nCons,ConsNzs,truemlv,NotEnoughNZSpace,DynNzSpace,NotEnoughRowSpace,DynRowSpace,ConstraintName{ncons1},ConstraintLowerBound(ncons1),ConstraintUpperBound(ncons1),ConstraintRow(ConsNzsT),ConstraintCol(ConsNzsT),ConstraintMatrix(ConsNzsT)';
%%%   funstr{i+2}=[funstr{i}(1:parens(2)-1),temp5,funstr{i}(parens(2):end)];
%%%   funstr{i+0}='%%%%%% This modified call to LoadConstraintVectorSparse is faster than the original because it avoids modifying large arrays inside LoadConstraintVectorSparse.';
%%%   funstr{i+1}=['consnzst=consnzs+[1:',subscripts{6},'];ncons1=ncons+1;'];
%%%   %funstr{i+1}=['ConsNzsT=ConsNzs+[1:',subscripts{6},'];ncons1=ncons+1;'];
%%%   %'@@@@@@@@@',kb
%%%  end
%%%%%% case {'loadconstraintvectorsparse'} %capital
%%%%%%  outflag(1)=1;
%%%%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%%%%  if howmany==6 && j>1
%%%%%%   funstr(i+2:end+2)=funstr(i:end);
%%%%%%   %temp4=findNext(funstrwords_b{i}(j),']',funstr{i},-1)
%%%%%%   temp5=',    ncons,consnzs,truemlv,notenoughnzspace,dynnzspace,notenoughrowspace,dynrowspace,constraintname{ncons+1},constraintlowerbound(ncons+1),constraintupperbound(ncons+1),constraintrow(consnzst),constraintcol(consnzst),constraintmatrix(consnzst)';
%%%%%%   %temp5=',    nCons,ConsNzs,truemlv,NotEnoughNZSpace,DynNzSpace,NotEnoughRowSpace,DynRowSpace,ConstraintName{nCons+1},ConstraintLowerBound(nCons+1),ConstraintUpperBound(nCons+1),ConstraintRow(ConsNzsT),ConstraintCol(ConsNzsT),ConstraintMatrix(ConsNzsT)';
%%%%%%   funstr{i+2}=[funstr{i}(1:parens(2)-1),temp5,funstr{i}(parens(2):end)];
%%%%%%   %funstr{i+2}=[funstr{i}(1:temp4-1),temp5,' = LoadConstraintVectorSparse',funstr{i}(parens(1):parens(2)-1),temp5,funstr{i}(parens(2):end)];
%%%%%%   funstr{i+0}='%%%%%% This modified call to LoadConstraintVectorSparse is faster than the original because it avoids modifying large arrays inside LoadConstraintVectorSparse.';
%%%%%%   funstr{i+1}=['consnzst=consnzs+[1:',subscripts{6},'];'];
%%%%%%   %funstr{i+1}=['ConsNzsT=ConsNzs+[1:',subscripts{6},'];'];
%%%%%%   %'@@@@@@@@@',kb
%%%%%%  end
%%% case {'initconstraintvector'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if ~isempty(parens) && j>1 && strcmp(funstrwords{i}{j-1},'call')
%%%   funstr{i}=['constraintvector(1:',subscripts{1},')=0;'];
%%%   %funstr{i}=['ConstraintVector(1:',subscripts{1},')=0;'];
%%%  end
%%% case {'data_read_character_anysize','data_read','data_read_character'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0 & j>1
%%%   eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%   argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');
%%%   funstr(i+1:end+1)=funstr(i:end);
%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(argWordOut)-1),...
%%%              strtrim(subscripts{2}),'=cellstr(data_read(',strtrim(subscripts{1}),',',...
%%%              strtrim(subscripts{3}),').''',funstr{i}(parens(2):end)];
%%%   funstr{i+1}=[funstrwords{i}{argWordOut},'=0;'];
%%%   %funstr(i:i+1),kb
%%%  end  
%%% case {'data_read_logical','data_read_real','data_read_integer'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0 & j>1
%%%   eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%   argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');
%%%   funstr(i+1:end+1)=funstr(i:end);
%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(argWordOut)-1),...
%%%              strtrim(subscripts{2}),'=data_read(',strtrim(subscripts{1}),',',...
%%%              strtrim(subscripts{3}),funstr{i}(parens(2):end)];
%%%   funstr{i+1}=[funstrwords{i}{argWordOut},'=0;'];
%%%   %funstr(i:i+1),kb
%%%  end  
%%% case {'get_unused_fileunit'}
%%%    outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0 & j>1
%%%   eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%   argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');
%%%   funstr(i+1:end+1)=funstr(i:end);
%%%   funstr{i}=['% ',funstr{i}];
%%%   funstr{i+1}=[funstrwords{i}{argWordOut},'=0;'];
%%%  end
%%% case {'get_portfolio_bmark'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0
%%%   eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%   argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');
%%%   funstr(i+5:end+5)=funstr(i:end);
%%%   funstr{i}='try';
%%%   funstr{i+1}=[funstr{i}(1:funstrwords_b{i}(argWordOut)-1),'[',strtrim(subscripts{2}),']=',...
%%%              'get_portfolio_b','(',strtrim(subscripts{1}),');',...
%%%              funstr{i}(parens(2)+1:end)];
%%%   funstr{i+2}=[funstrwords{i}{argWordOut},'=0;'];
%%%   funstr{i+3}='catch';
%%%   funstr{i+4}=[funstrwords{i}{argWordOut},'=-36;']; %this is a guess as to what it should be
%%%   funstr{i+5}='end';
%%%  end  
%%%%%% case {'get_portfolio_bmark'}
%%%%%%  outflag(1)=1;
%%%%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%%%%  if howmany>0
%%%%%%   eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%%%%   argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');
%%%%%%   funstr(i+1:end+1)=funstr(i:end);
%%%%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(argWordOut)-1),'[',strtrim(subscripts{2}),']=',...
%%%%%%              'get_portfolio_b','(',strtrim(subscripts{1}),')',...
%%%%%%              funstr{i}(parens(2)+1:end)];
%%%%%%   funstr{i+1}=[funstrwords{i}{argWordOut},'=0;'];
%%%%%%  end  
%%% case {'get_domain_daily_period'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0
%%%   eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%   argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');
%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(argWordOut)-1),'[',funstrwords{i}{argWordOut},...
%%%              ',dumvar2,',strtrim(subscripts{2}),']=',funstr{i}(funstrwords_b{i}(j):end)];
%%%  end  
%%% case {'data_load_all'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0 & j>1
%%%   eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%   argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');
%%%   funstr(i+1:end+1)=funstr(i:end);
%%%   funstr{i}=[funstr{i}(funstrwords_b{i}(j):end)];
%%%   funstr{i+1}=[funstrwords{i}{argWordOut},'=0;'];
%%%  end
%%% case {'get_portfolio_liquidity_wgtsubr'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0 & j>1
%%%   eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%   argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');
%%%   funstr(i+1:end+1)=funstr(i:end);
%%%   funstr{i}=[strtrim(subscripts{3}),'=get_portfolio_lwgt(',strtrim(subscripts{1}),',',strtrim(subscripts{4}),funstr{i}(parens(2):end)];
%%%   funstr{i+1}=[funstrwords{i}{argWordOut},'=0;'];
%%%  end
%%% case 'get_user_name'
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0
%%%   %funstr{i}=['[',strtrim(subscripts{1}),']=',funstrwords{i}{j},funstr{i}(parens(2)+1:end)];
%%%   funstr{i}=['[',strtrim(subscripts{1}),']=get_user',funstr{i}(parens(2)+1:end)];
%%%  end
%%% case 'drline'
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if ~isempty(parens) && j>1 && strcmp(funstrwords{i}{j-1},'call')
%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(j-1)-1),...
%%%              '[',funstr{i}(centercomma(3)+1:parens(2)-1),']=drline',...
%%%              funstr{i}(parens(1):centercomma(3)-1),');'];
%%%  end
%%% case 'fmt_ones_form_to_percent'
%%%  outflag(1)=1;
%%% case 'drgivn'
%%%    outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if ~isempty(parens) && j>1 && strcmp(funstrwords{i}{j-1},'call')
%%%   funstr{i}=['[',subscripts{15},',',subscripts{17},',',...
%%%              subscripts{19},',',subscripts{20},',',...
%%%              subscripts{21},',',subscripts{22},',',...
%%%              subscripts{24},',',subscripts{25},',',...
%%%              subscripts{26},']=',funstr{i}(funstrwords_b{i}(j):end)];
%%%  end
%%% case 'drstat'
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if ~isempty(parens) && j>1 && strcmp(funstrwords{i}{j-1},'call')
%%%   funstr{i}=['[',subscripts{10},',',subscripts{11},',',...
%%%              subscripts{13},',',subscripts{15},...
%%%              ']=',funstr{i}(funstrwords_b{i}(j):end)];
%%%   %'rrrrrrrrrrr',funstr{i},kb
%%%  end
%%% case 'domain_day_to_date2'
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if ~isempty(parens) && j>1 && strcmp(funstrwords{i}{j-1},'call')
%%%   funstr{i}=['[',subscripts{2},',',subscripts{3},']=',funstr{i}(funstrwords_b{i}(j):end)];
%%%   %'rrrrrrrrrrr',funstr{i},kb
%%%  end
%%% case 'getunusedcount'
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0
%%%   eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%   argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');
%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(argWordOut)-1),'[',funstrwords{i}{argWordOut},...
%%%              ',dumvar2,',strtrim(subscripts{2}),']=',funstr{i}(funstrwords_b{i}(j):end)];
%%%  end  
%%% case 'findunused'
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0
%%%   eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%   argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');
%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(argWordOut)-1),'[',funstrwords{i}{argWordOut},...
%%%              ',dumvar2,',funstr{i}(centercomma(1)+1:parens(2)-1),']=',funstr{i}(funstrwords_b{i}(j):end)];
%%%  end  
%%% case {'getcwd'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0 & j>1
%%%   eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%   argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');
%%%   funstr(i+1:end+1)=funstr(i:end);
%%%   funstr{i}=[strtrim(subscripts{1}),'=pwd',funstr{i}(parens(2)+1:end)];
%%%   funstr{i+1}=[funstrwords{i}{argWordOut},'=0;'];
%%%  end
%%%
%%%
%%%  %'jjjjjjjjjj',funstr{i},kb
%%%  
%%%
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% These next have to do with dealing with converting matlab engine stuff
%%% case {'matdomain_open'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0 && j>1 && strcmp(funstrwords{i}{j-1},'call')
%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(j-1)-1),strtrim(subscripts{1}),'=',...
%%%              funstr{i}(funstrwords_b{i}(j):end)];
%%%   
%%%  end
%%% case {'enggetmatrix'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0 && j>1
%%%   eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%   argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');
%%%   %'11111111',funstr{i},kb
%%%   % note that splitting up then-less if statements has to be done after this to work properly
%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(argWordOut)-1),'try, ',...
%%%              funstrwords{i}{argWordOut},'=evalin(''base'',[',...
%%%              subscripts{2},','';'']); outbuf=''''; catch, ',funstrwords{i}{argWordOut},'=0; ',...
%%%              'outbuf=[''??? '',lasterr]; end',funstr{i}(parens(2)+1:end)];
%%%%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(argWordOut)-1),...
%%%%%%              'if (exist(',strtrim(subscripts{2}),',''var'')) ',...
%%%%%%              funstrwords{i}{argWordOut},'=',strrep(subscripts{2},'''',' '),'; else ',...
%%%%%%              funstrwords{i}{argWordOut},'=0;',funstr{i}(parens(2)+1:end)];
%%%%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),strrep(subscripts{2},'''',' '),...
%%%%%%              funstr{i}(parens(2)+1:end)];   
%%%  end
%%% case {'engopen'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0 & j>1
%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),'1',...
%%%              funstr{i}(parens(2)+1:end)];   
%%%  end
%%% case {'mxgetm'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0 & j>1
%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),'size(',strtrim(subscripts{1}),',1',...
%%%              funstr{i}(parens(2):end)];   
%%%  end
%%% case {'mxgetn'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0 & j>1
%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),'size(',strtrim(subscripts{1}),',2',...
%%%              funstr{i}(parens(2):end)];   
%%%  end
%%% case {'mxgetpr'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0 & j>1
%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),strtrim(subscripts{1}),...
%%%              funstr{i}(parens(2)+1:end)];   
%%%  end
%%% case {'mxcreatefull'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0 & j>1
%%%   eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%   argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');   
%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(j)-1),'zeros(',strtrim(subscripts{1}),',',...
%%%              strtrim(subscripts{2}),funstr{i}(parens(2):end)];   
%%%  end
%%% case {'engoutputbuffer'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0 & j>1
%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(j-1)-1),'%',...
%%%              funstr{i}(funstrwords_b{i}(j):end)];
%%%  end
%%% case {'engevalstring'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0 & j>1
%%%   eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%   if eqLoc==-1 %weird subroutine call
%%%    funstr(i+1:end+1)=funstr(i:end);
%%%    funstr{i}=['try, evalin(''base'',',...
%%%               subscripts{2},'); outbuf=''''; catch, ',...
%%%               'outbuf=[''??? '',lasterr]; end',funstr{i}(parens(2)+1:end)];
%%%%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(argWordOut)-1),'outbuf = '...
%%%%%%              'evalc(',strtrim(subscripts{2}),')',funstr{i}(parens(2)+1:end)];
%%%    funstr{i+1}=['status=empt0(regexpi(outbuf,''???'',''once''));'];
%%%    %'vvvvvvvvvv11',funstr{i},kb
%%%   else
%%%    argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');   
%%%    funstr(i+1:end+1)=funstr(i:end);
%%%    funstr{i}=[funstr{i}(1:funstrwords_b{i}(argWordOut)-1),'try, evalin(''base'',',...
%%%               subscripts{2},'); outbuf=''''; catch, ',...
%%%               'outbuf=[''??? '',lasterr]; end',funstr{i}(parens(2)+1:end)];
%%%%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(argWordOut)-1),'outbuf = '...
%%%%%%              'evalc(',strtrim(subscripts{2}),')',funstr{i}(parens(2)+1:end)];
%%%    funstr{i+1}=[funstrwords{i}{argWordOut},'=empt0(regexpi(outbuf,''???'',''once''));'];
%%%   end
%%%   %funstr{i+1}=[funstrwords{i}{argWordOut},'=0;'];
%%%  end
%%% case {'gmomxcopyptrtoreal8','gmomxcopyptrtoreal4','mxcopyptrtoreal8'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0 && j>1 && strcmp(funstrwords{i}{j-1},'call')
%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(j-1)-1),strtrim(subscripts{2}),'=',...
%%%              strtrim(subscripts{1}),funstr{i}(parens(2)+1:end)];   
%%%  end
%%% case {'mxgetstring'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0 && j>1
%%%   eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%   argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');
%%%   funstr(i+1:end+1)=funstr(i:end);
%%%   funstr{i}=[funstr{i}(1:funstrwords_b{i}(j-1)-1),strtrim(subscripts{2}),'=',...
%%%              strtrim(subscripts{1}),funstr{i}(parens(2)+1:end)];
%%%   funstr{i+1}=[funstrwords{i}{argWordOut},'=0;'];
%%%  end
%%% case {'engclose'}
%%%  outflag(1)=1;
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0 & j>1
%%%   eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%   argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');
%%%   funstr{i}=[funstrwords{i}{argWordOut},'=0; %',funstr{i}];
%%%  end
%%%
%%%  
%%%  
%%%
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% some of these need to have temporary outputs so they don't mix things up
%%% case {trgtoptOutModList{:,1}}
%%%  % which case did it match
%%%  whichCase=find(strcmp(funstrwords{i}{j},{trgtoptOutModList{:,1}}));
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0
%%%%%%   goon=1;
%%%%%%   if j>1
%%%%%%    if ~strcmp(funstrwords{i}{j-1},'call')
%%%%%%     eqLoc=findNext(funstrwords_b{i}(j),'=',funstr{i},-1);
%%%%%%     if ~isempty(eqLoc)
%%%%%%      argWordOut=find(funstrwords_b{i}<eqLoc,1,'last');
%%%%%%      tempstr=[funstr{i}(1:funstrwords_b{i}(j-1)-1),'[',funstrwords{i}{argWordOut},','];
%%%%%%      goon=0;
%%%%%%     end
%%%%%%    end
%%%%%%   end
%%%%%%   if goon
%%%    tempstr=[funstr{i}(1:funstrwords_b{i}(j-1)-1),'['];
%%%%%%   end
%%%
%%%   numMods=length(trgtoptOutModList{whichCase,2});
%%%   tempstr2=cell(numMods,1);   ii=1;
%%%   for k=1:howmany
%%%    if any(k==trgtoptOutModList{whichCase,2})
%%%     tempstr=[tempstr,tempOut,num2str(trgtoptOutModList{whichCase,2}(ii))];
%%%     tempstr2{ii}=[subscripts{k},'=',tempOut,num2str(trgtoptOutModList{whichCase,2}(ii)),';'];
%%%     ii=ii+1;
%%%    else
%%%     tempflag=output_acceptable(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords,k,howmany,subscripts,centercomma,parens,fun_name,statementFunction,localVar,fortranfunwords,var_words,this_fun_name,funwordsML,TFops,typeDefs);
%%%     if tempflag %put in the copy of the input arg
%%%      temp4=subscripts{k};
%%%     else %just put in a placeholder dummy output arg
%%%      temp4=[dumvar,num2str(k)];
%%%     end
%%%     tempstr=[tempstr,temp4];
%%%    end
%%%    if k~=howmany, tempstr=[tempstr,',']; end
%%%   end
%%%   tempstr=[tempstr,'] = ',funstr{i}(funstrwords_b{i}(j):end)];
%%%   %now put tempstr2 into funstr
%%%   funstr(i+numMods:end+numMods)=funstr(i:end);
%%%   funstr{i}=tempstr;
%%%   funstr(i+1:i+numMods)=tempstr2;
%%%   %'bbbbbbbbbbbbb',tempstr,tempstr2,kb
%%%  end
end
%if length(fortranfunwords)>fortranfunwordslength,  end






function out=convertFormatField(in,firstOrLast,isString)
global sp
%this changes the fortran format field into the fprintf equivalent
%%%if ~isempty(strfind(in,'dates'))
%%% 'aaaaaaaaaaaa',in,kb
%%%end


if nargin<3, isString=0; end
if exist('firstOrLast','var') && ischar(firstOrLast) && strcmpi(firstOrLast,'r')
 isread=1; firstOrLast=0;
else
 isread=0;
end
if nargin<2, firstOrLast=0; end

out='';
in=deblank(in);
in=strtrim(in);
%in(in=='"')='''';


%'iiiiiiiiiiiiii',in,kb
if any(in=='(') % we have a recursive definition
 temp=find(in=='(');
 temp2=find(in=='>',1,'last');
 if ~isempty(temp2)
  temp=temp(find(temp>temp2,1,'first'));
 else
  temp=temp(1);
 end
 if ~inastring_f(in,temp) & ~inastring2_f(in,temp)
  rs='1';
  %'eeeeeeeee',in,keyboard
  if ~isempty((in(1:temp-1)))
   rs=(strrep(strrep(in(1:temp-1),'<',''),'>',''));
   %rs=str2num(strrep(strrep(in(1:temp-1),'<',''),'>',''));
  end
  tempr=findrights_f(temp,in);
  groups=getTopGroupsAfterLoc(in(temp+1:tempr-1),0);
  groupsStr='';
  for ii=1:length(groups)
   temp1=',';if ii==length(groups), temp1='';end
   if ii==1 | ii==length(groups)
    groupsStr=[groupsStr,convertFormatField(groups{ii},1),temp1];
   else
    groupsStr=[groupsStr,convertFormatField(groups{ii}),temp1];
   end
  end
  groupsStr=['[',groupsStr,']'];
  out=['repmat(',groupsStr,' ,1,',(rs),')'];
  %out=['repmat(',groupsStr,' ,1,',num2str(rs),')'];
  %'rrrrrrrrrrr',groups,groupsStr,out,kb
  return
 end
end

%',,,,,,,,,,,,,,,',in,out,kb
% first thing to do is to determine if it's a string
if any(in=='''') | isString
 out=in;
 if length(out)>2
  if strcmp(out(1:2),'''''')
   if ~strcmp(out(3),'''')
    out=out(2:end);
   end
  end
  % actually test the last 2 non white space, non / characters
  temp3=find(~isspace(out) & out~='/') ;
  if temp3(end)>2
   if strcmp(out(temp3(end)-1:temp3(end)),'''''')
    if ~strcmp(out(temp3(end)-2),'''')
     out=[out(1:temp3(end)-1),out(temp3(end)+1:end)];
    end
   end
  end
 end
else %OK, it's not a string
     %'errrrrrrrrrrr',in,out,keyboard
     %does it have a letter in it?
     % also fix the slashes to have commas in between them and call recursively
 
 if any(isletter(in))
  letterLoc=find(isletter(in));
  %does it have a repeat specification
  rs=[];
  if letterLoc>1
   rs=num2str(in(1:letterLoc-1));
  end
  b1=find(in=='<',1,'first');
  if ~isempty(b1)   b2=find(in=='>',1,'first');  rs=in(b1+1:b2-1);  end
  
  letters=find(isletter(in));
  letterSpec=in(letters(1));
  rest=in(letters(end)+1:end);
  preOut='';
  postOut='';
  if ~isempty(rs)
   preOut='repmat(';
   postOut=[',1,',rs,')'];
  end
  switch letterSpec
    case {'x','X'}
      out=[preOut,''' ''',postOut];
    case {'h','H'}
      out=['''',sp,'',in(letters(1)+1:end),''''];
    case {'a','A'}
      if ~isempty(rs)
       out=[preOut,'''%',rest,'c','',sp,'''',postOut];
      else
       out=[preOut,'''%',rest,'s','',sp,'''',postOut];
      end
%%%   case {'e','E'}
%%%    funstr{i},'99999999999999',kb
    case {'i','I'}
      if isread
       out=[preOut,'''%',rest,'u','',sp,'''',postOut];
      else
       out=[preOut,'''%',rest,'i','',sp,'''',postOut];
      end
    otherwise
      out=[preOut,'''%',rest,'f','',sp,'''',postOut];
  end  
 else
  switch in
    case ':'
      out=' ''\n '' ';
    case '$' %this is supposed to suppress a linefeed
      out='''''';
    otherwise
      out=in;
  end
  %out,in,'00000000000',kb
 end
end

% change over /'s


temp=find(out=='/');
temp2=find(out=='''');
if isempty(temp2)
 temp1=find(~inastring_f(out,temp));
else
 if isempty(temp)
  temp1=find(~inastring_f(out,temp));
 else
  temp1=find(~inastring_f(out,temp)|temp>temp2(end));
 end % if isempty(temp)
end % if isempty(temp2)
for ii=length(temp1):-1:1
 out=[out(1:temp(temp1(ii))-1),' ''\n '' ',out(temp(temp1(ii))+1:end)];
end

%%%%Bugged
%%%temp=find(out=='/');
%%%temp1=find(~inastring_f(out,temp));
%%%
%%%for ii=1:length(temp1)
%%% out=[out(1:temp1(ii)-1),' ''\n '' ',out(temp1(ii)+1:end)];
%%%end

%out=strrep(out,'/',' ''\n '' '); 

%This still can't do things like:
%  2(f14.12,2x)

%'+++++++++++++',in, out,kb
