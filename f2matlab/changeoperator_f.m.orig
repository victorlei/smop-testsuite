function [changedflag,funstr,argDelin]=changeoperator_f(i,operator,loc,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords,fortranfunwords,formats,localVar,typeDefs,var_words);
%declare_globals

nif=cell(2,1);neednif=0;r=char(10);
changedflag=0;
argDelin=zeros(1,4);
temp=find(~isspace(funstr{i}) & funstr{i}~='-' & funstr{i}~='+');
temp=temp(temp>(loc+length(operator)-1));temp=temp(1);
temp1=strcmp('''',operator(length(operator)));
leftarg_b=0;leftarg_e=0; rightarg_b=0;rightarg_e=0;
%First the right side.
if ~temp1
 if isnameletter(funstr{i}(temp)) | length(find(temp==funstrwords_b{i}))>0
  temp2=find(funstrwords_b{i}==temp);
  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,temp2,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
  if ~isempty(parens)
   rightarg_b=temp;
   rightarg_e=parens(2);
   % in rare cases such as this, we need another check
   %  Hx(Iperm(i))(Strbeg:Strend)>Hx(lm)(Strbeg:Strend)
   temp7=nextNonSpace(funstr{i},rightarg_e);
   if funstr{i}(temp7)=='('
    rightarg_e=findrights_f(temp7,funstr{i});
   end % if funstr{i}(nextNonSpace())=='('
  else
   rightarg_b=temp;
   rightarg_e=funstrwords_e{i}(temp2);
  end
  
  for iii=1:10
   % but if this is a type, it may not be the end...
   if length(funstr{i})>rightarg_e && funstr{i}(rightarg_e+1)=='.' && ...
        isnameletter(funstr{i}(rightarg_e+2))
    temp=rightarg_e+2;
    temp2=find(funstrwords_b{i}==temp);
    if ~strcmp(funstrwords{i}{temp2},'not')
     [howmany,subscripts,centercomma,parens]=hassubscript_f(i,temp2,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
     if ~isempty(parens)
      %rightarg_b=temp; %still begins back at the first word
      rightarg_e=parens(2);
     else
      %rightarg_b=temp;
      rightarg_e=funstrwords_e{i}(temp2);
     end
    end % if ~strcmp(funstrwords{i}{temp2},
   end % if length(funstr{i})>rightarg_e && funstr{i}(rightarg_e+1)=='.
  end % for i=1:10

 elseif isnumber(funstr{i}(temp)) | any(funstrnumbers_b{i}==temp)
  temp2=find(funstrnumbers_b{i}>=temp);   temp2=temp2(1);
  rightarg_b=temp;
  rightarg_e=funstrnumbers_e{i}(temp2);
 elseif ((strcmp(funstr{i}(temp),'('))|(strcmp(funstr{i}(temp),'['))|(strcmp(funstr{i}(temp),'{')))
  rightarg_b=temp;
  rightarg_e=findrights_f(temp,funstr{i},1);
 elseif ((strcmp(funstr{i}(temp),'''')))
  rightarg_b=temp;
  temp2=find(funstr{i}=='''');
  temp2=temp2(temp2>temp);
  rightarg_e=temp2(1);
 else % commas, etc
      % catches things like:
      % 99001 FORMAT ('1 QUICK CHECK FOR SPLINE ROUTINES',//)
  rightarg_b=temp+1;
  rightarg_e=temp; 
 end
end
%Next the left side.
temp=find(~isspace(funstr{i}));temp=temp(temp<loc);temp=temp(length(temp));
if length(find(temp==funstrnumbers_e{i}))>0
 temp2=find(funstrnumbers_e{i}==temp);
 leftarg_e=temp;
 leftarg_b=funstrnumbers_b{i}(temp2);
elseif length(find(temp==funstrwords_e{i}))>0
 temp2=find(funstrwords_e{i}==temp);
 leftarg_e=temp;
 leftarg_b=funstrwords_b{i}(temp2);
 
 for iii=1:10
  if leftarg_b>1 && funstr{i}(leftarg_b-1)=='.'
   temp=find(~isspace(funstr{i}));temp=temp(temp<(leftarg_b-1));temp=temp(length(temp));
   if strcmp(funstr{i}(temp),')')
    leftparen=findlefts_f(temp,funstr{i});
    if length(find(funstrwords_e{i}==(leftparen-1)))>0
     temp2=find(funstrwords_e{i}==(leftparen-1));
     leftarg_b=funstrwords_b{i}(temp2);
    else
     leftarg_b=leftparen;
    end
   elseif length(find(temp==funstrwords_e{i}))>0
    temp2=find(funstrwords_e{i}==temp);
    if ~strcmp(funstrwords{i}{temp2},'not')
     leftarg_b=funstrwords_b{i}(temp2);
    end
   end
  end
 end

elseif ((strcmp(funstr{i}(temp),')'))|(strcmp(funstr{i}(temp),']'))|(strcmp(funstr{i}(temp),'}')))
 leftarg_e=temp;
 leftparen=findlefts_f(temp,funstr{i});
 % for cases like: list_of_mnemonics(k)(1:plen) .eq. temp_mnemonic(1:plen)
 if funstr{i}(leftparen-1)==')'
  leftparen=findlefts_f(leftparen-1,funstr{i});
 end
 if length(find(funstrwords_e{i}==(leftparen-1)))>0
  temp2=find(funstrwords_e{i}==(leftparen-1));
  leftarg_b=funstrwords_b{i}(temp2);
  
  for iii=1:10
   if leftarg_b>1 && funstr{i}(leftarg_b-1)=='.'
    temp=find(~isspace(funstr{i}));temp=temp(temp<(leftarg_b-1));temp=temp(length(temp));
    if strcmp(funstr{i}(temp),')')
     leftparen=findlefts_f(temp,funstr{i});
     if length(find(funstrwords_e{i}==(leftparen-1)))>0
      temp2=find(funstrwords_e{i}==(leftparen-1));
      leftarg_b=funstrwords_b{i}(temp2);
     else
      leftarg_b=leftparen;
     end
    elseif length(find(temp==funstrwords_e{i}))>0
     temp2=find(funstrwords_e{i}==temp);
     if ~strcmp(funstrwords{i}{temp2},'not')
      leftarg_b=funstrwords_b{i}(temp2);
     end
    end
   end
  end
  
 else
  leftarg_b=leftparen;
 end
elseif ((strcmp(funstr{i}(temp),'''')))
 leftarg_e=temp;
 temp2=find(funstr{i}=='''');
 temp2=temp2(temp2<temp);
 leftarg_b=temp2(end);
else
 leftarg_b=temp+1;
 leftarg_e=temp;
end





%%%if strcmp(operator,'~=')
%%% if any(strcmpi(funstrwords{i},'TargetName')) && any(strcmpi(funstrwords{i},'ip')) && any(strcmpi(funstrwords{i},'DefaultTarget'))
%%%  funstr{i}(leftarg_b:leftarg_e),funstr{i}(rightarg_b:rightarg_e)
%%%  funstr{i},'rrrrrrrrrrrrr',operator,kb
%%% end
%%%end


argDelin=[leftarg_b,leftarg_e,rightarg_b,rightarg_e];
if ~temp1
 %So we have anything but a transpose operator
 %if the first word in either of the args is a string or char, then do it this way
 goon=0;
 %left arg
 temp4=find(funstrwords_b{i}>=leftarg_b & funstrwords_b{i}<=leftarg_e);
 if ~isempty(temp4)
  temp4=temp4(1);
  %fid=find(strcmp(funstrwords{i}{temp4},{localVar{:,1}}));
  temp5=varType(i,temp4,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords,localVar,typeDefs,var_words);
  if strcmp(temp5,'character')
   %if ~isempty(fid) && strcmp(localVar{fid,3},'character')
   goon=1;
  end % if ~isempty(fid) && strcmp(localVar{fid,
 end % if ~isempty(temp4)
     %right arg
 temp4=find(funstrwords_b{i}>=rightarg_b & funstrwords_b{i}<=rightarg_e);
 if ~isempty(temp4)
  temp4=temp4(1);
  temp5=varType(i,temp4,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords,localVar,typeDefs,var_words);
  if strcmp(temp5,'character')
%%%  fid=find(strcmp(funstrwords{i}{temp4},{localVar{:,1}}));
%%%  if ~isempty(fid) && strcmp(localVar{fid,3},'character')
   goon=1;
  end % if ~isempty(fid) && strcmp(localVar{fid,
 end % if ~isempty(temp4)


 
 switch operator
  case {'==','~='}
   if funstr{i}(leftarg_b)=='''' | funstr{i}(rightarg_b)=='''' | goon
    temp6=''; if operator(1)=='~', temp6='~'; end
    %'t6666666666666',funstr{i}(leftarg_b:leftarg_e),funstr{i}(rightarg_b:rightarg_e),kb
    if (all(isspace(funstr{i}(rightarg_b+1:rightarg_e-1))) & length(rightarg_b+1:rightarg_e-1)>0) | strcmpi(funstr{i}(rightarg_b:rightarg_e),'''''')
     funstr{i}=[funstr{i}(1:(leftarg_b-1)),temp6,'isempty(strtrim(',...
                funstr{i}(leftarg_b:leftarg_e),'))',funstr{i}((rightarg_e+1):end)];
    else
     % should this be strcmp or strcmpi? 
     % fortran is case sensitive for string comparison, so strcmp
     funstr{i}=[funstr{i}(1:(leftarg_b-1)),temp6,'strcmp(deblank(',funstr{i}(leftarg_b:leftarg_e),'),deblank(',funstr{i}(rightarg_b:rightarg_e),'))',funstr{i}((rightarg_e+1):end)];
    end
    changedflag=1;
   end
  case {'<','>','<=','>='}
   % fix these for character arrays to call strrel
   %funstr{i},'rrrrrrrrrrrrr2',operator,kb
   if funstr{i}(loc-1)~='=' && length(funstrwords{i})>0 && any(strcmp(funstrwords{i}{1},{'if','elseif'}))
    if funstr{i}(leftarg_b)=='''' | funstr{i}(rightarg_b)=='''' | goon
     funstr{i}=[funstr{i}(1:(leftarg_b-1)),'strrel(',funstr{i}(leftarg_b:leftarg_e),',',funstr{i}(rightarg_b:rightarg_e),',''',funstr{i}(loc:loc+length(operator)-1),''')',funstr{i}((rightarg_e+1):end)];
     changedflag=1;
%%%    funstr{i}(leftarg_b:leftarg_e),funstr{i}(rightarg_b:rightarg_e)
%%%    funstr{i},'rrrrrrrrrrrrr2',operator,kb
    end
   end
   


%%%  case {'=='}
%%%   if funstr{i}(leftarg_b)=='''' | funstr{i}(rightarg_b)=='''' | goon
%%%    funstr{i}=[funstr{i}(1:(leftarg_b-1)),'strcmpi(',funstr{i}(leftarg_b:leftarg_e),',',funstr{i}(rightarg_b:rightarg_e),')',funstr{i}((rightarg_e+1):end)];
%%%%%%    funstr{i}(leftarg_b:leftarg_e),funstr{i}(rightarg_b:rightarg_e)
%%%%%%    funstr{i},'rrrrrrrrrrrrr2',operator,kb
%%%    changedflag=1;
%%%   end
%%%  case {'~='}
%%%   if funstr{i}(leftarg_b)=='''' | funstr{i}(rightarg_b)=='''' | goon
%%%    funstr{i}=[funstr{i}(1:(leftarg_b-1)),'~strcmpi(',funstr{i}(leftarg_b:leftarg_e),',',funstr{i}(rightarg_b:rightarg_e),')',funstr{i}((rightarg_e+1):end)];
%%%%%%    funstr{i}(leftarg_b:leftarg_e),funstr{i}(rightarg_b:rightarg_e)
%%%%%%    funstr{i},'rrrrrrrrrrrrr2',operator,kb
%%%    changedflag=1;
%%%   end


%%%  case {'./','.*','.^'}
%%%   funstr{i}=[funstr{i}(1:loc-1),funstr{i}(loc+1:length(funstr{i}))];
%%%   changedflag=1;
%%%%%%   case '.*'
%%%%%%    funstr{i}=[funstr{i}(1:loc-1),'UniqueMultiplierPlaceholder',...
%%%%%%               funstr{i}(loc+2:length(funstr{i}))];
%%%%%%    changedflag=1;
%%%  case '*'
%%%   temp5=cell(2,4);
%%%   temp5{1,1}='';         temp5{1,2}='';         temp5{1,3}='';         temp5{1,4}='';
%%%   temp5{2,1}='';         temp5{2,2}='';         temp5{2,3}='';         temp5{2,4}='';
%%%   if want_op~=-1
%%%    if ((temp3(1)==0)&(temp3(2)==0))
%%%     %      funstr{i},'phone',kb
%%%     funstr{i}=[funstr{i}(1:loc-1),'.',funstr{i}(loc+1:length(funstr{i}))];
%%%     %funstr{i}=[funstr{i}(1:loc-1),'@#',funstr{i}(loc+1:length(funstr{i}))];
%%%     changedflag=1;
%%%
%%%%%%      for k=1:2
%%%%%%       if any(strcmp(tempstr(k),typs{4}))
%%%%%%        temp5{k,1}='spread(';    temp5{k,2}=',1,1)';
%%%%%%       elseif any(strcmp(tempstr(2),typs{5}))
%%%%%%        temp5{k,3}='[';         temp5{k,4}=']';
%%%%%%       end
%%%%%%      end
%%%%%%      %funstr{i},funstr{i}(leftarg_b:leftarg_e),funstr{i}(rightarg_b:rightarg_e),temp3,kb
%%%%%%      funstr{i}=[funstr{i}(1:(leftarg_b-1)),operators{temp4,2},'(',temp5{1,1},temp5{1,3},funstr{i}(leftarg_b:leftarg_e),temp5{1,4},temp5{1,2},',',temp5{2,1},temp5{2,3},funstr{i}(rightarg_b:rightarg_e),temp5{2,4},temp5{2,2},')',funstr{i}((rightarg_e+1):length(funstr{i}))];
%%%%%%      %funstr{i},temp3,kb
%%%%%%      neednif=1;
%%%     changedflag=1;
%%%    end
%%%   end
%%%  case '\'
%%%   funstr{i}=[funstr{i}(1:(leftarg_b-1)),'Inverse(',funstr{i}(leftarg_b:leftarg_e),').',funstr{i}(rightarg_b:rightarg_e),funstr{i}((rightarg_e+1):end)];
%%%%%%    funstr{i},funstr{i}(leftarg_b:leftarg_e),funstr{i}(rightarg_b:rightarg_e)
%%%%%%    disp('we have a left divide!'),kb
 end
else
%%% if exist('leftarg_b')==1
%%%  subscripts{1}=funstr{i}([leftarg_b:leftarg_e]);
%%%  goon=1;
%%%  nums=find(funstrnumbers_b{i}>=leftarg_b & funstrnumbers_b{i}<=leftarg_e);
%%%  wrds=find(funstrwords_b{i}>=leftarg_b & funstrwords_b{i}<=leftarg_e);
%%%  if length(nums)==0 & length(wrds)==1
%%%   if ~any(strcmp(subscripts{1},inoutother3))
%%%    goon=0;
%%%   end
%%%  end
%%%  %funstr{i},funstr{i}(leftarg_b:leftarg_e),'...........',goon,kb  
%%%  if goon
%%%%%% funstr{i},funstr{i}(leftarg_b:leftarg_e)
%%%   howmany=1;
%%%   parens(1)=leftarg_b;   parens(2)=leftarg_e;
%%%   tempstr=makeMATLABcallstring_mma(howmany,subscripts,[leftarg_b leftarg_e],parens,i,inf);
%%%   if any(strcmp(tempstr(1),typs{11})),temp3(1)=0;else temp3(1)=1;end
%%%   %tempstr=makeMATLABcallstring_mma(1,{funstr{i}(leftarg_b:leftarg_e)},[],[leftarg_b,leftarg_e],i,j);
%%%   if any(strcmp(tempstr(1),typs{3}))       %Complex argument
%%%    temp5{1}='Conjugate[';  temp5{2}=']';
%%%   else                                     %Real argument
%%%    temp5{1}='';        temp5{2}='';
%%%   end
%%%   if any(strcmp(tempstr(1),{typs{9}{:},typs{10}{:}}))       % 1-d argument
%%%    ;%Then we don't need the transpose part
%%%    temp6{1}='';  temp6{2}='';
%%%   else
%%%    temp6{1}='Transpose(';  temp6{2}=')';
%%%   end
%%%   if strcmp('.''',operators{temp4,1})     %Regular transpose
%%%    if temp3(1) %scalar, doesn't need transpose
%%%     funstr{i}=[funstr{i}(1:leftarg_e),funstr{i}((leftarg_e+1+length(operator)):length(funstr{i}))];
%%%    else 
%%%     funstr{i}=[funstr{i}(1:(leftarg_b-1)),temp6{1},funstr{i}(leftarg_b:leftarg_e),temp6{2},funstr{i}((leftarg_e+1+length(operator)):length(funstr{i}))];
%%%     %funstr{i}=[funstr{i}(1:(leftarg_b-1)),'Transpose[',funstr{i}(leftarg_b:leftarg_e),']',funstr{i}((leftarg_e+1+length(operator)):length(funstr{i}))];
%%%    end
%%%   else                                    %Conjugate transpose
%%%    if temp3(1)
%%%     funstr{i}=[funstr{i}(1:(leftarg_b-1)),temp5{1},funstr{i}(leftarg_b:leftarg_e),temp5{2},funstr{i}((leftarg_e+1+length(operator)):length(funstr{i}))];
%%%    else 
%%%     funstr{i}=[funstr{i}(1:(leftarg_b-1)),temp6{1},temp5{1},funstr{i}(leftarg_b:leftarg_e),temp5{2},temp6{2},funstr{i}((leftarg_e+1+length(operator)):length(funstr{i}))];
%%%     %funstr{i}=[funstr{i}(1:(leftarg_b-1)),'Transpose[',temp5{1},funstr{i}(leftarg_b:leftarg_e),temp5{2},']',funstr{i}((leftarg_e+1+length(operator)):length(funstr{i}))];
%%%    end
%%%   end
%%%   changedflag=1;
%%%%%% funstr{i},kb
%%%  end
%%% end
end
%%%if changedflag
%%% updatefunstr(i);
%%%end




%%%if ~temp1
%%% if isnameletter(funstr{i}(temp)) | length(find(temp==funstrwords_b{i}))>0
%%%  temp2=find(funstrwords_b{i}==temp);
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,temp2,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany>0
%%%   rightarg_b=temp;
%%%   rightarg_e=parens(2);
%%%  else
%%%   rightarg_b=temp;
%%%   rightarg_e=funstrwords_e{i}(temp2);
%%%  end
%%% elseif isnumber(funstr{i}(temp)) | any(funstrnumbers_b{i}==temp)
%%%  temp2=find(funstrnumbers_b{i}>=temp);   temp2=temp2(1);
%%%  rightarg_b=temp;
%%%  rightarg_e=funstrnumbers_e{i}(temp2);
%%% elseif ((strcmp(funstr{i}(temp),'('))|(strcmp(funstr{i}(temp),'['))|(strcmp(funstr{i}(temp),'{')))
%%%  rightarg_b=temp;
%%%  rightarg_e=findrights_f(temp,funstr{i});
%%% elseif ((strcmp(funstr{i}(temp),'''')))
%%%  rightarg_b=temp;
%%%  temp2=find(funstr{i}=='''');
%%%  temp2=temp2(temp2>temp);
%%%  rightarg_e=temp2(1);
%%% end
%%%end
