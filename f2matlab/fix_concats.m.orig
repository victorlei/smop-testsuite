function str=fix_concats(str,loc,funstr,funstrwords,funstrwords_b,funstrwords_e,funstrnumbers,funstrnumbers_b,funstrnumbers_e,fs_good,i,funwords,fortranfunwords,formats,localVar,typeDefs,var_words)

%Find entity (string delimitied with ' ', variable, or bracketed expression.

temp=find(~isspace(str));
beforeloc=temp(temp<loc); beforeloc=beforeloc(end);
afterloc =temp(temp>(loc+1)); afterloc=afterloc(1);
leftedge=[]; rightedge=[];
%first work on the before part

funstr{i},funstr{i}(1:loc+1)
[changedflag,tempstr,argDelin]=changeoperator_f(i,'//',loc,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords, fortranfunwords,formats,localVar,typeDefs,var_words);
funstr{i}(argDelin(1):argDelin(2)),funstr{i}(argDelin(3):argDelin(4))
'cccccccccccc',kb

switch str(beforeloc)
 case ''''
  found=0; numfound=0;
  for ii=beforeloc-1:-1:1
   if ~found
    if str(ii)==''''
     numfound=numfound+1;
     if ii==1
      leftedge=1;
      found=1;
     end
     if str(ii-1)~='''' & mod(numfound,2)==1
      leftedge=ii;
      found=1;
     end
    end
   end
  end
 case ']'
  leftedge=findlefts_f(beforeloc,str);
 case ')'
  leftedge=findlefts_f(beforeloc,str);
  %is this a subscript or function call?
  beforeLE=temp(temp<leftedge);
  temp1=find(funstrwords_e{i}==beforeLE(end));
  if ~isempty(temp1)
   [changedflag,tempstr,argDelin]=changeoperator_f(i,'//',loc,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords, fortranfunwords,formats,localVar,typeDefs,var_words);
   leftedge=argDelin(1);
   %leftedge=funstrwords_b{i}(temp1);
  end
 otherwise %go to the left
  [changedflag,tempstr,argDelin]=changeoperator_f(i,'//',loc,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords, fortranfunwords,formats,localVar,typeDefs,var_words);
%%%    if any(strcmp(funstrwords{i},'tmpname'))
%%%   'iiiiiiiiii',argDelin,funstr{i},funstr{i}(argDelin(1):argDelin(2)),kb
%%%  end  
  leftedge=argDelin(1);
  
%%%  beforeword=find(funstrwords_b{i}<loc);
%%%  beforeword=beforeword(end);
%%%  leftedge=funstrwords_b{i}(beforeword);
end

switch str(afterloc)
 case ''''
  found=0; numfound=0;
  for ii=afterloc+1:length(str)
   if ~found
    if str(ii)==''''
     numfound=numfound+1;
     if ii==length(str)
      rightedge=1;
      found=1;
     end
     if str(ii+1)~='''' & mod(numfound,2)==1
      rightedge=ii;
      found=1;
     end
    end
   end
  end
 case '['
  rightedge=findrights_f(afterloc,str);
 case '('
  rightedge=findrights_f(afterloc,str);
 otherwise %go to the right of the nearest word (possible indexed)
  [changedflag,tempstr,argDelin]=changeoperator_f(i,'//',loc,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords, fortranfunwords,formats,localVar,typeDefs,var_words);
%%%      if any(strcmp(funstrwords{i},'tmpname'))
%%%   'iiiiiiiiiirrr',argDelin,funstr{i},funstr{i}(argDelin(1):argDelin(2)),funstr{i}(argDelin(3):argDelin(4)),kb
%%%  end  
  rightedge=argDelin(4);
%%%  afterword=find(funstrwords_b{i}>loc);
%%%  afterword=afterword(1);
%%%  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,afterword,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
%%%  if howmany==0
%%%   rightedge=funstrwords_e{i}(afterword);
%%%  else
%%%   rightedge=parens(2);
%%%  end
end

str=[str(1:leftedge-1),'[',str(leftedge:beforeloc),',',str(afterloc:rightedge),']',str(rightedge+1:end)];

%%%beforeloc
%%%afterloc
%%%leftedge
%%%rightedge
%%%str(leftedge:beforeloc),str(afterloc:rightedge)
%%%str,'grrrrrrrrrr])',kb
