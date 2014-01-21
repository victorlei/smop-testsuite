function [out,outLine,j]=varType(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords,localVar,typeDefs,var_words)
  
out='';outLine={};

temp1=find(strcmp(funstrwords{i}{j},{localVar{:,1}}));
%'ccccccccc',kb
if ~isempty(temp1)
 if any(strcmp(localVar{temp1,3},var_words))
  out=localVar{temp1,3};
  outLine={localVar{temp1,:}};
 else
  [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
  temp5=funstrwords_e{i}(j);
  if howmany>0,   temp5=parens(2);  end
  temp2=nextNonSpace(funstr{i},temp5);
  %'vttttttttt',kb
  if funstr{i}(temp2)=='.'
   temp3=find(nextNonSpace(funstr{i},temp2)==funstrwords_b{i});
   if ~isempty(temp3)
%%%    if strcmp(funstrwords{i}{1},'tv1')
%%%     'tttttttt11',localVar,kb
%%%    end
    temp4=find(strcmp(localVar{temp1,3},{typeDefs{:,1}}));
    if ~isempty(temp4)
     [out,outLine,j]=varType(i,temp3,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords,typeDefs{temp4,2},typeDefs,var_words);
    end % if ~isempty(temp4)
   end
  else
   %derived type, but no '.' after, so out should be the type
   out=localVar{temp1,3};
   outLine={localVar{temp1,:}};
  end
 end % if any(strcmp(localVar{temp1,
end % if ~isempty(temp1)
