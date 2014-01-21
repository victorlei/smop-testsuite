%is this the declaring a var to be a type (so it has parens after it)
%  or is this the beginning or ending of a type definition
[howmany2,subscripts2,centercomma2,parens2]=hassubscript_f(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);

%'free222',funstr{i},kb

if howmany2>0
 % declaring a derived type variable
 localVar=insertLocalVar(localVar,funstrwords{i}{temp2},'type',strtrim(subscripts2{1}));
else
 % defining a derived type var

 % Now, are we at the end or the beginning of the definition?
 %'oooooooooo',funstr{i},kb
 if strcmp(funstrwords{i}{1},'end') %at the end
  temp3=localVar;
  setUpLocalVar
  goon2=1;
 else %at the beginning
  typeDefs{size(typeDefs,1)+1,1}=funstrwords{i}{j+1};
  typeDefs{size(typeDefs,1)  ,2}=localVar;
  localVar=temp3;
  funstr{i}=['%%% ',funstr{i}];
  [s,fs_good]=updatefunstr_1line_f(funstr,fs_good,i);
  goon2=0;
 end
end % if howmany>0
 

%%%if strcmp('time_series_type_module',this_fun_name)
%%% i
%%% funstr{i}
%%% temp3
%%% localVar
%%% funstrwords{i}{j}
%%% kb
%%%end