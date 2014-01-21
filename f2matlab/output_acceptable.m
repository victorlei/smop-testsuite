function [out]=output_acceptable(i,whichword,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords,whichsub,howmany,subscripts,centercomma,parens,fun_name,statementFunction,localVar,fortranfunwords,var_words,this_fun_name,funwordsML,TFops,allTypeDefs)

out=0;

% make sure the first thing in the subsscript is a word
if howmany==1
 fid=parens;
elseif howmany>1
 if whichsub==1
  fid(1)=parens(1);              fid(2)=centercomma(whichsub);
 elseif whichsub==howmany
  fid(1)=centercomma(whichsub-1);   fid(2)=parens(2);
 else
  fid(1)=centercomma(whichsub-1);   fid(2)=centercomma(whichsub);
 end
end

%'rrrrrrrrrrr',funstr{i},keyboard

if nnz((funstrwords_b{i}>fid(1))&(funstrwords_b{i}<fid(2)))>0 %must be some word in this subscript
 wordsin=find((funstrwords_b{i}>fid(1))&(funstrwords_b{i}<fid(2)));
 firstnonspace=find( (~isspace(funstr{i})) & [1:length(funstr{i})]>fid(1) & [1:length(funstr{i})]<fid(2) );
 if ~isempty(firstnonspace)
  firstnonspace=firstnonspace(1);
  if any(firstnonspace==funstrwords_b{i})
   matchedword=find(firstnonspace==funstrwords_b{i});
   %not an intrinsic word or fun_name or statementFunction or ...
   if ( ~any(strcmp(funwordsML,funstrwords{i}{matchedword})) && ...
        ~any(strcmp(funwords,funstrwords{i}{matchedword})) && ...
       ( ~any(strcmp(fun_name,funstrwords{i}{matchedword})) | ...
        strcmp(this_fun_name,funstrwords{i}{matchedword}) ) && ...
       ~any(strcmp(fortranfunwords,funstrwords{i}{matchedword})) && ...
       ~any(strcmp(var_words,funstrwords{i}{matchedword})) && ...
        ~any(strcmp(statementFunction,funstrwords{i}{matchedword})) && ...
        ~strcmpi(funstrwords{i}{matchedword},[TFops{1,2},TFops{1,3}]) && ...
        ~strcmpi(funstrwords{i}{matchedword},[TFops{2,2},TFops{2,3}]) ...
       ) || ...
        any(strcmp(funstrwords{i}{matchedword},{localVar{:,1}}))
    
    % there can't be any operators on the same level in the same subscript as this word
    c_b=cumsum((funstr{i}=='(' | funstr{i}=='{' ) - (funstr{i}==')' | funstr{i}=='}'));
    temp4=find(funstr{i}=='+' | ...
               funstr{i}=='-' | ...
               funstr{i}=='/' | ...
               funstr{i}=='*');
    temp4=temp4(temp4>fid(1) & temp4<fid(2));
    goon=1;
    if any(temp4)
     if any(c_b(temp4)==c_b(fid(1)+1))
      goon=0;
     end
    end
    % if this is cell array of strings with a subscript, then that is a problem
    % it's a bad idea because the cell array expands to a list in the output if there is a colon
    [temp5,temp6,temp7]=varType(i,matchedword,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords,localVar,allTypeDefs,var_words);
    if strcmp(temp5,'character') && length(temp6{5})>0
     [howmany,subscripts,centercomma,parens]=hassubscript_f(i,temp7,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
     if howmany>0
      if any(funstr{i}(parens(1):parens(2))==':')
       goon=0;
      end
     end
    end
      
%%%   if any(strcmpi(funstrwords{i},'LoadConstraintVectorSparse')) && strcmpi(funstrwords{i}{matchedword},'ConstraintName')
%%%    'rrrrrrrrrrr2',funstrwords{i}{matchedword},goon,keyboard
%%%   end

    if goon
     [howmany2,subscripts2,centercomma2,parens2]=hassubscript_f(i,matchedword,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
     lastnonspace=find( (~isspace(funstr{i})) & [1:length(funstr{i})]>fid(1) & [1:length(funstr{i})]<fid(2) );
     lastnonspace=lastnonspace(end);
     if howmany2==0
      lastoksub=funstrwords_e{i}(matchedword);
     else
      lastoksub=parens2(end);
     end
     %what about types

     
     done=0;
     while ~done
      if funstr{i}(lastoksub+1)=='.'
       temp2=find(funstrwords_b{i}==nextNonSpace(funstr{i},lastoksub+1));
       if ~isempty(temp2)
        [howmany2,subscripts2,centercomma2,parens2]=hassubscript_f(i,temp2,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
        if howmany2==0
         lastoksub=funstrwords_e{i}(temp2);
        else
         lastoksub=parens2(end);
        end % if howmany2==0
       end % if ~isempty(temp2)
      else
       done=1;
      end
     end
     
     if lastoksub==lastnonspace
      out=1;
     end
    end
   end
  end
 end
end
