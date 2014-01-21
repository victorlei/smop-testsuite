function parens2=needdouble(i,j,k,howmany,parens,centercomma,funstr,funstrwords,funstrwords_b,funstrwords_e,inoutother,cw);
parens2=cell(1,2);parens2{1}='';parens2{2}='';
if howmany==1
 left=parens(1); right=parens(2);
 subscript=funstr{i}(left:right);
else
 %funstr{i},i,j,keyboard
 if k==1
  left=parens(1); right=centercomma(1);
  subscript=funstr{i}(left:right);
 elseif k==howmany
  left=centercomma(length(centercomma)); right=parens(2);
  subscript=funstr{i}(left:right);
 else
  left=centercomma(k-1); right=centercomma(k);
  subscript=funstr{i}(left:right);
 end
end
totest=find((funstrwords_b{i}>left)&(funstrwords_b{i}<right));
if ~isempty(totest)
 for ii=1:length(totest)
  if length(find(strcmp(funstrwords{i}{totest(ii)},inoutother{3})))>0
   jj=find(strcmp(funstrwords{i}{totest(ii)},inoutother{3}));
   if ~(length(find(getfield(cw,inoutother{3}{jj})==round(getfield(cw,inoutother{3}{jj}))))<prod(size(getfield(cw,inoutother{3}{jj}))))
    parens2{1}='dble(';   parens2{2}=')';
   end
  end
 end
end
