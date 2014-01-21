function [out,howmany,subscripts,centercomma,parens]=iscomplexf(i,whichopenparen,temp,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords)
howmany=0;subscripts=[];centercomma=[];parens=[];out=0;goon=0;

temp1=find(~isspace(funstr{i})); temp1=temp1(temp1<temp(whichopenparen));
if ~isempty(temp1)
 temp1=temp1(end);
 if ~any(funstrwords_e{i}==temp1)
  parens(1)=temp(whichopenparen);
  rightparen=findrights_f(temp(whichopenparen),funstr{i});
  parens(2)=rightparen;
  left=funstr{i}=='(';
  right=funstr{i}==')';
  both_p=left-right;                 c_p=cumsum(both_p);
  leftbracket=funstr{i}=='[';
  rightbracket=funstr{i}==']';
  both_b=leftbracket-rightbracket;   c_b=cumsum(both_b);
  commas=find(funstr{i}==',');
  commas=commas(((commas>parens(1))&(commas<parens(2))));
  if ~isempty(commas)
   centercomma=commas(((c_p(commas)==c_p(parens(1)))&(c_b(commas)==c_b(parens(1)))));
  else
   centercomma=[];
  end
  howmany=length(centercomma)+1;
%%%  for j=1:howmany
%%%   if howmany==1
%%%    subscripts{j}=funstr{i}(parens(1)+1:parens(2)-1);
%%%   else
%%%    if j==1
%%%     subscripts{j}=funstr{i}(parens(1)+1:centercomma(1)-1);
%%%    elseif j==howmany
%%%     subscripts{j}=funstr{i}(centercomma(j-1)+1:parens(2)-1);
%%%    else
%%%     subscripts{j}=funstr{i}(centercomma(j-1)+1:centercomma(j)-1);
%%%    end
%%%   end
%%%  end
 end
end

if howmany==2
 out=1;
end