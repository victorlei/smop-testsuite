function [howmany,subscripts,centercomma,parens]=hassubscript(i,whichword,funstr2,funstrwords_e2)
declare_globals
if nargin<3
 funstr2=funstr;
 funstrwords_e2=funstrwords_e;
end
howmany=0;subscripts=[];centercomma=[];parens=[];out=0;
temp=find(funstr2{i}=='(');
if ~isempty(temp)
 temp=temp(temp>funstrwords_e2{i}(whichword));
 if ~isempty(temp)
  temp=temp(1);
  temp1=funstr2{i}(funstrwords_e2{i}(whichword)+1:temp-1);
  temp2=length(temp1);
  if temp2==0 
   out=1;
  elseif temp2==length(find(isspace(temp1)))
   out=1;
  end
  if out
   parens(1)=temp;
   rightparen=findrights(temp,funstr2{i});
   parens(2)=rightparen;
   left=funstr2{i}=='(';
   right=funstr2{i}==')';
   both_p=left-right;                 c_p=cumsum(both_p);
   leftbracket=funstr2{i}=='[';
   rightbracket=funstr2{i}==']';
   both_b=leftbracket-rightbracket;   c_b=cumsum(both_b);
   commas=find(funstr2{i}==',');
   commas=commas(((commas>parens(1))&(commas<parens(2))));
   if ~isempty(commas)
    centercomma=commas(((c_p(commas)==c_p(parens(1)))&(c_b(commas)==c_b(parens(1)))));
   else
    centercomma=[];
   end
   howmany=length(centercomma)+1;
   for j=1:howmany
    if howmany==1
     subscripts{j}=funstr2{i}(parens(1)+1:parens(2)-1);
    else
     if j==1
      subscripts{j}=funstr2{i}(parens(1)+1:centercomma(1)-1);
     elseif j==howmany
      subscripts{j}=funstr2{i}(centercomma(j-1)+1:parens(2)-1);
     else
      subscripts{j}=funstr2{i}(centercomma(j-1)+1:centercomma(j)-1);
     end
    end
   end
  end
 end
end
