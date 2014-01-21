function [howmany,s1,centercomma,parens]=hassubscript_f(i,whichword,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords)

temp=find(funstr{i}=='(' | funstr{i}=='{');

%%%if strcmp(funstrwords{i}{whichword},'character')
%%% 'oooooooooo',funstr{i},kb
%%%end
if ~isempty(temp)
 temp5=funstr{i}=='''';
 temp6=cumsum(temp5);
 temp7=temp6/2~=round(temp6/2);

 temp=temp(temp>funstrwords_e{i}(whichword)&~temp7(temp));
 %temp=temp(temp>funstrwords_e{i}(whichword)&~inastring_f(funstr{i},temp));
 %temp=temp(temp>funstrwords_e{i}(whichword));
 if ~isempty(temp)
  temp=temp(1);
  temp1=funstr{i}(funstrwords_e{i}(whichword)+1:temp-1);
  temp2=length(temp1);
  out=0;
  if temp2==0 
   out=1;
  elseif all(isspace(temp1))
   out=1;
  end

  if out
   parens(1)=temp;
   rightparen=findrights_f(temp,funstr{i},1);
   parens(2)=rightparen;
   left=(funstr{i}=='('|funstr{i}=='{') & ~temp7;
   %left(find(left))=~inastring_f(funstr{i},find(left));
   right=(funstr{i}==')'|funstr{i}=='}') & ~temp7;
   %right(find(right))=~inastring_f(funstr{i},find(right));
   both_p=left-right;                 c_p=cumsum(both_p);
   leftbracket=funstr{i}=='[' & ~temp7;
   %leftbracket(find(leftbracket))=~inastring_f(funstr{i},find(leftbracket));
   rightbracket=funstr{i}==']' & ~temp7;
   %rightbracket(find(rightbracket))=~inastring_f(funstr{i},find(rightbracket));
   both_b=leftbracket-rightbracket;   c_b=cumsum(both_b);
   commas=find(funstr{i}==',');
   commas=commas(((commas>parens(1))&(commas<parens(2))));
   commas=commas(find(~temp7(commas)));
   %commas=commas(find(~inastring_f(funstr{i},commas)));
   if ~isempty(commas)
    centercomma=commas(((c_p(commas)==c_p(parens(1)))&(c_b(commas)==c_b(parens(1)))));
   else
    centercomma=[];
   end
   howmany=length(centercomma)+1;
   for j=1:howmany
    if howmany==1
     s1{j}=funstr{i}(parens(1)+1:parens(2)-1);
    else
     if j==1
      s1{j}=funstr{i}(parens(1)+1:centercomma(1)-1);
     elseif j==howmany
      s1{j}=funstr{i}(centercomma(j-1)+1:parens(2)-1);
     else
      s1{j}=funstr{i}(centercomma(j-1)+1:centercomma(j)-1);
     end
    end
   end
   if howmany==1
    if length(find(~isspace(s1{1})))==0
     howmany=0;
    end % if length(find(~isspace(s1{1})))==0
   end % if howmany==1
  else
   howmany=0;
   s1=[];
   centercomma=[];
   parens=[];
  end
 else
  howmany=0;
  s1=[];
  centercomma=[];
  parens=[];
 end
else
 howmany=0;
 centercomma=[];
 parens=[];
 s1=[];
end











%%%temp=strfind(funstr{i},'(');
%%%
%%%%%%if strcmp(funstrwords{i}{whichword},'character')
%%%%%% 'oooooooooo',funstr{i},kb
%%%%%%end
%%%if ~isempty(temp)
%%% temp5=funstr{i}=='''';
%%% temp6=cumsum(temp5);
%%% temp7=temp6/2~=round(temp6/2);
%%%
%%% temp=temp(temp>funstrwords_e{i}(whichword)&~temp7(temp));
%%% %temp=temp(temp>funstrwords_e{i}(whichword)&~inastring_f(funstr{i},temp));
%%% %temp=temp(temp>funstrwords_e{i}(whichword));
%%% if ~isempty(temp)
%%%  temp=temp(1);
%%%  temp1=funstr{i}(funstrwords_e{i}(whichword)+1:temp-1);
%%%  temp2=length(temp1);
%%%  out=0;
%%%  if temp2==0 
%%%   out=1;
%%%  elseif all(isspace(temp1))
%%%   out=1;
%%%  end
%%%
%%%  if out
%%%   parens(1)=temp;
%%%   rightparen=findrights_f(temp,funstr{i},1);
%%%   parens(2)=rightparen;
%%%   left=funstr{i}=='(' & ~temp7;
%%%   %left(find(left))=~inastring_f(funstr{i},find(left));
%%%   right=funstr{i}==')' & ~temp7;
%%%   %right(find(right))=~inastring_f(funstr{i},find(right));
%%%   both_p=left-right;                 c_p=cumsum(both_p);
%%%   leftbracket=funstr{i}=='[' & ~temp7;
%%%   %leftbracket(find(leftbracket))=~inastring_f(funstr{i},find(leftbracket));
%%%   rightbracket=funstr{i}==']' & ~temp7;
%%%   %rightbracket(find(rightbracket))=~inastring_f(funstr{i},find(rightbracket));
%%%   both_b=leftbracket-rightbracket;   c_b=cumsum(both_b);
%%%   commas=find(funstr{i}==',');
%%%   commas=commas(((commas>parens(1))&(commas<parens(2))));
%%%   commas=commas(find(~temp7(commas)));
%%%   %commas=commas(find(~inastring_f(funstr{i},commas)));
%%%   if ~isempty(commas)
%%%    centercomma=commas(((c_p(commas)==c_p(parens(1)))&(c_b(commas)==c_b(parens(1)))));
%%%   else
%%%    centercomma=[];
%%%   end
%%%   howmany=length(centercomma)+1;
%%%   for j=1:howmany
%%%    if howmany==1
%%%     s1{j}=funstr{i}(parens(1)+1:parens(2)-1);
%%%    else
%%%     if j==1
%%%      s1{j}=funstr{i}(parens(1)+1:centercomma(1)-1);
%%%     elseif j==howmany
%%%      s1{j}=funstr{i}(centercomma(j-1)+1:parens(2)-1);
%%%     else
%%%      s1{j}=funstr{i}(centercomma(j-1)+1:centercomma(j)-1);
%%%     end
%%%    end
%%%   end
%%%   if howmany==1
%%%    if length(find(~isspace(s1{1})))==0
%%%     howmany=0;
%%%    end % if length(find(~isspace(s1{1})))==0
%%%   end % if howmany==1
%%%  else
%%%   howmany=0;
%%%   s1=[];
%%%   centercomma=[];
%%%   parens=[];
%%%  end
%%% else
%%%  howmany=0;
%%%  s1=[];
%%%  centercomma=[];
%%%  parens=[];
%%% end
%%%else
%%% howmany=0;
%%% centercomma=[];
%%% parens=[];
%%% s1=[];
%%%end
%%%
%%%
%%%
%%%
%%%
%%%
%%%
%%%
%%%
%%%
%%%
