function [outflag,howmany,subscripts,centercomma,parens]=inbracket2(str,spot);

% This is to find the components [var1,var2,etc]
outflag=0;
temp=findstr(str,'[');
temp1=findstr(str,']');
if length(temp(temp<spot))>length(temp1(temp1<spot))
 outflag=1;
end
howmany=0;subscripts=[];centercomma=[];parens=zeros(1,2);
if outflag
 if str(spot)=='[', temp3=1; else temp3=0; end
 found=0;
 leftbracket=str=='[';
 rightbracket=str==']';
 both_b=leftbracket-rightbracket;   c_b=cumsum(both_b);
 poss=leftbracket&(c_b==(c_b(spot)-temp3));
 poss_loc=find(poss);
 poss_loc=poss_loc(poss_loc<spot);
 try
 parens(1)=poss_loc(end);
 catch
  poss_loc,kb
 end
 parens(2)=findrights_f(parens(1),str);
 tempstr=str;
 tempstr(1:parens(1))='0';
 tempstr(parens(2):end)='0';
 leftp=tempstr=='(';
 rightp=tempstr==')';
 both_p=leftp-rightp;                 c_p=cumsum(both_p);
 
 temp=length(findstr(',',str(parens(1):parens(2))));
 if temp==0
  howmany=1;
  subscripts{1}=str(parens(1)+1:parens(2)-1);
 elseif temp>0
  centercomma=findstr(',',str);
  centercomma=centercomma(((centercomma<parens(2))&(centercomma>parens(1))));
  found=1;  cc2=[];
  for k=1:length(centercomma)
   %Make sure we are not in any parenthesis group or any other bracket group
   if ((c_b(centercomma(k))==c_b(parens(1)))&(c_p(centercomma(k))==0))
    cc2(found)=centercomma(k);
    found=found+1;
   end
  end
  centercomma=cc2;
  howmany=length(centercomma)+1;
  tempcc=[parens(1),centercomma,parens(2)];
  for i=1:length(tempcc)-1
   subscripts{i}=str(tempcc(i)+1:tempcc(i+1)-1);
  end % for i=1:length(centercomma)-1
 end
end
