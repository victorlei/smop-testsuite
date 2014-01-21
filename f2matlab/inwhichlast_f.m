function [outflag,howmany,subscripts,centercomma,parens,whichword]=inwhichlast_f(i,spot,funstr,varargin);

testcon=1;
outflag=0;
howmany=0;subscripts=[];centercomma=[];parens=[];whichword=[];

% this doesn't parse matlab correctly because of the transpose single quote.
temp5=funstr{i}=='''';
temp6=cumsum(temp5);
temp7=temp6/2~=round(temp6/2);

left=funstr{i}=='(' & ~temp7;
right=funstr{i}==')' & ~temp7;
both_p=left-right;                 c_p=cumsum(both_p);
leftbracket=funstr{i}=='[' & ~temp7;
rightbracket=funstr{i}==']' & ~temp7;
both_b=leftbracket-rightbracket;   c_b=cumsum(both_b);

%%%   left=funstr{i}=='(';
%%%   left(find(left))=~inastring_f(funstr{i},find(left));
%%%   right=funstr{i}==')';
%%%   right(find(right))=~inastring_f(funstr{i},find(right));
%%%   both_p=left-right;                 c_p=cumsum(both_p);
%%%   leftbracket=funstr{i}=='[';
%%%   leftbracket(find(leftbracket))=~inastring_f(funstr{i},find(leftbracket));
%%%   rightbracket=funstr{i}==']';
%%%   rightbracket(find(rightbracket))=~inastring_f(funstr{i},find(rightbracket));
%%%   both_b=leftbracket-rightbracket;   c_b=cumsum(both_b);

%%%left=funstr{i}=='(';
%%%right=funstr{i}==')';
%%%both_p=left-right;                 c_p=cumsum(both_p);
%%%leftbracket=funstr{i}=='[';
%%%rightbracket=funstr{i}==']';
%%%both_b=leftbracket-rightbracket;   c_b=cumsum(both_b);
goon=0;
%if
%(length(find(find(leftbracket)<spot))>length(find(find(rightbracket)<spot)))|(length(find(find(left)<spot))>length(find(find(right)<spot)))
if c_p(spot)>0 || c_b(spot)>0
 goon=1;
 % If there is a ( or [ on spot, we have to subtract 1
 if length(find(spot==find(left)))>0,  temp=1; else temp=0; end
 poss_p=find(left&(c_p==(c_p(spot)-temp)));
 %poss_p=find(left&((c_p-temp)==c_p(spot)));
 poss_p=poss_p(poss_p<spot);
 poss_p=[0 poss_p];
 poss_p=poss_p(end);
 temp=0;
 if ~isempty(find(leftbracket))
  if length(find(spot==find(leftbracket)))>0,  temp=1;  end
 end
 poss_b=find(leftbracket&(c_b==(c_b(spot)-temp)));
 %poss_b=find(leftbracket&((c_b-temp)==c_b(spot)));
 poss_b=poss_b(poss_b<spot);
 poss_b=[0 poss_b];
 poss_b=poss_b(end);
 j=max([poss_p poss_b]);
 if j==poss_b
  outflag=2; %bracket last
 else
  outflag=1; %paren last
 end
 parens(1)=j;
 if j~=0
  parens(2)=findrights_f(j,funstr{i},1);
 else
  parens(2)=0; outflag=0;
 end
end

%%%%if we are in parens, is there a word directly to the left?
%%%if outflag==1
%%% temp1=lastNonSpace(funstr{i},parens(1));
%%% temp2=find(temp1==funstrwords_e{i});
%%% if ~isempty(temp2)
%%%  whichword=temp2;
%%% end
%%%end

%funstr{i},'iwl',kb

