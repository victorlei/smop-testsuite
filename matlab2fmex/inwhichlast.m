function [outflag,howmany,subscripts,centercomma,parens,callword]=inwhichlast(i,spot);
%Key:
% outflag==0  ==>  in nothing at spot, return any mathematical nesting parenthesis, or [1 end]
% outflag==1  ==>  in bracket last
% outflag==2  ==>  in subscript last
% outflag==3  ==>  in make_converted function last
% outflag==4  ==>  in submitted subroutine call last
% outflag==5  ==>  in regular function call last (from intrinsics list)
% outflag==6  ==>  in regular function call last (not from intrinsics list (yet to be converted?))
declare_globals
outflag=0;howmany=0;subscripts=[];centercomma=[];parens=[];
temp1=find(~isspace(funstr{i}));
callword='';

left=funstr{i}=='(';
right=funstr{i}==')';
both_p=left-right;                 c_p=cumsum(both_p);
leftbracket=funstr{i}=='[';
rightbracket=funstr{i}==']';
both_b=leftbracket-rightbracket;   c_b=cumsum(both_b);
goon=0;
try
if (length(find(find(leftbracket)<spot))>length(find(find(rightbracket)<spot)))|(length(find(find(left)<spot))>length(find(find(right)<spot)))
 leftp=find(left);
 poss_p=leftp(leftp<spot);
 leftbracketp=find(leftbracket);
 poss_b=leftbracketp(leftbracketp<spot);
 poss=sort([poss_p poss_b]);
 for k=length(poss):-1:1
  if findrights(poss(k),funstr{i})>spot
   %if a word doesn't immediately precede this '(' then it is mathematical or superfluous
   %In this case, we should set j to the last unclosed '(' or '['
   %or return if there is no such '(' or '['.
   if funstr{i}(poss(k))=='['
    goon=1; j=poss(k); break
   else
    temp2=temp1(temp1<poss(k));
    if ~isempty(temp2)
     if length(funstrwords{i})>0
      if any(temp2(end)==funstrwords_e{i})
       if funstr{i}(poss(k))=='['
        goon=1; j=poss(k); break
       elseif funstr{i}(poss(k))=='('
        goon=1; j=poss(k); break
       end
      end
     end
    end
   end
  end
 end
end
catch
 funstr{i},spot,disp('is there a mismatched parenthesis or bracket?');keyboard
end
% if zzz
%  'eeeeeeeeee',goon,j,funstr{i},funstr{i}(1:j),zzz=0;kb
% end
if goon
 if strcmp(funstr{i}(j),'(')
  temp1=temp1(temp1<j);
  temp2=find(temp1(length(temp1))==funstrwords_e{i});
  if ~isempty(temp2)
   callword=temp2;
   if length(find(strcmpi(funstrwords{i}{temp2},inoutother3)))>0
    [howmany,subscripts,centercomma,parens]=hassubscript(i,temp2);
    if parens(2)>spot
     %In subscript last
     outflag=2;
    end
   elseif length(find(strcmpi(funstrwords{i}{temp2},make_words)))>0
    [howmany,subscripts,centercomma,parens]=hassubscript(i,temp2);
    if parens(2)>spot
     %In make_converted function last
     outflag=3;
    end
   elseif length(find(strcmpi(funstrwords{i}{temp2},filename_all)))>0
    [howmany,subscripts,centercomma,parens]=hassubscript(i,temp2);
    if parens(2)>spot
     %In submitted subroutine call last
     outflag=4;
    end
   elseif length(find(strcmpi(funstrwords{i}{temp2},intrinsics)))>0
    [howmany,subscripts,centercomma,parens]=hassubscript(i,temp2);
    if parens(2)>spot
     %In regular converted function (not make_*) last
     outflag=5;
    end
   elseif length(find(strcmpi(funstrwords{i}{temp2},funwords)))>0
    [howmany,subscripts,centercomma,parens]=hassubscript(i,temp2);
    if parens(2)>spot
     %In submitted subroutine call last
     outflag=6;
    end
   end
  else %no word immediately preceeds this parenthesis, but a mathematical ( or [ encompasses it
   if funstr{i}(j)=='[' | funstr{i}(j)=='('
    j2=findrights(j,funstr{i});
   end
   %outflag=0;howmany=1;subscripts{1}=funstr{i}(j+1:j2-1);centercomma=[];parens=[j j2];
   outflag=0;howmany=1;subscripts{1}=funstr{i};centercomma=[];parens=[1 length(funstr{i})];
  end
 elseif strcmp(funstr{i}(j),'[')
  [dummy,howmany,subscripts,centercomma,parens]=inbracket(i,spot,funstr);
  temp3=findrights(j,funstr{i});
  if temp3>spot
   %In bracket last
   outflag=1;
  end
 end
else
 outflag=0;howmany=1;subscripts{1}=funstr{i};centercomma=[];parens=[1 length(funstr{i})];
end
%funstr{i},funstr{i}(1:spot),kb
