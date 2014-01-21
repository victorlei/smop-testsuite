function [howmany,subscripts,centercomma,parens]=hasoutput(i,whichword)
declare_globals
howmany=0;subscripts=[];centercomma=[];parens=[];out=0;
temp1=find(~isspace(funstr{i}));
temp1=temp1(temp1<funstrwords_b{i}(whichword));
if isempty(temp1)
 howmany=0;
else
 [outflag]=inbracket(i,funstrwords_b{i}(whichword),funstr);
 if ((strcmp(funstr{i}(temp1(length(temp1))),'='))&(strcmp(funstr{i}(temp1(length(temp1))-1),']')))
  parens(2)=temp1(length(temp1))-1;
  parens(1)=findlefts(parens(2),funstr{i});
  temp=parens(1);
  left=1;right=0;found=0;last=temp;
  for j=temp+1:parens(2)
   if ((strcmp(funstr{i}(j),'('))|(strcmp(funstr{i}(j),'['))), left=left+1; end
   if ((strcmp(funstr{i}(j),')'))|(strcmp(funstr{i}(j),']'))), right=right+1; end
   if ((left-right==1)&(strcmp(funstr{i}(j),',')))
    howmany=howmany+1;
    subscripts{howmany}=funstr{i}(last+1:j-1);
    centercomma(howmany)=j;
    last=j;
   end
   if ((left-right==0)&(strcmp(funstr{i}(j),']')))
    howmany=howmany+1;
    subscripts{howmany}=funstr{i}(last+1:j-1);
   end
  end
 else
  howmany=1;
 end
end
