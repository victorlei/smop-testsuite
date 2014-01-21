function outflag=inmlcall(i,spot,funstr,funstrwords,funstrwords_b);
outflag=0;found=0;place=0;
for j=spot:-1:1
 if place==0
  if strcmp(funstr{i}(j),'(')
   found=found+1;
  elseif strcmp(funstr{i}(j),')')
   found=found-1;
  end
  if found==1
   place=j;
  end
 end
end
temp=find(funstrwords_b{i}(funstrwords_b{i}<place));
if ~isempty(temp)
 temp=temp(length(temp));
 if strcmp(funstrwords{i}{temp},'mlcall')
  outflag=1;
 end
end
