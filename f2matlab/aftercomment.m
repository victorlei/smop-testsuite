function out=aftercomment(funstr,row,column)

out=0;
if ~any(funstr{row}=='!')
 return
else
 temp=findstr(funstr{row},'!');temp=temp(1);
 if temp<=column
  out=1;
 end
end