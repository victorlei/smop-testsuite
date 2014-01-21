function out=gethighesttemp(funstr,typestr);
out=0;
if strcmp(typestr,'r')
 spots=findstr('mxTR',funstr);
 for i=1:length(spots)
  if str2num(funstr(spots(i)+4))>out
   out=str2num(funstr(spots(i)+4));
  end
 end
elseif strcmp(typestr,'c')
 spots=findstr('mxTC',funstr);
 for i=1:length(spots)
  if str2num(funstr(spots(i)+4))>out
   out=str2num(funstr(spots(i)+4));
  end
 end
elseif strcmp(typestr,'i')
 spots=findstr('mxTI',funstr);
 for i=1:length(spots)
  if str2num(funstr(spots(i)+4))>out
   out=str2num(funstr(spots(i)+4));
  end
 end
end
