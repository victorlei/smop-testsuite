function out=findNext(loc,str,funstr,direction)
% returns -1 if can't find what you ask

if nargin<4, direction=1; end

if direction==1
 found=strfind(funstr,str);
 if isempty(found)
  out=-1;
  return
 end
 found=found(found>loc);
 if isempty(found)
  out=-1;
  return
 end
 
 out=found(1);
 
else
 found=strfind(funstr,str);
 if isempty(found)
  out=-1;
  return
 end
 found=found(found<loc);
 if isempty(found)
  out=-1;
  return
 end
 
 out=found(end);
 
 
end