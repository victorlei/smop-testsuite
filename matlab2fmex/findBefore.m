function [out,outLoc]=findBefore(strIn,loc,strToFind)
%returns character and location for preceding character,
%or for preceding strToFind
%  if none, then out='', outLoc=-1

out='';
outLoc=-1;
if loc<2
 return
end

if nargin<3
 fthere=find(~isspace(strIn));
 fthereBefore=fthere(fthere<loc);
 if ~isempty(fthereBefore)
  out=strIn(fthereBefore(end));
  outLoc=fthereBefore(end);
 end % if ~isempty(fthereBefore)
else
 fthere=find(strIn==strToFind);
 fthereBefore=fthere(fthere<loc);
 if ~isempty(fthereBefore)
  out=strIn(fthereBefore(end));
  outLoc=fthereBefore(end);
 end % if ~isempty(fthereBefore)
end

