function [count,whichone]=typeofvar(str,inoutother,cw);
thisstr=str;
if ~isempty(findstr(str,'_f')), thisstr=str(1:(findstr(str,'_f')-1)); end
if length(find(strcmp(thisstr,inoutother{1})))>0, whichone=1; end
if length(find(strcmp(thisstr,inoutother{2})))>0, whichone=2; end
if length(find(strcmp(thisstr,inoutother{3})))>0, whichone=3; end
if ~isreal(getfield(cw,thisstr))
 count=1;
else
 count=0;
end
